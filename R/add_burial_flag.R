#' @title Detect sonde burial from DO and flag all parameters within the sonde
#' @export
#'
#' @description
#' Detects likely sonde burial from the dissolved oxygen (DO) record and flags every
#' parameter at the site during those periods. When a sonde is buried in sediment,
#' all measurements are affected, but the DO sensor gives the clearest signal: DO
#' gets noisy (spikes in both directions) and drifts low.
#'
#' Detection runs in three stages on the DO rows of the site, within groups that
#' are split at each site visit so no window crosses a cleaning:
#'
#' 1. **Rounds.** Points are "pieces" when they spike by at least
#'    `spike_threshold` from their neighbors or sit at or below `low_floor`. A
#'    point is flagged when the fraction of pieces in a trailing, centered, or
#'    leading time window reaches that window's fraction (and the window has
#'    enough data, `min_coverage`). Later rounds re-run the windows on the
#'    previous round's flags, with each fraction raised by 0.1 per round.
#' 2. **Band score.** Each DO value is mapped to a score between 0 and 1 by linear
#'    interpolation through fixed low-DO anchors (`fixed_anchors`/`fixed_scores`)
#'    and site-season anchors at the 1st percentile, median minus one sd, and
#'    median of DO (from `threshold_table`). Anchors are only kept while DO rises
#'    and the score falls. Points flagged by the rounds get `round_bonus` added.
#' 3. **Backward pass.** Walking back in time, each point takes the larger of its
#'    own score and a Gaussian-weighted, decayed average of the scores in the
#'    `lookahead_hours` after it, so a burial's score carries back to its onset.
#'
#' Points whose final score is above `flag_score` get "Possible burial", which is
#' then copied to every parameter at the same `DT_join`.
#'
#' The defaults are the best configuration from a staged parameter search over
#' 2023-2025 Poudre Sonde Network data (pooled F1 0.688 against technician
#' burial flags; see `poudre_sonde_network/scratch/misc/burial_parameter_search_jpd_09-23-26.Rmd`).
#' The thresholds are in mg/L, so DO must be in mg/L.
#'
#' Unlike most flagging functions, this function operates on site-level dataframes
#' containing multiple parameters rather than individual site-parameter combinations.
#'
#' TODO: `combine_datasets()` currently carries 24 hours of history into each
#' incremental run, but the trailing window is 30 hours. Until the history is
#' raised to 30 hours, burials that started before the 24-hour cutoff can be missed
#' at the start of an incremental batch.
#'
#' @param df A dataframe containing all parameters for a single site. The DO rows
#' must include:
#' - `DT_round`: Timestamp of each observation
#' - `DT_join`: Character timestamp used for joining measurements across parameters
#' - `mean`: DO value in mg/L
#' - `back1`, `front1`, `season`: From `generate_summary_statistics()`
#' - `flag`: Existing quality flags, with `visit_flag` at site visits (from
#'   `add_field_flag()`)
#' @param threshold_table A seasonal threshold table from `make_threshold_table()`,
#' with columns `site`, `parameter`, `season`, `t_mean01`, `t_median`, and `t_sd`.
#' Only the `do_parameter` rows are used. A site-season with no stats falls back to
#' the median of each stat across all sites for that season.
#' @param do_parameter Name of the DO parameter in `df$parameter` and in
#' `threshold_table$parameter`.
#' @param visit_flag Flag marking site visits. Detection windows are split at the
#' start of each run of points carrying this flag.
#' @param spike_threshold Minimum jump (mg/L) from the neighboring values for a
#' point to count as a spike.
#' @param low_floor DO (mg/L) at or below which a point counts as a piece.
#' @param trailing_hours,trailing_frac Length and piece fraction of the trailing window.
#' @param center_hours,center_frac Half-width and piece fraction of the centered window.
#' @param leading_hours,leading_frac Length and piece fraction of the leading window.
#' Set any fraction to `NA` to turn that window off.
#' @param min_coverage Minimum fraction of each window's length that must be covered
#' by data for the window to count.
#' @param rounds Number of window rounds.
#' @param fixed_anchors,fixed_scores DO values (mg/L) and scores of the fixed
#' low-DO anchors of the band score curve.
#' @param p01_score,sd_score,median_score Scores at the site-season 1st percentile,
#' median minus one sd, and median of DO.
#' @param round_bonus Score added to points flagged by the rounds (capped at 1).
#' @param lookahead_hours How far ahead the backward pass looks.
#' @param weight_sd_minutes Standard deviation of the backward pass's Gaussian weights.
#' @param decay Score decay per 15 minutes of elapsed time in the backward pass.
#' @param flag_score Points with a final score above this get "Possible burial".
#'
#' @return A dataframe with the same structure as the input, but with the `flag`
#' column updated to include "Possible burial" for all parameters when burial is
#' detected by the DO sensor.
#'
#' @examples
#' # Examples are temporarily disabled
#' @seealso [add_flag()]
#' @seealso [generate_summary_statistics()]
#' @seealso [make_threshold_table()]
#' @seealso [add_field_flag()]

add_burial_flag <- function(df, threshold_table,
                            do_parameter = "DO",
                            visit_flag = "site visit",
                            spike_threshold = 0.75,
                            low_floor = 4,
                            trailing_hours = 30, trailing_frac = 0.25,
                            center_hours = 7.5, center_frac = 0.4,
                            leading_hours = 9, leading_frac = 0.5,
                            min_coverage = 0.7,
                            rounds = 3,
                            fixed_anchors = c(0, 0.5, 1.5, 2.5, 3.5),
                            fixed_scores = c(1, 0.97, 0.875, 0.7, 0.7),
                            p01_score = 0.25, sd_score = 0.2, median_score = 0.05,
                            round_bonus = 0.5,
                            lookahead_hours = 3,
                            weight_sd_minutes = 60,
                            decay = 0.99,
                            flag_score = 0.6) {

  # Nothing to detect without DO
  if (!any(df$parameter %in% do_parameter)) return(df)

  site_name <- unique(stats::na.omit(df$site))
  if (length(site_name) != 1) {
    stop("add_burial_flag() expects a dataframe for a single site.")
  }

  required_cols <- c("DT_round", "DT_join", "mean", "back1", "front1", "season", "flag")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("add_burial_flag() needs these columns: ", paste(missing_cols, collapse = ", "),
         ". back1, front1, and season come from generate_summary_statistics().")
  }

  params <- list(spike_threshold = spike_threshold, low_floor = low_floor,
                 trailing_hours = trailing_hours, trailing_frac = trailing_frac,
                 center_hours = center_hours, center_frac = center_frac,
                 leading_hours = leading_hours, leading_frac = leading_frac,
                 min_coverage = min_coverage, rounds = rounds,
                 fixed_anchors = fixed_anchors, fixed_scores = fixed_scores,
                 p01_score = p01_score, sd_score = sd_score, median_score = median_score,
                 round_bonus = round_bonus, lookahead_hours = lookahead_hours,
                 weight_sd_minutes = weight_sd_minutes, decay = decay)

  # DO rows in time order
  do <- df %>%
    dplyr::filter(parameter %in% !!do_parameter) %>%
    dplyr::arrange(DT_round)

  stats_by_season <- burial_stats(threshold_table, site_name, do_parameter,
                                  active_seasons = unique(do$season[!is.na(do$mean)]))

  burial <- burial_score(do, stats_by_season, visit_flag, params)$score > flag_score
  burial <- !is.na(burial) & burial

  # Flag DO, then copy the flag to every other parameter at the same timestamp
  do <- add_flag(do, burial, "Possible burial")

  do_flags <- do %>%
    dplyr::select(DT_join, do_flag = flag)

  do_checked <- df %>%
    # Process all parameters except DO first to avoid duplicate flagging
    dplyr::filter(!parameter %in% !!do_parameter) %>%
    # Join with DO flags using timestamp to align readings across parameters
    dplyr::left_join(., do_flags, by = "DT_join") %>%
    # Add burial flag to all parameters at timestamps where DO indicates burial
    add_flag(grepl("Possible burial", do_flag), "Possible burial") %>%
    # Remove the temporary DO flag column we added
    dplyr::select(-do_flag) %>%
    # Add back the DO data, now flagged
    dplyr::bind_rows(do) %>%
    # Remove any duplicate rows that might have been created
    dplyr::distinct()

  return(do_checked)

}

# <<< Internal helpers for add_burial_flag() >>> ----
# These work on plain vectors from the time-ordered DO rows of one site.

#' Score DO points for burial
#'
#' @param do DO rows of one site, ordered by `DT_round`.
#' @param stats Output of `burial_stats()`.
#' @param visit_flag Flag marking site visits.
#' @param params Named list of the `add_burial_flag()` tuning arguments.
#' @return A list with `rounds` (logical, flagged by the window rounds), `band`
#' (band score), and `score` (final score after the backward pass).
#' @noRd
burial_score <- function(do, stats, visit_flag, params) {
  n <- nrow(do)
  t <- as.numeric(do$DT_round)

  # Neighbors only count when they are exactly one interval away, so values
  # across a data gap (e.g. an off-season redeployment) are not compared
  dt <- diff(t)
  interval <- if (any(dt > 0)) min(dt[dt > 0]) else NA_real_
  prev_ok <- c(FALSE, !is.na(dt) & dt == interval)
  next_ok <- c(!is.na(dt) & dt == interval, FALSE)
  back1 <- ifelse(prev_ok, do$back1, NA_real_)
  front1 <- ifelse(next_ok, do$front1, NA_real_)

  # Visit groups: a new group starts at the first point of each run of visit
  # flags. Offsetting time by group keeps every window inside its group.
  hit <- burial_has_token(do$flag, visit_flag)
  start <- hit & !c(FALSE, hit[-n])
  gid <- cumsum(start)
  key <- t + gid * 1e10
  same_prev <- c(FALSE, gid[-1] == gid[-n])
  same_next <- c(gid[-1] == gid[-n], FALSE)

  rounds <- burial_rounds(do$mean, back1, front1, key, same_prev, same_next, params)
  band <- burial_band_score(do$mean, do$season, stats, params)

  p <- ifelse(rounds, pmin(1, band + params$round_bonus), band)
  score <- burial_backward_pass(p, key,
                                fwd_secs = params$lookahead_hours * 3600,
                                sd_secs = params$weight_sd_minutes * 60,
                                decay = params$decay)

  list(rounds = rounds, band = band, score = score)
}

#' Seasonal DO stats for one site, with an across-site fallback
#'
#' @return A dataframe with `season`, `ctr` (median), `spr` (sd), and `low`
#' (1st percentile).
#' @noRd
burial_stats <- function(threshold_table, site_name, do_parameter, active_seasons) {
  required_cols <- c("site", "parameter", "season", "t_median", "t_sd", "t_mean01")
  missing_cols <- setdiff(required_cols, names(threshold_table))
  if (length(missing_cols) > 0) {
    stop("threshold_table needs these columns: ", paste(missing_cols, collapse = ", "),
         ". Regenerate it with make_threshold_table().")
  }

  tbl <- as.data.frame(threshold_table)
  tbl <- tbl[!is.na(tbl$parameter) & tbl$parameter == do_parameter, required_cols]
  if (nrow(tbl) == 0) {
    stop("threshold_table has no rows for parameter '", do_parameter, "'.")
  }

  seasons <- union(unique(stats::na.omit(tbl$season)), stats::na.omit(active_seasons))
  own <- tbl[!is.na(tbl$site) & tbl$site == site_name, ]
  idx <- match(seasons, own$season)
  out <- data.frame(season = seasons,
                    ctr = own$t_median[idx],
                    spr = own$t_sd[idx],
                    low = own$t_mean01[idx])

  # Fall back to the median of each stat across sites for that season
  fallback <- is.na(out$ctr) | is.na(out$spr) | is.na(out$low)
  for (k in which(fallback)) {
    in_season <- tbl[tbl$season %in% out$season[k], ]
    out$ctr[k] <- stats::median(in_season$t_median, na.rm = TRUE)
    out$spr[k] <- stats::median(in_season$t_sd, na.rm = TRUE)
    out$low[k] <- stats::median(in_season$t_mean01, na.rm = TRUE)
  }

  noted <- out$season[fallback & out$season %in% active_seasons]
  if (length(noted) > 0) {
    message("add_burial_flag(): no ", do_parameter, " stats for ", site_name, " in ",
            paste(noted, collapse = ", "), "; using the median across sites.")
  }

  out
}

#' Window rounds: spikes and the low floor, then the windowed passes
#' @return Logical vector, TRUE where the rounds flag a point.
#' @noRd
burial_rounds <- function(d, back1, front1, key, same_prev, same_next, params) {
  n <- length(d)
  i <- seq_len(n)
  s <- params$spike_threshold

  # Spikes: down or up from both neighbors, or a big jump from one neighbor
  # (that neighbor not itself a spike)
  dn <- burial_true(back1 - d >= s & front1 - d >= s)
  up <- burial_true(d - back1 >= s & d - front1 >= s)
  raw <- dn | up
  raw_prev <- c(FALSE, raw[-n]) & same_prev
  raw_next <- c(raw[-1], FALSE) & same_next
  big <- burial_true(abs(d - back1) >= s & !raw_prev) |
    burial_true(abs(d - front1) >= s & !raw_next)
  spike <- raw | big

  floor <- burial_true(d <= params$low_floor)
  piece <- spike | floor

  # Window indices, computed once and reused by every round
  mc <- params$min_coverage
  w_r <- params$trailing_hours * 3600
  w_c <- params$center_hours * 3600
  w_l <- params$leading_hours * 3600
  lo_r <- burial_win_before(key, w_r)
  lo_c <- burial_win_before(key, w_c)
  hi_c <- burial_win_after(key, w_c)
  hi_l <- burial_win_after(key, w_l)

  passes <- function(x, add) {
    out <- rep(FALSE, n)
    if (!is.na(params$trailing_frac)) {
      out <- out | (burial_roll_frac(x, lo_r, i) >= min(1, params$trailing_frac + add) &
                      key[i] - key[lo_r] >= mc * w_r)
    }
    if (!is.na(params$center_frac)) {
      out <- out | (burial_roll_frac(x, lo_c, hi_c) >= min(1, params$center_frac + add) &
                      key[i] - key[lo_c] >= mc * w_c & key[hi_c] - key[i] >= mc * w_c)
    }
    if (!is.na(params$leading_frac)) {
      out <- out | (burial_roll_frac(x, i, hi_l) >= min(1, params$leading_frac + add) &
                      key[hi_l] - key[i] >= mc * w_l)
    }
    out
  }

  flagged <- passes(piece, 0)
  # Later rounds run on the previous output, with each fraction raised 0.1
  if (params$rounds > 1) {
    for (r in 2:params$rounds) flagged <- flagged | passes(flagged, 0.1 * (r - 1))
  }

  flagged
}

#' Band score: DO mapped to 0-1 through fixed and site-season anchors
#' @noRd
burial_band_score <- function(d, season, stats, params) {
  out <- rep(NA_real_, length(d))
  for (k in seq_len(nrow(stats))) {
    if (anyNA(c(stats$ctr[k], stats$spr[k], stats$low[k]))) next
    rows <- which(season %in% stats$season[k] & !is.na(d))
    if (length(rows) == 0) next
    out[rows] <- burial_curve(stats$ctr[k], stats$spr[k], stats$low[k], params)(d[rows])
  }
  out
}

#' Build the band score curve for one site-season
#'
#' Anchors are kept only while DO rises and the score falls, so a site-season
#' whose 1st percentile sits under the fixed anchors just drops that anchor.
#' @noRd
burial_curve <- function(ctr, spr, low, params) {
  x <- c(params$fixed_anchors, low, ctr - spr, ctr)
  y <- c(params$fixed_scores, params$p01_score, params$sd_score, params$median_score)
  keep <- rep(FALSE, length(x))
  keep[1] <- TRUE
  lx <- x[1]
  ly <- y[1]
  for (k in 2:length(x)) {
    if (x[k] > lx && y[k] < ly) {
      keep[k] <- TRUE
      lx <- x[k]
      ly <- y[k]
    }
  }
  x <- x[keep]
  y <- y[keep]
  function(v) stats::approx(x, y, xout = v, rule = 2)$y
}

#' Backward decayed pass
#'
#' Walking back in time, each point takes the larger of its own score and the
#' Gaussian-weighted average of the decayed scores in the `fwd_secs` after it.
#' Missing scores stay missing and pass nothing on.
#' @noRd
burial_backward_pass <- function(p, key, fwd_secs, sd_secs, decay, step = 900) {
  score <- p
  # Last index strictly within the look-ahead
  hi <- findInterval(key + fwd_secs, key, left.open = TRUE)
  for (i in rev(seq_along(p))) {
    if (is.na(p[i]) || hi[i] <= i) next
    j <- (i + 1L):hi[i]
    v <- score[j]
    ok <- !is.na(v)
    if (!any(ok)) next
    dt <- key[j[ok]] - key[i]
    w <- exp(-dt * dt / (2 * sd_secs * sd_secs))
    carried <- sum(w * v[ok] * decay^(dt / step)) / sum(w)
    score[i] <- max(p[i], carried)
  }
  score
}

#' First index of the window from t - secs to t
#' @noRd
burial_win_before <- function(key, secs) {
  findInterval(key - secs, key, left.open = TRUE) + 1L
}

#' Last index of the window from t to t + secs
#' @noRd
burial_win_after <- function(key, secs) {
  findInterval(key + secs, key)
}

#' Fraction of TRUE values in x over rows lo..hi
#' @noRd
burial_roll_frac <- function(x, lo, hi) {
  cs <- c(0, cumsum(x))
  (cs[hi + 1] - cs[lo]) / (hi - lo + 1)
}

#' TRUE where x is TRUE and not NA
#' @noRd
burial_true <- function(x) !is.na(x) & x

#' Does each semicolon-delimited flag string contain `token` exactly?
#' @noRd
burial_has_token <- function(flag, token) {
  tokens <- strsplit(ifelse(is.na(flag), "", as.character(flag)), ";", fixed = TRUE)
  vapply(tokens, function(x) any(trimws(x) == token), logical(1))
}
