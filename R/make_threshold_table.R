#' @title Generate Threshold Table for QAQC
#' @export
#'
#' @description
#' A function designed to generate a threshold table for QAQC. This table
#' contains the thresholds for the mean, slope_behind, and standard deviation
#' of the mean for each site and season.
#'
#' @param df A data frame of cleaned, verified data
#'
#' @return A data frame with one row per season and the columns:
#' - `season`, `site`, `parameter`
#' - `t_mean01`, `t_mean99`: 1st and 99th percentiles of `mean`
#' - `t_slope_behind_01`, `t_slope_behind_99`: 1st percentile of negative and
#'   99th percentile of positive `slope_behind` values
#' - `t_sd_0199`: standard deviation of `mean` within the 1st-99th percentile range
#' - `t_median`: median of `mean`
#' - `t_sd`: standard deviation of `mean` across the full data set (unlike `t_sd_0199`)
#'
#' `t_mean01`, `t_median`, and `t_sd` are used by `add_burial_flag()` to build
#' the seasonal band score for DO.

make_threshold_table <- function(df){

  slope_down <- df %>%
    # Get threshold for negative slope data
    dplyr::filter(slope_behind < 0) %>%
    dplyr::group_by(season) %>%
    dplyr::summarize(f_slope_behind_01 = stats::quantile(slope_behind, 0.01, na.rm = TRUE))

  slope_up <- df %>%
    # Get threshold for positive slope data
    dplyr::filter(slope_behind > 0) %>%
    dplyr::group_by(season) %>%
    dplyr::summarize(f_slope_behind_99 = stats::quantile(slope_behind, 0.99, na.rm = TRUE))

  good_data_stats <- df %>%
    dplyr::group_by(season) %>%
    # join our slope data thresholds:
    dplyr::left_join(slope_up, by = "season") %>%
    dplyr::left_join(slope_down, by = "season") %>%
    # develop other thresholds across all data
    dplyr::mutate(f01 = stats::quantile(mean, 0.01, na.rm = TRUE),
                  f99 = stats::quantile(mean, 0.99, na.rm = TRUE),
                  # Center and spread of the full "good" data set (used by add_burial_flag())
                  f_median = stats::median(mean, na.rm = TRUE),
                  f_sd = stats::sd(mean, na.rm = TRUE)) %>%
    # THEN, GET STANDARD DEVIATION OF ONLYYYY VALUES WITHIN THE 1-99th PERCENTILE OF THAT GOOD DATA:
    dplyr::filter(mean > f01 & mean < f99) %>%
    # SD is the ONLY statistic that uses this winnowed-down data set in its development.
    # All else use the full, "good" data set.
    dplyr::summarize(site = paste0(unique(site)),
                     parameter = paste0(unique(parameter)),
                     t_mean01 = as.numeric(paste0(unique(f01))),
                     t_mean99 = as.numeric(paste0(unique(f99))),
                     t_slope_behind_01 = as.numeric(paste0(unique(f_slope_behind_01))),
                     t_slope_behind_99 = as.numeric(paste0(unique(f_slope_behind_99))),
                     # This stat is {{currently}} useless, but keeping for now:
                     t_sd_0199 = stats::sd(mean, na.rm = T),
                     t_median = unique(f_median),
                     t_sd = unique(f_sd))

  return(good_data_stats)

}
