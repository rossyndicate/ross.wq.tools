# Regression tests for the DO burial detection in add_burial_flag().
#
# `fixtures/burial/riverbluffs_2023.rds` is real published data: riverbluffs
# 2023 DO and Temperature from the HydroShare release (CSU_ROSS_2023_raw.parquet,
# `value_post_cal_corr`), with `site_visit` marking the site visit starts the
# reference notebook found (auto/user "site visit" flags and field-note visits).
# `fixtures/burial/riverbluffs_2023_stats.rds` holds the notebook's site-season
# DO stats for the same slice (median, sd, 1st percentile, excluding technician
# burial flags), named like make_threshold_table() output.
#
# `fixtures/golden/golden_burial_riverbluffs_2023.rds` was captured once by
# running the reference implementation, the search winner of
# poudre_sonde_network/scratch/misc/burial_parameter_search_jpd_09-23-26.Rmd
# (`build_base()` then `run_config(winner, base)`), on that slice. The package
# port must reproduce its scores and flags.

fixture <- readRDS(test_path("fixtures", "burial", "riverbluffs_2023.rds"))
stats_tbl <- readRDS(test_path("fixtures", "burial", "riverbluffs_2023_stats.rds"))
golden <- readRDS(test_path("fixtures", "golden", "golden_burial_riverbluffs_2023.rds"))

# Site-level dataframe shaped like the pipeline's at add_burial_flag() time
site_df <- fixture %>%
  dplyr::mutate(DT_join = as.character(DT_round),
                flag = ifelse(site_visit, "site visit", NA_character_)) %>%
  dplyr::select(-site_visit) %>%
  split(f = .$parameter) %>%
  purrr::map(~ .x %>% dplyr::arrange(DT_round) %>% generate_summary_statistics()) %>%
  dplyr::bind_rows()

# Default tuning arguments of add_burial_flag(), as burial_score() takes them
default_params <- function() {
  args <- formals(add_burial_flag)
  keep <- setdiff(names(args), c("df", "threshold_table", "do_parameter",
                                 "visit_flag", "flag_score"))
  lapply(args[keep], eval)
}

test_that("burial scores match the reference implementation", {
  do <- site_df %>%
    dplyr::filter(parameter == "DO") %>%
    dplyr::arrange(DT_round)
  stats <- burial_stats(stats_tbl, "riverbluffs", "DO", unique(do$season))
  scores <- burial_score(do, stats, "site visit", default_params())

  expect_equal(do$DT_round, golden$DT_round)
  expect_equal(scores$score, golden$prob, tolerance = 1e-9)
})

test_that("DO burial flags match the reference and propagate to other parameters", {
  out <- add_burial_flag(site_df, stats_tbl)

  do_out <- out %>%
    dplyr::filter(parameter == "DO") %>%
    dplyr::arrange(DT_round)
  expect_equal(grepl("Possible burial", do_out$flag), golden$flag)

  burial_times <- do_out$DT_join[golden$flag]
  temp_out <- out %>% dplyr::filter(parameter == "Temperature")
  expect_equal(grepl("Possible burial", temp_out$flag),
               temp_out$DT_join %in% burial_times)

  expect_equal(nrow(out), nrow(site_df))
  expect_setequal(names(out), names(site_df))
})

test_that("a site without DO is returned unchanged", {
  temp_only <- site_df %>% dplyr::filter(parameter == "Temperature")
  expect_identical(add_burial_flag(temp_only, stats_tbl), temp_only)
})

test_that("missing inputs give informative errors", {
  expect_error(add_burial_flag(site_df, dplyr::select(stats_tbl, -t_median)),
               "make_threshold_table")
  expect_error(add_burial_flag(dplyr::select(site_df, -back1), stats_tbl),
               "generate_summary_statistics")
})
