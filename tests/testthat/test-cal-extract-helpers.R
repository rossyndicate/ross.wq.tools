# Regression tests for the rvest -> xml2 migration (issue #26).
#
# `tests/fixtures/golden/*.rds` were captured once, before the migration,
# by running the *unmodified* rvest-based cal_extract_*() functions against
# the real calibration report fixtures in `tests/fixtures/calibration_reports/`
# (field files + benchtop_calibrations, sourced from poudre_sonde_network).
# These tests confirm the xml2-based functions now in R/ produce identical
# output, so the suite never needs rvest installed to run.

fixtures_dir <- test_path("fixtures", "calibration_reports")
golden_dir <- test_path("fixtures", "golden")

# Mirrors the per-div sensor identification/dispatch that
# cal_extract_markup_data() does internally, so we can validate the sensor
# extractors directly against files (like the benchtop fixture) that don't
# follow the site_YYYYMMDD_HHMM filename convention cal_extract_markup_data()
# requires.
extract_divs <- function(path) {
  html_markup <- xml2::read_html(path)
  html_divs <- html_markup %>% xml2::xml_find_all(".//div")

  div_results <- html_divs %>%
    purrr::map(function(div) {
      sensor <- div %>%
        xml2::xml_find_all(".//table") %>%
        xml_table_list() %>%
        purrr::pluck(1) %>%
        tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
        dplyr::mutate(sensor = janitor::make_clean_names(sensor)) %>%
        dplyr::pull(sensor)

      if (sensor %in% c("chlorophyll_a", "conductivity", "fdom", "p_h_orp", "pressure", "rdo", "turbidity")) {
        switch(
          EXPR = sensor,
          "conductivity" = cal_extract_conductivity_data(div),
          "rdo" = cal_extract_rdo_data(div),
          "p_h_orp" = cal_extract_ph_orp_data(div),
          "pressure" = cal_extract_pressure_data(div),
          "turbidity" = cal_extract_turbidity_data(div),
          "fdom" = cal_extract_fdom_data(div),
          "chlorophyll_a" = cal_extract_chla_data(div)
        )
      } else {
        NULL
      }
    })

  names(div_results) <- paste0("div_", seq_along(div_results))
  purrr::compact(div_results)
}

# cal_extract_markup_data()'s internal split(f = list(site, sensor), ...)
# orders its output by locale string collation, and testthat forces
# LC_COLLATE="C" for reproducibility -- a different collation than the
# en_US.UTF-8 session the golden RDS was captured under. That changes list
# *order* for mixed-case names (e.g. "pH" vs "Pressure") without changing
# content, so sort recursively by name before comparing.
sort_by_names_recursive <- function(x) {
  if (is.list(x) && !is.null(names(x))) {
    x <- x[order(names(x))]
    x <- lapply(x, sort_by_names_recursive)
  }
  x
}

test_that("cal_extract_markup_data() matches pre-migration rvest output", {
  result <- cal_extract_markup_data(field_cal_dir = fixtures_dir)
  golden <- readRDS(file.path(golden_dir, "golden_cal_extract_markup_data.rds"))
  expect_equal(sort_by_names_recursive(result), sort_by_names_recursive(golden))
})

field_files <- list.files(fixtures_dir, pattern = "\\.html$", full.names = TRUE)
benchtop_files <- list.files(file.path(fixtures_dir, "benchtop"), pattern = "\\.html$", full.names = TRUE)

for (f in c(field_files, benchtop_files)) {
  test_that(paste0("sensor div extraction matches pre-migration rvest output: ", basename(f)), {
    result <- extract_divs(f)
    golden_name <- paste0("golden_divs_", tools::file_path_sans_ext(basename(f)), ".rds")
    golden <- readRDS(file.path(golden_dir, golden_name))
    expect_equal(result, golden)
  })
}
