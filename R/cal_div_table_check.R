#' @title Check whether a captioned calibration table exists and has expected columns
#' @export
#'
#' @description
#' Validates a single named table within a sensor div's table list before
#' that table is used to extract calibration coefficients. `table_list` is
#' the kind of structure produced by `xml_table_list()` in
#' `cal_extract_helpers.R` and named via `janitor::make_clean_names()` on
#' each table's `<caption>` text (e.g. `"sensor"`, `"calibration_details"`,
#' `"slope_and_offset_1"`, `"orp"`). This function confirms `table_name` is
#' actually present, has at least one row, and that pivoting it wide
#' (`names_from = X1, values_from = X2` -- the same convention every table
#' in these reports is consumed with) produces all of `col_names`.
#'
#' Used throughout the `cal_extract_*_data()` functions (`cal_extract_helpers.R`)
#' to branch around calibration data that doesn't exist for a given sensor.
#' This happens for two real reasons: a sensor with no calibration performed
#' (e.g. "Factory Defaults" -- its div only ever has the "sensor" metadata
#' table and nothing else), or a calibration variant that omits certain rows
#' (e.g. a conductivity calibration with a cell constant but no offset row).
#'
#' @param table_list A named list of data frames, one per captioned table in
#'   a sensor div (e.g. `list(sensor = ..., calibration_details = ...)`).
#' @param table_name Character scalar. The name (caption) of the table to
#'   look for within `table_list`.
#' @param col_names Character vector. Column names expected to exist after
#'   pivoting `table_list[[table_name]]` wide (`names_from = X1,
#'   values_from = X2`).
#'
#' @return `TRUE` if `table_name` exists in `table_list`, is non-empty, and
#'   pivoting it wide produces every name in `col_names`; `FALSE` otherwise
#'   -- including the case where `table_list` has only one table (just the
#'   "sensor" metadata table, meaning no calibration data exists at all for
#'   that sensor).
#'
#' @seealso [cal_extract_chla_data()]
#' @seealso [cal_extract_conductivity_data()]
cal_div_table_check <- function(table_list, table_name, col_names) {

  # Check if initial function conditions are met ----
  # table list conditions
  if(!is.list(table_list)) stop("table_list argument must be a LIST with named indices.")
  if(is.null(names(table_list))) stop("table_list argument must be a list with NAMED indices.")
  # table name conditions
  if(!rlang::is_scalar_character(table_name)) stop("{{table_name}} argument must be a SCALAR CHARACTER.")
  # Column names conditions
  if(!is.character(col_names)) stop("{{col_names}} argument must be a CHARACTER data type.")

  # Check if table is in table list ----
  # If there is only one table cal div table check fails
  # There is only the parameter metadata in this div
  if(length(table_list) <= 1) return(FALSE)
  # Check if table name is in table list
  if(!purrr::pluck_exists(table_list[[table_name]])) return(FALSE)

  # Check if the table has any data ----
  table <- table_list[[table_name]]
  if(nrow(table) == 0) return(FALSE)

  # Check if column names are in the table ----
  table <- table %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)
  col_name_check <- all(col_names %in% names(table))

  # If both table check and column name check pass return TRUE, else return FALSE
  return(col_name_check)

}
