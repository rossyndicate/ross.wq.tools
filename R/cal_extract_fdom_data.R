#' @title Extract FDOM Data from Calibration Markup
#' @export
cal_extract_fdom_data <- function(div) {
  # Instantiate a named div tables list ----
  div_tables <- div %>%
    html_elements("table") %>%
    discard(\(x) length(html_elements(x, "caption")) == 0) %>% # remove tables with no captions (empty placeholder tables)
    html_table() %>% # extract the tabtle data
    set_names( # use the table captions to name the tables in the div_tables list
      {div %>%
          html_elements("caption") %>%
          html_text() %>%
          janitor::make_clean_names()
      }
    )

  # Initial div data availability check ----
  div_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "sensor",
    col_names = c("sensor", "serial_number", "last_calibrated")
  )

  # If there is only 1 table in the div exit early and return metadata ...
  if (!div_check) {
    div_metadata <- div_tables[["sensor"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      dplyr::rename(sensor_serial = serial_number) %>%
      dplyr::mutate(
        sensor = janitor::make_clean_names(sensor),
        calibration_coefs = NULL,
        driftr_input = NULL
      )
    return(div_metadata)
  }
  # Else, continue with the function...

  # FDOM div metadata ----
  div_metadata <- div_tables[["sensor"]] %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)  %>%
    dplyr::rename(sensor_serial = serial_number) %>%
    dplyr::mutate(sensor = janitor::make_clean_names(sensor))

  # In-situ calibration coefficients ----
  # Check if the table we expect for the calibration coefficients exists in the structure we expect...
  cal_coef_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "slope_and_offset_1",
    col_names = c("slope", "offset")
  )

  if(cal_coef_check) {
    calibration_coefs <- div_tables[["slope_and_offset_1"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      separate_wider_delim(offset, delim = " ", names = c("offset", "units")) %>%
      dplyr::mutate(dplyr::across(c("slope", "offset"), ~as.numeric(.x)))
  } else {
    calibration_coefs <- NULL
  }

  # Return ----
  fdom_cal_info <- dplyr::bind_cols(div_metadata, calibration_coefs)

  return(fdom_cal_info)
}
