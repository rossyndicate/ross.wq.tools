#' @title Extract pH and ORP Data from Calibration Markup
#' @export
cal_extract_ph_orp_data <- function(div) {

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
      dplyr::mutate(sensor = str_split(sensor, "/")) %>%
      unnest(sensor) %>%
      dplyr::mutate(
        sensor = janitor::make_clean_names(sensor),
        calibration_coefs = NULL,
        driftr_input = NULL
      )
    return(div_metadata)
  }
  # Else, continue with the function...

  # pH and ORP metadata ----
  div_metadata <- div_tables[["sensor"]] %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)  %>%
    dplyr::rename(sensor_serial = serial_number) %>%
    dplyr::mutate(sensor = str_split(sensor, "/")) %>%
    unnest(sensor) %>%
    dplyr::mutate(sensor = janitor::make_clean_names(sensor))

  # pH ----
  ## In-situ calibration coefficients ----
  # Check if the table we expect for the calibration coefficients exists in the structure we expect...
  cal_coef_check_ph_1 <- cal_div_table_check(
    table_list = div_tables,
    table_name = "slope_and_offset_1",
    col_names = c("slope", "offset")
  )

  cal_coef_check_ph_2 <- cal_div_table_check(
    table_list = div_tables,
    table_name = "slope_and_offset_2",
    col_names = c("slope", "offset")
  )

  if(cal_coef_check_ph_1 && cal_coef_check_ph_2){

    calibration_coefs_ph <- div_tables[c("slope_and_offset_1", "slope_and_offset_2")] %>%
      map_dfr(function(ph_coef_df){
        pivot_df <- ph_coef_df %>%
          tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)
      }) %>%
      separate_wider_delim(slope, delim = " ", names = c("slope", "slope_units")) %>%
      separate_wider_delim(offset, delim = " ", names = c("offset", "offset_units")) %>%
      dplyr::mutate(
        point = c(1,2),
        dplyr::across(c(slope, offset), ~as.numeric(.x))
      ) %>%
      dplyr::relocate(point, .before = "slope")

  } else {
    calibration_coefs_ph <- NULL
  }

  # ORP ----
  ## In-situ calibration coefficients ----
  # Check if the table we expect for the calibration coefficients exists in the structure we expect...
  cal_coef_check_orp <- cal_div_table_check(
    table_list = div_tables,
    table_name = "orp",
    col_names = c("orp_solution", "offset", "temperature")
  )

  if(cal_coef_check_orp){
    calibration_coefs_orp <- div_tables[["orp"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      dplyr::mutate(
        slope = 1,
        orp_solution = janitor::make_clean_names(orp_solution)
      ) %>%
      separate_wider_delim(offset, delim = " ", names = c("offset", "offset_units")) %>%
      dplyr::mutate(dplyr::across(c(slope, offset), ~as.numeric(.x))) %>%
      dplyr::select(slope, offset, offset_units)
  } else {
    calibration_coefs_orp <- NULL
  }

  # Return ----
  ph_info <- div_metadata %>%
    dplyr::filter(sensor == "p_h") %>%
    dplyr::bind_cols(calibration_coefs_ph)

  orp_info <- div_metadata %>%
    dplyr::filter(sensor == "orp") %>%
    dplyr::bind_cols(calibration_coefs_orp)

  ph_orp_info <- dplyr::bind_rows(ph_info, orp_info)

  return(ph_orp_info)
}
