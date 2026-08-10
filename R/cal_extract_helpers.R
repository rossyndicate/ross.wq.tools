# Internal replacement for rvest::html_table(), scoped to the key/value
# tables used in In-Situ calibration report HTML (every table in these
# reports is consumed via pivot_wider(names_from = X1, values_from = X2) --
# no header row, no rowspan/colspan).
xml_table <- function(table_node) {
  rows <- xml2::xml_find_all(table_node, ".//tr")
  cells <- lapply(rows, function(r) {
    trimws(xml2::xml_text(xml2::xml_find_all(r, "./td|./th")))
  })
  n_col <- max(lengths(cells), 0)
  if (n_col == 0) return(as.data.frame(matrix(nrow = 0, ncol = 0)))
  padded <- lapply(cells, function(x) { length(x) <- n_col; x })
  df <- as.data.frame(do.call(rbind, padded), stringsAsFactors = FALSE)
  names(df) <- paste0("X", seq_len(n_col))
  df
}

# Mirrors `rvest::html_elements("table") %>% rvest::html_table()`: applies
# xml_table() across a nodeset of <table> elements and returns a list.
xml_table_list <- function(table_nodes) {
  lapply(table_nodes, xml_table)
}

#' @title Extract Chlorophyll-a data from calibration file html markup
#' @param div A read-in HTML div element containing calibration data tables to be parsed.
#' @export
cal_extract_chla_data <- function(div) {
  div_tables <- div %>%
    xml2::xml_find_all(".//table") %>%
    purrr::discard(\(x) length(xml2::xml_find_all(x, ".//caption")) == 0)  %>%
    xml_table_list() %>%
    purrr::set_names(
      {div %>%
          xml2::xml_find_all(".//caption") %>%
          xml2::xml_text() %>%
          janitor::make_clean_names()
      }
    )

  div_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "sensor",
    col_names = c("sensor", "serial_number", "last_calibrated")
  )

  if (!div_check){
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

  div_metadata <- div_tables[["sensor"]] %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
    dplyr::rename(sensor_serial = serial_number) %>%
    dplyr::mutate(sensor = janitor::make_clean_names(sensor))

  cal_coef_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "slope_and_offset_1",
    col_names = c("slope", "offset")
  )

  if(cal_coef_check) {
    calibration_coefs <- div_tables[["slope_and_offset_1"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      tidyr::separate_wider_delim(offset, delim = " ", names = c("offset", "units")) %>%
      dplyr::mutate(dplyr::across(c(slope, offset), ~as.numeric(.x)))
  } else {
    calibration_coefs <- NULL
  }

  chla_cal_info <- dplyr::bind_cols(div_metadata, calibration_coefs)
  return(chla_cal_info)
}

#' @title Extract Conductivity Data from Calibration Markup
#' @param div A read-in HTML div element containing calibration data tables to be parsed.
#' @export
cal_extract_conductivity_data <- function(div) {
  div_tables <- div %>%
    xml2::xml_find_all(".//table") %>%
    purrr::discard(\(x) length(xml2::xml_find_all(x, ".//caption")) == 0) %>%
    xml_table_list() %>%
    purrr::set_names(
      {div %>%
          xml2::xml_find_all(".//caption") %>%
          xml2::xml_text() %>%
          janitor::make_clean_names()
      }
    )

  div_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "sensor",
    col_names = c("sensor", "serial_number", "last_calibrated")
  )

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

  div_metadata <- div_tables[["sensor"]] %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)  %>%
    dplyr::rename(sensor_serial = serial_number) %>%
    dplyr::mutate(sensor = janitor::make_clean_names(sensor))

  cal_slope_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "calibration_details",
    col_names = c("cell_constant")
  )

  cal_offset_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "calibration_details",
    col_names = c("offset")
  )

  if(cal_slope_check && cal_offset_check){
    calibration_coefs <- div_tables[["calibration_details"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      tidyr::separate_wider_delim(offset, delim = " ", names = c("offset", "units")) %>%
      dplyr::select(slope = cell_constant, offset, units) %>%
      dplyr::mutate(dplyr::across(c(slope, offset), ~as.numeric(.x)))
  } else if(cal_slope_check && !cal_offset_check) {
    calibration_coefs <- div_tables[["calibration_details"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      dplyr::mutate(offset = 0, units = "µS/cm") %>%
      dplyr::select(slope = cell_constant, offset, units) %>%
      dplyr::mutate(dplyr::across(c(slope, offset), ~as.numeric(.x)))
  } else {
    calibration_coefs <- NULL
  }

  cond_cal_info <- dplyr::bind_cols(div_metadata, calibration_coefs)
  return(cond_cal_info)
}

#' @title Extract FDOM Data from Calibration Markup
#' @param div A read-in HTML div element containing calibration data tables to be parsed.
#' @export
cal_extract_fdom_data <- function(div) {
  div_tables <- div %>%
    xml2::xml_find_all(".//table") %>%
    purrr::discard(\(x) length(xml2::xml_find_all(x, ".//caption")) == 0) %>%
    xml_table_list() %>%
    purrr::set_names(
      {div %>%
          xml2::xml_find_all(".//caption") %>%
          xml2::xml_text() %>%
          janitor::make_clean_names()
      }
    )

  div_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "sensor",
    col_names = c("sensor", "serial_number", "last_calibrated")
  )

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

  div_metadata <- div_tables[["sensor"]] %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)  %>%
    dplyr::rename(sensor_serial = serial_number) %>%
    dplyr::mutate(sensor = janitor::make_clean_names(sensor))

  cal_coef_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "slope_and_offset_1",
    col_names = c("slope", "offset")
  )

  if(cal_coef_check) {
    calibration_coefs <- div_tables[["slope_and_offset_1"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      tidyr::separate_wider_delim(offset, delim = " ", names = c("offset", "units")) %>%
      dplyr::mutate(dplyr::across(c("slope", "offset"), ~as.numeric(.x)))
  } else {
    calibration_coefs <- NULL
  }

  fdom_cal_info <- dplyr::bind_cols(div_metadata, calibration_coefs)
  return(fdom_cal_info)
}

#' @title Extract pH and ORP Data from Calibration Markup
#' @param div A read-in HTML div element containing calibration data tables to be parsed.
#' @export
cal_extract_ph_orp_data <- function(div) {
  div_tables <- div %>%
    xml2::xml_find_all(".//table") %>%
    purrr::discard(\(x) length(xml2::xml_find_all(x, ".//caption")) == 0) %>%
    xml_table_list() %>%
    purrr::set_names(
      {div %>%
          xml2::xml_find_all(".//caption") %>%
          xml2::xml_text() %>%
          janitor::make_clean_names()
      }
    )

  div_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "sensor",
    col_names = c("sensor", "serial_number", "last_calibrated")
  )

  if (!div_check) {
    div_metadata <- div_tables[["sensor"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      dplyr::rename(sensor_serial = serial_number) %>%
      dplyr::mutate(sensor = stringr::str_split(sensor, "/")) %>%
      tidyr::unnest(sensor) %>%
      dplyr::mutate(
        sensor = janitor::make_clean_names(sensor),
        calibration_coefs = NULL,
        driftr_input = NULL
      )
    return(div_metadata)
  }

  div_metadata <- div_tables[["sensor"]] %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)  %>%
    dplyr::rename(sensor_serial = serial_number) %>%
    dplyr::mutate(sensor = stringr::str_split(sensor, "/")) %>%
    tidyr::unnest(sensor) %>%
    dplyr::mutate(sensor = janitor::make_clean_names(sensor))

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
      purrr::map_dfr(function(ph_coef_df){
        pivot_df <- ph_coef_df %>%
          tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)
      }) %>%
      tidyr::separate_wider_delim(slope, delim = " ", names = c("slope", "slope_units")) %>%
      tidyr::separate_wider_delim(offset, delim = " ", names = c("offset", "offset_units")) %>%
      dplyr::mutate(
        point = c(1,2),
        dplyr::across(c(slope, offset), ~as.numeric(.x))
      ) %>%
      dplyr::relocate(point, .before = "slope")
  } else {
    calibration_coefs_ph <- NULL
  }

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
      tidyr::separate_wider_delim(offset, delim = " ", names = c("offset", "offset_units")) %>%
      dplyr::mutate(dplyr::across(c(slope, offset), ~as.numeric(.x))) %>%
      dplyr::select(slope, offset, offset_units)
  } else {
    calibration_coefs_orp <- NULL
  }

  ph_info <- div_metadata %>%
    dplyr::filter(sensor == "p_h") %>%
    dplyr::bind_cols(calibration_coefs_ph)

  orp_info <- div_metadata %>%
    dplyr::filter(sensor == "orp") %>%
    dplyr::bind_cols(calibration_coefs_orp)

  ph_orp_info <- dplyr::bind_rows(ph_info, orp_info)
  return(ph_orp_info)
}

#' @title Extract Pressure Data from Calibration Markup
#' @param div A read-in HTML div element containing calibration data tables to be parsed.
#' @export
cal_extract_pressure_data <- function(div) {
  div_tables <- div %>%
    xml2::xml_find_all(".//table") %>%
    purrr::discard(\(x) length(xml2::xml_find_all(x, ".//caption")) == 0) %>%
    xml_table_list() %>%
    purrr::set_names(
      {div %>%
          xml2::xml_find_all(".//caption") %>%
          xml2::xml_text() %>%
          janitor::make_clean_names()
      }
    )

  div_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "sensor",
    col_names = c("sensor", "serial_number", "last_calibrated")
  )

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

  div_metadata <- div_tables[["sensor"]] %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)  %>%
    dplyr::rename(sensor_serial = serial_number) %>%
    dplyr::mutate(sensor = janitor::make_clean_names(sensor))

  cal_coef_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "calibration_details",
    col_names = c("zero_offset")
  )

  if(cal_coef_check) {
    calibration_coefs <- div_tables[["calibration_details"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      tidyr::separate_wider_delim(zero_offset, delim = " ", names = c("offset", "units")) %>%
      dplyr::mutate(
        slope = 1,
        dplyr::across(c(offset), ~as.numeric(.x))
      ) %>%
      dplyr::select(slope, offset, units)
  } else {
    calibration_coefs <- NULL
  }

  pressure_cal_info <- dplyr::bind_cols(div_metadata, calibration_coefs)
  return(pressure_cal_info)
}

#' @title Extract RDO Data from Calibration Markup
#' @param div A read-in HTML div element containing calibration data tables to be parsed.
#' @export
cal_extract_rdo_data <- function(div) {
  div_tables <- div %>%
    xml2::xml_find_all(".//table") %>%
    purrr::discard(\(x) length(xml2::xml_find_all(x, ".//caption")) == 0) %>%
    xml_table_list() %>%
    purrr::set_names(
      {div %>%
          xml2::xml_find_all(".//caption") %>%
          xml2::xml_text() %>%
          janitor::make_clean_names()
      }
    )

  div_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "sensor",
    col_names = c("sensor", "serial_number", "last_calibrated")
  )

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

  div_metadata <- div_tables[["sensor"]] %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)  %>%
    dplyr::rename(sensor_serial = serial_number) %>%
    dplyr::mutate(sensor = janitor::make_clean_names(sensor))

  cal_coef_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "calibration_details",
    col_names = c("slope", "offset")
  )

  if(cal_coef_check){
    calibration_coefs <- div_tables[["calibration_details"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      tidyr::separate_wider_delim(offset, delim = " ", names = c("offset", "units")) %>%
      dplyr::mutate(dplyr::across(c(slope, offset), ~as.numeric(.x)))
  } else {
    calibration_coefs <- NULL
  }

  rdo_cal_info <- dplyr::bind_cols(div_metadata, calibration_coefs)
  return(rdo_cal_info)
}

#' @title Extract Turbidity Data from Calibration Markup
#' @param div A read-in HTML div element containing calibration data tables to be parsed.
#' @export
cal_extract_turbidity_data <- function(div) {
  div_tables <- div %>%
    xml2::xml_find_all(".//table") %>%
    purrr::discard(\(x) length(xml2::xml_find_all(x, ".//caption")) == 0) %>%
    xml_table_list() %>%
    purrr::set_names(
      {div %>%
          xml2::xml_find_all(".//caption") %>%
          xml2::xml_text() %>%
          janitor::make_clean_names()
      }
    )

  div_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "sensor",
    col_names = c("sensor", "serial_number", "last_calibrated")
  )

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

  div_metadata <- div_tables[["sensor"]] %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names)  %>%
    dplyr::rename(sensor_serial = serial_number) %>%
    dplyr::mutate(sensor = janitor::make_clean_names(sensor))

  cal_coef_check <- cal_div_table_check(
    table_list = div_tables,
    table_name = "calibration_details",
    col_names = c("slope", "offset")
  )

  if(cal_coef_check) {
    calibration_coefs <- div_tables[["calibration_details"]] %>%
      tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
      tidyr::separate_wider_delim(offset, delim = " ", names = c("offset", "units")) %>%
      dplyr::mutate(dplyr::across(c("slope", "offset"), ~as.numeric(.x)))
  } else {
    calibration_coefs <- NULL
  }

  turb_cal_info <- dplyr::bind_cols(div_metadata, calibration_coefs)
  return(turb_cal_info)
}
