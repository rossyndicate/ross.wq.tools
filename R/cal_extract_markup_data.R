#' @title Extract Calibration Data from HTML Markup
#' @export
#'
#' @description
#' Extracts calibration coefficients and drift correction information from HTML
#' calibration report files. This function processes both field and benchtop
#' calibration files, extracting slope/offset parameters and pre/post measurement
#' data for each sensor. Organizes the extracted data into a nested list structure
#' by year and site-parameter combinations, with field calibrations taking
#' priority over benchtop calibrations when both are available.
#'
#' @param field_cal_dir Character string specifying the directory path containing
#'   field calibration HTML files (default: data/calibration_reports/)
#'
#' @seealso [load_calibration_data()]
#' @seealso [cal_join_sensor_calibration_data()]

cal_extract_markup_data <- function(field_cal_dir = here::here("data", "raw", "sensor", "calibration_reports")) {

  # Input validation ----
  if (!is.character(field_cal_dir) || length(field_cal_dir) != 1) {
    stop("field_cal_dir must be a single character string")
  }

  # Directory validation ----
  if (!dir.exists(field_cal_dir)) {
    stop("field_cal_dir does not exist: ", field_cal_dir)
  }

  # Prepare field calibrations for extraction ====
  # Identify and filter field calibration HTML files
  f_cal_paths <- list.files(field_cal_dir, pattern = ".html", full.names = T)
  f_cal_paths <- purrr::discard(f_cal_paths, ~grepl("vulink|virridy", .x, ignore.case = T))

  if (length(f_cal_paths) == 0){
    stop("No HTML calibration files found in field_cal_dir: ", field_cal_dir)
  }

  # Extract site names and datetime information from field calibration file paths
  # No error handling internally in this map -- assuming that file paths always
  # have the right information in the right structure

  f_cal_info <- f_cal_paths %>%
    purrr::map(function(path_str){
      str_list <- basename(path_str) %>%
        stringr::str_split_1("_|\\.") %>%
        stringr::str_squish()

      # Parse site name from filename
      site <- str_list[1]

      # Parse and convert datetime to UTC
      date <- str_list[2:3] %>%
        stringr::str_flatten(collapse = " ") %>%
        stringr::str_squish() %>%
        lubridate::ymd_hm(tz = "America/Denver") %>%
        lubridate::with_tz(tzone = "UTC")

      cal_info <- tibble::tibble(site = site, date = date)
      return(cal_info)
    })

  # Extract calibration information from all HTML files ====

  # Load HTML markup from all calibration files
  cal_html <- purrr::map(f_cal_paths, rvest::read_html)

  # Extract calibration data from each HTML file
  cal_data <- purrr::map2_dfr(
    cal_html, f_cal_info,
    function(html_markup, calibration_information){

      # Extract overall calibration metadata from first table
      file_information <- html_markup %>%
        rvest::html_elements("table") %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        tidyr::pivot_wider(names_from = X1, values_from = X2) %>%
        janitor::clean_names() %>%
        dplyr::mutate(instrument = janitor::make_clean_names(instrument)) %>%
        dplyr::rename(sonde_serial = serial_number)

      # Extract each div element with sensor-specific calibration data
      html_divs <- html_markup %>%
        rvest::html_elements("div")

      # Process each sensor div for calibration coefficients
      html_div_info <- html_divs %>%
        purrr::map_dfr(function(div){
          # Identify sensor type from div metadata
          sensor <- div %>%
            rvest::html_elements("table") %>%
            rvest::html_table() %>%
            purrr::pluck(1) %>%
            tidyr::pivot_wider(names_from = X1, values_from = X2, names_repair = janitor::make_clean_names) %>%
            dplyr::mutate(sensor = janitor::make_clean_names(sensor)) %>%
            dplyr::pull(sensor)

          # Extract sensor-specific calibration data using appropriate extraction function
          if (sensor %in% c("chlorophyll_a", "conductivity", "fdom", "p_h_orp", "pressure", "rdo", "turbidity")){
            div_table_data <- switch(
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
            return()
          }
          return(div_table_data)
        })

      # Combine metadata with extracted sensor data
      markup_data <- tibble::tibble(
        calibration_information,
        file_information,
        html_div_info
      ) %>%
        # Standardize site names using fix_sites() function
        ross.wq.tools::fix_site_names() %>%
        # Standardize sensor parameter names to match sensor data conventions
        dplyr::mutate(
          sensor = dplyr::case_when(
            sensor == "chlorophyll_a" ~ "Chl-a Fluorescence",
            sensor == "conductivity" ~ "Specific Conductivity",
            sensor == "fdom" ~ "FDOM Fluorescence",
            sensor == "p_h" ~ "pH",
            sensor == "orp" ~ "ORP",
            sensor == "pressure" ~ "Pressure",
            sensor == "rdo" ~ "DO",
            sensor == "turbidity" ~ "Turbidity",
            .default = sensor
          )
        ) %>%
        # Rename datetime columns for clarity
        dplyr::rename(file_date = date, sonde_date = created, sensor_date = last_calibrated)

      return(markup_data)
    })

  # Organize extracted calibration information ====
  # Merge field and benchtop calibrations, prioritizing field calibrations
  calibration_list <- cal_data %>%
    # Filter out bad calibration data
    dplyr::filter(
      sensor_date != "Factory Defaults",
      (!is.na(slope) & !is.na(offset)),
      !is.infinite(slope),
      !is.infinite(offset)
    ) %>%
    # Organize columns by data type
    dplyr::select(
      # Site and sensor identification
      site, sensor,
      # Datetime information
      file_date, sonde_date, sensor_date,
      # Instrument identification
      sonde_serial, sensor_serial,
      # Calibration data
      slope, offset, units, slope_units, offset_units, point
    ) %>%
    # Parse datetime columns
    dplyr::mutate(
      sonde_date = lubridate::mdy(sonde_date),
      sensor_date = lubridate::mdy(sensor_date)
    ) %>%
    # Before splitting up the data, we need to determine which calibrations are good and bad
    ross.wq.tools::cal_annotate() %>%
    # Structure calibration data by year and site-parameter combinations
    # Split by year
    split(f = lubridate::year(.$file_date)) %>%
    purrr::map(\(year_data){
      # Split each year by site-parameter combinations
      site_param_split_list <- year_data %>%
        split(f = list(.$site, .$sensor), sep = "-", drop = TRUE) %>%
        purrr::discard(\(site_param_df) nrow(site_param_df) == 0) %>%
        # De-duplicate the data
        purrr::map_dfr(cal_deduplicate) %>%
        split(f = list(.$sensor, .$sensor_serial), sep = "-") %>%
        discard(~is.null(.) || nrow(.) == 0) %>%
        map_dfr(function(sensor_serial_df) {

          # Find succession for pH
          if(all(!is.na(sensor_serial_df$point))) {

            point1_df <- sensor_serial_df %>% dplyr::filter(point == 1)
            point2_df <- sensor_serial_df %>% dplyr::filter(point == 2)

            point1_processed <- ross.wq.tools::cal_succession(point1_df)

            point2_processed <- ross.wq.tools::cal_succession(point2_df)

            joined_dfs <- dplyr::bind_rows(point1_processed, point2_processed) %>%
              dplyr::arrange(sensor_date, point)

            return(joined_dfs)

          } else {
            # not ph - process normally
            return(ross.wq.tools::cal_succession(sensor_serial_df))
          }
        }) %>%
        # Transform back to site-parameter
        split(f = list(.$site, .$sensor), sep = "-", drop = TRUE) %>%
        purrr::discard(\(site_param_df) nrow(site_param_df) == 0)

      return(site_param_split_list)
    })

  # Return organized calibration data structure
  return(calibration_list)
}
