#' @title Deduplicate calibrations
#' @export

cal_deduplicate <- function(df) {
  df %>%
    # Deduplicate calibrations by selecting most recent per day
    dplyr::group_by(sonde_date, sensor_serial, point) %>%
    dplyr::slice_max(file_date, n = 1, with_ties = F) %>%
    dplyr::ungroup() %>%
    # Select most recent calibration per sensor
    dplyr::group_by(sensor_date, sensor_serial, point) %>%
    dplyr::slice_min(file_date, n = 1, with_ties = F) %>%
    dplyr::ungroup() %>%
    # Remove any remaining duplicates
    dplyr::distinct()
}
