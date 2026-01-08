#' @title Calibration succession determination
#' @export

cal_succession <- function(df) {

  # Split the data between good and bad calibrations and then bind them again
  # Check if we have any good calibrations
  good_calibrations <- df %>%
    dplyr::filter(correct_calibration)

  if (nrow(good_calibrations) > 0) {
    good_calibrations <- good_calibrations %>%
      dplyr::arrange(sensor_date) %>%
      dplyr::mutate(
        sensor_date_lead = dplyr::lead(sensor_date, 1),
        slope_lead = dplyr::lead(slope, 1),
        offset_lead = dplyr::lead(offset, 1),
        sensor_date_lag = dplyr::lag(sensor_date, 1),
        slope_lag = dplyr::lag(slope, 1),
        offset_lag = dplyr::lag(offset, 1),
      )
  } else {
    # If no good calibrations, return the df as is
    good_calibrations <- df %>%
      dplyr::mutate(
        sensor_date_lead = NA,
        slope_lead = NA,
        offset_lead = NA,
        sensor_date_lag = NA,
        slope_lag = NA,
        offset_lag = NA
      )
    return(good_calibrations)
  }

  # Check if we have any bad calibrations
  bad_calibrations <- df %>%
    dplyr::filter(!correct_calibration)

  # If we have bad calibrations, add the expected columns to them too
  if (nrow(bad_calibrations) > 0) {
    bad_calibrations <- bad_calibrations %>%
      dplyr::mutate(
        sensor_date_lead = NA,
        slope_lead = NA,
        offset_lead = NA,
        sensor_date_lag = NA,
        slope_lag = NA,
        offset_lag = NA
      )
  }

  # Combine them
  calibrations <- dplyr::bind_rows(good_calibrations, bad_calibrations) %>%
    dplyr::arrange(sensor_date) %>%
    # forward fill the next good calibration
    tidyr::fill(
      sensor_date_lead, slope_lead, offset_lead,
      .direction = "downup"
    ) %>%
    # backward fill the last good calibration
    tidyr::fill(
      sensor_date_lag, slope_lag, offset_lag,
      .direction = "updown"
    )

  return(calibrations)
}
