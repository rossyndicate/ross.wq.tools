#' @title Create calibration windows between consecutive good calibrations
#' @export
#'
#' @description
#' Segments joined sensor-calibration data into windows bounded by consecutive good
#' calibrations. The function handles complex interactions between bad calibrations
#' and sensor swaps by creating windows only when calibrations meet specific criteria
#' for valid back calibration analysis.
#'
#' Windows are created by splitting data based on changes in sensor serial numbers
#' and then further segmenting by consecutive good calibrations. This approach ensures
#' that each window contains sensor data collected between two reliable calibration
#' points from the same sensor, enabling accurate drift detection and correction.
#'
#' The function handles edge cases where bad calibrations occur between good ones,
#' sensor swaps interrupt calibration sequences, and missing calibration data.
#' Only windows with valid calibration endpoints are retained for analysis.
#'
#' @param sensor_calibration_data_list Nested list from join_sensor_calibration_data()
#'   organized by year and site-parameter combinations. Must include sensor_serial,
#'   correct_calibration flags, and sensor_date_lead columns.
#'
#' @return Nested list structure (year -> site-parameter -> calibration chunks)
#' where each chunk contains sensor data between two consecutive good calibrations
#' from the same sensor. Chunks include slope_final and offset_final columns
#' determined by calibration quality. Chunks missing sonde data are excluded.
#'
#' @details
#' The segmentation process occurs in two stages:
#' 1. Split by sensor serial number changes to handle sensor swaps
#' 2. Split by consecutive good calibrations within each sensor period
#'
#' Final slope and offset values are assigned based on calibration quality, using
#' current calibration parameters for good calibrations and lagged parameters for
#' bad calibrations. This enables appropriate parameter selection for subsequent
#' drift correction analysis.
#'
#' Special considerations apply to pH parameters which require additional preparation
#' to select appropriate calibration points before back calibration processing.
#'
#' Additional considerations for data preparation:
#'
#' Window creation must account for interactions between bad calibrations and sensor swaps.
#' A longer calibration window is only valid when:
#' - The former good calibration and bad calibration use the same sensor serial
#' - Both calibrations reference the same latter good calibration
#'
#' pH parameters require special handling to select appropriate calibration points
#' (point 1 vs point 2) before proceeding with back calibration analysis.
#'
#' @examples
#' \dontrun{
#' joined_data <- join_sensor_calibration_data(sensor_list, calibration_data)
#' calibration_windows <- cal_prepare_calibration_windows(joined_data)
#' }
#'
#' @seealso [cal_join_sensor_calibration_data()]

cal_prepare_calibration_windows <- function(sensor_calibration_data_list) {
  # Process each year of joined sensor-calibration data
  prepped_yearly_site_param_chunks <- sensor_calibration_data_list %>%
    map(function(year){ # Iterate over each year's site-parameter list
      # Process each site-parameter combination within the year
      prepped_yearly_site_param_chunks <- year %>%
        map(function(site_param_df){

          # Split data into calibration windows based on sensor changes and good calibrations
          # This creates chunks of sensor data between consecutive reliable calibration points
          calibration_chunks <- site_param_df %>%
            # Split by consecutive sensor serial numbers to handle sensor swaps
            groupdata2::splt(
              n = "auto",
              method = "l_starts",
              starts_col = "sensor_serial"
            ) %>%
            map(function(snsr_srl_list){
              # Split by consecutive good calibrations within each sensor period
              snsr_srl_list %>%
                # Convert sensor_date_lead to character for grouping
                dplyr::mutate(
                  sensor_date_lead_chr = dplyr::case_when(
                    is.na(sensor_date_lead) ~ NA_character_,
                    sensor_date_lead == "" ~ NA_character_,
                    TRUE ~ as.character(sensor_date_lead)
                  )
                ) %>%
                groupdata2::splt(
                  n = "auto",
                  method = "l_starts",
                  # Split when good calibrations change to create windows
                  starts_col = "sensor_date_lead_chr"
                ) %>%
                # Remove chunks with missing sonde data
                discard(~all(is.na(.x$sonde_serial)))
            }) %>%
            unlist(recursive = FALSE) %>%
            map(function(cal_window) {
              # Assign final calibration parameters based on calibration quality
              cal_window %>%
                dplyr::mutate(
                  slope_final = ifelse(correct_calibration, slope, slope_lag),
                  offset_final = ifelse(correct_calibration, offset, offset_lag)
                )
            })

          return(calibration_chunks)
        })
    })
  # Return nested structure: year -> site-parameter -> calibration chunks
  # Each chunk contains sensor data bounded by two consecutive calibrations
  # enabling temporal interpolation of calibration parameters
  return(prepped_yearly_site_param_chunks)
}


