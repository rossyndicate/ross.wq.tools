#' @title Prepare sensor data for back calibration by inverting to raw values and creating calibration windows
#' @export
#'
#' @description
#' Transforms sensor data back to raw values and creates calibration windows for
#' drift correction analysis. The function processes joined sensor-calibration data
#' by first inverting calibrated measurements to their original raw form using the
#' inverse linear model transformation, then segments the data into windows bounded
#' by consecutive good calibrations.
#'
#' Raw data transformation is essential for drift detection because it reveals the
#' actual sensor response patterns that occur between calibrations. The function
#' handles missing or bad calibrations by skipping transformation when no valid
#' next calibration exists, preventing invalid corrections downstream.
#'
#' @param sensor_calibration_data_list Nested list containing joined sensor and
#'   calibration data from join_sensor_calibration_data(), organized by year
#'   and site-parameter combinations. Must include calibration flags and
#'   lead calibration parameters for transformation.
#'
#' @return Nested list structure (year -> site-parameter -> calibration chunks)
#' where each calibration chunk contains:
#' - Raw sensor values (mean_raw) computed via inverse linear transformation
#' - Original calibrated values for comparison
#' - Calibration window boundaries defined by consecutive good calibrations
#' - All original sensor metadata and timestamps
#'
#' Chunks with no valid next calibration have slope_lead, offset_lead, and
#' mean_raw set to NA to prevent invalid transformations.
#'
#' @details
#' The function performs two key operations:
#' 1. **Raw value recovery**: Uses cal_inv_lm() to transform calibrated sensor
#'    readings back to raw values using the inverse of the linear calibration model
#' 2. **Window segmentation**: Creates data chunks between consecutive good
#'    calibrations using the sensor_date_lead_char column as grouping criteria
#'
#' This preparation enables downstream drift detection algorithms to analyze the
#' actual sensor response patterns that occur during deployment periods.
#'
#' @examples
#' \dontrun{
#' # Prepare calibration windows from joined data
#' joined_data <- join_sensor_calibration_data(sensor_list, calibration_data)
#' calibration_windows <- cal_prepare_calibration_windows(joined_data)
#'
#' # Check structure of prepared windows
#' names(calibration_windows)  # Years
#' names(calibration_windows$`2024`)  # Site-parameter combinations
#' length(calibration_windows$`2024`$`site1-Temperature`)  # Number of chunks
#' }
#'
#' @seealso [join_sensor_calibration_data()]
#' @seealso [cal_inv_lm()]
#' @seealso [back_calibrate()]

cal_prepare_calibration_windows <- function(sensor_calibration_data_list) {
  # Process each year of joined sensor-calibration data
  prepped_yearly_site_param_chunks <- sensor_calibration_data_list %>%
    map(function(year){ # Iterate over each year's site-parameter list
      # Process each site-parameter combination within the year
      prepped_yearly_site_param_chunks <- year %>%
        map(function(site_param_df){
          # Split data by calibration dates to create original calibration periods
          site_param_df <- site_param_df %>%
            group_by(file_date) %>%
            group_split() %>%
            map_dfr(function(cal_chunk_og){
              # Check if the chunk has something that it can turn back into
              # If there is no next valid calibration to transform into,
              # we want skip the raw transformation and prevent it from
              # transforming down the line by setting slope and offset as NA
              if (all(is.na(cal_chunk_og)) || all(cal_chunk_og$correct_calibration_lead == FALSE, na.rm = TRUE)) {
                tweaked_chunk <- cal_chunk_og %>%
                  mutate(
                    slope_lead = NA,
                    offset_lead = NA,
                    mean_raw = NA
                  )
                return(tweaked_chunk)
              }
              # Transform the data into its raw form
              tweaked_chunk <- cal_chunk_og %>%
                cal_inv_lm(
                  df = .,
                  obs_col = "mean",
                  slope_col = "slope",
                  offset_col = "offset"
                )
              return(tweaked_chunk)
            })
          calibration_chunks <- site_param_df %>%
            # Ensure sensor_date_lead is properly formatted as a string
            # groupdata2:splt does not work with dates so we have to set
            # the starting column as a character
            mutate(
              sensor_date_lead_chr = case_when(
                is.na(sensor_date_lead) ~ NA_character_,
                sensor_date_lead == "" ~ NA_character_,
                TRUE ~ as.character(sensor_date_lead)
              )
            ) %>%
            # We want to group by when correct calibration column changes
            groupdata2::splt(
              n = "auto",
              method = "l_starts",
              # We know that sensor data lead is `tidyr::fill`'ed with good calibrations.
              # Doing this allows us to make a group of a calibration chunk that is
              # now defined as the chunk of data between two sequential GOOD calibrations.
              starts_col = "sensor_date_lead_chr"
            ) %>%
            # Remove chunks with missing sonde data
            discard(~all(is.na(.x$sonde_serial)))
          return(calibration_chunks)
        })
    })
  # Return nested structure: year -> site-parameter -> calibration chunks
  # Each chunk contains sensor data bounded by two consecutive calibrations
  # enabling temporal interpolation of calibration parameters
  return(prepped_yearly_site_param_chunks)
}
