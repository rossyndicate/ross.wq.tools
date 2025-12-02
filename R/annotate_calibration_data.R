#' @title Annotate sensor calibration data as good or bad
#' @export
#'
#' @description
#' Evaluates calibration data quality using statistical outlier detection based on
#' offset and slope parameters. Calibrations are flagged as good or bad by comparing
#' each calibration's parameters against the distribution of all calibrations for
#' that sensor type, using a 3-standard deviation threshold.
#'
#' The function processes calibration data by sensor type, calculates statistical
#' bounds for offset and slope parameters, and flags calibrations that fall outside
#' acceptable ranges. This approach assumes that the majority of calibrations are
#' good and that truly problematic calibrations will appear as statistical outliers.
#' The method can be adjusted by modifying the threshold criteria.
#'
#' @param raw_calibration_df A dataframe containing calibration data with nested
#' calibration coefficients. Must include:
#' - sensor: Sensor type identifier
#' - calibration_coefs: Nested list column containing offset and slope values
#'
#' @return A dataframe with the same structure as input, plus a `correct_calibration`
#' logical column indicating whether each calibration meets quality criteria:
#' - TRUE: Calibration parameters are within acceptable statistical bounds
#' - FALSE: Calibration parameters are outliers or contain invalid values
#' - All columns from original calibration data are preserved
#'
#' The function handles sensors with missing data by evaluating only available
#' parameters and marks calibrations as bad when insufficient valid data exists.
#'
#' @details
#' The current implementation uses a 3-standard deviation threshold for outlier
#' detection, but this method is not fully vetted and subject to change. The
#' approach compares all calibrations against each other within sensor types,
#' which assumes that bad calibrations represent a minority of the dataset.
#'
#' @examples
#' \dontrun{
#' # Process calibration data with quality flags
#' # `read_calibration_file()` is pseudo code, not a real function
#' calibration_data <- read_calibration_file("calibration_data.csv")
#' annotated_data <- annotate_calibration_data(calibration_data)
#'
#' # Check flagging results
#' table(annotated_data$correct_calibration)
#' }

annotate_calibration_data <- function(raw_calibration_df) {
  # NOTE: Some parameters may have NA offset values that should be treated as 0.
  # This handling may occur in upstream extraction functions but needs verification.

  # Split data by sensor type and apply outlier detection
  annotated_calibration_data <- raw_calibration_df %>%
    split(f = .$sensor) %>%
    map(unnest, cols = calibration_coefs) %>%
    map_dfr(function(sensor_df){
      clean_df <- sensor_df %>%
        mutate(
          offset = as.numeric(offset),
          slope = as.numeric(slope)
        )

      # Check for valid finite data in each parameter
      offset_has_data <- any(is.finite(clean_df$offset), na.rm = TRUE)
      slope_has_data <- any(is.finite(clean_df$slope), na.rm = TRUE)

      # Calculate 3-sigma bounds only for parameters with finite data
      if (offset_has_data) {
        finite_offsets <- clean_df$offset[is.finite(clean_df$offset)]
        offset_mean <- mean(finite_offsets)
        offset_sd <- sd(finite_offsets)
        offset_ci_lower <- offset_mean - (3*offset_sd)
        offset_ci_upper <- offset_mean + (3*offset_sd)
      }

      if (slope_has_data) {
        finite_slopes <- clean_df$slope[is.finite(clean_df$slope)]
        slope_mean <- mean(finite_slopes)
        slope_sd <- sd(finite_slopes)
        slope_ci_lower <- slope_mean - (3*slope_sd)
        slope_ci_upper <- slope_mean + (3*slope_sd)
      }

      # Apply quality criteria based on available parameters
      annotated_df <- clean_df %>%
        mutate(
          correct_calibration = case_when(
            # Both parameters available - check both
            offset_has_data & slope_has_data ~
              is.finite(offset) & is.finite(slope) &
              between(offset, offset_ci_lower, offset_ci_upper) &
              between(slope, slope_ci_lower, slope_ci_upper),
            # Only offset available
            offset_has_data & !slope_has_data ~
              is.finite(offset) & between(offset, offset_ci_lower, offset_ci_upper),
            # Only slope available
            !offset_has_data & slope_has_data ~
              is.finite(slope) & between(slope, slope_ci_lower, slope_ci_upper),
            # No valid data available
            TRUE ~ FALSE
          )
        )
      return(annotated_df)
    })
  return(annotated_calibration_data)
}
