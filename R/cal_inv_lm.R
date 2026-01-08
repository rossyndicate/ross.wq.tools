#' @title Back Calibration Orchestrator
#' @export
#'
#' @description
#' Orchestrates the complete back calibration workflow for sensor data chunks
#' bounded by two calibrations. This function determines the appropriate
#' calibration pathway based on sensor parameter type and applies the
#' corresponding sequence of correction functions. Standard sensors follow a
#' three-step process using temporal interpolation between calibrations, while
#' pH sensors require specialized processing with unit conversions.
#'
#' @param prepped_snsr_cal_df Tibble containing prepared sensor calibration data
#'   chunk from cal_prepare_calibration_windows(), bounded by two calibrations
#'   and containing raw values from inverse transformation
#'
#' @return For recognized parameters, returns the back-calibrated dataframe with
#'   the last row removed (boundary calibration point). Contains original data
#'   plus calibrated values and validation results. Returns NULL for
#'   unrecognized parameter types.
#'
#' @details
#' **Standard Sensor Pathway** (Chl-a, FDOM, ORP, Pressure, Specific Conductivity,
#' RDO, Turbidity):
#' 1. Calculate temporal weights using cal_wt()
#' 2. Apply temporal interpolation between calibrations using cal_lin_trans_lm()
#' 3. Validate results and create final values using cal_check()
#'
#' **pH Sensor Pathway** (specialized processing):
#' 1. Calculate temporal weights using cal_wt()
#' 2. Convert pH to millivolt units using cal_lm_pH()
#' 3. Apply temporal inverse transformation using cal_lin_trans_inv_lm_pH()
#' 4. Validate results using cal_check()
#'
#' The function removes the last row from each chunk as it represents the
#' boundary calibration point used for interpolation rather than sensor data.
#'
#' @examples
#' \dontrun{
#' # Process prepared calibration windows
#' calibration_windows <- cal_prepare_calibration_windows(joined_data)
#'
#' # Back calibrate a single chunk
#' chunk <- calibration_windows$`2024`$`site1-Temperature`[[1]]
#' corrected_chunk <- cal_back_calibrate(chunk)
#' }
#'
#' @seealso [cal_wt()]
#' @seealso [cal_inv_lm()]
#' @seealso [cal_lin_trans_lm()]
#' @seealso [cal_check()]
#' @seealso [cal_lm_pH()]
#' @seealso [cal_lin_trans_inv_lm_pH()]
#' @seealso [cal_prepare_calibration_windows()]

cal_inv_lm <- function(df, obs_col, slope_col, offset_col){

  # Create output column name for raw values
  raw_col <- paste0(obs_col, "_raw")

  # Apply inverse linear model transformation: x = (y - b) / m
  df <- df %>%
    dplyr::mutate(!!raw_col := (.data[[obs_col]] - .data[[offset_col]]) / .data[[slope_col]])

  return(df)
}
