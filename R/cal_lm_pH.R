#' @title pH Linear Model Transformation
#' @export
#'
#' @description
#' Converts pH observations to millivolt units using two-segment linear models
#' from three-point pH calibration. This function applies forward linear
#' transformation (pH to mV) to enable subsequent back-calibration processing
#' in the correct instrumental units. Uses pH value thresholds to select
#' between low-range and high-range linear segments based on the sensor's
#' three-point calibration characteristics.
#'
#' @param df Tibble containing sensor data bounded by two calibrations
#' @param obs_col Character string specifying the column name containing pH
#'   observations recorded in HydroVu
#' @param lm_coefs_col Character string specifying the column name containing
#'   linear model coefficients with two slope/offset pairs
#'
#' @seealso [cal_wt()]
#' @seealso [cal_lin_trans_inv_lm_pH()]
#' @seealso [cal_three_point_drift_pH()]

cal_lm_pH <- function(df, obs_col, slope_col, offset_col) {

  # Create output column names for pH to mV conversion
  mv_col <- paste0(obs_col, "_raw")

  # Convert pH to millivolts
  df <- df %>%
    dplyr::mutate(
      # Apply linear transformation for low pH range: y = mx + b
      !!mv_col := (.data[[slope_col]] * .data[[obs_col]]) + .data[[offset_col]]
    )

  return(df)
}
