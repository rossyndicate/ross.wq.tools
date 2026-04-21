#' @title One Point Drift Calibration
#' @export
#'
#' @description
#' Applies single-point drift correction using pre- and post-measurement
#' calibration standards. This function corrects for sensor drift by calculating
#' the difference between expected and observed standard values, then applying a
#' temporally-weighted correction across the calibration window. Used primarily
#' for Chlorophyll-a, FDOM, ORP, Pressure, Specific Conductivity, and RDO
#' sensors that require single-point drift correction.
#'
#' @param df Tibble containing sensor data bounded by two calibrations
#' @param lm_trans_col Character string specifying the column name containing
#'   linearly transformed data from cal_lin_trans_lm()
#' @param drift_corr_col Character string specifying the column name containing
#'   drift correction information
#' @param wt_col Character string specifying the column name containing temporal
#'   weight parameters
#'
#' @seealso [cal_wt()]
#' @seealso [cal_lin_trans_lm()]
#' @seealso [cal_check()]


# Instead of drift_corr_col, we are going to set a pre and post column for now.
# before we do this we are going to want to do a weight column based on "drift groups",
# ths is those consecutive sections of data that are flagged with drift.

cal_one_point_drift <- function(df, lm_trans_col, pre_col, post_col, wt_col){

  # Create output column name for drift-corrected values
  transformed_col <- paste0(str_split_1(lm_trans_col, "_")[1], "_drift_trans")

  # Extract drift calibration data from the latest field note information

  # We will do this by finding the relevant pre and post values in the
  # data
  pre_drift_back_calibration <- df[[pre_col]][[nrow(df)]]
  post_drift_back_calibration <- df[[post_col]][[nrow(df)]]

  # Handle missing calibration data
  if (is.na(pre_drift_back_calibration) || is.na(post_drift_back_calibration)) {
    df <- df %>%
      mutate(!!transformed_col := NA_integer_)
    return(df)
  }

  # Extract calibration standard values for drift calculation

  # (The following thoughts are specific to turbidity)
  # This doesn't work with collected field values either. The difference between
  # The drift data at the end and the correct data later is often ~100ish points,
  # 5 number summary of the difference between pre and post field collected values

  # This chunk of code is from the deprecated version of this function.
  # expected_standard <- as.numeric(drift_back_calibration %>% pull(post_measurement))
  # observed_standard <- as.numeric(drift_back_calibration %>% pull(pre_measurement))

  # Calculate drift offset between expected and observed standards
  standard_delta <- post_drift_back_calibration - pre_drift_back_calibration

  # Apply temporally-weighted drift correction: C_t = m_t + w_t(delta_S)
  df <- df %>%
    mutate(!!transformed_col := .data[[lm_trans_col]] + (.data[[wt_col]]*standard_delta))

  return(df)
}
