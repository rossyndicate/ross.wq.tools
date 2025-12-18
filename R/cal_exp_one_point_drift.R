#' @title Exponential One Point Drift Calibration
#' @export
#'
#' @description
#' Applies single-point drift correction using pre- and post-measurement
#' calibration standards with exponential weighting to account for non-linear
#' biofilm accumulation. This function corrects for sensor drift by calculating
#' the difference between expected and observed standard values, then applying a
#' temporally-weighted correction across the calibration window using exponential
#' weighting that assumes faster drift accumulation over time.
#'
#' @param df Tibble containing sensor data bounded by two calibrations
#' @param lm_trans_col Character string specifying the column name containing
#'   linearly transformed data from cal_lin_trans_lm()
#' @param pre_col Character string specifying the column name for pre-cleaning measurement
#' @param post_col Character string specifying the column name for post-cleaning measurement
#' @param wt_col Character string specifying the column name containing temporal
#'   weight parameters
#' @param drift_type Character string specifying drift model: "linear" (default) or "exponential"
#' @param correction_type Character string specifying correction method: "additive" (default) or "multiplicative"
#'
#' @seealso [cal_wt()]
#' @seealso [cal_lin_trans_lm()]
#' @seealso [cal_check()]

cal_exp_one_point_drift <- function(df, lm_trans_col, pre_col, post_col, wt_col,
                                    drift_type = "linear", correction_type = "additive"){

  # Create output column name for drift-corrected values
  transformed_col <- paste0(str_split_1(lm_trans_col, "_")[1], "_drift_trans")

  # Extract drift calibration data from the latest field note information
  pre_drift_back_calibration <- df[[pre_col]][[nrow(df)]]
  post_drift_back_calibration <- df[[post_col]][[nrow(df)]]

  # Handle missing calibration data
  if (is.na(pre_drift_back_calibration) || is.na(post_drift_back_calibration)) {
    df <- df %>%
      dplyr::mutate(!!transformed_col := NA_integer_)
    return(df)
  }

  # Apply exponential weighting if requested
  if (drift_type == "exponential") {
    df <- df %>%
      dplyr::mutate(wt_exp = (exp(.data[[wt_col]]) - 1) / (exp(1) - 1))
    wt_col_to_use <- "wt_exp"
  } else {
    wt_col_to_use <- wt_col
  }

  # Apply correction based on type
  if (correction_type == "multiplicative") {
    # Multiplicative correction (for proportional drift like biofilm)
    drift_ratio <- post_drift_back_calibration / pre_drift_back_calibration

    df <- df %>%
      dplyr::mutate(!!transformed_col := .data[[lm_trans_col]] * (1 - .data[[wt_col_to_use]] + .data[[wt_col_to_use]] * drift_ratio))

  } else {
    # Additive correction (original approach)
    standard_delta <- post_drift_back_calibration - pre_drift_back_calibration

    df <- df %>%
      dplyr::mutate(!!transformed_col := .data[[lm_trans_col]] + (.data[[wt_col_to_use]] * standard_delta))
  }

  return(df)
}
