#' @title Linear Transition Between Linear Models
#' @export
#'
#' @description
#' Applies temporally-weighted linear transformation between two consecutive
#' calibrations. This function interpolates between the slope and offset
#' parameters from calibration 1 (earliest) and calibration 2 (latest) using
#' temporal weights to create a smooth transition across the calibration window.
#' The transformation accounts for gradual sensor drift by blending calibration
#' parameters based on time position.
#'
#' @param df Tibble containing sensor data bounded by two calibrations. Must
#'   include calibration parameters for the temporal interpolation.
#' @param raw_col Character string specifying the column name containing raw
#'   observation values from the inverse linear model transformation
#' @param slope_from_col Character string specifying the column name containing
#'   slope coefficients from the first (earliest) calibration
#' @param offset_from_col Character string specifying the column name containing
#'   offset coefficients from the first (earliest) calibration
#' @param slope_to_col Character string specifying the column name containing
#'   slope coefficients from the second (latest) calibration
#' @param offset_to_col Character string specifying the column name containing
#'   offset coefficients from the second (latest) calibration
#' @param wt_col Character string specifying the column name containing temporal
#'   weight parameters for interpolation between calibrations
#'
#' @return The input dataframe with an additional column containing linearly
#'   transformed values. Column name follows the pattern:
#'   "{raw_col_prefix}_lm_trans" (e.g., "mean_lm_trans" from "mean_raw").
#'   Returns NA values when calibration parameters are missing.
#'
#' @details
#' The temporal interpolation formula is:
#' y = (m₁ - wt(m₁ - m₂))x + (b₁ - wt(b₁ - b₂))
#' where wt represents the temporal weight between 0 and 1.
#'
#' @seealso [cal_wt()]
#' @seealso [cal_inv_lm()]
#' @seealso [cal_one_point_drift()]
#' @seealso [cal_two_point_drift()]

cal_lin_trans_lm <- function(df, raw_col,
                             slope_from_col, offset_from_col,
                             slope_to_col, offset_to_col,
                             wt_col){

  # Create output column name for linearly transformed values
  transformed_col <- paste0(str_split_1(raw_col, "_")[1], "_lm_trans")

  # Apply temporally-weighted linear model: y = (m_1-wt(m_1-m_2))x+(b_1-wt(b_1-b_2))
  df <- df %>%
    dplyr::mutate(
      slope_delta = !!sym(slope_from_col) - !!sym(slope_to_col),
      offset_delta = !!sym(offset_from_col) - !!sym(offset_to_col),
      !!transformed_col := ((!!sym(slope_from_col)-(.data[[wt_col]]*slope_delta))*.data[[raw_col]])+(!!sym(offset_from_col)-(.data[[wt_col]]*offset_delta))
    ) %>%
    select(-slope_delta, -offset_delta)  # Clean up temporary columns

  return(df)
}
