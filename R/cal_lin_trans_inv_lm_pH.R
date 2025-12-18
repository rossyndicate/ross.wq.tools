#' @title Linear Transition Between Inverse Linear Models (pH Specific)
#'
#' @description
#' Converts millivolt values back to pH units using temporally-weighted inverse
#' linear transformation between two consecutive calibrations. This function
#' applies two-segment inverse linear models with temporal interpolation to
#' account for sensor drift across the calibration window. Uses pH value
#' thresholds to select between low-range and high-range inverse transformations
#' based on the original pH observations.
#'
#' @param df Tibble containing sensor data bounded by two calibrations
#' @param mv_col Character string specifying the column name containing
#'   millivolt values from pH linear model transformation
#' @param slope_from_col Character string specifying the column name containing
#'   slope coefficients from the first (earliest) calibration
#' @param offset_from_col Character string specifying the column name containing
#'   offset coefficients from the first (earliest) calibration
#' @param slope_to_col Character string specifying the column name containing
#'   slope coefficients from the second (latest) calibration
#' @param offset_to_col Character string specifying the column name containing
#'   offset coefficients from the second (latest) calibration
#' @param wt_col Character string specifying the column name containing temporal
#'   weight parameters
#'
#' @return The input dataframe with an additional column containing pH values
#'   converted from millivolts. Column name follows the pattern:
#'   "{mv_col_prefix}_lm_trans" (e.g., "mean_lm_trans" from "mean_raw").
#'
#' @details
#' The inverse temporal interpolation formula is:
#' x = (y - (b₁ - wt(b₁ - b₂))) / (m₁ - wt(m₁ - m₂))
#' where wt represents the temporal weight between 0 and 1.
#'
#' @seealso [cal_wt()]
#' @seealso [cal_lm_pH()]
#' @seealso [cal_three_point_drift_pH()]

cal_lin_trans_inv_lm_pH <- function(df = ., mv_col,
                                    slope_from_col, offset_from_col,
                                    slope_to_col, offset_to_col,
                                    wt_col) {

  # Create output column names for pH conversion
  ph_col <- paste0(str_split_1(mv_col, "_")[1], "_lm_trans")

  # Convert millivolts back to pH using temporally-weighted inverse transformation
  df <- df %>%
    dplyr::mutate(
      slope_delta = !!sym(slope_from_col) - !!sym(slope_to_col),
      offset_delta = !!sym(offset_from_col) - !!sym(offset_to_col),
      # Inverse transformation: x = (y-(b_1-wt(b_1-b_2)))/(m_1-wt(m_1-m_2))
      !!ph_col := (.data[[mv_col]] - (!!sym(offset_from_col) - (.data[[wt_col]] * offset_delta))) / (!!sym(slope_from_col) - (.data[[wt_col]] * slope_delta))
    ) %>%
    dplyr::select(-c(slope_delta, offset_delta))

  return(df)
}
