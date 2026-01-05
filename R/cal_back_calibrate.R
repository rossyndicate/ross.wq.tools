#' @title Back Calibration Orchestrator
#' @export
#'
#' @description
#' Orchestrates the complete back calibration workflow for sensor data chunks
#' bounded by two calibrations. This function determines the appropriate
#' calibration pathway based on sensor parameter type and applies the
#' corresponding sequence of correction functions. Standard sensors follow a
#' five-step process, turbidity uses two-point drift correction, and pH sensors
#' require specialized three-point processing with unit conversions.
#' pH sensor calibration pathway (specialized three-point processing)
#' pH calibration requires special handling due to:
#' - Slopes in mV/pH units and offsets in mV units (not direct pH units)
#' - Three-point calibration creating two slope transitions
#' - Need for unit conversion from pH to mV for back-calibration
#'
#' @param prepped_snsr_cal_df Tibble containing prepared sensor calibration data
#'   chunk from prepare_sensor_calibration_data(), bounded by two calibrations
#'
#' @seealso [cal_wt()]
#' @seealso [cal_inv_lm()]
#' @seealso [cal_lin_trans_lm()]
#' @seealso [cal_one_point_drift()]
#' @seealso [cal_two_point_drift()]
#' @seealso [cal_lm_pH()]
#' @seealso [cal_lin_trans_inv_lm_pH()]
#' @seealso [cal_three_point_drift_pH()]
#' @seealso [cal_check()]

cal_back_calibrate <- function(prepped_snsr_cal_df) {

  # Identify sensor parameter type for calibration pathway selection ----
  parameter <- unique(prepped_snsr_cal_df$parameter)

  # Calculate temporal weights for calibration interpolation ====
  chunk_w_time <- prepped_snsr_cal_df %>%
    cal_wt(df = ., dt_col = "DT_round")

  # Apply temporally-weighted linear transformation between calibrations ====
  # Standard sensor calibration pathway
  if (parameter %in% c("Chl-a Fluorescence", "FDOM Fluorescence", "ORP", "Pressure", "Specific Conductivity", "DO", "Turbidity")) {
    back_calibrated_chunk <- chunk_w_time %>%
      cal_inv_lm(
        df = .,
        obs_col = "mean_cleaned",
        slope_col = "slope",
        offset_col = "offset"
      ) %>%
      cal_lin_trans_lm(
        df = .,
        raw_col = "mean_cleaned_raw",
        slope_from_col = "slope_final", offset_from_col = "offset_final",
        slope_to_col = "slope_lead", offset_to_col = "offset_lead",
        wt_col = "wt"
      )
  }

  # pH sensor calibration pathway
  if (parameter == "pH"){
    back_calibrated_chunk <- chunk_w_time %>%
      cal_lm_pH(
        df = .,
        obs_col = "mean_cleaned",
        slope_col = "slope",
        offset_col = "offset"
      ) %>%
      cal_lin_trans_inv_lm_pH(
        df = .,
        mv_col = "mean_cleaned_raw",
        slope_from_col = "slope_final", offset_from_col = "offset_final",
        slope_to_col = "slope_lead", offset_to_col = "offset_lead",
        wt_col = "wt"
      ) %>%
      dplyr::group_by(DT_round) %>%
      # TODO: update this filter to mean_cleaned
      dplyr::filter(is.na(mean) | (mean < 7 & point == 1) | (mean >= 7 & point == 2)) %>%
      slice_min(point) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(DT_round)
  }

  # Validate calibration results and create final calibrated values ====
  checked_df <- back_calibrated_chunk  %>%
    cal_check(df = ., obs_col = "mean_cleaned", lm_trans_col = "mean_lm_trans")

  # Reorder the final columns to make post hoc analysis easier ====
  final_df <- checked_df %>%
    dplyr::select(
      # DT sensor reading columns
      DT_round,
      # Field ID columns
      site, sonde_serial, parameter,
      # Sensor reading transformation columns
      mean_cleaned, mean_cleaned_raw, mean_lm_trans, mean_cleaned_cal, cal_check,
      # Sensor information
      sensor_serial,
      # DT calibration information columns
      file_date, sonde_date, sensor_date_lag, sensor_date, sensor_date_lead,
      # Calibration information columns
      correct_calibration,
      slope_lag, offset_lag,
      slope, offset,
      slope_final, offset_final,
      slope_lead, offset_lead,
      wt
      # Remove everything else
    )

  # Return ====
  return(final_df)
}


