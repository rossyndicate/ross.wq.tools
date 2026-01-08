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
#' For a detailed walkthrough of using this function, `cal_prepare_calibration_windows()`, `cal_join_sensor_calibration_data()` and `load_calibration_data()`,
#' see `vignette("fixing_calibrations")`.
#'
#' @param prepped_snsr_cal_df Tibble containing prepared sensor calibration data
#'   chunk from `cal_prepare_calibration_windows()`, bounded by two calibrations.
#'
#'  Must contain the following columns:
#'  From `munge_api_data()` and `tidy_api_data`: DT_round, site, parameter, mean (or other obs_column)
#'  From `cal_prepare_calibration_windows()`: sonde_serial, sensor_serial, file_date, sonde_date,
#'  sensor_date, sensor_date_lag, sensor_date_lead, correct_calibration, slope_lag, offset_lag,
#'  slope, offset, slope_lead, offset_lead, slope_final, offset_final
#'
#' @param obs_column String indicating the column name of the observations to be back-calibrated (default: "mean")
#'
#' @return For recognized parameters, returns the back-calibrated dataframe with the following columns added:
#'  -   `<obs_column>_raw`: Raw sensor values after inverse linear transformation
#'  -   `<obs_column>_lm_trans`: Linearly transformed values between calibrations
#'  -   `<obs_column>_cal`: Final calibrated values after back calibration was performed. If `correct_calibration` == `FALSE`,
#'  this column contains the calibrated values; if `TRUE`, it contains the original observations.
#'  -   `cal_check`: Logical flag indicating whether calibration was successful (`TRUE`) or failed (`FALSE`)
#'
#' Columns from `cal_prepare_calibration_windows()` are retained for context but any additional columns will be removed.
#'
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

cal_back_calibrate <- function(prepped_snsr_cal_df, obs_column = "mean") {

  # ---- Argument Checks ----

  if(is.character(obs_column) == FALSE | length(obs_column) != 1){
    stop("obs_column must be a single character string representing the column name of observations.")
  }

  # Make sure all the required columns are present in prepped_snsr_cal_df
  required_cols <- c(obs_column, "site", "parameter", "DT_round", "sonde_serial", "sensor_serial",
                     "file_date", "sonde_date", "sensor_date_lag", "sensor_date", "sensor_date_lead",
                     "correct_calibration",
                     "slope_lag", "offset_lag",
                     "slope", "offset",
                     "slope_lead", "offset_lead", "slope_final", "offset_final")

  if (!all(required_cols %in% colnames(prepped_snsr_cal_df))) {
    missing_cols <- required_cols[!required_cols %in% colnames(prepped_snsr_cal_df)]
    stop(paste0("The following required columns are missing from prepped_snsr_cal_df: ",
                paste(missing_cols, collapse = ", ")))
  }


  # Identify sensor parameter type for calibration pathway selection ----
  parameter <- unique(prepped_snsr_cal_df$parameter)

  # Calculate temporal weights for calibration interpolation ====
  chunk_w_time <- prepped_snsr_cal_df %>%
    cal_wt(df = ., dt_col = "DT_round")

  #create obs_raw_col which will match the outputs of `cal_inv_lm()` and `cal_lin_trans_inv_lm_pH()`
  obs_column_raw <- paste0(obs_column, "_raw")
  # Apply temporally-weighted linear transformation between calibrations ====
  # Standard sensor calibration pathway
  if (parameter %in% c("Chl-a Fluorescence", "FDOM Fluorescence", "ORP", "Pressure", "Specific Conductivity", "DO", "Turbidity")) {
    back_calibrated_chunk <- chunk_w_time %>%
      cal_inv_lm(
        df = .,
        obs_col = obs_column,
        slope_col = "slope",
        offset_col = "offset"
      ) %>%
      cal_lin_trans_lm(
        df = .,
        raw_col = obs_column_raw,
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
        obs_col = obs_column,
        slope_col = "slope",
        offset_col = "offset"
      ) %>%
      cal_lin_trans_inv_lm_pH(
        df = .,
        mv_col = obs_column_raw,
        slope_from_col = "slope_final", offset_from_col = "offset_final",
        slope_to_col = "slope_lead", offset_to_col = "offset_lead",
        wt_col = "wt"
      ) %>%
      dplyr::group_by(DT_round) %>%
      dplyr::filter(is.na(!!sym(obs_column)) | (!!sym(obs_column) < 7 & point == 1) | (!!sym(obs_column) >= 7 & point == 2)) %>%
      slice_min(point) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(DT_round)
  }



  # Validate calibration results and create final calibrated values ====

  obs_column_lm_trans <- paste0(obs_column, "_lm_trans")
  obs_column_cal <- paste0(obs_column, "_cal")

  checked_df <- back_calibrated_chunk  %>%
    cal_check(df = ., obs_col = obs_column, lm_trans_col = obs_column_lm_trans)


  # Reorder the final columns to make post hoc analysis easier ====
  final_df <- checked_df %>%
    dplyr::select(
      # DT sensor reading columns
      DT_round,
      # Field ID columns
      site, sonde_serial, parameter,
      # Sensor reading transformation columns
      !!sym(obs_column), !!sym(obs_column_raw), !!sym(obs_column_lm_trans), !!sym(obs_column_cal), cal_check,
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


