#' ROSS WQ TOOLS: Quality Assurance and Quality Control for Poudre Sonde Network Data
#'
#' Tools for processing, flagging, and analyzing water quality data
#' from the Fort Collins Watershed monitoring network. This package provides
#' functions for data validation, quality control flagging, and summary statistics.
#' This package contains additional functions not included in it's parent package
#' `fcw.qaqc`
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate arrange select left_join desc n case_when lead
#' @importFrom purrr map2
#' @importFrom stats lm na.omit lag
#' @importFrom utils flush.console
#' @importFrom data.table :=
#' @importFrom rlang sym
#' @importFrom lubridate %within%
#'
#' @name ross.wq.tools
#' @keywords internal
"_PACKAGE"
