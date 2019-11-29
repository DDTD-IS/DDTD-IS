
#' @title Simple function that excludes empirically correlating features
#' @description This function simply removes columns that are empirically correlating to other features.
#' Empirically correlating features are one sort of inadmissible features as defined in the thesis' document in chapter 2.3.2.3.
#' The corresponding correlation matrix can be found in appendix A.
#' Those columns or features are not loaded into the shiny application for the primary data analysis.
#' Empirically correlating features will only be included in the tab "Summary".
#' @param data \code{data.frame}\cr
#' The survey dataset from which non-relevant features are to be discarded
#' @family Helper
#' @export
exclude_empirical_correlating_features = function (data) {
  data = data[!colnames(data) %in% c("BU01",
                                     "BU09_02",
                                     "BU09_03",
                                     "SC11",
                                     "SC07_04")]
}