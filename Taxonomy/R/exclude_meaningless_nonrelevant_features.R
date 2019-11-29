#' @title Simple function that excludes meaningless features
#' @description This function simply removes columns that are considered as meaningless. Meaningless features are one
#' sort of inadmissible features as defined in the thesis' document in chapter 2.3.2.3.
#' Those columns or features are not loaded into the shiny application.
#' @param data \code{data.frame}\cr
#' The survey dataset from which non-relevant features are to be discarded
#' @family Helper
#' @export
exclude_meaningless_nonrelevant_features = function (data) {
  data = data[!colnames(data) %in% c(
    "CD11_01",
    "CD11_02",
    "CD11_03",
    "CD11_04",
    "CD11_05",
    "CD11_06",
    "CD11_07",
    "CD11_08",
    "CD11_09"
  )]
}
