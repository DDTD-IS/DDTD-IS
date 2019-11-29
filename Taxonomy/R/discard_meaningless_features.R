

#' @title Simple function that excludes meaningless features
#' @description This function simply removes columns that are considered as meaningless. Meaningless features are one
#' sort of inadmissible features as defined in the thesis' document in chapter 2.3.2.3.
#' Those columns or features are not loaded into the shiny application.
#' @param data \code{data.frame}\cr
#' The survey dataset from which non-relevant features are to be discarded
#' @family Helper
#' @export
discard_meaningless_features = function (data) {
  data = data[!colnames(data) %in% c(
    "SERIAL",
    "CD13_01",
    "MAILSENT",
    "CASE",
    "REF",
    "QUESTNNR",
    "MODE",
    "STARTED",
    "CD13",
    "TIME001",
    "TIME002",
    "TIME003",
    "TIME004",
    "TIME005",
    "TIME_SUM",
    "LASTDATA",
    "FINISHED",
    "Q_VIEWER",
    "LASTPAGE",
    "MAXPAGE",
    "MISSING",
    "MISSREL",
    "TIME_RSI",
    "DEG_TIME"
  )]
}