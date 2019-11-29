#' @title Simple function extracting numeric features and indices
#' @description This function takes a data frame as input and returns a list with a reduced data set with only numeric columns and a vector with their index in the
#' original data frame
#' @param dataset \code{data.frame}\cr
#'   The dataframe from which numeric features and indices shall be extracted
#' @return \code{dataset}\cr
#' A reduced data set with only numeric columns
#' @return \code{cols}\cr
#' A vector with the index of all numeric columns as in the original data frame
#' @family Helper
#' @export
get_numeric <-
  function(dataset) {
    cols_numeric <-
      apply(matrix(colnames(dataset)), 1, function(x) {
        is.numeric(dataset[, x]) && !is.ordered(dataset[, x])
      })
    cols = which(cols_numeric == T)
    dataset = dataset[cols]
    return(list(dataset, cols))
  }
