#' @title Simple function converting logical features to factor features
#' @description This function takes a data frame as input and returns a data frame that has all initally features of class \code{logical} converted to features of class \code{factor}
#' @param dataset \code{data.frame}\cr
#'  The dataframe of which logical features shall be converted to factor features
#' @return \code{dataset}\cr
#' A dataset that has all initally features of class \code{logical} converted to features of class \code{factor}
#' @family Helper
#' @export
logical_to_factor <-
  function(dataset) {
    cols_logical <-
      apply(matrix(colnames(dataset)), 1, function(x) {
        is.logical(dataset[, x])
      })
    cols = which(cols_logical == T)
    for (i in cols) {
      dataset[, i] = structure(
        as.factor(dataset[, i]),
        class = c("avector", "factor"),
        comment = comment(dataset[, i])
      )
    }
    return(dataset)
  }
