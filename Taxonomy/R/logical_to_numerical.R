#' @title Simple function converting logical features to numerical features
#' @description This function takes a data frame as input and returns a data frame that has all initally features of class \code{logical} converted to features of class \code{numeric}
#' @param dataset \code{data.frame}\cr
#'  The dataframe of which logical features shall be converted to numerical features
#' @return \code{dataset}\cr
#' A dataset that has all initally features of class \code{logical} converted to features of class \code{numeric}
#' @family Helper
#' @export
logical_to_numerical <-
  function(dataset) {
    cols_logical <-
      apply(matrix(colnames(dataset)), 1, function(x) {
        is.logical(dataset[, x])
      })
    cols = which(cols_logical == T)
    for (i in cols) {
      dataset[, i] = structure(
        as.numeric(dataset[, i]),
        class = c("avector", "numeric"),
        comment = comment(dataset[, i])
      )
    }
    return(list(dataset, cols))
  }
