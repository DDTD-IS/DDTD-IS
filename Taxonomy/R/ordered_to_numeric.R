#' @title Simple function converting ordered features to numeric features
#' @description This function takes a data frame as input and returns a data frame that has all initally features of class \code{ordered} converted to features of class \code{numeric}
#' @param dataset \code{data.frame}\cr
#'  The dataframe of which ordered features shall be converted to numeric features
#' @return \code{dataset}\cr
#' A dataset that has all initally features of class \code{ordered} converted to features of class \code{numeric}
#' @family Helper
#' @export
ordered_to_numeric <-
  function(dataset) {
    cols_ordered <-
      apply(matrix(colnames(dataset)), 1, function(x) {
        is.ordered(dataset[, x])
      })
    cols = which(cols_ordered == T)
    for (i in cols) {
      dataset[, i] = structure(
        as.numeric(dataset[, i]),
        class = c("avector", "numeric"),
        comment = comment(dataset[, i])
      )
    }
    return(dataset)
  }
