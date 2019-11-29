#' @title Simple function converting ordered features to factor features
#' @description This function takes a data frame as input and returns a data frame that has all initally features of class \code{ordered} converted to features of class \code{factor}
#' @param dataset \code{data.frame}\cr
#'  The dataframe of which ordered features shall be converted to factor features
#' @return \code{dataset}\cr
#' A dataset that has all initally features of class \code{ordered} converted to features of class \code{factor}
#' @family Helper
#' @export
ordered_to_factor <-
  function(dataset) {
    cols_ordered <-
      apply(matrix(colnames(dataset)), 1, function(x) {
        is.ordered(dataset[, x])
      })
    cols = which(cols_ordered == T)
    for (i in cols) {
      dataset[, i] = structure(
        as.factor(dataset[, i]),
        class = c("avector", "factor"),
        comment = comment(dataset[, i])
      )
    }
    return(dataset)
  }
