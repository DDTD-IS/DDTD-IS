#' @title Simple function converting ordered features to integer features
#' @description This function takes a data frame as input and returns a data frame that has all initally features of class \code{ordered} converted to features of class \code{integer}
#' @param dataset \code{data.frame}\cr
#'  The dataframe of which ordered features shall be converted to integer features
#' @return \code{dataset}\cr
#' A dataset that has all initally features of class \code{ordered} converted to features of class \code{integer}
#' @family Helper
#' @export
ordered_to_integer <-
  function(dataset) {
    cols_integer <-
      apply(matrix(colnames(dataset)), 1, function(x) {
        is.integer(dataset[, x])
      })
    cols = which(cols_integer == T)
    for (i in cols) {
      dataset[, i] = structure((as.integer(dataset[, i]) - 1),
                               class = c("avector", "integer"),
                               comment = comment(dataset[, i])
      )
    }
    return(dataset)
  }
