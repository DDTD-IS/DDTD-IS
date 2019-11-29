#' @title Simple function that removes all attributes associated with a feature
#' @description This function simply removes any attributes from the features of a data frame (comments, class, ...)
#' @param dataset \code{data.frame}\cr
#'  The dataframe of which the attributes shall be removed
#' @return \code{dataset}\cr
#' A dataset without attributes
#' @family Helper
#' @export
remove_attributes <-
  function(dataset) {
    for (i in colnames(dataset)) {
      attr(dataset[, deparse(as.name(i))], "comment") <- NULL
      attr(dataset[, deparse(as.name(i))], "T") <- NULL
      attr(dataset[, deparse(as.name(i))], "F") <- NULL
      if (length(class(dataset[, i])) >= 2) {
        attr(dataset[, deparse(as.name(i))], "class") <-
          class(dataset[, i])[2]
      }
      
    }
    return(dataset)
  }
