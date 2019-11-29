#' @title Simple function for standardizes the survey data
#' @description This function takes the survey data frame as input and returns a z-transformed survey data frame.
#' @param dataset \code{data.frame}\cr
#'  The survey dataframe that needs to be standardized
#' @return \code{dataset}\cr
#' The z-transformed survey data frame.
#' @family Helper
#' @export
standardize_data = function(dataset) {
  # get numeric columns
  cols = unlist(get_numeric(dataset)[2])
  # scale dataset (z-transformation)
  for (x in cols) {
    dataset[, x] = structure((dataset[, x] - mean(dataset[, x], na.rm = T)) / sd(dataset[, x], na.rm = T),
                             class = c("avector", "numeric"),
                             comment = comment(dataset[, x])
    )
  }
  return(dataset)
}
