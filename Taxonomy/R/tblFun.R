

#' @title Simple function to compute percentage statisticss
#' @description This function extends the functionality of the built-in \link[base]{table} function as it adds the relative proportions of the features compared to all other features
#' @param x \code{vector}\cr
#' Vector with qualitative feature values, such as feature of class \code{factor}\cr
#' @return \code{res}\cr
#' Table reporting on value counts and corresponding relative proportions
#' @return \code{selected_cols_aggregated}\cr
#' A vector containing selected features (aggregated over all considered principle component)
#' @family Helper
#' @export
tblFun <-
  function(x) {
    tbl <- table(x)
    res <- cbind(tbl, round(prop.table(tbl) * 100, 2))
    colnames(res) <- c('Count', 'Percentage')
    res
  }
