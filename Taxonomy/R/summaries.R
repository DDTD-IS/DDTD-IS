#' @title Survey summary
#' @description This function takes the survey data frame as input and writes a .csv file to the current working directory reporting on data type, column number,
#' number of empty cells, number of NAs, feature comments
#' @param dataset \code{data.frame}\cr
#'  The survey dataframe for which the summary shall be computed
#' @family Additional
#' @export
Summaries = function(dataset, filename) {
  Summary <- data.frame("Attribute" = colnames(dataset))
  Summary$DataType <-  apply(matrix(1:nrow(Summary)), 1, function(x) {
    # handle datatype POSIXct, POSIXt
    if (length(class(dataset[, x])) > 2) {
      d = class(dataset[, x])[2]
      paste(d[1], d[2], sep = ",")
    } else{
      d = class(dataset[, x])[2]
    }
  })
  
  # Count column numbers of summary
  Summary$ColNumber = 1:nrow(Summary)
  
  # Create empty strings counter
  # count all kinds of "empty" strings
  Summary$NoOfEmptyStrings <-
    apply(dataset, 2, function (x)
      sum(grepl("^\\s+$|^$", x)))
  # count number of NAs
  Summary$NoOfNA <- apply(dataset, 2, function (x)
    sum(is.na(x)))
  
  # add comments
  Summary$comment <-
    apply(matrix(1:ncol(dataset)), 1, function (x) {
      comment(dataset[, x])
    })
  Summary$comment <-
    vapply(Summary$comment, paste, collapse = ", ", character(1L))
  # Export summary table
  write.table(
    Summary,
    paste("./Output/Summary_",filename, ".csv", sep = ""),
    sep = ";",
    row.names = FALSE
  )
}
