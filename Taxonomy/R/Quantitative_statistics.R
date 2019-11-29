#' @title Survey statistics for quantitative features
#' @description This function takes the survey data frame as input and produces a summary sheet for each quantitative feature.
#' This sheet includes a table reporting on simple measures: feature, min, max, mean, median, vairance, standard deviation, first quartile, third quartile, interquartilerange.
#' Additionally, a histogram shows the underlying distribution of feature values.
#' @param dataset \code{data.frame}\cr
#'  The survey dataframe for which the statistics shall be computed
#' @family Additional
#' @export
Quantitative_statistics = function(dataset) {
  # Select columns with integer/numeric
  dataset <- ordered_to_integer(dataset)
  cols_numeric <-
    apply(matrix(colnames(dataset)), 1, function(x) {
      is.numeric(dataset[, x])
    })
  cols = which(cols_numeric == T)
  
  # Compute min, max, mean, median, var, sd & quantiles
  Summaries_NI = as.data.frame(
    cbind(
      colnames(dataset[, cols]),
      apply(dataset[, cols], 2, min, na.rm = T),
      apply(dataset[, cols], 2, max, na.rm = T),
      apply(dataset[, cols], 2, mean, na.rm = T),
      apply(dataset[, cols], 2, median, na.rm = T),
      apply(dataset[, cols], 2, var, na.rm = T),
      apply(dataset[, cols], 2, sd, na.rm = T),
      timeSeries::colQuantiles(dataset[, cols], prob = 0.25),
      timeSeries::colQuantiles(dataset[, cols], prob = 0.75),
      timeSeries::colQuantiles(dataset[, cols], prob = 0.75) - timeSeries::colQuantiles(dataset[, cols], prob = 0.25)
    )
  )
  # Change names of columns/rows
  colnames(Summaries_NI) = c("Feature",
                             "Min",
                             "Max",
                             "Mean",
                             "Median",
                             "Variance",
                             "Sd",
                             "Q1",
                             "Q3",
                             "IQR")
  rownames(Summaries_NI) = 1:nrow(Summaries_NI)
  
  # plot histograms for all columns with datatype id or integer
  for (i in 1:length(cols)) {
    # plot table (with customized font size)
    tt <-
      gridExtra::ttheme_default(core = list(fg_params = list(cex = 0.6)),
                                colhead = list(fg_params = list(cex = 0.7)))
    tbl1 <-
      gridExtra::tableGrob(Summaries_NI[i, ], theme = tt, rows = NULL)
    
    p1 <- qplot(dataset[, cols[i]], geom = "histogram", bins = 30)
    p1 <- p1 + xlab("Value range")
    p1 <- p1 + ylab("No. of occurrences")
    p1 <-
      p1 + theme(text = element_text(size = 10), axis.text = element_text(size = 8))
    p1 <-
      p1 + scale_x_continuous(
        labels = function(z)
          format(
            z,
            big.mark = ",",
            decimal.mark = ".",
            scientific = FALSE
          )
      )
    p1 <-
      p1 + scale_y_continuous(
        labels = function(z)
          format(
            z,
            big.mark = ",",
            decimal.mark = ".",
            scientific = FALSE
          )
      )
    
    
    # plot boxplots for all columns with datatype id or integer
    p2 <-
      qplot(paste("column", cols[i], sep = ""), dataset[, cols[i]], geom = "boxplot")
    p2 <- p2 + xlab(colnames(dataset[cols[i]]))
    p2 <- p2 + ylab("Value range")
    p2 <-
      p2 + theme(text = element_text(size = 10), axis.text = element_text(size = 8))
    p2 <-
      p2 + scale_x_discrete(
        labels = function(z)
          format(
            z,
            big.mark = ",",
            decimal.mark = ".",
            scientific = FALSE
          )
      )
    p2 <-
      p2 + scale_y_continuous(
        labels = function(z)
          format(
            z,
            big.mark = ",",
            decimal.mark = ".",
            scientific = FALSE
          )
      )
    
    # Plot table and charts into one object
    p <-
      gridExtra::arrangeGrob(
        tbl1,
        gridExtra::arrangeGrob(p1, p2, ncol = 2, widths = c(2.5, 1)),
        nrow = 2,
        as.table = TRUE,
        heights = c(1, 4)  ,
        top = grid::textGrob(
          paste(c("Meaning: ", comment(dataset[, cols[i]])), collapse = ""),
          check.overlap = T,
          just = "top"
        )
      )
    ggsave(
      filename = paste(
        c(
          "./Output/Images/quantitative_variables/",
          cols[i],
          " ",
          colnames(dataset[cols[i]]),
          ".png"
        ),
        collapse = ""
      ),
      plot = p,
      width = 8,
      height = 6
    )
    
  }
  
}
