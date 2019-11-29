#' @title Survey statistics for qualitative features
#' @description This function takes the survey data frame as input and produces a summary sheet for each qualitative feature.
#' This sheet includes a table reporting on simple measures: feature, number of nominal levels, number of entries, ratio between number of levels and number of entries (rows), number of NAs
#' Additionally, a bar-chart shows the underlying distribution of feature values with associated counts and relative proportions
#' @param dataset \code{data.frame}\cr
#'  The survey dataframe for which the statistics shall be computed
#' @family Additional
#' @export
Qualitative_statistics = function(dataset) {
  dataset = data
  ## Factor levels & Characters
  # Select columns with factor/character as data type
  cols_factor_char <-
    apply(matrix(colnames(dataset)), 1, function(x) {
      is.factor(dataset[, x]) | is.character(dataset[, x])
    })
  cols = which(cols_factor_char == T)
  # exclude cols/attributes with only NAs
  cols = cols[!as.data.frame(apply(dataset[cols], 2, function (x)
    sum(is.na(x))))[, 1] == nrow(dataset)]
  
  # Compute no. of factor levels
  Summaries_FC = as.data.frame(cbind(
    colnames(dataset[, cols]),
    sapply(cols, function(x)
      nlevels(as.factor(unlist(
        dataset[, x]
      )))),
    nrow(dataset)
  ))
  
  
  # Change names of columns/rows
  colnames(Summaries_FC) = c("Feature", "NoOfLevels", "NoOfEntries")
  rownames(Summaries_FC) = 1:nrow(Summaries_FC)
  
  # Compute ratio of factor levels/no. of entries
  Summaries_FC$NoOfLevels = as.integer(as.character(Summaries_FC$NoOfLevels))
  Summaries_FC$RatioLevelsEntries = Summaries_FC$NoOfLevels / nrow(dataset)
  
  # Count number of NAs
  Summaries_FC$NoOfNAs =  apply(dataset[cols], 2, function (x)
    sum(is.na(x)))
  
  # Count entries for factor levels (for all factors & characters)
  for (i in 1:length(cols)) {
    # plot table (with customized font size)
    tt <-
      gridExtra::ttheme_default(core = list(fg_params = list(cex = 0.6)),
                                colhead = list(fg_params = list(cex = 0.7)))
    tbl2 <-
      gridExtra::tableGrob(Summaries_FC[i, ], theme = tt, rows = NULL)
    
    # Count no. of occurences
    CountFactors = as.data.frame(table(dataset[, cols[i]], useNA = "no"))
    
    # Change colnames & data type
    colnames(CountFactors) = c("Level", "NoOfOccurences")
    CountFactors$NoOfOccurences = as.integer(as.character(CountFactors$NoOfOccurences))
    
    # Reorder levels according to no. of occurences (change order)
    CountFactors = CountFactors[order(CountFactors$NoOfOccurences, decreasing = T), ]
    CountFactors$Level = factor(CountFactors$Level, levels =
                                  CountFactors$Level[order(CountFactors$NoOfOccurences, decreasing = F)])
    
    # Calculate proportion
    CountFactors$Proportion = round(CountFactors$NoOfOccurences / nrow(dataset), 3)
    CountFactors$Level = factor(CountFactors$Level,
                                ordered = T,
                                exclude = NULL)#
    
    
    # Change format (Add big marks (separate 1000s) & convert percentages)
    for (j in 2:3) {
      CountFactors[, j] = as.numeric(CountFactors[, j])
    }
    CountFactors$OccFormated = format(CountFactors$NoOfOccurences,
                                      big.mark = ",",
                                      scientific = FALSE)
    CountFactors$PropFormated = paste(format(round(CountFactors$Proportion *
                                                     100, 1), nsmall = 1), " %", sep = "")
    
    # plot frequency
    p3 <-  ggplot(data = CountFactors)
    p3 <-
      p3 + geom_bar(aes(x = Level, y = NoOfOccurences),
                    stat = "identity",
                    position = "dodge")
    p3 <-
      p3 + geom_label(
        aes(
          x = nrow(CountFactors):1 + 0.01,
          y = NoOfOccurences + 0.01 * max(NoOfOccurences),
          label = OccFormated
        ),
        hjust = 0,
        vjust = 0,
        size = 2.3
      )
    p3 <-
      p3 + geom_label(
        aes(
          x = nrow(CountFactors):1 - 0.01,
          y = NoOfOccurences + 0.01 * max(NoOfOccurences),
          label = PropFormated
        ),
        hjust = 0,
        vjust = 1,
        size = 2.3
      )
    p3 <- p3 + coord_flip()
    p3 <- p3 + xlab("Factor")
    p3 <- p3 + ylab("No. of occurrences")
    p3 <-
      p3 + theme(text = element_text(size = 10), axis.text = element_text(size = 8))
    p3 <-
      p3 + scale_y_continuous(
        labels = function(z)
          format(
            z,
            big.mark = ",",
            decimal.mark = ".",
            scientific = FALSE
          ),
        limits = c(0, max(CountFactors$NoOfOccurences) * 1.1)
      )

    # Plot table and chart into one object
    p <-
      gridExtra::arrangeGrob(
        tbl2,
        p3,
        nrow = 2,
        as.table = TRUE,
        heights = c(1, 4),
        top = grid::textGrob(paste(c(
          "Meaning: ", comment(dataset[, cols[i]])
        ), collapse = ""), just = "top")
      )
    ggsave(
      filename = paste(
        c(
          "./Output/Images/qualitative_variables/",
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
