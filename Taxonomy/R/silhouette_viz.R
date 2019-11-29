

#' @title Silhouette display with standard color palette
#' @description This function is based on the function \link[factoextra]{fviz_silhouette}. It visualizes a silhouette object and applies the standard color palette (used for the shiny application)
#' The reader is referred to the original documentation \link[factoextra]{fviz_silhouette}.
#' @family UI
#' @export
silhouette_viz <-
  function (sil.obj,
            label = FALSE,
            return.summary = TRUE,
            ...)
  {
    colors = c(
      "#1F77B4",
      "#FF7F0E",
      "#2CA02C",
      "#D62728",
      "#9575D2",
      "#8C564B",
      "#E377C0",
      "#7F7F7F",
      "#BCBD22",
      "#17BECF"
    )
    if (inherits(sil.obj, c("eclust", "hcut", "pam", "clara",
                            "fanny"))) {
      df <- as.data.frame(sil.obj$silinfo$widths)
    }
    else if (inherits(sil.obj, "silhouette"))
      df <- as.data.frame(sil.obj[, 1:3])
    else
      stop("Don't support an oject of class ", class(sil.obj))
    df <- df[order(df$cluster,-df$sil_width),]
    if (!is.null(rownames(df)))
      df$name <- factor(rownames(df), levels = rownames(df))
    else
      df$name <- as.factor(1:nrow(df))
    df$cluster <- as.factor(df$cluster)
    mapping <-
      aes_string(
        x = "name",
        y = "sil_width",
        color = "cluster" ,
        fill = "cluster"
      )
    p <-
      ggplot(df, mapping) + geom_bar(stat = "identity")  + scale_fill_manual(values =
                                                                               colors) + scale_colour_manual(values = colors) +
      labs(
        y = "Silhouette width Si",
        x = "",
        title = paste0(
          "Clusters silhouette plot ",
          "\n Average silhouette width: ",
          round(mean(df$sil_width),
                2)
        )
      ) + ggplot2::ylim(c(NA, 1)) + geom_hline(
        yintercept = mean(df$sil_width),
        linetype = "dashed",
        color = "red"
      )
    p <- p + theme_minimal()
    p <- ggpubr::ggpar(p, ...)
    if (!label)
      p <-
      p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    else if (label)
      p <- p + theme(axis.text.x = element_text(angle = 45))
    ave <- tapply(df$sil_width, df$cluster, mean)
    n <- tapply(df$cluster, df$cluster, length)
    sil.sum <-
      data.frame(
        cluster = names(ave),
        size = n,
        ave.sil.width = round(ave,
                              2)
      )
    if (return.summary)
      return(list(p, sil.sum))
    
  }
