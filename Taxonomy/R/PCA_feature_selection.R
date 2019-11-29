
#' @title Principle component analysis for feature selection
#' @description This function applies data-driven principle component (PCA) based filtering technique for feature selection. The PCA implementation is based on \link[PCAmixdata]{PCAmix}. The reader is referred to chapter 4.2, for a detailed
#' justification and reasoning of this approach. Features are extracted from a predefined number of principle components either based on an absolute or on a relative measurement of feature contributions.
#'  Feature contributions can be aggregated over all considered principle component or put in an absolute order across components.
#' @param dataframe \code{data.frame}\cr
#' A data frame consisting of quantitative and qualitative features
#' @param weights \code{vector}\cr
#' A vector of weights for both quantitative and qualitative features
#' @param no_components \code{integer}\cr
#' The maximal number of components to be considered
#' @param absolute_relative \code{absolute_relative = c("absolute", "relative")}\cr
#' a character indicating whether features are extracted based on an absolute or on a relative measurement of feature contributions.
#' @param no_abs_features \code{integer}\cr
#' Integeger value defining the number of principle components from which features can be potentially extracted
#' @return \code{selected_cols}\cr
#' A vector containing selected features (absolute order across components)
#' @return \code{selected_cols_aggregated}\cr
#' A vector containing selected features (aggregated over all considered principle component)
#' @family Clustering
#' @export
PCA_feature_selection <-
  function(dataframe,
           no_components,
           absolute_relative = c("absolute", "relative"),
           no_abs_features = 2,
           weights = NULL) {
    dataframe = logical_to_factor(dataframe)
    dataframe = ordered_to_integer(dataframe)
    
    split = PCAmixdata::splitmix(remove_attributes(dataframe))
    X1 <- split$X.quanti
    X2 <- split$X.quali
    if (!is.null(weights)) {
      weight.col.quanti = as.numeric(weights[colnames(X1)])
      weight.col.quali = as.numeric(weights[colnames(X2)])
    } else{
      weight.col.quanti = NULL
      weight.col.quali = NULL
    }
    
    PCA = PCAmixdata::PCAmix(
      X.quanti = X1,
      X.quali = X2,
      weight.col.quanti = weight.col.quanti,
      weight.col.quali = weight.col.quali,
      ndim = 5,
      rename.level = T
    )
    if (absolute_relative == "absolute") {
      contributions = rbind(PCA$quanti$contrib, PCA$quali$contrib)[, 1:no_components]
    } else if (absolute_relative == "relative") {
      contributions = rbind(PCA$quanti$contrib.pct, PCA$quali$contrib.pct)[, 1:no_components]
    }
    if (no_components == 1) {
      contributions_aggregates = as.data.frame(cbind(contributions, "sum" = contributions))
    } else{
      contributions_aggregates = transform(as.data.frame(contributions), sum =
                                             rowSums(contributions))
      
    }
    contributions_aggregates = contributions_aggregates[order(contributions_aggregates$sum, decreasing = T),]
    selected_cols_aggregated = rownames(contributions_aggregates[1:no_abs_features,])
    selected_cols = rep(rownames(as.data.frame(contributions)), no_components)[order(contributions, decreasing = T)]
    selected_cols = unique(selected_cols)
    selected_cols = selected_cols[1:no_abs_features]
    return(list(selected_cols, selected_cols_aggregated))
  }
