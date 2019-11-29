#' @title Principle component analysis for cluster visualisation
#' @description This function applies the technique of principle component analysis (PCA) based on a data frame of mixed data.
#' The reader is referred to the R function \link[PCAmixdata]{PCAmix}, on which the PCA implementation is based. The user may pass a cluster assignment, which leads
#' to the PCA result being split accordingly. In other words, the result is either one data frame of \code{k} columns (the new dimensions resulting from applying PCA), or multiple
#' dataframes in a list of data frames depending on the number of clusters from the provided cluster assignment \code{cluster}. For details about the PCA, the reader is referred to
#' \link[stats]{cmdscale}
#' @param dataframe \code{data.frame}\cr
#' A data frame consisting quantitative as well as qualitative data to be processes by \link[PCAmixdata]{PCAmix}
#' @param weights \code{vector}\cr
#' A vector of weights for both, the quantitative as well as qualitative features
#' @param cluster \code{vector}\cr
#' A clustering result according to which the PCA result is being split
#' @return \code{cluster_groups}\cr
#' A list containing either one or multiple data frames (depending on the value of \code{cluster}), that represesent the supplied data frame in two-dimensional space.
#' @return \code{dims}\cr
#' Additionally, a vector containing the eigenvalues of the first two principle components is returned, indicating the explained variance of the first components.
#' @family Clustering
#' @export
pca_clustering <-
  function(cluster, dataframe, weights = NULL) {
    dataframe_pca = logical_to_factor(dataframe)
    dataframe_pca = ordered_to_integer(dataframe)
    split = PCAmixdata::splitmix(remove_attributes(dataframe_pca))
    X1 <- split$X.quanti
    X2 <- split$X.quali
    if (!is.null(weights)) {
      weight.col.quanti = as.numeric(weights[colnames(X1)])
      weight.col.quali = as.numeric(weights[colnames(X2)])
    } else{
      weight.col.quanti = NULL
      weight.col.quali = NULL
    }
    result_pca = PCAmixdata::PCAmix(
      X.quanti = X1,
      X.quali = X2,
      ndim = 2,
      weight.col.quanti = weight.col.quanti,
      weight.col.quali = weight.col.quali,
      rename.level = T
    )
    data = result_pca$scores
    colnames = c("X1", "X2")
    
    colnames(data) = colnames
    # data.frame with data and cluster assignment from clustering result
    data_clusters <- data.frame(data, clusters = cluster)
    # create appropriate variable to be handled by geom_point
    data_clusters$clusters = data_clusters$clusters
    data_clusters$clusters_name = paste("Cluster", data_clusters$clusters)
    # Data frame with cluster assignment 
    cluster_groups = split(data_clusters, data_clusters$clusters)
    dims = round(as.numeric(result_pca$eig[, 2][1:2]), 2)
    return(list(cluster_groups, dims))
    
  }
