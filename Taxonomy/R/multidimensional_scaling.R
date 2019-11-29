#' @title Multidimensional scaling for cluster visualisation
#' @description This function applies the technique of multidimensional scaling (MDS) based on a distance matrix. The user may pass a cluster assignment, which leads
#' to the MDS result being split accordingly. In other words, the result is either one data frame of \code{k} columns (the new dimensions resulting from applying MDS), or multiple
#' dataframes in a list of data frames depending on the number of clusters from the provided cluster assignment \code{cluster}. For details about the MDS, the reader is referred to
#' \link[stats]{cmdscale}
#' @param distance_matrix \code{distance matrix}\cr
#'   From the original documentation: a distance structure such as that returned by dist or a full symmetric matrix containing the dissimilarities.
#'   character variables will be converted to factor. NAs are tolerated.
#' @param k \code{integer}\cr
#' the maximum dimension of the space which the data are to be represented in; must be in {1, 2, …, n-1}.
#' @param cluster \code{vector}\cr
#' A clustering result according to which the MDS result is being split
#' @return \code{cluster_groups}\cr
#' A list containing either one or multiple data frames (depending on the value of \code{cluster}), that represent the initial distances from the \code{distance matrix}
#' in k-dimensional space
#' @family Clustering
#' @export
multidimensional_scaling <-
  function(distance_matrix,
           cluster = NULL,
           k = 2) {
    if (is.null(cluster)) {
      cluster = rep(1, dim(distance_matrix)[1])
    }
    result_mds = cmdscale(distance_matrix, k = k, eig = TRUE)
    data <- result_mds$points
    # data.frame with data and cluster assignment from clustering result if provided
    data_clusters <- data.frame(data, clusters = cluster)
    data_clusters$clusters = data_clusters$clusters
    data_clusters$clusters_name = paste("Cluster", data_clusters$clusters)
    cluster_groups = split(data_clusters, data_clusters$clusters)
    return(cluster_groups)
  }
