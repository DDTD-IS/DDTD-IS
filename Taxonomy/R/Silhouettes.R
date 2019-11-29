#' @title Wrapper function for silhouette measure computation
#' @description This function wraps the function \link[cluster]{silhouette}. It takes a distance matrix and a clustering solution as input
#'  and returns a list containing summary statistics, i.e. silhouette width/measure for individual clusters and the across all clusters (average silhouette widht/measure)
#'  the resulting cluster assignments based on the passed \code{number_clusters}
#' @param Clustering \code{vector}\cr
#'   A cluster assignment as a result from any clustering algorithm
#' @param dist \code{distance matrix}\cr
#'   Distance matrix of class \code{dist} that is used as input for silhouette measure calculation
#' @return \code{Silhouette}\cr
#' data frame with summary statistics including: cluster number, cluster size, intra-cluster silhouette width/measure
#' @return \code{avg_sil}\cr
#' Overall absolute silhouette width/measure
#' @family Clustering
#' @export
Silhouettes <-
  function(Clustering, dist) {
    sil = cluster::silhouette(x = as.integer(Clustering), dist = dist)
    Silhouette = silhouette_viz(sil)[[2]]
    avg_sil = mean(sil[, 3])
    return(list(Silhouette, avg_sil))
  }
