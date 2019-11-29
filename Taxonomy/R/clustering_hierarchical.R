#' @title Wrapper function for hierarchical clustering algorithms
#' @description This function wraps the functions \code{hclust} and \code{cutree} and returns a list containing the original clustering as computed by \code{hclust} and
#'  the resulting cluster assignments based on the passed \code{number_clusters}
#' @param number_clusters \code{integer}\cr
#'   Integer value determining the number of clusters used by \code{cutree}, i.e. the clustering result will have as many clusters as specified by \code{number_clusters}
#' @param distance_matrix \code{distance matrix}\cr
#'   Distance matrix of class \code{dist} that is used as input for hierarchical clustering algorithms
#' @param method \code{method = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" , "centroid" )}\cr
#' Selection of the hierarchical clustering algorithm, default is \code{ward.D2}
#' @return \code{clustering}\cr
#' Original clustering, i.e. the standard output from \code{hclust}
#' @return \code{clusterCut}\cr
#' Cluster assignment as the result of applying \code{clusterCut} to the original clustering, i.e. the standard output from \code{hclust}
#' @family Clustering
#' @export
clustering_hierarchical <-
  function(distance_matrix,
           method = "ward.D2",
           number_clusters) {
    clustering = hclust(distance_matrix, method = method)
    clusterCut = cutree(clustering, k = number_clusters)
    return(list("clustering" = clustering, "clusterCut" = clusterCut))
  }
