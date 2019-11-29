#' @title Wrapper function for partitioning around medoids (PAM) algorithm
#' @description This function wraps the function \code{pam} from the package \link[cluster]{pam} and returns a vector of the resulting cluster assignments.
#' @param distance_matrix \code{distance matrix}\cr
#'   Distance matrix of class \code{dist} that is used as input for PAM clustering algorithms
#' @param number_clusters \code{vector}\cr
#' positive integer specifying the number of clusters, less than the number of observations.
#' @return \code{clusterCut}\cr
#' Resulting cluster assignment
#' @seealso
#' The reader is referred to the original documentation of the pam clustering algorithm \url{https://cran.r-project.org/web/packages/cluster/cluster.pdf}
#' @family Clustering
#' @export
clustering_pam <-
  function(distance_matrix, number_clusters) {
    pam_res = pam(x = distance_matrix, diss = TRUE, k = number_clusters)
    clusterCut = pam_res$clustering
    return("clusterCut" = clusterCut)
  }
