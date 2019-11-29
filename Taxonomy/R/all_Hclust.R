#' @title Silhouette recommendation for hierarchical clustering algorithms
#'
#' @description This function takes a distance matrix of class \code{dist} as input and iteratively calculates the Silhouette measure for
#' any potential number of clusters (specified by No_Clusters) and any potential hierarchical cluster algorithm from the set of:
#' "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" , "centroid". The result is a recommendation, indicating
#'  the best objective configuration, i.e. a vector: \code{sil_recommendation},
#' with (1) the algorithm, (2) the number of clusters, (3) the respective Silhouette value
#'
#' @param No_Clusters \code{integer}\cr
#'   Integer value indicating the maximium number of clusters that are considered for calculating the recommendation
#' @param dist \code{distance matrix}\cr
#'   Distance matrix of class \code{dist} that is used as input for hierarchical clustering algorithms
#' @return \code{sil_recommendation}\cr
#' a vector,i.e. the objectively best configuration of: (1) the algorithm, (2) the number of clusters, (3) the respective Silhouette value
#' @family Clustering
#' @export
all_Hclust <-
  function(No_Clusters,  dist) {
    max_sil = 0
    for (j in c(
      "ward.D",
      "ward.D2",
      "single",
      "complete",
      "average",
      "mcquitty",
      "median" ,
      "centroid"
    )) {
      Clusterings_all = numeric()
      Silhouettes_all = numeric()
      for (i in 2:No_Clusters) {
        Clusterings_all = cbind(
          Clusterings_all,
          clustering_hierarchical(
            distance_matrix = dist,
            method = j,
            number_clusters = i
          )$clusterCut
        )
        Silhouettes_all = cbind(Silhouettes_all,
                                Silhouettes(Clusterings_all[, (i - 1)], dist = dist)[[2]])
      }
      max_index = which.max(Silhouettes_all)
      if (Silhouettes_all[max_index] > max_sil) {
        max_sil = Silhouettes_all[max_index]
        sil_recommendation = c(j, max_index + 1, max(Silhouettes_all))
      }
    }
    return(sil_recommendation)
  }
