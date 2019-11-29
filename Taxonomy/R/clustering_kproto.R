#' @title Wrapper function for kprototypes algorithm
#' @description This function wraps the function \code{kproto.default} and returns a vector of the resulting cluster assignments.
#' @param x \code{data.frame}\cr
#'   dataset to be used for k-prototypes clustering.
#' @param centers \code{integer or data.frame}\cr
#'   Either the number of clusters, a vector specifying indices of initial prototypes, or a data frame of prototypes of the same coloumns as x.
#' @param lambda \code{vector}\cr
#' Parameter > 0 to trade off between Euclidean distance of numeric variables and simple matching coefficient between categorical variables.
#' Also a vector of variable specific factors is possible where the order must correspond to the order of the variables in the data.
#' In this case all variables' distances will be multiplied by their corresponding lambda value.
#' @return \code{clusterCut}\cr
#' Resulting cluster assignment
#' @seealso
#' The reader is referred to the original documentation of the k prototypes clustering algorithm \url{https://cran.r-project.org/web/packages/clustMixType/clustMixType.pdf}
#' @family Clustering
#' @export
clustering_kproto <-
  function (x, centers, lambda) {
    if (!is.null(lambda)) {
      lambda = as.numeric(lambda[colnames(x)])
    } else{
      lambda = rep(1, ncol(x))
    }
    x = logical_to_factor(x)
    x = ordered_to_integer(x)
    k = centers
    res = clustMixType::kproto(x, k, lambda, nstart = 10)
    
    return("clusterCut" = res$cluster)
  }
?kproto
