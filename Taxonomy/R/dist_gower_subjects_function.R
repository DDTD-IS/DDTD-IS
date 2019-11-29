#' @title Wrapper function for the Gower coefficient
#' @description This function wraps the function \code{gowdis} from the package \link[FD]{gowdis} and returns a distance matrix as a result of the application of the
#' Gower coefficient. The reader is referred to the details as provided in \link[FD]{gowdis}.
#' @param data \code{data.frame}\cr
#'   From the original documentation: matrix or data frame containing the variables. Variables can be numeric, ordered, or factor.
#'   Symmetric or asymmetric binary variables should be numeric and only contain 0 and 1.
#'   character variables will be converted to factor. NAs are tolerated.
#' @param asym.bin \code{vector}\cr
#' vector listing the asymmetric binary variables in x
#' @param alwaysGower \code{logical}\cr
#' logical indicating if the Gower coefficient should always be used or not. If TRUE, and only numeric features supplied, then euclidean distances are calculated
#' @param weights \code{vector}\cr
#' vector listing the weights for the variables in x. Can be missing, in which case all variables have equal weights.
#' @return \code{Distance_matrix}\cr
#' Resulting distance matrix
#' @references
#' Podani, J. (1999) Extending Gower's general coefficient of similarity to ordinal characters. Taxon 48:331-340.
#' @family Clustering
#' @export
dist_gower_subjects_function <-
  function(data,
           weights = NULL,
           asym.bin = NULL,
           alwaysGower = FALSE) {
    logical = logical_to_numerical(data)
    data = remove_attributes(data)
    asym.bin = logical[[2]]
    if (!is.null(weights)) {
      weights = as.numeric(weights[colnames(data)])
    } else{
      weights = rep(1, ncol(data))
    }
    dc <- sapply(data, data.class)
    if (all(dc == "numeric") & !alwaysGower)
      Distance_matrix <- dist(data)
    else {
      K <-
        sapply(data[, dc == "factor", drop = FALSE], function(x)
          length(levels(x)))
      bin <- names(K)[K == 2]
      data[, bin] <- sapply(data[, bin], function(x)
        as.numeric(x) -
          1)
      if (any(dc == "logical"))
        data[, dc == "logical"] <- sapply(data[, dc == "logical"],
                                          as.numeric)
      Distance_matrix <-
        FD::gowdis(
          x = data,
          asym.bin = asym.bin,
          w = weights,
          ord = "podani"
        )
    }
    return(Distance_matrix)
  }
