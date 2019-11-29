#' @title Decision tree for feature selection
#' @description This function only works for the provided classification data (tree_data.rds). It produces the rpart decision tree
#' that is displayed in chapter 4.2 in the thesis' document. The decision tree is computed based on the identified set of unweighted, relevant features
#' (CD04, CD05, CD06, CD02 and CD08C). The reader is referred to chapter 4.2 for further details.
#' @param dataset \code{tree_data}\cr
#'  The supplied data frame named "tree_data"
#' @family Additional
#' @export
decision_tree = function (tree_data) {
  tree_data[, 6] = as.factor(tree_data[, 6])
  tree_data = ordered_to_integer(tree_data)
  prediction_data = tree_data[, 1:5]
  tree_model = rpart(clusters ~ ., data = tree_data, control = list(cp = -10))
  tree.pred = predict(tree_model, newdata = prediction_data, type = "class")
  pred_error = mean(as.character(tree.pred) != as.character(tree_data$clusters)) *
    100
  rpart.plot::rpart.plot(
    tree_model,
    extra = 102,
    box.palette = "Greys" ,
    type = 3,
    tweak  = 1.2
  )
  
}
