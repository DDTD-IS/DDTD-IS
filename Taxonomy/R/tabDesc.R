#' @title Function handling tab descriptions
#' @family UI
#' @export
tabDesc <-
function(x){
    if (!is.na(x)){
      tags$p(class = "tabDesc", x)
    }
    else{
      NULL
    }
  }
