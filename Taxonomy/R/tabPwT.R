#' @title Modified tabs
#' @family UI
#' @export
tabPwT <-
function(title, ...) {
    tabPanel(title,
             div(
               class = "divheader tabTitlePanel",
               #tabTitle(title),
               div(class = "tabTitlePanel-end")
             ),
             ...)
  }
