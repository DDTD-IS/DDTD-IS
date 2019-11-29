#' @title Modified navbarPage from shiny
#' @family UI
#' @export
mainPage <-
  function(title,
           ...,
           id = "dashboard",
           thead = NULL,
           header = NULL,
           footer = NULL,
           windowTitle = title) {
    pageTitle = title
    navbarClass = "navbar navbar-default"
    tabs = list(...)
    tabset = shiny:::buildTabset(tabs, "nav navbar-nav", NULL, id)
    containerDiv = div(class = "container", div(class = "navbar-header",
                                                span(class = "navbar-brand", pageTitle)), tabset$navList)
    contentDiv = div(class = "container-fluid")
    if (!is.null(header))
      contentDiv = tagAppendChild(contentDiv, div(class = "row", header))
    contentDiv = tagAppendChild(contentDiv, tabset$content)
    if (!is.null(footer))
      contentDiv = tagAppendChild(contentDiv, div(class = "row", footer))
    bootstrapPage(
      title = windowTitle,
      thead,
      tags$nav(class = navbarClass, role = "navigation", containerDiv),
      contentDiv
    )
    
  }
