#=================================================Front Page=================================================
#' @title User interface for front page
#' @description This function implements all UI elements under the tab "Start". Implementations are based on the functionality provided by
#' the shiny R package and shinydashboard R package.
#' @family UI
#' @export
Start <-
  function()
    div(
      class = "frontp",
      # front banner
      div(class = "front-banner",
          div(class = "imgcon"),
          div(
            class = "hcon", h1("Interactive Taxonomy Development for Spare Parts Supply Chains")
          )),
      tags$p(
        class = "intro", "The purpose of this application is to enable an interactive taxonomy development for spare parts supply chains based on cluster analysis techniques.
        "
      ),
      HTML("<br>"),
      tags$p(
        class = "intro", "The underlying data comes from a comprehensive company survey that has been carried out
        with spare parts logistics and services experts of manufacturing companies."
      ),
      HTML("<br>"),
      tags$p(
        class = "intro", "The application is structured into three main tabs, namely, Taxonomy, Summary, and Help."
      ),
      tags$li(
        tags$p(
          class = "intro", span(class = "bold", "Taxonomy:"), "This tab provides the user with the main clustering functionalities. The clustering results are presented in a visual manner."
        )
      ),
      tags$li(
        tags$p(
          class = "intro", span(class = "bold", "Summary:"), "Based on a particular clustering solution, the summary tab provides the user with comprehensive descriptive summaries."
        )
      ),
      tags$li(
        tags$p(
          class = "intro", span(class = "bold", "Help:"), "This tab contains some more details on how to use the various functionalities of the application."
        )
      )
    )
