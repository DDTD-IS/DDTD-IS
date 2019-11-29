#=================================================Summary Tab=================================================
#' @title User interface for summary functionality
#' @description This function implements all UI elements under the tab "Summary". Implementations are based on the functionality provided by
#' the shiny R package and shinydashboard R package.
#' @family UI
#' @export
Summary_ui <-
  function ()
  {
    #ui for dashboard
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(fluidRow(column(
        12,
        column(
          3,
          box(
            width = NULL,
            height = NULL ,
            status = "primary",
            solidHeader = TRUE,
            title = tags$p(style = "font-size:20px; color: #FFFFFF ; font-style: bold;", "Update Datatables"),
            column(
              12,
              
              tags$head(tags$style(
                HTML(
                  '#update_datatables{background-color: #3482B4; font-size: 150%; color:#FFFFFF}'
                )
              )),
              actionButton(inputId = "update_datatables", label = "Update")
            )
          )
        )
      )),
      fluidRow(column(
        12,
        
        column(
          width = 4,
          box(
            width = NULL,
            height = NULL ,
            status = "primary",
            solidHeader = TRUE,
            title = tags$p(style = "font-size:20px; color: #FFFFFF ; font-style: bold;", "Average/Centroid"),
            column(12,
                   DT::dataTableOutput("Average_Centroid"))
          )
        ),
        column(
          width = 4,
          box(
            width = NULL,
            height = NULL ,
            status = "primary",
            solidHeader = TRUE,
            title = tags$p(style = "font-size:20px; color: #FFFFFF ; font-style: bold;", "Hypothetical Median"),
            column(12,
                   DT::dataTableOutput("Hypothetical_Median"))
          )
        ),
        column(
          width = 4,
          box(
            width = NULL,
            height = NULL ,
            status = "primary",
            solidHeader = TRUE,
            title = tags$p(style = "font-size:20px; color: #FFFFFF ; font-style: bold;", "Centrotype"),
            column(12,
                   
                   DT::dataTableOutput("Centrotype"))
          )
        )
        
      )))
    )
    
  }
