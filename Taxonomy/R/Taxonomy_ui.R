#=================================================Taxonomy Tab=================================================
#' @title User interface for interactive clustering
#' @description This function implements all UI elements under the tab "Taxonomy". Implementations are based on the functionality provided by
#' the shiny R package and shinydashboard R package.
#' @family UI
#' @export
#' @import shinydashboard
Taxonomy_ui <-
  function ()
  {
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(fluidRow(
        column(3,
               column(
                 12,
                 box(
                   width = NULL,
                   height = NULL ,
                   status = "primary",
                   solidHeader = TRUE,
                   title = tags$p(style = "font-size:20px; color: #FFFFFF ; font-style: bold;", "Feature Selection"),
                   selectizeInput(
                     inputId = "feature_selection",
                     label = tags$h3("Select Features", style = "font-size: 14px; color: grey"),
                     choices = structure(
                       c("all" , "selected", "PCA"),
                       .Names = c("All features", "Selected features", "PCA selection")
                     ),
                     selected = "selected"
                   ),
                   conditionalPanel(
                     condition = "input.feature_selection == 'selected'",
                     selectizeInputF(
                       inputId = "selected_feature_selection",
                       NULL,
                       multiple = T,
                       options = list(placeholder = "Enter a feature name or select from list",
                                      searchField = colnames(data))
                     )
                   ),
                   conditionalPanel(
                     condition = "input.feature_selection == 'all'",
                     selectizeInputF(
                       inputId = "all_selected",
                       NULL,
                       multiple = T,
                       options = list(placeholder = "Enter a feature name or select from list",
                                      searchField = colnames(data))
                     )
                   ),
                   conditionalPanel(
                     condition = "input.feature_selection == 'PCA'",
                     selectizeInput(
                       inputId = "absolute_relative",
                       label = tags$h3("Absolute vs. relative contributions", style = "font-size: 12px; color: grey"),
                       choices = structure(
                         c("absolute" , "relative"),
                         .Names = c("Absolute contributions", "Relative contributions")
                       ),
                       selected = "absolute"
                     ),
                     radioButtons(
                       inputId = "PCA_aggregate_nonaggregated",
                       label = tags$h3("Feature selection method", style = "font-size: 12px; color: grey"),
                       choices = c("aggregated", "not aggregated"),
                       selected = c("aggregated")
                     ),
                     conditionalPanel(
                       condition = "input.PCA_aggregate_nonaggregated == 'aggregated' | input.PCA_aggregate_nonaggregated == 'not aggregated'",
                       numericInput(
                         inputId = "no_abs_features",
                         label = "Number of features",
                         value = 4,
                         min = 2
                       )
                     ),
                     numericInput(
                       inputId = "no_components",
                       label = "Considered PCs",
                       value = 2,
                       min = 2
                     ),
                     selectizeInputF(
                       inputId = "PCA_feature_selection",
                       NULL,
                       multiple = T,
                       options = list(placeholder = "Enter a feature name or select from list",
                                      searchField = colnames(data))
                     )
                     
                   )
                 ),
                 box(
                   width = NULL,
                   height = NULL ,
                   status = "primary",
                   solidHeader = TRUE,
                   title = tags$p(style = "font-size:20px; color: #FFFFFF ; font-style: bold;", "Algorithms"),
                   selectizeInputF(
                     inputId = "algorithm",
                     label = tags$h3("Algorithm", style = "font-size: 12px; color: grey"),
                     choices = c("hierarchical", "kprototypes", "pam"),
                     selected = "hierarchical",
                     multiple = F
                   ),
                   conditionalPanel(
                     condition = "input.algorithm == 'hierarchical'",
                     selectizeInputF(
                       inputId = "distance_measure",
                       label = tags$h3("Distance Measure", style = "font-size: 12px; color: grey"),
                       choices = c("Gower distance"),
                       selected = "Gower distance",
                       multiple = F
                     ),
                     selectizeInputF(
                       inputId = "method",
                       label = tags$h3("Clustering method", style = "font-size: 12px; color: grey"),
                       choices = c(
                         "ward.D",
                         "ward.D2",
                         "single",
                         "complete",
                         "average",
                         "mcquitty",
                         "median" ,
                         "centroid"
                       ),
                       selected = "ward.D",
                       multiple = F
                     )
                   ),
                   conditionalPanel(
                     condition = "input.algorithm == 'hierarchical' || input.algorithm == 'kprototypes' || input.algorithm == 'pam'",
                     sliderInput(
                       inputId = "cluster_numbers",
                       label = "Number of clusters",
                       min = 1,
                       max = 10,
                       value = 3,
                       step = 1
                     )
                   )
                   
                   
                 ),
                 conditionalPanel(
                   condition = "input.algorithm == 'hierarchical' | input.algorithm == 'pam'",
                   box(
                     width = NULL,
                     height = NULL ,
                     status = "primary",
                     solidHeader = TRUE,
                     title = tags$p(style = "font-size:20px; color: #FFFFFF ; font-style: bold;", "Recommendation"),
                     tags$head(tags$style(
                       HTML(
                         '#start_calculation{background-color: #3482B4; font-size: 150%; color:#FFFFFF}'
                       )
                     )),
                     tags$head(tags$style(
                       HTML('#recommendation{font-size: 15px; color: black}')
                     )),
                     textOutput(outputId = "recommendation"),
                     actionButton(inputId = "start_calculation", label = "Update")
                   )
                 ),
                 conditionalPanel(
                   condition = "input.algorithm == 'hierarchical' | input.algorithm == 'pam'",
                   box(
                     width = NULL,
                     height = NULL ,
                     status = "primary",
                     solidHeader = TRUE,
                     title = tags$p(style = "font-size:20px; color: #FFFFFF ; font-style: bold;", "Improvement"),
                     tags$head(tags$style(
                       HTML(
                         '#calculate_improvement{background-color: #3482B4; font-size: 150%; color:#FFFFFF; margin-bottom: 5px;}'
                       )
                     )),
                     tags$head(tags$style(
                       HTML('#improvement{font-size: 15px; color: black}')
                     )),
                     
                     
                     actionButton(inputId = "calculate_improvement", label = "Update"),
                     textOutput("Instruction_improvement"),
                     tags$head(
                       tags$style("#Instruction_improvement{color: grey;
                                  font-size: 14px;}")
                       ),
                     DT::dataTableOutput("Improvement_summary")
                     )
                   
                 )
                 )),
        
        column(
          9,
          
          box(
            width = NULL,
            height = NULL ,
            status = "primary",
            solidHeader = TRUE,
            title = tags$p(style = "font - size:20px;
                           color:#FFFFFF ; font-style: bold;", "Clustering"),
            column(
              6,
              plotly::plotlyOutput(outputId = "clustering", height = "400px")
            ),
            column(
              6,
              plotly::plotlyOutput(outputId = "clustering2", height = "400px")
            )
            
            ),
          
          box(
            width = NULL,
            height = NULL ,
            status = "primary",
            solidHeader = TRUE,
            title = tags$p(style = "font-size:20px; color: #FFFFFF ; font-style: bold;", "Clustering Summary"),
            column(
              6,
              DT::dataTableOutput("clustering_summary"),
              tags$head(tags$style(
                HTML(
                  '#discard_selected{background-color: #3482B4; font-size: 150%; color:#FFFFFF}'
                )
              )),
              actionButton(inputId = "discard_selected", label = "Discard selected")
            ),
            column(
              6,
              plotly::plotlyOutput(outputId = "silhouette", height = "500px")
            )
            
            
          )
        )
      ))
      )
    
  }
