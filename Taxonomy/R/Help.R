#=================================================Help Page=================================================
#' @title User interface for help page
#' @description This function implements all UI elements under the tab "Help". Implementations are based on the functionality provided by
#' the shiny R package and shinydashboard R package.
#' @family UI
#' @export
Help <-
  function(){
    div(
      class = "frontp",
      h3("Taxonomy"),
      div(
        tags$img(class = "help-pngs", src = "overview.png")
        ),
        tags$p(
          "As the overview shows, there are four different boxes that require the user to make certain
          choices. The two remaining boxes are visual representations and summaries of a particular clustering result.
          Each box will be explained in more detail throughout this help page."
        ),
        h4("Feature Selection"),
        tags$p(
          "The feature selection box provides a server side input logic, that allows the user to interactively search and add features as part of
          the survey. After a feature has been identified and added, a new clustering solution on the new set of features is calculated"
        ),
      tags$img(class = "help-input", src = "feature_selection.png"),
      h4("Algorithms"),
      tags$p(
        "The user is supposed to selected from hierarchical algorithms (Single, Complete, Average, McQuitty, Ward, Centroid and Median), partitioning around medoids (PAM),
        or k-prototypes. PAM and all hierarchical algorithms work directly on a distance matrix. So far the user can only choose the so-calles Gower distance coefficient.
        Besides the algorithm selection, the user can interactively change the number of desired clusters by adjusting the slider input accordingly."
      ),
      tags$img(class = "help-input", src = "algorithm.png"),
      h4("Recommendation"),
      tags$p(
        "For hierarchical algorithms as well as for PAM, a recommendation based on the silhouette measure is calculated, which indicates the objectively 'best'
        number of clusters. If the user clicks on 'Update', the respective recommendet solution is visualised."
      ),
      tags$img(class = "help-input", src = "recommendation.png"),
      h4("Improvement"),
      tags$p(
        "This functionality searches through all available features for potential improvements. The improvement potential is determined by the silhouette measure again.
        Every additional feature that would increase the current average silhoutte width is identified and presented in a tabular form to the user."
      ),
      tags$img(class = "help-input", src = "improvement.png")
      ,
      h4("Clustering"),
      tags$p(
        "This box provides two visual representations of a particular clustering result, namely, multidimensional scaling (MDS) and principle component analysis (PCA).
        Both techniques are dimensionality reduction techniques and attempt to represent a high dimensional dataset in lower dimensional space."
      ),
      tags$img(class = "help-pngs", src = "MDS_PCA.png"),
      h4("Clustering Summary"),
      tags$p(
        "The last box of the main tab provides further information about a particular clustering result. The table on the left reports feature averages of the single resulting clusters.
        The silhouette display on the right hand side shows  silhouette measures for every single entity (a single bar) and the overall average silhouette width
        (the red dashed line)."
      ),
      tags$img(class = "help-pngs", src = "silhouette.png"),
      h3("Summary"),
        tags$p(
          "The second main tab of the web application is the summary tab. It provides the user with comprehensive descriptive summaries based on a particular clustering solution.
          Besides the average or centroid entity, the hypothetical median as well as the centrotype entity is revealed for every single cluster of a particular solution.
          The user starts the calculation by clicking on 'Update'"
        ),
      div(
        tags$img(class = "help-input", src = "summary_avg.png")
      )
      )
  }