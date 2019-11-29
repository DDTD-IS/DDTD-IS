#=================================================Summary Tab=================================================
#' @title Server logic of shiny web application
#' @description This function implements the complete server logic. All calculations (MDS-visualization, PCA visualisation,
#' silhouette measure calculation, silhouette visualization, all table outputs, recommendation calculation, improvement calculation)
#' are handled by this server function. The server function reacts to user inputs as specified in \link[Taxonomy]{Taxonomy_ui} and \link[Taxonomy]{Summary_ui}.
#' @family Server
#' @export
Server_logic = function (input, output, session, data) {
  data = clean_data(data)
  data = discard_meaningless_features(data)
  data = discard_nonrelevant_features(data)
  data_original = data
  data = exclude_meaningless_nonrelevant_features(data)
  data = exclude_empirical_correlating_features(data)
  
  data_standardized = standardize_data(data)
  
  weights = c(
    "CD05" = 2,
    "CD08C" = 3,
    "CD06" = 2,
    "CD04" = 1,
    "SC06_01" = 1,
    "CD02" = 1
  )
  
  weights = c(weights, structure(rep(
    1, ncol(data_standardized) - length(weights)
  ), names = colnames(data_standardized)[!colnames(data_standardized) %in% names(weights)]))
  preselected_num = c("CD05", "CD06", "CD04")
  preselected = c("CD05", "CD08C",  "CD04", "CD06", "CD02")
  dataframe = reactive({
    data.frame(data_standardized)
  })
  observe({
    updateSelectizeInput(
      session,
      'selected_feature_selection',
      selected = preselected,
      choices = colnames(data),
      server = TRUE
    )
  })
  
  observeEvent(input$discard_selected, {
    discard = colnames(clustering()[[4]])[input$clustering_summary_rows_selected]
    if (input$feature_selection == "selected") {
      features = input$selected_feature_selection
      features = features[!features %in% c(discard)]
      updateSelectizeInput(
        session,
        'selected_feature_selection',
        selected = features,
        choices = colnames(dataframe()),
        server = TRUE
      )
    } else if (input$feature_selection == "PCA") {
      features = input$PCA_feature_selection
      features = features[!features %in% c(discard)]
      updateSelectizeInput(
        session,
        'PCA_feature_selection',
        selected = features,
        choices = colnames(dataframe()),
        server = TRUE
      )
    }
  })
  
  # Feature selection logic
  observeEvent(
    c(
      input$algorithm,
      input$distance_measure,
      input$PCA_aggregate_nonaggregated,
      input$absolute_relative,
      input$no_components,
      input$percentage,
      input$no_abs_features
    ),
    {
      if (input$algorithm == "hierarchical") {
        if (input$distance_measure == "Gower distance") {
          selected = preselected
          choices = colnames(dataframe())
          all = choices
        }
      } else if (input$algorithm == "kprototypes") {
        selected = preselected
        choices = colnames(dataframe())
        all = choices
      } else if (input$algorithm == "pam") {
        selected = preselected
        choices = colnames(dataframe())
        all = choices
      }
      if (input$feature_selection == "selected") {
        selected = selected
      } else if (input$feature_selection == "PCA") {
        PCA_selection = PCA_feature_selection(
          dataframe()[, choices],
          no_components = input$no_components,
          absolute_relative = input$absolute_relative,
          no_abs_features = input$no_abs_features,
          weights = weights
        )
        if (input$PCA_aggregate_nonaggregated == "not aggregated") {
          selected = PCA_selection[[1]]
        } else if (input$PCA_aggregate_nonaggregated == "aggregated") {
          selected = PCA_selection[[2]]
        }
      }
      updateSelectizeInput(
        session,
        'all_selected',
        selected = all,
        choices = choices,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        'selected_feature_selection',
        selected = selected,
        choices = choices,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        'PCA_feature_selection',
        selected = selected,
        choices = choices,
        server = TRUE
      )
    }
  )
  
  # Clustering logic
  clustering = reactive({
    if (input$feature_selection == "selected") {
      dataframe = dataframe()[colnames(dataframe()) %in% input$selected_feature_selection]
      data_orig = data[colnames(dataframe()) %in% input$selected_feature_selection]
    } else if (input$feature_selection == "PCA") {
      dataframe = dataframe()[colnames(dataframe()) %in% input$PCA_feature_selection]
      data_orig = data[colnames(dataframe()) %in% input$PCA_feature_selection]
    } else if (input$feature_selection == "all") {
      dataframe = dataframe()[colnames(dataframe()) %in% input$all_selected]
      data_orig = data[colnames(dataframe()) %in% input$all_selected]
    }
    dataframe_all = data.frame(dataframe, data[colnames(data[!colnames(data) %in%
                                                               colnames(dataframe)])])
    dataframe_all_orig = data.frame(data_orig, data_original[colnames(data_original[!colnames(data_original) %in%
                                                                                      colnames(dataframe)])])
    dataframe_all_orig = ordered_to_integer(dataframe_all_orig)
    if (isolate({
      input$algorithm == "hierarchical"
    })) {
      dataframe = na.omit(dataframe)
      dist = dist_gower_subjects_function(dataframe, weights = weights)
      method = input$method
      if(method == ""){
        updateSelectizeInput(session, inputId = "method", selected = "ward.D")
      }
      
      clustering = clustering_hierarchical(dist,
                                           method = method,
                                           number_clusters = input$cluster_numbers)$clusterCut
      recommendation = all_Hclust(5, dist = dist)
    } else if (isolate({
      input$algorithm == "kprototypes"
    })) {
      dataframe = na.omit(dataframe)
      clustering = clustering_kproto(
        x = dataframe,
        centers = input$cluster_numbers,
        lambda = weights
      )
      recommendation = numeric()
      dist = dist_gower_subjects_function(dataframe, weights = weights)
    } else if (isolate({
      input$algorithm == "pam"
    })) {
      dataframe = na.omit(dataframe)
      dist = dist_gower_subjects_function(dataframe, weights = weights)
      pam_result = clustering_pam(distance_matrix = dist,
                                  number_clusters = input$cluster_numbers)
      clustering = pam_result
      recommendation = all_PAM(5, dist = dist)
    }
    cluster_groups_mds = multidimensional_scaling(distance_matrix = dist, cluster = clustering)
    data_orig = ordered_to_integer(data)
    cluster_groups_pca = try(pca_clustering(cluster = clustering,
                                            dataframe = dataframe,
                                            weights = weights))
    summary = data.frame()
    columns = character()
    feature_names = character()
    for (i in 1:length(cluster_groups_mds)) {
      for (j in 1:ncol(dataframe)) {
        feature_names = c(feature_names, comment(dataframe()[, colnames(dataframe)][, j]))
        if (is.numeric(dataframe[, j])) {
          summary[j, i] = round(mean(data_orig[c(rownames(cluster_groups_mds[[i]])), colnames(dataframe)][, j], na.rm = T), 2)
        } else{
          mode = tblFun(data_orig[c(rownames(cluster_groups_mds[[i]])), colnames(dataframe)][, j])
          max_feature = rownames(mode)
          max_mode = which.max(mode[, 1])
          value = paste(as.character(max_feature[max_mode]),
                        "(",
                        as.character(mode[max_mode, 2]),
                        "%)")
          summary[j, i] = value
          
        }
      }
      columns = c(columns, paste("Cluster", i))
    }
    feature_names = unique(feature_names)
    summary = as.data.frame(cbind(feature_names, summary))
    colnames(summary) = c("Comment", columns)
    row.names(summary) = as.character(colnames(dataframe))
    
    list(
      clustering,
      cluster_groups_mds,
      summary,
      dataframe,
      dist,
      dist,
      cluster_groups_pca,
      dataframe_all,
      dataframe_all_orig,
      recommendation
    )
  })
  
  # Summary tab logic
  summary_all = eventReactive(input$update_datatables, {
    data_original = ordered_to_integer(data_original)
    sym_dist = as.matrix(clustering()[[5]], diag = 1)
    clustering_medoids = data.frame(clustering()[[4]], clusters = clustering()[[1]])
    cluster_groups_medoids = split(clustering_medoids, clustering_medoids$clusters)
    medoids_index = numeric()
    index_test = numeric()
    for (i in 1:length(cluster_groups_medoids)) {
      cluster_groups_medoids_single = cluster_groups_medoids[[i]][, 1:ncol(clustering()[[4]])]
      dist_medoids = sym_dist[rownames(cluster_groups_medoids_single), ]
      medoids = as.numeric(names(which.min(rowSums(dist_medoids))))
      medoids_index = c(medoids_index, medoids)
    }
    medoids_all = t(clustering()[[9]][medoids_index, ])
    summary_all = data.frame()
    median_all = data.frame()
    columns_all = character()
    feature_names_all = character()
    for (i in 1:length(clustering()[[2]])) {
      for (j in 1:ncol(clustering()[[9]])) {
        feature_names_all = c(feature_names_all, comment(clustering()[[9]][, colnames(clustering()[[9]])][, j]))
        if (is.numeric(clustering()[[9]][, j])) {
          summary_all[j, i] = round(mean(data_original[c(rownames(clustering()[[2]][[i]])), colnames(clustering()[[9]])][, j], na.rm = T), 2)
          median_all[j, i] = median(data_original[c(rownames(clustering()[[2]][[i]])), colnames(clustering()[[9]])][, j], na.rm = T)
        } else{
          mode = tblFun(data_original[c(rownames(clustering()[[2]][[i]])), colnames(clustering()[[9]])][, j])
          max_feature = rownames(mode)
          max_mode = which.max(mode[, 1])
          mode_value = paste(as.character(max_feature[max_mode]),
                             "(",
                             as.character(mode[max_mode, 2]),
                             "%)")
          summary_all[j, i] = mode_value
          median_all[j, i] = mode_value
        }
      }
      
      columns_all = c(columns_all, paste("Cluster", i))
    }
    feature_names_all = unique(feature_names_all)
    #summary_all
    summary_all = as.data.frame(cbind(feature_names_all, summary_all))
    colnames(summary_all) = c("Comment", columns_all)
    row.names(summary_all) = as.character(colnames(clustering()[[9]]))
    #median_all
    median_all = as.data.frame(cbind(feature_names_all, median_all))
    colnames(median_all) = c("Comment", columns_all)
    row.names(median_all) = as.character(colnames(clustering()[[9]]))
    #medoids_all
    medoids_all = as.data.frame(cbind(feature_names_all, medoids_all))
    colnames(medoids_all) = c("Comment", columns_all)
    clusters = ncol(clustering()[[4]])
    pred_error = numeric()
    tree_model = numeric()
    return(
      list(
        summary_all,
        median_all,
        medoids_all,
        clusters,
        tree_model,
        pred_error,
        data_original
      )
    )
  })
  
  # Improvement logic
  silhouette_improvement = eventReactive(input$calculate_improvement, {
    improvement = numeric()
    withProgress(
      message = 'Looking for improvement',
      value = 0,
      min = 0,
      max = ncol(clustering()[[8]]),
      {
        for (i in colnames(clustering()[[8]][(!colnames(clustering()[[8]]) %in%
                                              colnames(clustering()[[4]]))])) {
          dataframe = data.frame(clustering()[[8]][i], clustering()[[8]][colnames(clustering()[[4]])])
          dataframe = na.omit(dataframe)
          dist = dist_gower_subjects_function(dataframe, weights = weights)
          if (input$algorithm == "hierarchical") {
            recommendation_new = all_Hclust(5, dist = dist)
          } else if (input$algorithm == "pam") {
            recommendation_new = all_PAM(5, dist = dist)
          }
          recommendation = clustering()[[10]]
          if (recommendation_new[3] > recommendation[3]) {
            what = c(i, round(as.numeric(recommendation_new[3]), 2))
            improvement = rbind(improvement, what)
          }
          # Increment the progress bar, and update the detail text.
          incProgress(1, detail = paste(colnames(clustering()[[8]][i])))
        }
      }
    )
    improvement = as.data.frame(improvement)
    row.names(improvement) = c(1:nrow(improvement))
    return(improvement)
  })
  
  # MDS plot output
  output$clustering <- renderPlotly({
    data_clusters = as.data.frame(do.call(rbind.data.frame, clustering()[[2]]))
    colors = c(
      "#1F77B4",
      "#FF7F0E",
      "#2CA02C",
      "#D62728",
      "#9575D2",
      "#8C564B",
      "#E377C0",
      "#7F7F7F",
      "#BCBD22",
      "#17BECF"
    )
    for (i in unique(data_clusters$clusters)) {
      if (i == 1) {
        names = character()
        colors = colors[1:length(unique(data_clusters$clusters))]
        pString <-
          paste(
            "d <-plot_ly(data_clusters, x = ~X1, y = ~X2, mode = 'markers', symbol = ~clusters_name,
            color = ~clusters_name, colors = colors, marker = list(size = 8))
            d <- layout(d, xaxis = list(zeroline = FALSE),yaxis = list(zeroline = FALSE), title = 'MDS Visualization', shapes = list(",
            "list(type = 'circle', xref = 'X1', x0 = min(data_clusters[which(data_clusters$clusters ==",
            eval(i),
            "), 'X1']),
            x1 = max(data_clusters[which(data_clusters$clusters ==",
            eval(i),
            "), 'X1']),
            yref = 'X2', y0 = min(data_clusters[which(data_clusters$clusters ==",
            eval(i),
            "), 'X2']),
            y1 = max(data_clusters[which(data_clusters$clusters ==",
            eval(i),
            "), 'X2']),
            fillcolor =",
            eval(paste("colors[", i, "],")),
            "line = list(color =",
            eval(paste("colors[", i, "]")),
            "), opacity = 0.2)"
            )
      } else if (i == length(unique(data_clusters$clusters))) {
        pString = paste(
          pString,
          ",list(type = 'circle', xref = 'X1', x0 = min(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X1']),
          x1 = max(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X1']),
          yref = 'X2', y0 = min(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X2']),
          y1 = max(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X2']),
          fillcolor =",
          eval(paste("colors[", i, "],")),
          "line = list(color =",
          eval(paste("colors[", i, "]")),
          "), opacity = 0.2)))"
          )
      } else{
        pString = paste(
          pString,
          ",list(type = 'circle', xref = 'X1', x0 = min(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X1']),
          x1 = max(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X1']),
          yref = 'X2', y0 = min(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X2']),
          y1 = max(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X2']),
          fillcolor =",
          eval(paste("colors[", i, "],")),
          "line = list(color =",
          eval(paste("colors[", i, "]")),
          "), opacity = 0.2)"
          )
      }
      
    }
    eval(parse(text = pString))
    
  })
  
  # PCA plot output
  output$clustering2 <- renderPlotly({
    data_clusters = as.data.frame(do.call(rbind.data.frame, clustering()[[7]][[1]]))
    colors = c(
      "#1F77B4",
      "#FF7F0E",
      "#2CA02C",
      "#D62728",
      "#9575D2",
      "#8C564B",
      "#E377C0",
      "#7F7F7F",
      "#BCBD22",
      "#17BECF"
    )
    
    for (i in unique(data_clusters$clusters)) {
      if (i == 1) {
        names = character()
        colors = colors[1:length(unique(data_clusters$clusters))]
        pString <-
          paste(
            "d <-plot_ly(data_clusters, x = ~X1, y = ~X2, mode = 'markers', symbol = ~clusters_name,
            color = ~clusters_name, colors = colors, marker = list(size = 8))
            d <- layout(d, xaxis = list(title = 'PC1 (",
            eval(paste(clustering()[[7]][[2]][1])),
            "% )', zeroline = FALSE),
            yaxis = list(title = 'PC2 (",
            eval(paste(clustering()[[7]][[2]][2])),
            "% )', zeroline = FALSE), title = 'PCA Visualization', shapes = list(",
            "list(type = 'circle', xref = 'X1', x0 = min(data_clusters[which(data_clusters$clusters ==",
            eval(i),
            "), 'X1']),
            x1 = max(data_clusters[which(data_clusters$clusters ==",
            eval(i),
            "), 'X1']),
            yref = 'X2', y0 = min(data_clusters[which(data_clusters$clusters ==",
            eval(i),
            "), 'X2']),
            y1 = max(data_clusters[which(data_clusters$clusters ==",
            eval(i),
            "), 'X2']),
            fillcolor =",
            eval(paste("colors[", i, "],")),
            "line = list(color =",
            eval(paste("colors[", i, "]")),
            "), opacity = 0.2)"
            )
      } else if (i == length(unique(data_clusters$clusters))) {
        pString = paste(
          pString,
          ",list(type = 'circle', xref = 'X1', x0 = min(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X1']),
          x1 = max(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X1']),
          yref = 'X2', y0 = min(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X2']),
          y1 = max(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X2']),
          fillcolor =",
          eval(paste("colors[", i, "],")),
          "line = list(color =",
          eval(paste("colors[", i, "]")),
          "), opacity = 0.2)))"
          )
      } else{
        pString = paste(
          pString,
          ",list(type = 'circle', xref = 'X1', x0 = min(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X1']),
          x1 = max(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X1']),
          yref = 'X2', y0 = min(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X2']),
          y1 = max(data_clusters[which(data_clusters$clusters ==",
          eval(i),
          "), 'X2']),
          fillcolor =",
          eval(paste("colors[", i, "],")),
          "line = list(color =",
          eval(paste("colors[", i, "]")),
          "), opacity = 0.2)"
          )
      }
    }
    eval(parse(text = pString))
    
  })
  
  # Silhouette display/plot output
  output$silhouette <- renderPlotly({
    m <- list(
      l = 100,
      r = 50,
      b = 50,
      t = 100,
      pad = 4
    )
    sil = cluster::silhouette(x = clustering()[[1]], dist = clustering()[[6]])
    ggplotly(silhouette_viz(sil, label = FALSE, print.summary = TRUE)[[1]]) %>%
      layout(margin = m)
  })
  
  # Table output summary (Tab 'Taxonomy')
  output$clustering_summary <- DT::renderDataTable({
    clustering()[[3]]
  })
  
  # Table output Average_Centroid (Tab 'Summary')
  output$Average_Centroid <- DT::renderDataTable({
    df = summary_all()[[1]]
    clusters = summary_all()[[4]]
    DT::datatable(df,
                  options = list(
                    lengthMenu = c(10, 50, 120),
                    pageLength = 113,
                    rowCallback = htmlwidgets::JS(
                      'function(row, data, index, rowId) {',
                      'console.log(rowId)',
                      'if(rowId <= ',
                      clusters - 1,
                      ') {',
                      'row.style.backgroundColor = "lightblue";',
                      '}',
                      '}'
                    )
                  ))
    
  })
  
  # Table output Hypothetical_Median (Tab 'Summary')
  output$Hypothetical_Median <- DT::renderDataTable({
    df = summary_all()[[2]]
    clusters = summary_all()[[4]]
    DT::datatable(df,
                  options = list(
                    lengthMenu = c(10, 50, 120),
                    pageLength = 113,
                    rowCallback = htmlwidgets::JS(
                      'function(row, data, index, rowId) {',
                      'console.log(rowId)',
                      'if(rowId <= ',
                      clusters - 1,
                      ') {',
                      'row.style.backgroundColor = "lightblue";',
                      '}',
                      '}'
                    )
                  ))
    
  })
  
  # Table output Centrotype (Tab 'Summary')
  output$Centrotype <- DT::renderDataTable({
    df = summary_all()[[3]]
    clusters = summary_all()[[4]]
    DT::datatable(df,
                  options = list(
                    lengthMenu = c(10, 50, 120),
                    pageLength = 113,
                    rowCallback = htmlwidgets::JS(
                      'function(row, data, index, rowId) {',
                      'console.log(rowId)',
                      'if(rowId <= ',
                      clusters - 1,
                      ') {',
                      'row.style.backgroundColor = "lightblue";',
                      '}',
                      '}'
                    )
                  ))
    
  })
  
  # Update logic of 'Recommendation'
  observeEvent(input$start_calculation, {
    updateSliderInput(session, 'cluster_numbers', value = clustering()[[10]][2])
    if(input$algorithm == "hierarchical"){
      updateSelectizeInput(session, 'method', selected = clustering()[[10]][1])
    }
    
  })
  
  
  # Output of 'Recommendation'
  output$recommendation <- renderText({
    
    paste(
      "The Silhouette measure recommends method:",
      clustering()[[10]][1],
      "and number of clusters:",
      clustering()[[10]][2]
    )
    
    
  })
  
  # Instructions of 'Improvement'
  output$Instruction_improvement <- renderText({
    paste(
      "Click 'Update' in order to search through all features for potential improvements w.r.t the silhouette measure"
    )
  })
  
  # Table output of 'Improvement'
  output$Improvement_summary <- DT::renderDataTable({
    improvement = as.data.frame(silhouette_improvement())
    if (nrow(improvement) == 0) {
      improvement = data.frame("feature" = "no improvement possible", "silhouette" = "....")
      DT::datatable(improvement)
    } else{
      colnames(improvement) = c("feature", "silhouette")
      DT::datatable(improvement)
    }
  })
}