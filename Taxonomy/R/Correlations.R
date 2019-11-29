#' @title Correlation heatmap
#' @description This function takes the survey data frame as input and outputs a heatmap visualisation of the correlation matrix (folder: "./Output/Images/correlations/"). The user
#' passes the survey dataset and can specifies a threshold (minimum degree of correlation), a threshold count (number of correlations with other features above the threshold),
#' a p.value (signicifance level for correlation significance test) and a method (either pearson or spearman)
#' @param dataset \code{data.frame}\cr
#'  The survey dataframe for which correations shall be computed
#' @param threshold \code{data.frame}\cr
#'  minimum required degree of correlation to be shown in the heatmap visualisation
#' @param threshold_count \code{data.frame}\cr
#'  required number of correlations with other features above the threshold in order to be shown in the heatmap visualisation
#' @param p.value \code{data.frame}\cr
#'  required signicifance level for correlation significance test to be shown in the heatmap visualisation
#' @family Additional
#' @export
Correlations = function(dataset, threshold = 0, threshold_count = 0, p.value = 0, method = "pearson") {
  Correlation_Selection = function(dataset, threshold = 0, threshold_count = 1, p.value = 0, method = c("pearson", "spearman")){
    dataset = data
    dataset = as.data.frame(get_numeric(dataset)[[1]])
    # Compute pearson or spearman coefficients
    cor <- round(cor(dataset, method = as.character(method), use = "pairwise.complete.obs"),2)
    # perform Significance tests for correlations on reduced dataset
    pvalue = apply(dataset, 2, function(y){
      apply(dataset, 2, function(x) {
        cor.test(x = x, y = y, method = as.character(method))$p.value})
    })
    
    cor_pvalues = data.table::melt(pvalue, na.rm = F)$value
    cor_overview = data.table::melt(cor, na.rm = F)
    cor_overview = cbind(cor_overview, cor_pvalues)
    # remove NAs from data.frame
    cor_overview = na.omit(cor_overview)
    # set correlation coefficient of matching attributes to 0
    cor_overview$value[which(cor_overview$Var1 == cor_overview$Var2)] = 0
    # exclude attributes with correlation coefficient smaller than function argument threshold and pvalue smaller than function argument p.value
    cor_overview = cor_overview[which(abs(cor_overview$value) > threshold),]
    cor_overview = cor_overview[which(abs(cor_overview$cor_pvalues) < p.value),]
    # exclude duplicates, i.e. the same two attributes just switched in order
    dat.sort = t(apply(cor_overview, 1, sort))
    cor_overview = cor_overview[!duplicated(dat.sort),]
    cor_overview = cor_overview[order(cor_overview$value, decreasing = T),]
    # Count no. of occurrences of correllations above 'threshold'
    cor_overview_count = plyr::count(cor_overview, "Var1")
    colnames(cor_overview_count) = c("Var2", "Count1")
    cor_overview_count = merge(cor_overview_count, plyr::count(cor_overview, "Var2"), "Var2", all = T)
    cor_overview_count[is.na(cor_overview_count)] = 0
    cor_overview_count$Count = cor_overview_count$Count1 + cor_overview_count$freq
    cor_overview_count = cor_overview_count[,c("Var2", "Count")]
    cor_overview_count = cor_overview_count[which(cor_overview_count$Count>threshold_count),]
    data_red = dataset[,which(colnames(dataset) %in% cor_overview_count$Var2)]
    cor_red  = round(cor(data_red, method=as.character(method), use = "na.or.complete"),2)
    
    return(list(data_red, cor_overview_count, cor_overview, cor,  cor_red))
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  
  cormatgraphic <- function(cormat, title) {
    cormat1 <- cormat
    # only take half of the matrix
    upper_tri <- get_lower_tri(cormat1)
    
    # data.table::melt the correlation matrix for use with ggplot
    melted_cor <- data.table::melt(upper_tri, na.rm = TRUE)
    
    # plot the matrix
    ggheatmap <- ggplot(melted_cor, aes(Var1, Var2, fill = value))+
      theme_bw()+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "red", high = "#0E9E09", mid = "white",
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name=paste(title, "(label >|.2|)")) +
      theme_minimal()+ # minimal theme so font is not too big
      theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                       size = ncol(cormat)*2, hjust = 1),
            axis.text.y = element_text(size = ncol(cormat)*2))+
      coord_fixed() + xlab(c(""))
    ggcorrmat <- ggheatmap +
      geom_text(aes(Var1, Var2, label = value), data = subset(melted_cor, abs(value)>0.2), color = "black", size = ncol(cormat)/2) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.4, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = ncol(cormat), barheight = ncol(cormat)/4,
                                   title.position = "top", title.hjust = 0.5))
    return(ggcorrmat)
  }
  Reduced_plot = !sum(c(threshold, threshold_count, p.value)) == 0

  if(Reduced_plot == F){
    cor = Correlation_Selection(dataset = dataset, method = method)[[4]]
    if(method == "pearson"){
      label = "Pearson Correlation\n"
      filename = "pearson_full"
    }else if(method == "spearman"){
      label = "Spearman Correlation\n"
      filename = "spearman_full"
    }
    ggcor <- cormatgraphic(cor, label)
    ggsave(filename=paste(c("./Output/Images/correlations/", filename, ".png"), collapse=""), plot = ggcor, width = ncol(ggcor)*2, height = ncol(ggcor)*2)
  }
  else if(Reduced_plot == T) {
    # Plot correlations for reduced number of attributes
    if(method == "pearson"){
      label = "Pearson Correlation\n"
      filename = paste("pearson_red_", threshold, "_", threshold_count, "_", p.value)
    }else if(method == "spearman"){
      label = "Spearman Correlation\n"
      filename = paste("spearman_red_", threshold, "_", threshold_count, "_", p.value)
    }
    
    cor_reduced <- Correlation_Selection(dataset = dataset, threshold = threshold, threshold_count = threshold_count, p.value = p.value, method = method)[[5]]
    ggcor_red <- cormatgraphic(cor_reduced, label)
    ggsave(filename=paste(c("./Output/Images/correlations/", filename, ".png"), collapse=""), plot = ggcor_red, width = ncol(cor_reduced), height = ncol(cor_reduced))

    
  }
  
}