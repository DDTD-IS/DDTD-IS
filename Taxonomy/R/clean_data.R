#' @title Tailored data cleaning function
#' @description This function is tailored to the survey dataset as provided by the University. The reader is refered to chapter 4.2.
#' In summary, this the main tasks are: normalizations, type conversions, aggregations, deletions, feature extractions
#' @param dataset \code{data.frame}\cr
#'   Survey dataset to be cleaned
#' @return \code{data}\cr
#' cleaned survey dataset
#' @family Helper
#' @export
clean_data <-
  function(dataset) {
    data = dataset
    
    remove_NA_columns = function(dataset) {
      dataset = dataset[colSums(!is.na(dataset)) > 0]
      return(dataset)
    }
    
    remove_NA_rows = function(dataset) {
      dataset = dataset[rowSums(!is.na(dataset)) > 0]
      return(dataset)
    }
    
    remove_ZeroVariance_features = function (dataset) {
      dataset = dataset[!caret::nearZeroVar(dataset, saveMetrics = T)$zeroVar]
      return(dataset)
    }
    
    # =====================================================Beginning of section: Type conversions======================================================
    # data$SC07_01 ============================== cast free text character values to numeric values
    data$SC07_01 = structure(
      as.numeric(gsub("[^0-9]", "", c(data$SC07_01))),
      class = c("avector", "numeric"),
      comment = comment(data$SC07_01)
    )
    # replace all occurring NAs with Zeros (0) as in this case NA does most likely not refer to a missing value but to the fact that no warehouses on the level "Warehouses: Central" exist
    data$SC07_01[is.na(data$SC07_01)] = 0
    
    # data$SC07_02 ============================== cast free text character values to numeric values
    data$SC07_02 = structure(
      as.numeric(gsub("[^0-9]", "", c(data$SC07_02))),
      class = c("avector", "numeric"),
      comment = comment(data$SC07_02)
    )
    # replace all occurring NAs with Zeros (0) as in this case NA does most likely not refer to a missing value but to the fact that no warehouses on the level "Warehouses: Regional" exist
    data$SC07_02[is.na(data$SC07_02)] = 0
    
    # data$SC07_03 ============================== cast free text character values to numeric values
    data$SC07_03 = structure(
      as.numeric(gsub("[^0-9]", "", c(data$SC07_03))),
      class = c("avector", "numeric"),
      comment = comment(data$SC07_03)
    )
    # replace all occurring NAs with Zeros (0) as in this case NA does most likely not refer to a missing value but to the fact that no warehouses on the level "Warehouses: Sub-regional" exist
    data$SC07_03[is.na(data$SC07_03)] = 0
    
    # data$SC07_04 ============================== cast free text character values to numeric values
    data$SC07_04 = structure(
      as.numeric(gsub("[^0-9]", "", c(data$SC07_04))),
      class = c("avector", "numeric"),
      comment = comment(data$SC07_04)
    )
    # replace all occurring NAs with Zeros (0) as in this case NA does most likely not refer to a missing value but to the fact that no warehouses on the level "Warehouses: Local" exist
    data$SC07_04[is.na(data$SC07_04)] = 0
    
    # data$GB02_01 ============================== cast free text character values to numeric values
    data$GB02_01 = structure(
      as.numeric(gsub("[^0-9]", "", c(data$GB02_01))),
      class = c("avector", "numeric"),
      comment = comment(data$GB02_01)
    )
    
    # data$BU10 ============================== cast Yes, No to logical values TRUE, FALSE
    data$BU10 = plyr::revalue(data$BU10, c(
      "Yes" = TRUE,
      "No" = FALSE,
      "[NA] Not answered" = NA
    ))
    data$BU10 = structure(
      as.logical(data$BU10),
      class = c("avector", "logical"),
      comment = comment(data$BU10)
    )
    attr(data$BU10, "T") = "Checked"
    attr(data$BU10, "F") = "Not checked"
    
    # data$CD03 ============================== cast Yes:..., No to logical values TRUE, FALSE
    data$CD03 = plyr::revalue(data$CD03, c(
      "Yes:" = TRUE,
      "No" = FALSE,
      "[NA] Not answered" = NA
    ))
    data$CD03 = structure(
      as.logical(data$CD03),
      class = c("avector", "logical"),
      comment = comment(data$CD03)
    )
    attr(data$CD03, "T") = "Checked"
    attr(data$CD03, "F") = "Not checked"
    
    data$SC04 = plyr::revalue(data$SC04, c("[NA] Not answered" = NA))
    data$SC05 = plyr::revalue(data$SC05, c("[NA] Not answered" = NA))
    # =====================================================End of section:Type conversions======================================================
    
    # =====================================================Beginning of section: Feature Extraction ===================================================
    data$SC07 = structure(
      as.numeric(rowSums(
        cbind(data$SC07_01, data$SC07_02, data$SC07_03, data$SC07_04),
        na.rm = T
      )),
      class = c("avector", "numeric"),
      comment = as.character("Overall number of warehouses")
    )
    
    # data$CD08C ============================== extract new feature combining CD08_01, CD08_02, CD08_03, CD08_04. Three possible values: electronic, non-electronix, mixed derived as follows:
    # CD08_01: IT-based (on-demand) = electronic, CD08_02: IT-based (automatic) = electronic, CD08_03: paper-based = non-electronic, CD08_04: personal meetings = non-electronic
    data$CD08C = structure(
      as.character(as.logical(rowSums(
        cbind(data$CD08_01, data$CD08_02), na.rm = T
      ))),
      class = c("avector", "character"),
      comment = as.character("Information sharing mode")
    )
    data$CD08C = plyr::revalue(data$CD08C,
                               c("TRUE" = "electronic", "FALSE" = "non-electronic"))
    data$CD08C = structure(
      as.character(data$CD08C),
      class = c("avector", "character"),
      comment = comment(data$CD08C)
    )
    data$CD08C[(data$CD08_01 |
                  data$CD08_02) &
                 (data$CD08_03 | data$CD08_04)] = "mixed"
    data$CD08C = structure(
      as.factor(data$CD08C),
      class = c("avector", "factor"),
      comment = comment(data$CD08C)
    )
    
    # data$SC03C ============================== extract new feature revaluing SC03
    data$SC03C = as.logical(plyr::revalue(
      data$SC03,
      c(
        "No, it is managed by a logistics service provider" = TRUE,
        "No, it is managed by others:" = TRUE,
        "Yes" = FALSE
      )
    ))
    data$SC03C = structure(
      as.logical(data$SC03C),
      class = c("avector", "logical"),
      comment = as.character("managed by others yes/no")
    )
    
    # data$BU08C ============================== extract new feature revaluing BU08
    data$BU08C = as.logical(plyr::revalue(
      data$BU08,
      c(
        "Yes, rarely" = TRUE,
        "Yes, regularly" = TRUE,
        "No" = FALSE
      )
    ))
    data$BU08C = structure(
      as.logical(data$BU08C),
      class = c("avector", "logical"),
      comment = as.character("competitors yes/No")
    )
    
    # data$BU09C ============================== extract new feature combining BU09_01, BU09_02, BU09_03. Three possible values: preventive, predictive, both derived as follows:
    # First two possbilities ("already applied", "In introduction, first steps") = TRUE; ("planned for the future", "not applicable") = FALSE
    data$BU09C = numeric(length = 85)
    data$BU09C[which(data$BU09_01 == 1 |
                       data$BU09_02 == 1 |
                       data$BU09_02 == 2 |
                       data$BU09_01 == 2)] = "preventive"
    data$BU09C[which(data$BU09_03 == 1 |
                       data$BU09_03 == 2)] = "predictive"
    data$BU09C[which((data$BU09_03 == 1 |
                        data$BU09_03 == 2) &
                       (
                         data$BU09_01 == 1 |
                           data$BU09_02 == 1 |
                           data$BU09_02 == 2 | data$BU09_01 == 2
                       )
    )] = "both"
    data$BU09C[which(data$BU09C == "0")] = "none"
    data$BU09C = structure(
      as.factor(data$BU09C),
      class = c("avector", "factor"),
      comment = "Maintenance type"
    )
    
    # =====================================================End of section:Feature Engineering ===================================================
    
    # =====================================================Beginning of section: Aggregations, Inconsistencies and Deletions======================================================
    
    
    # remove rows with only NAs
    data =  remove_NA_rows(data)
    # remove columns with only NAs
    data =  remove_NA_columns(data)
    # remove Zero Variance features as those do not provide further information to the model but increase the complexity in terms of size
    data = remove_ZeroVariance_features(data)
    # SC03, SC03_03 ============================== Join Attributes: SC03, SC03_03
    data$SC03 = plyr::revalue(
      data$SC03,
      c(
        "No, it is managed by a logistics service provider" = "LSP",
        "Yes" = "Company",
        "No, it is managed by others:" = "Both"
      )
    )
    # data$SC03_03 ============================== Assumption: data$SC03_03[c(4,8,22,31,35)] = "partially LSP", "partly by LSP", "mixed model: self+3PL", "Both: partially ourselves, partially outsourced", "partly managed by LSP" == Both
    data$SC03[c(4, 8, 22, 31, 35)] = "Both"
    # data$SC03_03 ============================== Assumption: data$SC03_03[6] = "own logistics subsidiary" = Company
    data$SC03[6] = "Company"
    # data$SC03_03 ============================== Assumption: data$SC03_03[c(16,25)] = "3PL outsources", "2LSP" = LSP
    data$SC03[c(16, 25)] = "LSP"
    # delete SC03_03 from data.frame
    data = data[!colnames(data) %in% c("SC03_03")]
    
    # GB01_10a ============================== Discard free text field GB01_10a -> 34 NAs and no additional information by "We are 3PL", "Construction, Mining"
    # delete GB01_10a from data.frame
    data = data[!colnames(data) %in% c("GB01_10a")]
    
    # SC02_14a ============================== Discard free text field SC02_14a -> 35 NAs and no additional information by "Inventory planning and holding partly separated"
    # delete SC02_14a from data.frame
    data = data[!colnames(data) %in% c("SC02_14a")]
    
    # SC02 ============================== Recompute SC02, as SC02_13: "Not any" is included in original feature SC02, which is assumed to be missleading
    data$SC02 = structure(
      as.numeric(rowSums(
        cbind(
          data$SC02_01,
          data$SC02_02,
          data$SC02_03,
          data$SC02_04,
          data$SC02_05,
          data$SC02_06,
          data$SC02_07,
          data$SC02_08,
          data$SC02_09,
          data$SC02_10,
          data$SC02_11,
          data$SC02_12
        ),
        na.rm = T
      )),
      class = c("avector", "numeric"),
      comment = comment(data$SC02)
    )
    
    # BU11_04a ============================== Discard free text field BU11_04a -> 33 NAs no reasonable aggregation and no additional information by "performance-based contracts", "On-site spares, expert support level", "Spare parts availability", "location and payment", "N/a"
    # update count BU11 accordingly
    data$BU11[which(!is.na(data$BU11_04a))] = data$BU11[which(!is.na(data$BU11_04a))] - 1
    # delete BU11_04a from data.frame
    data = data[!colnames(data) %in% c("BU11_04a")]
    
    # data$SC07_05 ============================== Assumption: data$SC07_05[33]  = "Global: 115" == Central: 115
    data$SC07[33] = data$SC07[33] + 115
    # delete SC07_05 from data.frame
    data = data[!colnames(data) %in% c("SC07_05")]
    
    
    # data$CD02_14a ============================== Assumption: data$CD02_14a[19]  = "Warehousing" == Inventory holding
    data$CD02_04[19] = TRUE
    # delete CD02_14a from data.frame
    data = data[!colnames(data) %in% c("CD02_14a")]
    
    # CD02 ============================== Recompute CD02, as CD02_13: "Not any" is included in original feature CD02, which is assumed to be missleading
    data$CD02 = structure(
      as.numeric(rowSums(
        cbind(
          data$CD02_01,
          data$CD02_02,
          data$CD02_03,
          data$CD02_04,
          data$CD02_05,
          data$CD02_06,
          data$CD02_07,
          data$CD02_08,
          data$CD02_09,
          data$CD02_10,
          data$CD02_11,
          data$CD02_12
        ),
        na.rm = T
      )),
      class = c("avector", "numeric"),
      comment = comment(data$CD02)
    )
    
    # data$CD03_01 ============================== Assumption: data$CD03_01 -> planned outsourcing not relevant for data analysis, potentially for further ideation later
    # delete CD03_01 from data.frame
    data = data[!colnames(data) %in% c("CD03_01")]
    
    # CD05_04a ============================== Discard free text field CD05_04a -> 33 NAs no reasonable aggregation and no additional information by "performance-based contracts", "On-site spares, expert support level", "Spare parts availability"
    # delete CD05_04a from data.frame
    data = data[!colnames(data) %in% c("CD05_04a")]
    
    # CD05_01-CD05_03 ============================== Assumption: if organisations have connected IT systems to partners (LSP, dealers, repair shops) (CD09_01-CD09_03), they share information with each other
    data$CD05_01[data$CD09_01] = TRUE
    data$CD05_02[data$CD09_02] = TRUE
    data$CD05_03[data$CD09_03] = TRUE
    # recompute CD05 only based on CD05_01-CD05_03
    data$CD05 = structure(
      as.numeric(rowSums(
        cbind(data$CD05_01, data$CD05_02, data$CD05_03), na.rm = T
      )),
      class = c("avector", "numeric"),
      comment = comment(data$CD05)
    )
    
    # CD06_09a ============================== Discard free text field CD06_09a -> 35 NAs no reasonable aggregation and no additional information by "Include supplier" and "Sorry I don't know"
    # delete CD06_09a from data.frame
    data = data[!colnames(data) %in% c("CD06_09a")]
    # delete CD06_09 from data.frame -> logical value indicating if "others" is checked. Since free text field CD06_09a has been regarded as empty/unchecked for every entity -> discard
    data = data[!colnames(data) %in% c("CD06_09")]
    # reduce count for CD06[4] and CD06[47] by 1, since "Include supplier" and "Sorry I don't know" is not regarded as additional information and has been discarded previously
    data$CD06[4] = data$CD06[4] - 1
    data$CD06[47] = data$CD06[47] - 1
    
    # CD07_09a ============================== Discard free text field CD06_09a -> 35 NAs no reasonable aggregation and no additional information by "I don't know" and "I don't know this either"
    data = data[!colnames(data) %in% c("CD07_09a")]
    # delete CD07_09 from data.frame -> logical value indicating if "others" is checked. Since free text field CD07_09a has been regarded as empty/unchecked for every entity -> discard
    data = data[!colnames(data) %in% c("CD07_09")]
    # reduce count for CD07[16] to by 1, since "I don't know" and "I don't know this either" is not regarded as additional information and has been discarded previously
    data$CD07[16] = as.ordered(as.numeric(data$CD07[16] - 1))
    data$CD07[47] = data$CD07[47] - 1
    
    # CD10_01 ============================== Discard free text field CD10_01 -> 10 NAs no reasonable aggregation and no additional information
    # delete CD10_01 from data.frame
    data = data[!colnames(data) %in% c("CD10_01")]
    # =====================================================End of section: Aggregations, Inconsistencies and Deletions======================================================
    
    # =====================================================Beginning of section: More type conversions: Selected factors to ordered factors======================================================
    # data$CD02 ============================== convert data$CD02 to ordered factor
    data$CD02 = structure(
      as.ordered(data$CD02),
      class = c("avector", "ordered"),
      comment = comment(data$CD02)
    )
    # data$CD04 ============================== convert data$CD04 to ordered factor
    data$CD04 = structure(
      as.ordered(data$CD04),
      class = c("avector", "ordered"),
      comment = comment(data$CD04)
    )
    # data$CD05 ============================== convert data$CD05 to ordered factor
    data$CD05 = structure(
      as.ordered(data$CD05),
      class = c("avector", "ordered"),
      comment = comment(data$CD05)
    )
    # data$CD06 ============================== convert data$CD06 to ordered factor
    data$CD06 = structure(
      as.ordered(data$CD06),
      class = c("avector", "ordered"),
      comment = comment(data$CD06)
    )
    # =====================================================End of section:  More type conversions: Selected factors to ordered factors======================================================
    
    # return cleaned and processed data.frame
    return (data)
  }
