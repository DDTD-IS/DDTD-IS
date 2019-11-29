

#' @title Wrapper function for selectizeInput
#' @description A simple wrapper function that allows selectizeInput with selectize = FALSE
#' @family UI
#' @export
selectizeInputF <-
  function(inputId, ..., label = NULL, options , width)
    shiny:::selectizeInput(inputId,
                           label = label,
                           ...,
                           options = NULL,
                           width = NULL)
