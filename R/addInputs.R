#' @title Add an HTML slider
#' @description Add an HTML slider to the page
#' @param name element id of slider
#' @param min minimum value of slider
#' @param max maximum value of slider
#' @param step increment for the slider
#' @import DOM
#' @export
addSlider <- function(name = "slider", min = 0, max = 10, step = 1) {
  pageNo <- p.env$pageNo
  #if we change to SVG tags: remove '.1.1'
  htmlTag <- paste0('<input id ="', paste0(name, '.1.1'),
                    '" type = "range"',
                    'min = "', min,
                    '" max = "', max,
                    '" step = "', step,
                    '" />')
  #append to page
  DOM::appendChild(pageNo,
                   child = DOM::htmlNode(htmlTag),
                   response = DOM::nodePtr())

  # append invisible paragraph to record values:
  DOM::appendChild(pageNo,
                   DOM::htmlNode('<p id="para"></p>'),
                   response = NULL)
}

#' @title showValue (slider only)
#' @description show the value of the slider
#' @param value to pass value of slider (fixed to value when used within a function
#'  that is to be processed back in R)
#' @import DOM
#' @export
showValue <- function(value) {
  pageNo <- p.env$pageNo
  newPara <- DOM::htmlNode(paste('<p id="para">', value, '</p>'))
  DOM::replaceChild(pageNo, newPara, DOM::css("#para"), async=TRUE)
  invisible(NULL)

}

# CALLBACK FUNCTION
#' @title sliderCallback (slider only)
#' @description for passing control of slider through another userdefined function
#' @param f  A user defined function that has an argument called 'value' to pass the value of the slider
#' @import DOM
#' @export
sliderCallback <- function(f) {
  sliderValue <- function(ptr) {
    pageNo <- p.env$pageNo
    value <- DOM::getProperty(pageNo, ptr, "value", async = TRUE, callback = f)
  }
}
