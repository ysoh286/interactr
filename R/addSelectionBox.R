#' @title Add a linking selection box
#' @description allows user to draw a selection rectangle which captures points within that region
#' Currently only works for (lattice) scatterplots and for a single SVG plot only.
#' @param plotNum the plot number on the page (from left to right, starting from 1)
#' @param el the element id/tag to capture (points only)
#' @param f function name (character value/string) to pass through
#' @import DOM
#' @export
#'
addSelectionBox <- function(plotNum = 1, el, f) {

  ##for now, this is only for attaching to a single SVG only.
  #currently only  works with capturing point index.

  pageNo <- p.env$pageNo

  if(is.null(pageNo)) {
    stop("No page to send to! Start a new page using draw().")
  }

  panel <- findPanel(el)

  ## append a selection box:
  addPolygon("selectRect", panel, class = "selectRect", attrs = list(fill.opacity = "0", pointer.events = "none"))

  DOM::appendChild(pageNo,
                   child = DOM::javascript(paste0("var panelId = '", panel, "';" )))

  DOM::appendChild(pageNo,
                  child = DOM::javascript(paste0("var pointId = '", el, ".1';")))

  ## identify svg:
  DOM::appendChild(pageNo,
                  child = DOM::javascript(paste0("var svg = document.getElementsByTagName('svg')[", plotNum-1,"]")))

  ## attach js - TODO: should only run ONCE.

  file <- readLines(system.file("js", "selection-box.js", package="interactr"))
  file <- gsub("hello", f, file)

  DOM::appendChild(pageNo,
                   child = DOM::javascript(paste(file, collapse = "\n")),
                   response = DOM::nodePtr())

  js <- "svg.addEventListener('mouseup', MouseUp, false);
  svg.addEventListener('mousedown', MouseDown, false);
  svg.addEventListener('mousemove', MouseDrag, false); "

  DOM::appendChild(pageNo,
              child = DOM::javascript(js))

  invisible(NULL)

  # for now this only returns the indexes.
  # could return coordinates.

}

# CALLBACK FUNCTION
#' @title Callback for linking selection box
#' @description returns a function to allow for linking to selection box (index capturing only)
#' @param f function to pass indices of points to when captured
#' @import DOM
#' @export
boxCallback <- function(f) {
  hello <- function(ptr) {
    pageNo <- p.env$pageNo
    ## get indices from data-select:
    index <- DOM::getAttribute(pageNo,
                               ptr,
                              "data-select",
                              async = TRUE,
                              callback = f)
  }
}
