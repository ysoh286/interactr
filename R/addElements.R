# Be able to add elements to a page (for drawing):
#' @title addPolygon
#' @description add a polygon to the page with certain attributes
#' @param id a name/id for the polygon element
#' @param panel panel/viewport you want to attach this polygon to
#' @param class a class/name to group attributes specified
#' @param attrs a list of attributes
#' @import DOM
#' @export
addPolygon <- function(id, panel, class, attrs = NULL) {
  pageNo <- p.env$pageNo

  # for this to occur: requires page to exist and graphs to be drawn first.
  panelObj <- DOM::getElementById(pageNo,
                             panel,
                             response = DOM::nodePtr())

  newPolygon <- DOM::createElementNS(pageNo,
                                "http://www.w3.org/2000/svg",
                                "polygon")

  DOM::appendChild(pageNo,
                  newPolygon,
                  parent = panelObj,
                  response = DOM::nodePtr())

  DOM::setAttribute(pageNo,
                    newPolygon,
                    "id",
                    paste0(id, ".1.1"))

  DOM::setAttribute(pageNo,
                    newPolygon,
                    "class",
                    class)

  if (!is.null(attrs)) {
    setClasses(className = class, attrs)
  }

  invisible(NULL)
}

#' @title addLine
#' @description add a line/smooth/curve to the page with certain attributes
#' @param id a name/id for the line element
#' @param panelID panel/viewport to attach this line to
#' @param class a class/grouping for specified attributes
#' @param attrs a list of attributes for this line
#' @import DOM
#' @export
addLine <- function(id, panelID, class, attrs = NULL) {
  pageNo <- p.env$pageNo
  # create a new element + set its attributes:
  newLine <- DOM::createElementNS(pageNo,
                               "http://www.w3.org/2000/svg",
                               "polyline")

  #find panel: DEFINE VIEWPORT TO WHERE YOU'D LIKE TO ATTACH TO.
  panel <- DOM::getElementById(pageNo,
                               panelID,
                              response = DOM::nodePtr())
  DOM::appendChild(pageNo,
                  newLine,
                  parent = panel,
                  response = DOM::nodePtr())

  ## set id and classes:
  ## to do: need validation code to make sure ID has not been taken
  DOM::setAttribute(pageNo, newLine, "id", paste0(id, ".1.1"))
  DOM::setAttribute(pageNo, newLine, "class", class)

  if (!is.null(attrs)) {
    setClasses(class, attrs)
  }

  invisible(NULL)
}


## BACK END FUNCTION - trialling css:
setClasses <- function(className, attrs, async = FALSE) {
  #need to validate the attributes.
  names(attrs) <- gsub("[.]", "-", names(attrs))
  vec <- unlist(attrs)
  cssAttrs <- paste0(names(vec), ":", vec, "; ", collapse = "")
  cssRule <- paste0(".", className, " { ", cssAttrs, " }")

  #append cssRule:
  pageNo <- p.env$pageNo
  i <- p.env$i
  sheets <- p.env$sheets
  DOM::insertRule(pageNo,
                  sheets[1],
                  cssRule,
                  async = async,
                  i)
  i <- i + 1
  assign("i", i, p.env)

  invisible(NULL)
}
