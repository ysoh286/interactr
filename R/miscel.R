## Other functions that may be useful: generally for conversion and point change.

#' @title returnRange
#' @description returns the range of a certain element (box, segment)
#' @param el element to determine range from
#' @export
returnRange <- function(el) {
  # requires validation if a grid object is a segment or something else.
  grob <- grid::grid.get(el)
  # sort by class... for now
  if (any(class(grob) == "polygon")) {
    coords <- grid::convertX(grob$x, "native", valueOnly = TRUE)
    max <- max(coords, na.rm = TRUE)
    min <- min(coords, na.rm = TRUE)
    range <- c(min, max)
  } else {
    x <- grid::convertX(grob$x, "native", valueOnly = TRUE)
    y <- grid::convertY(grob$y, "native", valueOnly = TRUE)
    range <- list(x = x, y = y)
  }
  return(range)
}

#' @title convertXY
#' @description convert a set of x and y values to return a set of co-ordinates (svg)
#' @param x x values (native)
#' @param y y values (native)
#' @param panel the panel/viewport that original values lie within
#' @export
convertXY <- function(x, y, panel) {
  #ASSUMES: gridSVG mappings are present (ie draw function must be called first/ runs within RDOM.Rcall)
  #convert coordinates to SVG to draw new polygon:
  svg_x <- gridSVG::viewportConvertX(panel, x, "native")
  svg_y <- gridSVG::viewportConvertY(panel, y, "native")
  # make pt string:
  pt <-  paste(svg_x, svg_y, sep = ",", collapse = " ")
  return(pt)
}

#' @title setPoints
#' @description allows setting attributes/styles of points over a range/index values
#' @param el element/class group to control
#' @param type either an index of points (row observations), or a string of coordinates to plot ("index" or "coords")
#' @param value vector of indices, or a character vector of coordinates
#' @param attrs list of attributes and stylings to apply (optional)
#' @import DOM
#' @export
setPoints <- function(el, type, value, attrs = NULL) {
  pageNo <- p.env$pageNo
# ideally, range could be a 'range' as well, rather than an index value.
  if (type == "index") {

    idTags <- paste0(el, ".1.", value)

    #filter attributes to replace names with period to -:
    names(attrs) <- gsub("[.]", "-", names(attrs))

    setStyles <- function(obj) {
      lapply(names(attrs), function(nm) {
         DOM::setAttribute(pageNo,
                           obj,
                           nm,
                           attrs[[nm]],
                           async = TRUE)
        invisible(NULL)
      })
    }

  #getElements and run the styling:
    sapply(idTags, function(x) {
                    obj <- DOM::getElementById(pageNo,
                                               x,
                                               response = DOM::nodePtr(),
                                               async = TRUE,
                                               callback = setStyles) })

    } else if (type == "coords") {
    newRegion <- DOM::getElementById(pageNo,
                                    paste0(el, '.1.1'),
                                    response = DOM::nodePtr(),
                                    async = TRUE,
                                    callback = function(newRegion) {
                                      DOM::setAttribute(pageNo,
                                                        newRegion,
                                                        "points",
                                                        value,
                                                        async = TRUE)
                                    })
  } else {
    stop("Invalid input type!")
  }
}


#' @title findElement
#' @description Find an element based upon its tag. Used for dealing with systems
#' that do not have a consistent naming scheme such as ggplot2, iNZightPlots.
#' @param tag a part of an element name
#' @export
findElement <- function(tag) {
  if(!is.character(tag)) {
    stop("tag of element must be a character value!")
  }
  return(grid::grid.grep(tag, grep = TRUE)$name)
}

#' @title findPanel
#' @description Find the panel/viewport in which an element resides in
#' @param el element id/or part of a tag
#' @export
findPanel <- function(el) {
    if (!is.character(el)) {
      stop("Element name must be of character value!")
    }
  #find grob:
  grob <- grid::grid.grep(el, grep=TRUE)
  listing <- grid::grid.ls(print=FALSE, view=TRUE)
  panelName <- listing$vpPath[listing$name == grob$name]
  #drop root:
  panelName <- gsub("^ROOT::", "", panelName)
  #plot must be drawn to browser. Must check for gridSVG mappings.
  panel <- tail(gridSVG::getSVGMappings(panelName, "vp"), n = 1)
  return(panel)
}

#' @title computeBars
#' @description Return a set of points for highlighting bar plots/histograms
#' @param el element id/or part of a tag
#' @param panel panel that this element belongs to
#' @param data the dataset to pass through
#' @param var the group variable (that belongs to barplot/histogram)
#' @export
computeBars <- function(el, panel, data, var) {

  if (nrow(data) == 0) {
    pt <- ""
  } else {
    # the table function does not report zeroes! :(
    u <- sort(unique(data[,var]))
    # return sums for each: u should be in the order of the bar plot itself.
    # TODO: sometimes it's not quite right...
    counts <- sapply(u, function(x) {
                        sum(data[,var] == x)
                    })

    pp <- grid::grid.get(el)
    x <- grid::convertX(pp$x, "native", valueOnly = TRUE)
    w <- grid::convertX(pp$width, "native", valueOnly = TRUE)

    #x values: create x co-ordinates
    n <- length(x) * 2
    gg <- numeric(n)
    gg1 <- ifelse((1:n) %% 2 == 0, w, 0)
    gg2 <- rep(x, each = 2) + gg1
    gg3 <- rep(gg2, each = 2)

    # get y values: create y co-ordinates
    ycount <- rep(counts, each = 4)
    ll <- length(ycount)
    y1 <- seq(1, ll, by = 4)
    y2 <- seq(4, ll, by = 4)
    ycount[y1] = 0
    ycount[y2] = 0

    #convert coordinates into svg:
    pt <- convertXY(gg3, ycount, panel)
  }
  return(pt)
}
