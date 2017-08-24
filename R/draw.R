#' @title Draw plots and add interactions
#' @description Used to send plots to the browser and its interactivity attached.
#'   You can also draw plots with no interactions if you like.
#' @param pl plot
#' @param target element to attach interactions to
#' @param interactions interactions to attach to target
#' @param new.page specifies drawing on a new web page
#' @import DOM
#' @export

draw <- function(pl, target = NULL, interactions = NULL, new.page = FALSE) {
  svg <- convertToSVG(pl)
  ##send to browser - initial set up:
  if (new.page == TRUE) {

    pageNo <<- DOM::htmlPage()
    assign("pageNo", pageNo, p.env)

    #set up a stylesheet:
    DOM::appendChild(pageNo,
                    DOM::htmlNode('<style type="text/css"></style>'),
                    parent = DOM::css("head"))

    sheets <- DOM::styleSheets(pageNo)
    assign("sheets", sheets, p.env)
    assign("i", 0, p.env)
    #i <<- 0

  }
  pageNo <- p.env$pageNo
  DOM::appendChild(pageNo,
                   child = DOM::svgNode(XML::saveXML(svg)),
                   ns = TRUE)
  ## attach interactions:
  if (!is.null(interactions)) {
    addInteractions(target, interactions)
  }
}

#' addInteractions
#' Function to attach interactions to a single plot object existing on a web page
#' @param target plot object to target
#' @param interactions list of interactions to attach to target
#' @import DOM
#' @export
addInteractions <- function(target, interactions) {
  if (is.null(target)) {
    stop("You need to specify an object to target.")
  }
  pageNo <- p.env$pageNo
  ## screen through interactions that are CSS driven vs JS driven
  valid.int <- validate(interactions)
  ## identify which require setAttribute:
  jsInt <- valid.int$jsInt
  ##find element:
  plotObj <- DOM::getElementById(pageNo,
                                 paste0(target,".1.1"),
                                 response = DOM::nodePtr())
  DOM::setAttribute(pageNo,
                    plotObj,
                    "pointer-events",
                    "all")

  lapply(names(jsInt), function(nm) {
    if(is.function(match.fun(jsInt[[nm]]))) {
      jsInt[[nm]] <- paste0("RDOM.Rcall('", jsInt[[nm]], "', this, ['ptr'], null)")
    }
    DOM::setAttribute(pageNo,
                     plotObj,
                     nm,
                    jsInt[[nm]])
    invisible(NULL)
  })

  ## which are css:
  cssInt <- valid.int$cssInt
  i <- p.env$i
  sheets <- p.env$sheets
  for (cssRule in names(cssInt)) { ## TODO: vectorise
    DOM::setAttribute(pageNo,
                      plotObj,
                      "class",
                      cssRule)
    DOM::insertRule(pageNo, sheets[1], cssInt[[cssRule]], i)
    i <- i + 1

  }
  assign("i", i, p.env)
  invisible(NULL)
}


######## Backend functions:
# Validate interactions - make sure user's input correct event-to-function/css-to-styles in:
validate <- function(interactions) {
  #validate events defined:
  keywords = c("onmouseover", "onmouseout", "onmousemove", "onmousedown", "onmouseup", "onclick",
                  "ondblclick", "onmouseleave", "onselect", "oncontextmenu",
                  #keyboard events
                  "onkeydown", "onkeypress", "onkeyup",
                  #object events?
                  "onabort", "onbeforeload", "onerror", "onhashchange", "onload", "onpageshow",
                  "onresize", "onscroll", "onunload",
                  #form events?
                  "onblur", "onchange", "onfocus", "onfocusin", "oninput", "oninvalid", "onreset",
                  "onsearch", "onselect", "onsubmit",
                  #drag events:
                  "ondrag", "ondragend", "ondragleave", "ondragover", "ondragstart", "ondrop",
                  "ontoggle")
  css.keywords <- c("hover")

  if(!all(names(interactions) %in% c(keywords, css.keywords))) {
    stop("Invalid event defined!")
  }

  jsInt <- interactions[which(names(interactions) %in% keywords)]
  cssInt <- interactions[which(names(interactions) %in% css.keywords)]

  return(list(jsInt = jsInt, cssInt = cssInt))
}

# convert to SVG: converts svg using gridSVG
# convert plot to svg - for R plots:
convertToSVG <- function(x = NULL) {
  ## TODO: check if there really is anything drawn on the current graphics device!
  #do not print - because tags changes from reprinting  - user should call listElements first
  svgall <- gridSVG::grid.export(NULL, exportMappings = "inline", exportCoords = "inline")
  svg <- svgall$svg
  mappings <- svgall$mappings
  coords <- svgall$coords
  gridSVG::gridSVGCoords(coords)
  gridSVG::gridSVGMappings(mappings)
  return(svg)
}
