# Setting styles...

#' @title styleHover
#' @description allows user to style a hover interaction
#' @param attrs styling options/attributes (currently fill and fill.opacity)
#' @export
styleHover <- function(attrs) {

  if (!is.list(attrs)) {
    stop("Attributes should be in a list!")
  }

  #generateCSS- should be for all list in attr:
  fill <- paste0("fill:", attrs$fill, "; ")
  fillop <- paste0("fill-opacity:", attrs$fill.opacity, "; ")
  # TODO: vectorise + expand further to other styles in css!

  #temp fix:
  cssRule <- paste0(".hover:hover {", fill, fillop, " pointer-events: all; }")
  return(cssRule)

}

hide <- function(el) {
  ## ideally be able to specify the element to hide
  cssRule <- paste0(".hidden { visibility: hidden; }")
  return(cssRule)
}

show <- function(el) {
  cssRule <- paste0(".show { visibility: visible; }")
  return(cssRule)
}
