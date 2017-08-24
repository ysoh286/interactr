#' @title listElements
#' @description Function tries to list elements drawn (currently derived from grid objects)
#' for further reference to add interactions.
#' @param x a plot that can be stored
#' @param prefix to add a prefix to a plot if many are plotted (for lattice plots)
#' @export
listElements <- function(x, prefix = NULL) {
  UseMethod("listElements")
}

#for recorded plots (ie base)
#' @export
listElements.recordedplot <- function(x, prefix = NULL) {

  if (!requireNamespace("gridGraphics", quietly = TRUE)) {
    stop(paste("We require the gridGraphics package for this.",
               "Please use install.packages('gridGraphics') to install.",
               sep = "\n"))
  }

  print(x)
  gridGraphics::grid.echo(prefix = prefix)
  grid::grid.ls()

}

## for lattice plots:
#' @export
listElements.trellis <- function(x, prefix = NULL) {
  print(x, prefix = prefix)
  grid::grid.ls()
}

## for ggplot2:
#' @export
listElements.ggplot <- function(x, prefix = NULL) {
  print(x)
  grid::grid.force()
  grid::grid.ls()
}

## for iNZight plots:
#' @export
listElements.inzplotoutput <- function(x, prefix = NULL) {
  #print(x)
  grid::grid.ls()
}

##other things to try: a javascript graphing library.
