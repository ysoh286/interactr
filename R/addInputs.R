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

# add a table:
#' @title Add a table
#' @description Add an HTML table to the page
#' @param name element id of table
#' @param df a data frame/table to be displayed
#' @param col.names.only option to choose if column names are only displayed (T/F)
#' @import DOM
#' @export
addTable <- function(name, df = NULL, col.names.only = FALSE) {
  pageNo <- p.env$pageNo
  
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop(paste("We require the knitr package for this.",
               "Please use install.packages('knitr') to install.",
               sep = "\n"))
  }

  if(is.null(name)) {
  stop("Please give a name to your table!")
  }
  
  if (is.null(df)) {
    tbl <- paste0("<table id = '", name,"'></table>")
  } else if (!is.data.frame(df)) {
  stop("df is not in a data frame format!")
  } else if (col.names.only == TRUE) {
    tbl <- as.character(knitr::kable(colnames(df),
                                    "html",
                                    table.attr = paste0("id = ", "'", name, "'")))
  } else {
    tbl <- as.character(knitr::kable(df,
                                    "html",
                                    table.attr = paste0("id = ", "'", name, "'")))
  }

  DOM::appendChild(pageNo,
              DOM::htmlNode(tbl),
              response = DOM::htmlNode())
  invisible(NULL)
}

# update a table:
#' @title Update a table
#' @description Update an existing table on the page (to be used inside a function)
#' @param name element id of table
#' @param df a data frame/table to be displayed
#' @param col.names.only option to choose if column names are only displayed (T/F)
#' @import DOM
#' @export

updateTable <- function(name, df = NULL, col.names.only = FALSE) {
  pageNo <- p.env$pageNo
  if(is.null(name)) {
  stop("Please give a name to your table!")
  }

  if (!is.data.frame(df)) {
  stop("df is not in a data frame format!")
  }

  if (is.null(df)) {
  DOM::replaceChild(pageNo,
                    DOM::htmlNode(paste("<table id = '", name,"'></table>")),
                    DOM::css(paste0("#", name)),
                    response = DOM::htmlNode())
    invisible(NULL)
  }

  # need a validation step to make sure that element actually exists...

  ktbl <- as.character(knitr::kable(df,
                                    "html", table.attr = paste0("id = ", "'", name, "'")))
  DOM::replaceChild(pageNo,
                    DOM::htmlNode(ktbl),
                    DOM::css(paste0("#", name)),
                    response = DOM::htmlNode(),
                    async = TRUE)
}

#' @title Add text to a page
#' @description Add text to a page(paragraphs only)
#' @param name element id of text
#' @param text text to be put in
#' @param html T/F value - if text is html code or not
#' @import DOM
#' @export
addText <- function(name, text, html = FALSE) {
  pageNo <- p.env$pageNo
  if (is.null(name)) {
    stop("Requires a name!")
  }
  if (!is.character(text)) {
    stop("Please put in text as a string/characters!")
  }
  if(html == TRUE) {
    DOM::appendChild(pageNo,
                    DOM::htmlNode(text),
                    response = DOM::htmlNode())
  } else {
    DOM::appendChild(pageNo,
                     DOM::htmlNode(paste0("<p id = '", name, "'>", text," <p>")),
                     response = DOM::htmlNode())
  }
}