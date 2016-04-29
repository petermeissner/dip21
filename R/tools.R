#' function for browsing content
#' @export
#' @param x the thing to be looked upon
bcont <- function(x, browse=TRUE){
  tf <- tempfile(fileext = ".html")
  writeLines(as.character(x), tf, useBytes = TRUE)
  if(browse) browseURL(tf)
  return(tf)
}

#' make automatically named list
#'
#' ricardo: \link{http://stackoverflow.com/a/21059868/1144966}
named_list <- function(...){
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}


#' pipe operator
#' @export
`%>%` <- dplyr::`%>%`
