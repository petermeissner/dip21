#' getting search page with session
#' @importFrom  dplyr '%>%'
#' @import httr
#' @import stringr
#' @import xml2
#' @import hellno
#' @export
init_search_page <- function(force=FALSE){
  # cache return
  if( !is.null(cache$init_search_page) & !is.null(cache$init_search_page_time) & force==FALSE ){
    if( as.numeric(Sys.time()) - cache$init_search_page_time <= 60 ){
      return(cache$init_search_page)
    }
  }

  # got to search page to get session
  url_search  <-
    "http://dipbt.bundestag.de/dip21.web/bt" %>%
    GET() %>%
    content("text") %>%
    str_extract_all("<a.*?searchProcedures.*?</a>") %>%
    unlist() %>%
    '['(1) %>%
    read_html(url_search) %>%
    xml_find_one("//a") %>%
    xml_attr("href") %>%
    paste0("http://dipbt.bundestag.de",.)

  # get sessioned-link
  search_request <-
    GET(url_search)

  search_page <-
    content(search_request, as="text", encoding = "latin1")

  # check if session is good
  no_session <-
    search_page %>%
    str_detect("Sie wurden vom System abgemeldet")

  # get options
  search_page_parsed <-
    search_page %>%
    read_html()

  # test results
  stopifnot(!no_session)

  # store cache
  cache$init_search_page_time <- as.numeric(Sys.time())
  cache$init_search_page <- search_page_parsed

  # return
  return(search_page_parsed)
}




