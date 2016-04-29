#' function giving back details on a dip21_search request
#' @param xml the dip21_search(res="content") result
#' @export
dip21_details <- function(xml){
  if(is.list(xml)){
    xml <- xml$content
  }
  res <- list(xml)
  for( i in seq_len(npages_search_results(xml)-1) ){
    xml <-
      parse_and_fill_form(
        xml,
        base_url =  "http://dipbt.bundestag.de",
        submit=list(method=">")
      ) %>%
      httr::content()
    res[[i+1]] <- xml
    Sys.sleep( abs(rnorm(1,0.37,0.25)) )
  }
  return(res)
}
