
#' parsing, filling out and sending form
#' @export
#' @param xml HTML/XML which contains the form. Forms will be parsed by rvest::html_form().
#' @param base_url the base url to send form to
#' @param submit which submit to use. See \link{submit_request}
parse_and_fill_form <- function(xml, base_url="", submit=NULL, ...){
  # extract form information
  form <-
    rvest::html_form(xml)[[1]]

  # filling out form
  form <-
    rvest::set_values(form, ...)

  # preparing form request / choosing submit
  request <-
    submit_request(form, submit=submit)

  # url
  url <-
    xml2::url_absolute(form$url, base_url)

  # form-away / return
  if(request$method == "POST"){
    # form-away
    tmp <-
      httr::POST(url = url, body = request$values, encode = request$encode)

    # return
    return(tmp)
  }else if(request$method == "GET"){
    # form-away
    tmp <-
      httr::POST(url = url, body = request$values, encode = request$encode)

    # return
    return(tmp)
  }else{
    stop("Unknown method; ", request$method)
  }
}

