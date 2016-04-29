
#' doing the search
#' @param wahlperiode legislative term use e.g. wahlperioden(18) to get the
#' right value to put into the form
#' @param vorgangstyp type of proceeding; use e.g. vorgangstypen("Gesetzgebung")
#' to get the right values to put in.
#' @param suchwort search term
#' @param nummer "Drucksache" document number related to the proceeding
#' @param force option passed along to init_search_page(), whether or not force
#'  re-initialisation of seeerver session
#' @param res A character vector naming the results to be returned:
#'  nhits - number of procedings found
#'  npages - number of pages that show procedings
#'  trefferliste - lsit of hits with some very basic information
#'  content - the content of the response to the HTTP request send
#' @export
dip21_search <- function(
  wahlperiode = 18,
  vorgangstyp = "Alle",
  nummer      = NULL,
  suchwort    = NULL,
  force       = FALSE,
  res         = c("nhits", "trefferliste")
){
  # init session and send form
  form_response <-
    parse_and_fill_form(
      init_search_page(),
      base_url =  "http://dipbt.bundestag.de",
      submit=list(method="Suchen"),
      wahlperiode = paste(wahlperioden(wahlperiode), collapse = " "),
      vorgangstyp = paste(vorgangstypen(vorgangstyp), collapse = " "),
      suchwort    = paste(suchwort,    collapse = " "),
      nummer      = paste(nummer,    collapse = " ")
    )
  form_content <- httr::content(form_response)

  # store in cache for debugging
  cache$last_request        <-
    form_response
  cache$last_request_fields <-
    form_response$request$options$postfields %>%
    rawToChar() %>%
    strsplit("&") %>%
    unlist()

  # return
  results <- list()
  if("nhits"        %in% res){ results$nhits        <- n_search_results(form_content)       }
  if("page"         %in% res){ results$page         <- page_search_results(form_content)  }
  if("npages"       %in% res){ results$npages       <- npages_search_results(form_content)  }
  if("trefferliste" %in% res){ results$trefferliste <- trefferliste(form_content)           }
  if("content"      %in% res){ results$content      <- form_content                         }
  return(results)
}


#' function for getting trefferliste from
#' @param xml xml from which to extract form for send
trefferliste <- function(xml){
  tmp <-
    parse_and_fill_form(
      xml,
      base_url =  "http://dipbt.bundestag.de",
      submit=list(method="Dateiausgabe Trefferliste"),
      exportType = "csv"
    ) %>%
    httr::content()

  tmp <-
    substring(tmp, gregexpr('"', tmp)[[1]][1], nchar(tmp)) %>%
    textConnection() %>%
    read.table(sep=";", header=TRUE)

  return(tmp)
}


#' function for getting options
#' @export
dip21_search_options <-
  function(){
    # cache return
    if( !is.null(cache$dip21_search_options) ){
      return(cache$dip21_search_options)
    }

    # doing work
    xml <- init_search_page()
    stopifnot( any(class(xml) %in% c("xml_node", "xml_document")) )

    # get selects
    tmp <-
      xml_find_all(xml,"//select")

    # get names
    select_names <-
      tmp %>%
      xml_attr("name")

    # get options
    options <-
      tmp %>%
      lapply(
        function(x){
          x <- xml_find_all(x, xpath="option")
          data.frame(
            value = xml_attr(x, "value"),
            label = xml_text(x)
          )
        }
      )

    # put names and options together
    for(i in seq_along(options) ) {
      options[[i]] <- cbind(options[[i]], select=select_names[i])
    }

    # add names to list items
    names(options) <- select_names

    # store cache
    cache$dip21_search_options <- options

    # return
    return(options)
  }

#' function returning options for legislative terms
#' @param wp a regular expression used to look up labels and return their values
#' @export
wahlperioden <- function(regex=NULL) {
  df <- dip21_search_options()[[1]][,1:2]
  if( is.null(regex) ){
   warning("no value supplied to function, returning options available")
   return(df)
  }
  df$value[grep(regex, df$label, ignore.case = TRUE)]
}

#' function returning options for tpe of proceeding
#' @param vt a regular expression used to look up labels and return their values
#' @export
vorgangstypen  <- function(regex=NULL) {
  df <- dip21_search_options()[[2]][,1:2]
  if( is.null(regex) ){
    warning("no value supplied to function, returning options available")
    return(df)
  }
  df$value[grep(regex, df$label, ignore.case = TRUE)]
}


#' function extractiong number of search result hits
#' @param xml the seach result to extract number from
#' @export
n_search_results <- function(xml){
  xml %>%
    rvest::html_nodes(xpath="//label[contains(text(), 'Treffer')]") %>%
    rvest::html_text() %>%
    stringr::str_extract("\\d*\\)") %>%
    stringr::str_replace("\\)","")  %>%
    as.numeric()
}


#' function extracting number of search result hit pages
#' @param xml the seach result to extract number from
#' @export
npages_search_results <- function(xml){
  xml %>%
    rvest::html_nodes(xpath="//label[contains(text(), 'Treffer')]") %>%
    rvest::html_text() %>%
    stringr::str_extract("von \\d*") %>%
    stringr::str_extract("\\d+")  %>%
    as.numeric()
}

#' function extracting current number of search result hit page
#' @param xml the seach result to extract number from
#' @export
page_search_results <- function(xml){
  xml %>%
    rvest::html_nodes(xpath="//label[contains(text(), 'Treffer')]") %>%
    rvest::html_text() %>%
    stringr::str_extract("\\d* von") %>%
    stringr::str_extract("\\d+")  %>%
    as.numeric()
}

















