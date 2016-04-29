# this file is a derived work from the rvest package
# written by Hadley Wickham and under copy right of RStudio
# source: https://github.com/hadley/rvest/blob/21066f2407e9d905cd1af360d46ca26c9e6433b3/R/form.R#L279

#' alternative submit_request to rvest's original submit request
submit_request <-
  function (form, submit = NULL)
  {
    submits <- Filter(function(x) {
      identical(tolower(x$type), "submit")
    }, form$fields)

    nsubmits <- Filter(function(x) {
      !identical(tolower(x$type), "submit")
    }, form$fields)

    # if list take name and vakue as inputs
    if (is.list(submit)) {
      submits[[1]]$name  <- names(submit)[1]
      submits[[1]]$value <- submit[[1]]
      submit <- submits[[1]]
    }

    # if character filter by name
    if (is.character(submit)){
      submit <- Filter(function(x){x$name==submit},submits)[[1]]
    }

    # if null choose first
    if (is.null(submit)) {
      submit <- submits[[1]]
      message("Submitting with '", submit$name, "'")
    }

    # handle method
    method <- form$method
    if (!(method %in% c("POST", "GET"))) {
      warning("Invalid method (", method, "), defaulting to GET",
              call. = FALSE)
      method <- "GET"
    }

    # url
    url <- form$url

    # fields
    fields <- nsubmits
    fields[submit$name] <- list(submit)
    fields <- Filter(function(x) length(x$value) > 0, fields)
    values <- rvest::pluck(fields, "value")
    names(values) <- names(fields)

    # return
    list(method = method, encode = form$enctype, url = url,
         values = values)
  }

