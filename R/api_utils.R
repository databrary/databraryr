# Internal helpers for interacting with the Databrary Django API.

#' Validate that a value is a single positive integer (e.g. vol_id, record_id).
#' @noRd
assert_positive_integer <- function(x, name = deparse(substitute(x))) {
  assertthat::assert_that(
    length(x) == 1,
    msg = paste(name, "must have length 1")
  )
  assertthat::assert_that(
    is.numeric(x),
    msg = paste(name, "must be numeric")
  )
  assertthat::assert_that(x >= 1, msg = paste(name, "must be >= 1"))
  assertthat::assert_that(
    x == floor(x),
    msg = paste(name, "must be an integer")
  )
  invisible(TRUE)
}

#' @noRd
resp_has_body <- function(response) {
  length(httr2::resp_body_raw(response)) > 0
}

#' @noRd
ensure_leading_slash <- function(path) {
  assertthat::assert_that(assertthat::is.string(path))
  if (startsWith(path, "/")) {
    path
  } else {
    paste0("/", path)
  }
}

#' @noRd
build_query_params <- function(params) {
  if (length(params) == 0) {
    return(NULL)
  }

  keep <- !vapply(params, is.null, logical(1))
  params <- params[keep]
  lapply(params, function(value) {
    if (is.logical(value)) {
      # API expects lowercase true/false
      tolower(as.character(value))
    } else {
      value
    }
  })
}

#' @noRd
perform_api_get <- function(path,
                            params = list(),
                            rq = NULL,
                            vb = FALSE,
                            parser = NULL,
                            normalize = TRUE,
                            response_type = c("json", "raw", "text")) {
  response_type <- match.arg(response_type)

  request <- rq
  if (is.null(request)) {
    request <- databraryr::make_default_request()
  }

  url <- paste0(DATABRARY_BASE_URL, ensure_leading_slash(path))
  request <- httr2::req_url(request, url)

  query <- build_query_params(params)
  if (!is.null(query) && length(query) > 0) {
    request <- do.call(httr2::req_url_query, c(list(request), query))
  }

  response <- tryCatch(
    httr2::req_perform(request),
    httr2_error = function(cnd) {
      if (vb) {
        message("Request failed for ", url, ": ", conditionMessage(cnd))
      }
      NULL
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  status <- httr2::resp_status(response)
  if (status == 204L || !resp_has_body(response)) {
    return(NULL)
  }

  body <- switch(
    response_type,
    json = {
      payload <- httr2::resp_body_json(response)
      if (isTRUE(normalize)) {
        payload <- snake_case_list(payload)
      }
      payload
    },
    raw = httr2::resp_body_raw(response),
    text = httr2::resp_body_string(response)
  )

  if (!is.null(parser) && is.function(parser)) {
    body <- parser(body)
  }
  body
}

#' @noRd
collect_paginated_get <- function(path,
                                  params = list(),
                                  rq = NULL,
                                  vb = FALSE,
                                  normalize = TRUE) {
  next_url <- paste0(DATABRARY_BASE_URL, ensure_leading_slash(path))
  first_iter <- TRUE
  query <- build_query_params(params)

  aggregated <- list()

  while (!is.null(next_url)) {
    request <- rq
    if (is.null(request)) {
      request <- databraryr::make_default_request(refresh = first_iter)
    }

    request <- httr2::req_url(request, next_url)
    if (first_iter && !is.null(query) && length(query) > 0) {
      request <- do.call(httr2::req_url_query, c(list(request), query))
    }

    resp <- tryCatch(
      httr2::req_perform(request),
      httr2_error = function(cnd) {
        if (vb) {
          message("Request failed for ", next_url, ": ", conditionMessage(cnd))
        }
        NULL
      }
    )

    if (is.null(resp)) {
      return(NULL)
    }

    body <- httr2::resp_body_json(resp)
    if (isTRUE(normalize)) {
      body <- snake_case_list(body)
    }

    page_results <- body$results
    if (is.null(page_results)) {
      if (is.list(body) && length(body) > 0 && (is.null(names(body)) || all(names(body) == ""))) {
        page_results <- body
      } else {
        page_results <- list()
      }
    }

    aggregated <- c(aggregated, page_results)

    next_url <- body[["next"]]
    if (!is.null(next_url) && !startsWith(next_url, "http")) {
      next_url <- paste0(DATABRARY_BASE_URL, ensure_leading_slash(next_url))
    }
    if (!is.null(next_url)) {
      next_url <- sub("^http://", "https://", next_url)
    }

    first_iter <- FALSE
  }

  aggregated
}

#' @noRd
camel_to_snake <- function(x) {
  x <- gsub("(.)([A-Z][a-z]+)", "\\1_\\2", x)
  tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", x))
}

#' @noRd
snake_case_list <- function(obj) {
  if (is.list(obj)) {
    names_list <- names(obj)
    if (!is.null(names_list)) {
      names(obj) <- vapply(names_list, camel_to_snake, character(1))
    }
    obj <- lapply(obj, snake_case_list)
    obj
  } else if (is.vector(obj) && !is.null(names(obj))) {
    names(obj) <- vapply(names(obj), camel_to_snake, character(1))
    obj
  } else {
    obj
  }
}

#' @noRd
perform_api_post <- function(path,
                             body = list(),
                             rq = NULL,
                             vb = FALSE,
                             normalize = TRUE) {
  request <- rq
  if (is.null(request)) {
    request <- databraryr::make_default_request()
  }

  url <- paste0(DATABRARY_BASE_URL, ensure_leading_slash(path))
  request <- httr2::req_url(request, url)
  request <- httr2::req_method(request, "POST")
  
  if (!is.null(body) && length(body) > 0) {
    request <- httr2::req_body_json(request, body)
  }

  response <- tryCatch(
    httr2::req_perform(request),
    httr2_error = function(cnd) {
      if (vb) {
        message("POST request failed for ", url, ": ", conditionMessage(cnd))
      }
      NULL
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  status <- httr2::resp_status(response)
  if (status == 204L || !resp_has_body(response)) {
    return(TRUE)
  }

  payload <- httr2::resp_body_json(response)
  if (isTRUE(normalize)) {
    payload <- snake_case_list(payload)
  }
  payload
}

#' @noRd
perform_api_patch <- function(path,
                              body = list(),
                              rq = NULL,
                              vb = FALSE,
                              normalize = TRUE) {
  request <- rq
  if (is.null(request)) {
    request <- databraryr::make_default_request()
  }

  url <- paste0(DATABRARY_BASE_URL, ensure_leading_slash(path))
  request <- httr2::req_url(request, url)
  request <- httr2::req_method(request, "PATCH")
  
  if (!is.null(body) && length(body) > 0) {
    request <- httr2::req_body_json(request, body)
  }

  response <- tryCatch(
    httr2::req_perform(request),
    httr2_error = function(cnd) {
      if (vb) {
        message("PATCH request failed for ", url, ": ", conditionMessage(cnd))
      }
      NULL
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  status <- httr2::resp_status(response)
  if (status == 204L || !resp_has_body(response)) {
    return(TRUE)
  }

  payload <- httr2::resp_body_json(response)
  if (isTRUE(normalize)) {
    payload <- snake_case_list(payload)
  }
  payload
}

#' @noRd
perform_api_delete <- function(path,
                               rq = NULL,
                               vb = FALSE) {
  request <- rq
  if (is.null(request)) {
    request <- databraryr::make_default_request()
  }

  url <- paste0(DATABRARY_BASE_URL, ensure_leading_slash(path))
  request <- httr2::req_url(request, url)
  request <- httr2::req_method(request, "DELETE")

  response <- tryCatch(
    httr2::req_perform(request),
    httr2_error = function(cnd) {
      if (vb) {
        message("DELETE request failed for ", url, ": ", conditionMessage(cnd))
      }
      NULL
    }
  )

  if (is.null(response)) {
    return(FALSE)
  }

  # DELETE typically returns 204 No Content or 200 OK
  TRUE
}

