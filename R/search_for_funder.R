#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Report Information About A Funder.
#'
#' @param search_string String to search.
#' @param approved_only Logical. When TRUE (default) only approved funders are
#'   returned. Set to FALSE to include unapproved funders as well.
#' @param rq An `httr2` request object. Default is NULL.
#'
#' @returns A data frame with information about the funder.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' search_for_funder("national+science+foundation")
#' }
#'
#' @export
search_for_funder <-
  function(search_string = "national science foundation",
           approved_only = TRUE,
           vb = options::opt("vb"),
           rq = NULL) {
    assertthat::assert_that(length(search_string) == 1)
    assertthat::assert_that(is.character(search_string))
    search_string <- gsub("[+]", " ", search_string)
    pattern <- stringr::str_trim(search_string)
    
    assertthat::assert_that(is.logical(approved_only), length(approved_only) == 1)
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    
    assertthat::assert_that(is.null(rq) |
                              ("httr2_request" %in% class(rq)))
    
    params <- list()
    if (!approved_only) {
      params$all <- "true"
    }
    
    funders <- collect_paginated_get(
      path = API_FUNDERS,
      params = params,
      rq = rq,
      vb = vb
    )

    if (is.null(funders) || length(funders) == 0) {
      if (vb) message("No funders available from API.")
      return(NULL)
    }
    
    funder_tbl <- purrr::map_dfr(funders, function(entry) {
      tibble::tibble(
        funder_id = entry$id,
        funder_name = entry$name,
        funder_is_approved = entry$is_approved
      )
    })

    if (!nzchar(pattern)) {
      return(funder_tbl)
    }

    matches <- stringr::str_detect(
      stringr::str_to_lower(funder_tbl$funder_name),
      stringr::str_to_lower(pattern)
    )
    result <- funder_tbl[matches, , drop = FALSE]

    if (nrow(result) == 0) {
      if (vb) message("No funders matched query '", search_string, "'.")
      return(NULL)
    }

    result
  }
