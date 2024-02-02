#' Get Stats About Databrary.
#'
#' `get_db_stats` returns basic summary information about
#' the institutions, people, and data hosted on 'Databrary.org'.
#'
#' @param type Type of Databrary report to run "institutions", "people", "data"
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2` request object.
#' 
#' @returns A data frame with the requested data or NULL if there is no new information.
#'
#' @examples
#' \donttest{
#' get_db_stats()
#' get_db_stats("stats")
#' get_db_stats("people") # Information about the newest authorized investigators.
#' get_db_stats("places") # Information about the newest institutions.
#' }
#' @export
get_db_stats <- function(type = "stats", vb = FALSE, rq = NULL) {
  # Check parameters
  assertthat::assert_that(length(type) == 1)
  assertthat::assert_that(is.character(type))
  assertthat::assert_that(
    type %in% c(
      "institutions",
      "places",
      "people",
      "datasets",
      "data",
      "volumes",
      "stats",
      "numbers"
    )
  )
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  if (is.null(rq)) {
    if (vb) message("No request object supplied. Using default.")
    rq <- make_default_request()
  }
  rq <- rq |>
    httr2::req_url(GET_ACTIVITY_DATA)
  
  resp <- tryCatch(
    httr2::req_perform(rq),
    httr2_error = function(cnd)
      NULL
  )
  
  # d$id <- NULL
  # d$affiliation <- NULL
  # d$sortname <- NULL
  # d$prename <- NULL
  # d$party <- NULL
  # d$institution <- NULL
  
  if (is.null(resp) | httr2::resp_status(resp) != 200) {
    if (vb)
      message("No content returned.")
    resp
  } else {
    r <- httr2::resp_body_json(resp)
    
    id <- NULL
    affiliation <- NULL
    sortname <- NULL
    prename <- NULL
    party <- NULL
    institution <- NULL
    
    if (type == "people") {
      d <- tibble::as_tibble(r$activity$party)
      if (!is.null(d)) {
          dplyr::filter(
            d,
            !is.na(id),
            !is.na(affiliation),
            !is.na(sortname),
            !is.na(prename)
          )
      } else {
        NULL
      }
    }
    if (type %in% c("institutions", "places")) {
      d <- tibble::as_tibble(r$activity$party)
      if ("institution" %in% names(d)) {
        dplyr::filter(d, !is.na(id),!is.na(institution))
      } else {
        NULL
      }
    }
    if (type %in% c("datasets", "volumes", "data")) {
      d <- tibble::as_tibble(r$activity$volume)
      if (!is.null(d)) {
        dplyr::filter(d, !is.na(id))
      } else {
        NULL
      }
    }
    if (type %in% c("stats", "numbers")) {
      tibble::tibble(
        date = Sys.time(),
        investigators = unlist(r$stats$authorized[5]),
        affiliates = unlist(r$stats$authorized[4]),
        institutions = unlist(r$stats$authorized[6]),
        datasets_total = r$stats$volumes,
        datasets_shared = r$stats$shared,
        n_files = r$stats$assets,
        hours = r$stats$duration / (1000 * 60 * 60),
        TB = r$stats$bytes / (1e12)
      ) # seems incorrect
    }
  }
}
