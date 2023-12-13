#' Get Stats About Databrary.
#'
#' `get_db_stats` returns basic summary information about
#' the institutions, people, and data hosted on 'Databrary.org'.
#'
#' @param type Type of Databrary report to run "institutions", "people", "data"
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with the requested data or NULL if there is no new information.
#' @examples
#' \donttest{
#' get_db_stats()
#' get_db_stats("stats")
#' get_db_stats("people") # Information about the newest authorized investigators.
#' get_db_stats("places") # Information about the newest institutions.
#' }
#' @export
get_db_stats <- function(type = "stats", vb = FALSE, rq) {
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
  
  rq <- rq |>
    httr2::req_url(GET_ACTIVITY_DATA)
  
  # r <- GET_db_contents(URL_components = '/api/activity')
  
  #resp <- httr2::req_perform(rq)
  resp <- tryCatch(
    httr2::req_perform(rq),
    httr2_error = function(cnd)
      NULL
  )
  
  # if (is.null(r)) {
  #   message("No content returned.")
  #   r <- NULL
  # } else {
  #   if (type == "people") {
  #     d <- tibble::as_tibble(r$activity$party)
  #     if (!is.null(d)) {
  #       d <-
  #         dplyr::filter(
  #           d,
  #           !is.na(d$id),
  #           !is.na(d$affiliation),
  #           !is.na(d$sortname),
  #           !is.na(d$prename)
  #         )
  #     } else {
  #       d <- NULL
  #     }
  #   }
  #   if (type %in% c("institutions", "places")) {
  #     d <- tibble::as_tibble(r$activity$party)
  #     if ("institution" %in% names(d)) {
  #       d <- dplyr::filter(d,!is.na(d$id),!is.na(d$institution))
  #     } else {
  #       d <- NULL # No new institutions
  #     }
  #   }
  #   if (type %in% c("datasets", "volumes", "data")) {
  #     d <- tibble::as_tibble(r$activity$volume)
  #     if (!is.null(d)) {
  #       d <- dplyr::filter(d,!is.na(d$id))
  #     } else {
  #       d <- NULL
  #     }
  #   }
  #   if (type %in% c("stats", "numbers")) {
  #     d <- data.frame(
  #       date = Sys.time(),
  #       investigators = r$stats$authorized[5],
  #       affiliates = r$stats$authorized[4],
  #       institutions = r$stats$authorized[6],
  #       datasets_total = r$stats$volumes,
  #       datasets_shared = r$stats$shared,
  #       n_files = r$stats$assets,
  #       hours = r$stats$duration / (1000 * 60 * 60),
  #       TB = r$stats$bytes / (1e12)
  #     ) # seems incorrect
  #   }
  # }
  
  
  if (is.null(resp) | httr2::resp_status(resp) != 200) {
    if (vb)
      message("No content returned.")
    return(NULL)
  } else {
    r <- httr2::resp_body_json(resp)
    if (type == "people") {
      d <- tibble::as_tibble(r$activity$party)
      if (!is.null(d)) {
        d <-
          dplyr::filter(
            d,
            !is.na(d$id),
            !is.na(d$affiliation),
            !is.na(d$sortname),
            !is.na(d$prename)
          )
      } else {
        d <- NULL
      }
    }
    if (type %in% c("institutions", "places")) {
      d <- tibble::as_tibble(r$activity$party)
      if ("institution" %in% names(d)) {
        d <- dplyr::filter(d,!is.na(d$id),!is.na(d$institution))
      } else {
        d <- NULL # No new institutions
      }
    }
    if (type %in% c("datasets", "volumes", "data")) {
      d <- tibble::as_tibble(r$activity$volume)
      if (!is.null(d)) {
        d <- dplyr::filter(d,!is.na(d$id))
      } else {
        d <- NULL
      }
    }
    if (type %in% c("stats", "numbers")) {
      d <- tibble::tibble(
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
  return(d)
}
