#' @eval options::as_params()
#' @name options_params
#' 
NULL

#' Get Stats About Databrary.
#'
#' `get_db_stats` returns basic summary information about
#' the institutions, people, and data hosted on 'Databrary.org'.
#'
#' @param type Type of Databrary report to run "institutions", "people", "data"
#' @param rq An `httr2` request object.
#'
#' @returns A data frame with the requested data or NULL if there is 
#' no new information.
#'
#' @inheritParams options_params
#' 
#' @examples
#' \donttest{
#' get_db_stats()
#' get_db_stats("stats")
#' get_db_stats("people") # Information about the newest authorized investigators.
#' get_db_stats("places") # Information about the newest institutions.
#' }
#' @export
get_db_stats <- function(type = "stats",
                         vb = options::opt("vb"),
                         rq = NULL) {
  # Check parameters
  assertthat::assert_that(length(type) == 1)
  assertthat::assert_that(is.character(type))
  assertthat::assert_that(
    type %in% c(
      "institutions",
      "places",
      "people",
      "researchers",
      "investigators",
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
    if (vb) {
      message("\nNULL request object. Will generate default.")
      message("Not logged in. Only public information will be returned.")
    }
    rq <- databraryr::make_default_request()
  }
  stats <- perform_api_get(
    path = API_ACTIVITY_SUMMARY,
    rq = rq,
    vb = vb
  )
  
  if (is.null(stats)) {
    message("Cannot access requested resource on Databrary. Exiting.")
    return(NULL)
  }
  
  if (type %in% c("stats", "numbers")) {
    tibble::tibble(
      date = Sys.time(),
      investors = stats$authorized_users,
      datasets_total = stats$total_volumes,
      datasets_shared = stats$public_volumes,
      n_files = stats$total_files,
      hours = stats$total_duration_hours,
      TB = stats$total_storage_tb
    )
  } else {
    tibble::as_tibble(stats$recent_activity)
  }
}
