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
    # Map new API field names to output
    tibble::tibble(
      date = Sys.time(),
      institutions = if (!is.null(stats$institutions)) stats$institutions else NA_integer_,
      affiliates = if (!is.null(stats$affiliates)) stats$affiliates else NA_integer_,
      investigators = if (!is.null(stats$investigators)) stats$investigators else NA_integer_,
      hours_of_recordings = if (!is.null(stats$hours_of_recordings)) stats$hours_of_recordings else NA_integer_,
      # Legacy fields (may not be present in new API)
      authorized_users = if (!is.null(stats$authorized_users)) stats$authorized_users else NA_integer_,
      total_volumes = if (!is.null(stats$total_volumes)) stats$total_volumes else NA_integer_,
      public_volumes = if (!is.null(stats$public_volumes)) stats$public_volumes else NA_integer_,
      total_files = if (!is.null(stats$total_files)) stats$total_files else NA_integer_,
      total_duration_hours = if (!is.null(stats$total_duration_hours)) stats$total_duration_hours else NA_real_,
      total_storage_tb = if (!is.null(stats$total_storage_tb)) stats$total_storage_tb else NA_real_
    )
  } else {
    # For other types, return the raw stats as a tibble
    tibble::as_tibble(stats)
  }
}
