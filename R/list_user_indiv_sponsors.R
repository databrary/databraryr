#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Individual Sponsors for a User
#'
#' @param user_id Target party ID.
#' @param rq An `httr2`-style request object. If NULL, then a new request will
#' be generated using `make_default_request()`.
#'
#' @returns A data frame with information about a user's institutional sponsors.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_user_indiv_sponsors() # Default is Rick Gilmore (user_id 6)
#' }
#' }
#'
#' @export

list_user_indiv_sponsors <- function(user_id = 6,
                                     vb = options::opt("vb"),
                                     rq = NULL) {
  # Check parameters
  assertthat::assert_that(is.numeric(user_id))
  assertthat::assert_that(sum(user_id > 0) == length(user_id))
  
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  purrr::map(
    user_id,
    get_single_user_indiv_sponsors,
    vb = vb,
    rq = rq,
    .progress = TRUE
  ) |>
    purrr::list_rbind()
}

#---------------------------
get_single_user_indiv_sponsors <- function(user_id = NULL,
                                           vb = NULL,
                                           rq = NULL) {
  this_party <- get_party_by_id(party_id = user_id,
                                vb = vb,
                                rq = rq)
  
  if ('currentSponsors' %in% names(this_party)) {
    if (is.null(this_party$currentSponsors)[1]) {
      if (vb)
        message("No individual sponsors for user_id: ", user_id)
      NULL
    } else {
      if (vb) 
        message("Retrieving 'currentSponsors' from user_id: ", user_id)
      purrr::map(
        this_party$currentSponsors,
        unpack_user_indiv_sponsor_list,
        user_id,
        .progress = TRUE
      ) |>
        purrr::list_rbind()
    }
  } else {
    if (vb)
      message("No institutional sponsorships for user_id: ", user_id)
    NULL
  }
}

#----------------------------
unpack_user_indiv_sponsor_list <- function(x, user_id) {
  sponsor_user_id <- x$id
  sponsor_firstName <- x$firstName
  sponsor_lastName <- x$lastName
  sponsor_affiliation_name <- x$affiliation$name
  
  data.frame(
    user_id = user_id,
    sponsor_user_id = sponsor_user_id,
    sponsor_firstName = sponsor_firstName,
    sponsor_lastName = sponsor_lastName,
    sponsor_affiliation_name = sponsor_affiliation_name
  )
}