#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Institutional Sponsors for a User
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
#' list_user_inst_sponsors() # Default is Rick Gilmore (user_id 6)
#' }
#' }
#'
#' @export

list_user_inst_sponsors <- function(user_id = 6,
                                    vb = options::opt("vb"),
                                    rq = NULL) {
  # Check parameters
  assertthat::assert_that(is.numeric(user_id))
  assertthat::assert_that(sum(user_id > 0) == length(user_id))
  
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  purrr::map(user_id, get_single_user_inst_sponsors, vb = vb, rq = rq,
             .progress = TRUE) |>
    purrr::list_rbind()
}

#---------------------------
get_single_user_inst_sponsors <- function(user_id = NULL,
                                          vb = NULL,
                                          rq = NULL) {
  this_party <- get_party_by_id(party_id = user_id,
                                vb = vb,
                                rq = rq)
  
  if ('institutionSponsorships' %in% names(this_party)) {
    purrr::map(this_party$institutionSponsorships,
               unpack_user_sponsor_list,
               user_id) |>
      purrr::list_rbind()
  } else {
    if (vb)
      message("No institutional sponsorships for user_id: ", user_id)
    NULL
  }  
}

#----------------------------
unpack_user_sponsor_list <- function(x, user_id) {
  sponsorship_id <- NULL
  inst_id <- NULL
  inst_name <- NULL
  inst_url <- NULL
  
  sponsorship_id <- x[[1]]
  inst_id <- x$institution$id
  inst_name <- x$institution$name
  inst_url <- x$institution$url
  
  data.frame(user_id = user_id,
             sponsorship_id = sponsorship_id,
             inst_id = inst_id,
             inst_name = inst_name,
             inst_url = inst_url)
}