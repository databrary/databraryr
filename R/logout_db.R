#' Log Out of Databrary.org.
#'
#' @param vb A boolean value. If TRUE provides verbose output.
#' @param rq An `httr2` request object. Defaults to NULL.
#' 
#' @returns TRUE if logging out succeeds, FALSE otherwise.
#' 
#' @examples
#' \donttest{
#' logout_db()
#' }
#' @export
logout_db <- function(vb = FALSE, rq = NULL){
  # Check parameters
  assertthat::assert_that(is.logical(vb))
  
  # url <- "https://nyu.databrary.org/api/user/logout"
  # r <- httr::POST(url)
  if (is.null(rq)) {
    if (vb) message("Empty request. Generating new one.")
    rq <- make_default_request()
  }
  rq <- rq |>
    httr2::req_url(LOGOUT)
  
  r <- httr2::req_perform(rq)

  # if (httr::status_code(r) == 200) {
  #   if (vb) message('Logout Successful.')
  #   httr::set_config(httr::config(cookie = NULL))
  #   return(TRUE)
  # } else {
  #   if (vb) message(paste('Logout Failed, HTTP status ', httr::status_code(r), '\n', sep="" ))
  #   return(FALSE)
  # }

  delete_cookie <- file.remove(rq$options$cookiefile)
  if (httr2::resp_status(r) == 200 & delete_cookie) {
    if (vb) message('Logout Successful.')
    TRUE
  } else {
    if (vb) message(paste0('Logout Failed, HTTP status: ', httr2::resp_status(r), '.\n'))
    FALSE
  }
}
