#' List Activity History in Databrary Session.
#'
#' If a user has access to a volume and session, this function returns the
#' history of modifications to that session.
#'
#' @param session_id Selected session/slot number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with the activity history on a session/slot.
#' @examples
#' \donttest{
#' \dontrun{
#' list_session_activity(session_id = 6256) # History of session 6256. Only
#' shown to researchers who have write privileges on this session.
#' }
#' }
#' @export
list_session_activity <- function(session_id = 6256, vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # Make URL, GET(), and handle response ---------------------------
  r <-
    GET_db_contents(URL_components = paste0('/api/slot/', session_id,
                                            '/activity'),
                    vb = vb)
  r
}
