#' List Activity In A Databrary Volume
#'
#' If a user has access to a volume, this command lists the modification
#' history of the volume as a 
#'
#' @param vol_id Selected volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2` request object. Defaults to NULL.
#'
#' @returns A list with the activity history on a volume.
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # The following will only return output if the user has write privileges
#' # on the volume.
#'
#' list_volume_activity(vol_id = 1) # Activity on volume 1.
#' }
#' }
#' @export
list_volume_activity <-
  function(vol_id = 1,
           vb = FALSE,
           rq = NULL) {
    # Check parameters
    assertthat::assert_that(length(vol_id) == 1)
    assertthat::assert_that(is.numeric(vol_id))
    assertthat::assert_that(vol_id > 0)
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    if (vb)
      message('list_volume_activity()...')
    
    if (is.null(rq)) {
      if (vb) {
        message("NULL request object. Will generate default.")
        message("Not logged in. Only public information will be returned.")  
      }
      rq <- databraryr::make_default_request()
    }
    rq <- rq %>%
      httr2::req_url(sprintf(GET_VOLUME_ACTIVITY, vol_id))
    
    resp <- tryCatch(
      httr2::req_perform(rq),
      httr2_error = function(cnd) {
        NULL
      }
    )
    
    # TODO: Parse nested list into tibble/data.frame
    if (!is.null(resp)) {
      res <- httr2::resp_body_json(resp)
      if (!(is.null(res))) {
        res
      }
    } else {
      resp
    }
  }
