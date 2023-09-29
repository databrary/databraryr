#' Assets in Databrary Volume.
#'
#' @param vol_id Target volume number.
#' @param vb A boolean value.
#' @returns A data frame with information about all assets in a volume.
#' @examples
#' list_assets_in_volume() # Assets in volume 2
#' @export
list_assets_in_volume <- function(vol_id = 2, vb = FALSE) {
  
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # Error handling
  if (!is.numeric(vol_id)) {
    stop("vol_id must be numeric.")
  }
  if (vol_id < 1) {
    stop("vol_id must be >= 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be a logical value.")
  }

  sl <- list_sessions(vol_id = vol_id, vb = vb)
  if (!is.null(sl)) {
    if (vb) message(" Session data exists.")
    s_ids <- sl$session_id
    if (length(s_ids) >= 1) {
      a <- lapply(s_ids, list_assets_in_session, vb = vb)
      a <- plyr::rbind.fill(a)
      return(a)      
    } else {
     NULL 
    }
  } else {
    if (vb) message(paste0(" Session list for volume ", vol_id, " unavailable."))
    return(NULL)
  }
}
