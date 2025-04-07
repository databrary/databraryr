#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Owner of a Databrary Volume.
#'
#' In Databrary 2.0, each volume has a single owner.
#'
#' @param vol_id Selected volume number. Default is volume 1.
#' @param add_id A logical value. Include the volume ID in the output.
#' Default is TRUE.
#' @param rq An `httr2` request object. If NULL (the default)
#' a request will be generated, but this will only permit public information
#' to be returned.
#'
#' @returns A data frame with information about a volume's owner(s).
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' list_volume_owners() # Lists information about the owners of volume 1.
#' }
#' @export
list_volume_owners <- function(vol_id = 1,
                               add_id = TRUE,
                               vb = options::opt("vb"),
                               rq = NULL) {
  # Check parameters
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(sum(vol_id >= 1) == length(vol_id))
  
  assertthat::assert_that(length(add_id) == 1)
  assertthat::assert_that(is.logical(add_id))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  if (vb)
    message("Summarizing owners for n=", length(vol_id), " volumes.")
  purrr::map(
    vol_id,
    list_single_volume_owner,
    add_id = add_id,
    vb = vb,
    rq = rq,
    .progress = "Volume funding: "
  ) %>%
    purrr::list_rbind()
}

#-------------------------------------------------------------------------------
# Helper function for handling lists
list_single_volume_owner <-
  function(vol_id = NULL,
           add_id = NULL,
           vb = NULL,
           rq) {
    resp <- get_volume_by_id(vol_id = vol_id, vb = vb, rq = rq)
    
    if (is.null(resp)) {
      message("Cannot access requested resource on Databrary. Exiting.")
      return(resp)
    } else {
      assertthat::assert_that("list" %in% class(resp$ownerConnection))
      owner <- resp$ownerConnection$user
      owner_inst <- resp$ownerConnection$institution
      
      out_df <- tibble::tibble(
        user_id = owner$id,
        user_name = paste0(owner$lastName, ", ", owner$firstName),
        user_orcid = owner$orcid,
        user_inst = owner_inst$name,
        inst_id = owner_inst$id,
      )
      
      if (add_id) {
        out_df <- dplyr::mutate(out_df, vol_id = vol_id)
      }
      out_df
    }
  }
