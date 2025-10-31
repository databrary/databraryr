#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Lists Keywords And Tags For A Volume.
#'
#' @param vol_id Target volume number.
#' @param rq An `httr2` request object. Default is NULL.
#'
#' @returns A data frame with the requested data.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' list_volume_tags()
#' }
#' @export
list_volume_tags <- function(vol_id = 1,
                             vb = options::opt("vb"),
                             rq = NULL) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  tags <- perform_api_get(
    path = sprintf(API_VOLUME_TAGS, vol_id),
    rq = rq,
    vb = vb
  )

  if (is.null(tags) || length(tags) == 0) {
    return(NULL)
  }

  tags
}

#-------------------------------------------------------------------------------
extract_vol_tag <- function(tag_list_item) {
  tibble::tibble(tag_id = tag_list_item$id, tag_weight = tag_list_item$weight)
}
