#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Retrieves URL Links From A Databrary Volume.
#'
#' @param vol_id Target volume number.
#' @param rq An `httr2` request object.
#'
#' @returns A data frame with the requested data.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_links() # Links from volume 1
#' }
#' }
#' @export
list_volume_links <- function(vol_id = 1,
                              vb = options::opt("vb"),
                              rq = NULL) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  links <- perform_api_get(
    path = sprintf(API_VOLUME_LINKS, vol_id),
    rq = rq,
    vb = vb
  )

  if (is.null(links) || length(links) == 0) {
    return(NULL)
  }

  purrr::map_dfr(links, function(link) {
    tibble::tibble(
      link_id = link$id,
      link_label = link$title,
      link_url = link$url,
      link_description = link$description,
      link_release_level = link$release_level
    )
  })
}
