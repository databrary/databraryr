#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Lists Funders Associated With a Databrary Volume.
#'
#' @param vol_id Target volume number.
#' @param add_id A logical value. Include the volume ID in the output.
#' Default is TRUE.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object.
#'
#' @returns A data frame with funder information for the volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_funding() # Funding for volume 1
#'
#' list_volume_funding(vol_id = c(1:10))
#' }
#' }
#'
#' @export
list_volume_funding <- function(vol_id = 1,
                                add_id = TRUE,
                                vb = options::opt("vb"),
                                rq = NULL) {
  # Check parameters
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(sum(vol_id >= 1) == length(vol_id))
  
  assertthat::assert_that(length(add_id) == 1)
  assertthat::assert_that(is.logical(add_id))
  
  validate_flag(vb, "vb")
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))

  if (vb)
    message("Summarizing funding for n=", length(vol_id), " volumes.")
  
  purrr::map(vol_id, function(id) {
    fundings <- perform_api_get(
      path = sprintf(API_VOLUME_FUNDINGS, id),
      rq = rq,
      vb = vb
    )
    
    if (is.null(fundings) || length(fundings) == 0) {
      return(NULL)
    }
    
    rows <- purrr::map_dfr(fundings, function(entry) {
      funder <- entry$funder
      tibble::tibble(
        funder_id = funder$id,
        funder_name = funder$name,
        funder_is_approved = funder$is_approved,
        funder_awards = entry$awards
      )
    })
    if (add_id) {
      rows <- dplyr::mutate(rows, vol_id = id)
    }
    rows
  }) %>%
    purrr::list_rbind()
}
