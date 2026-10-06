#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Enable a Category for a Volume
#'
#' @description Add a single category to a volume's enabled set. This is
#' additive -- existing enabled categories are preserved. No-op if the
#' category is already enabled.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param category_id Numeric category identifier to enable.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} on success (or if already enabled), \code{NULL} on
#'   failure.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' enable_volume_category(vol_id = 1, category_id = 1)
#' }
#' }
#' @export
enable_volume_category <- function(
  vol_id = 1,
  category_id,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(category_id, "category_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  current <- get_volume_enabled_categories(vol_id = vol_id, vb = vb, rq = rq)
  if (is.null(current)) {
    current <- list()
  }

  current_ids <- vapply(current, function(c) as.integer(c$id), integer(1))
  if (as.integer(category_id) %in% current_ids) {
    if (vb) message("Category ", category_id, " is already enabled for volume ", vol_id)
    return(TRUE)
  }

  updated_ids <- c(current_ids, as.integer(category_id))
  set_volume_enabled_categories(
    vol_id = vol_id,
    category_ids = updated_ids,
    vb = vb,
    rq = rq
  )
}
