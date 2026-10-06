#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Replace a Folder in Databrary Volume (PUT)
#'
#' @description Sends a PUT request to fully replace a folder's writable
#' fields. \code{name} is required (non-empty); other fields are optional and
#' default to server-side values when omitted (the underlying serializer
#' marks them \code{required=FALSE}). Use \code{\link{patch_folder}} for
#' partial updates when you don't want full-replacement semantics.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param folder_id Numeric folder identifier. Must be a positive integer.
#' @param name New folder name. Required, non-empty after trim.
#' @param release_level Optional release level (e.g. \code{"PRIVATE"},
#'   \code{"SHARED"}, \code{"EXCERPTS"}, \code{"PUBLIC"}).
#' @param source_date Optional folder date. A length-1 \code{Date} object or
#'   ISO \code{"YYYY-MM-DD"} string.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A list with the updated folder's metadata (same shape as
#'   \code{\link{get_folder_by_id}}), or \code{NULL} if the update fails.
#'
#' @seealso \code{\link{patch_folder}}, \code{\link{create_folder}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' update_folder(vol_id = 1, folder_id = 42, name = "Replacement name")
#' }
#' }
#' @export
update_folder <- function(
  vol_id = 1,
  folder_id,
  name,
  release_level = NULL,
  source_date = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(folder_id, "folder_id")

  assertthat::assert_that(is.character(name), length(name) == 1)
  assertthat::assert_that(nzchar(trimws(name)), msg = "name must not be empty")

  if (!is.null(release_level)) {
    assertthat::assert_that(
      is.character(release_level),
      length(release_level) == 1,
      nzchar(trimws(release_level))
    )
  }

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  body <- list(name = name)
  if (!is.null(release_level)) body$release_level <- release_level
  if (!is.null(source_date)) {
    body$source_date <- coerce_iso_date(source_date, "source_date")
  }

  folder <- perform_api_put(
    path = sprintf(API_FOLDER_DETAIL, vol_id, folder_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(folder)) {
    if (vb) {
      message(
        "Failed to replace folder ", folder_id, " in volume ", vol_id
      )
    }
    return(NULL)
  }

  folder
}
