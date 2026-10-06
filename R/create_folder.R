#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Create Folder in Databrary Volume
#'
#' @description Create a new folder in a Databrary volume. \code{name} is
#' required and must be non-empty. Folders group files together but, unlike
#' sessions, do not carry structured date / default-record metadata.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param name Display name for the folder. Required, non-empty after trim.
#' @param release_level Optional release level for the folder
#'   (e.g. \code{"PRIVATE"}, \code{"SHARED"}, \code{"EXCERPTS"},
#'   \code{"PUBLIC"}). The server validates the choice.
#' @param source_date Optional folder date. A length-1 \code{Date} object or
#'   ISO \code{"YYYY-MM-DD"} string.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A list with the created folder's metadata (same shape as
#'   \code{\link{get_folder_by_id}}), or \code{NULL} if creation fails.
#'
#' @seealso \code{\link{update_folder}}, \code{\link{patch_folder}},
#'   \code{\link{delete_folder}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Minimal folder
#' create_folder(vol_id = 1, name = "Stimuli")
#'
#' # Folder with a release level and date
#' create_folder(
#'   vol_id = 1,
#'   name = "Stimuli",
#'   release_level = "SHARED",
#'   source_date = as.Date("2024-03-15")
#' )
#' }
#' }
#' @export
create_folder <- function(
  vol_id = 1,
  name,
  release_level = NULL,
  source_date = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")

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

  folder <- perform_api_post(
    path = sprintf(API_VOLUME_FOLDERS, vol_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(folder)) {
    if (vb) {
      message("Failed to create folder in volume ", vol_id)
    }
    return(NULL)
  }

  folder
}
