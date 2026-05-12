#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Partially Update a Folder in Databrary Volume
#'
#' @description Sends a PATCH request to update selected fields of an existing
#' folder. Only provided arguments are sent; omitted fields are left
#' unchanged on the server. The server rejects blanking out a previously
#' non-empty \code{name}.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param folder_id Numeric folder identifier. Must be a positive integer.
#' @param name Optional new folder name. If provided, must be a non-empty
#'   length-1 string.
#' @param release_level Optional release level (e.g. \code{"PRIVATE"},
#'   \code{"SHARED"}, \code{"EXCERPTS"}, \code{"PUBLIC"}). Server validates
#'   the choice.
#' @param source_date Optional folder date. A length-1 \code{Date} object or
#'   ISO \code{"YYYY-MM-DD"} string.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A list with the updated folder's metadata (same shape as
#'   \code{\link{get_folder_by_id}}), or \code{NULL} if the update fails or
#'   no fields were provided.
#'
#' @seealso \code{\link{update_folder}}, \code{\link{create_folder}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' patch_folder(vol_id = 1, folder_id = 42, name = "Renamed folder")
#' }
#' }
#' @export
patch_folder <- function(
  vol_id = 1,
  folder_id,
  name = NULL,
  release_level = NULL,
  source_date = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(folder_id, "folder_id")

  if (!is.null(name)) {
    assertthat::assert_that(is.character(name), length(name) == 1)
    assertthat::assert_that(
      nzchar(trimws(name)),
      msg = "name must not be empty"
    )
  }

  if (!is.null(release_level)) {
    assertthat::assert_that(
      is.character(release_level),
      length(release_level) == 1,
      nzchar(trimws(release_level))
    )
  }

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  body <- list()
  if (!is.null(name)) body$name <- name
  if (!is.null(release_level)) body$release_level <- release_level
  if (!is.null(source_date)) {
    body$source_date <- coerce_iso_date(source_date, "source_date")
  }

  if (length(body) == 0) {
    if (vb) {
      message("No fields provided to update for folder ", folder_id)
    }
    return(NULL)
  }

  folder <- perform_api_patch(
    path = sprintf(API_FOLDER_DETAIL, vol_id, folder_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(folder)) {
    if (vb) {
      message(
        "Failed to update folder ", folder_id, " in volume ", vol_id
      )
    }
    return(NULL)
  }

  folder
}
