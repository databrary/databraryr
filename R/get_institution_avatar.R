#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Download Institution Avatar Image
#'
#' @description Download an institution's avatar image from Databrary. The
#' image can be saved to a file or returned as raw bytes for further
#' processing.
#'
#' @param institution_id Numeric institution identifier. Must be a positive
#'   integer.
#' @param dest_path Optional character string specifying the destination file
#'   path or directory where the avatar should be saved. If a directory is
#'   provided, the filename will be determined from the response headers or
#'   will default to `institution_<id>_avatar.jpg`. If `NULL` (the default),
#'   the raw image bytes are returned instead of being saved to disk.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return If `dest_path` is provided, returns the full path to the saved
#'   file (character string). If `dest_path` is `NULL`, returns the raw
#'   image bytes. Returns `NULL` if the avatar is not found or inaccessible.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Download avatar as raw bytes
#' avatar_bytes <- get_institution_avatar(institution_id = 1)
#'
#' # Download and save avatar to specific file
#' avatar_path <- get_institution_avatar(
#'   institution_id = 1,
#'   dest_path = "institution_1_avatar.jpg"
#' )
#'
#' # Download and save to directory (filename auto-determined)
#' avatar_path <- get_institution_avatar(
#'   institution_id = 1,
#'   dest_path = "avatars/"
#' )
#'
#' # With verbose output
#' get_institution_avatar(institution_id = 1, vb = TRUE)
#' }
#' }
#' @export
get_institution_avatar <- function(institution_id = 1,
                                   dest_path = NULL,
                                   vb = options::opt("vb"),
                                   rq = NULL) {
  assertthat::assert_that(is.numeric(institution_id))
  assertthat::assert_that(length(institution_id) == 1)
  assertthat::assert_that(institution_id > 0)
  assertthat::assert_that(institution_id == floor(institution_id), msg = "institution_id must be an integer")

  if (!is.null(dest_path)) {
    assertthat::assert_that(assertthat::is.string(dest_path))
  }

  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))

  # Build URL
  avatar_url <- sprintf(API_INSTITUTION_AVATAR, institution_id)
  full_url <- paste0(DATABRARY_BASE_URL, avatar_url)

  # Create request
  if (is.null(rq)) {
    req <- make_default_request()
  } else {
    req <- rq
  }

  # Build the request with the avatar URL
  req <- req |>
    httr2::req_url(full_url) |>
    httr2::req_method("GET") |>
    httr2::req_error(
      is_error = function(resp) {
        FALSE
      }
    )

  if (vb) {
    message("Requesting avatar for institution ", institution_id)
  }

  # Perform request
  tryCatch({
    resp <- httr2::req_perform(req)

    # Check response status
    status <- httr2::resp_status(resp)
    if (status != 200) {
      if (vb) {
        message(
          "Institution ",
          institution_id,
          " avatar not found or inaccessible (status: ",
          status,
          ")"
        )
      }
      return(NULL)
    }

    # Get raw bytes
    avatar_bytes <- httr2::resp_body_raw(resp)

    if (is.null(dest_path)) {
      # Return raw bytes
      if (vb) {
        message("Downloaded ", length(avatar_bytes), " bytes")
      }
      return(avatar_bytes)
    } else {
      # Resolve destination path
      # If dest_path is a directory, determine filename from response headers or URL
      final_path <- dest_path
      if (dir.exists(dest_path)) {
        # Try to get filename from content-disposition header
        filename <- "downloaded_file"
        content_disp <- httr2::resp_header(resp, "content-disposition")

        if (!is.null(content_disp) &&
              grepl("filename=", content_disp)) {
          # Extract filename from content-disposition header
          filename_match <- regmatches(content_disp,
                                       regexpr("filename=([^;]+)", content_disp))
          if (length(filename_match) > 0) {
            filename <- sub("filename=", "", filename_match)
            filename <- gsub("^\"|\"$", "", filename) # Remove quotes
            filename <- trimws(filename)
          }
        } else {
          # Fallback: use default filename when content-disposition lacks filename
          filename <- paste0("institution_", institution_id, "_avatar.jpg")
        }

        final_path <- file.path(dest_path, filename)
      }

      # Create parent directory if needed
      parent_dir <- dirname(final_path)
      if (!dir.exists(parent_dir)) {
        dir.create(parent_dir,
                   recursive = TRUE,
                   showWarnings = FALSE)
      }

      # Save to file
      writeBin(avatar_bytes, final_path)

      if (vb) {
        message("Saved avatar to: ",
                final_path,
                " (",
                length(avatar_bytes),
                " bytes)")
      }

      return(normalizePath(final_path))
    }
  }, error = function(e) {
    if (vb) {
      message("Error downloading avatar for institution ",
              institution_id,
              ": ",
              e$message)
    }
    NULL
  })
}
