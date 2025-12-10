#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get User Avatar
#'
#' @description Download a user's avatar image from Databrary. Returns raw
#' bytes if no destination path is specified, or saves to disk and returns the
#' file path.
#'
#' @param user_id Numeric. The ID of the user whose avatar to download.
#' @param dest_path Optional character string specifying where to save the
#'   avatar. Can be either a file path or a directory. If a directory is
#'   provided, the filename will be automatically determined from the response
#'   headers or will default to "user_<id>_avatar.jpg". If `NULL` (the
#'   default), the function returns raw bytes instead of saving to disk.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return If `dest_path` is `NULL`, returns raw bytes. If `dest_path` is
#'   specified, returns the full path where the avatar was saved. Returns
#'   `NULL` if the user has no avatar or if an error occurs.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Get avatar as raw bytes
#' avatar_bytes <- get_user_avatar(user_id = 5)
#'
#' # Save avatar to specific file
#' get_user_avatar(user_id = 5, dest_path = "avatar.jpg")
#'
#' # Save avatar to directory (filename auto-determined)
#' get_user_avatar(user_id = 5, dest_path = "~/avatars/")
#'
#' # With verbose output
#' get_user_avatar(user_id = 5, dest_path = "avatar.jpg", vb = TRUE)
#' }
#' }
#' @export
get_user_avatar <- function(user_id,
                            dest_path = NULL,
                            vb = options::opt("vb"),
                            rq = NULL) {
  # Validate user_id
  assertthat::assert_that(length(user_id) == 1)
  assertthat::assert_that(is.numeric(user_id) || is.integer(user_id))
  assertthat::assert_that(user_id > 0)

  # Validate dest_path
  if (!is.null(dest_path)) {
    assertthat::assert_that(assertthat::is.string(dest_path))
  }

  # Validate vb
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  # Validate rq
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # Build URL path
  path <- sprintf(API_USER_AVATAR, user_id)

  if (vb) {
    message("Getting user avatar for user ID: ", user_id)
  }

  # Set up request
  if (is.null(rq)) {
    rq <- make_default_request()
  }

  # Perform request
  resp <- tryCatch(
    {
      rq |>
        httr2::req_url_path_append(path) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
    },
    error = function(e) {
      if (vb) {
        message("Error downloading user avatar: ", conditionMessage(e))
      }
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  # Check for errors
  if (httr2::resp_status(resp) != 200) {
    if (vb) {
      message(
        "Failed to download user avatar. Status: ",
        httr2::resp_status(resp)
      )
    }
    return(NULL)
  }

  # Get avatar bytes
  avatar_bytes <- httr2::resp_body_raw(resp)

  # If no destination path, return bytes
  if (is.null(dest_path)) {
    if (vb) {
      message("Returning avatar as raw bytes (", length(avatar_bytes), " bytes)")
    }
    return(avatar_bytes)
  }

  # Save to file
  # Resolve destination path
  # If dest_path is a directory, determine filename from response headers or URL
  final_path <- dest_path
  if (dir.exists(dest_path)) {
    # Try to get filename from content-disposition header
    filename <- "downloaded_file"
    content_disp <- httr2::resp_header(resp, "content-disposition")

    if (!is.null(content_disp) && grepl("filename=", content_disp)) {
      # Extract filename from content-disposition header
      filename_match <- regmatches(content_disp, regexpr("filename=([^;]+)", content_disp))
      if (length(filename_match) > 0) {
        filename <- sub("filename=", "", filename_match)
        filename <- gsub('^"|"$', '', filename)  # Remove quotes
        filename <- trimws(filename)
      }
    } else {
      # Fallback: use URL path basename
      url_path <- sprintf(API_USER_AVATAR, user_id)
      filename <- paste0("user_", user_id, "_avatar.jpg")
    }

    final_path <- file.path(dest_path, filename)
  }

  # Ensure parent directory exists
  parent_dir <- dirname(final_path)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE)
  }

  # Write to file
  tryCatch(
    {
      writeBin(avatar_bytes, final_path)
      if (vb) {
        message("Avatar saved to: ", final_path)
      }
      return(normalizePath(final_path))
    },
    error = function(e) {
      if (vb) {
        message("Error saving avatar to file: ", conditionMessage(e))
      }
      return(NULL)
    }
  )
}