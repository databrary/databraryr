# R/utils.R
#
# Utility functions.

#------------------------------------------------------------------------------
#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Duration (In ms) Of A File.
#'
#' @param vol_id Volume ID.
#' @param session_id Session ID containing the asset.
#' @param asset_id Asset number.
#' @param types_w_durations Asset types that have valid durations.
#' @param rq An `httr2` request object. Default is NULL.
#'
#' @returns Duration of a file in ms.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' get_file_duration() # default is a public video from volume 1
#' }
#'
#' @export
get_file_duration <- function(vol_id = 2,
                              session_id = 9,
                              asset_id = 2,
                              types_w_durations = c(-600, -800),
                              vb = options::opt("vb"),
                              rq = NULL) {
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(length(vol_id) == 1)

  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id > 0)
  assertthat::assert_that(length(session_id) == 1)

  assertthat::assert_that(is.numeric(asset_id))
  assertthat::assert_that(asset_id > 0)
  assertthat::assert_that(length(asset_id) == 1)
  
  assertthat::assert_that(is.atomic(types_w_durations))
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))

  types_w_durations <- as.character(types_w_durations)

  asset <- perform_api_get(
    path = sprintf(API_SESSION_FILE_DETAIL, vol_id, session_id, asset_id),
    rq = rq,
    vb = vb
  )

  if (is.null(asset)) {
    message("Cannot access requested resource on Databrary. Exiting.")
    return(NULL)
  }

  format <- asset$format
  format_id_chr <- as.character(format$id)

  if (!is.na(format_id_chr) && !(format_id_chr %in% types_w_durations)) {
    if (vb) {
      message("Asset format does not include duration metadata.")
    }
    return(NULL)
  }

  duration_value <- asset$duration

  if (is.null(duration_value)) {
    if (vb) {
      message("Duration metadata not available for the requested asset.")
    }
    return(NULL)
  }

  duration_value <- suppressWarnings(as.numeric(duration_value))

  if (is.na(duration_value)) {
    return(NULL)
  }

  round(duration_value * 1000)
}
  
  #----------------------------------------------------------------------------
  #' Extract Databrary Permission Levels.
  #'
  #' @returns An array with the permission levels that can be assigned to data.
  #'
  #' @inheritParams options_params
  #'
  #' @examples
  #' \donttest{
  #' get_permission_levels()
  #' }
  #'
  #' @export
get_permission_levels <- function(vb = options::opt("vb")) {
  enums <- get_permission_levels_enums()
  enums$volume_access_levels
}
  
  #----------------------------------------------------------------------------
  #' Convert Timestamp String To ms.
  #'
  #' @param HHMMSSmmm a string in the format "HH:MM:SS:mmm"
  #'
  #' @returns A numeric value in ms from the input string.
  #'
  #' @examples
  #' HHMMSSmmm_to_ms() # 01:01:01:333 in ms
  #' @export
  HHMMSSmmm_to_ms <- function(HHMMSSmmm = "01:01:01:333") {
    # Check parameters
    if (!is.character(HHMMSSmmm)) {
      stop("HHMMSSmmm must be a string.")
    }
    
    if (stringr::str_detect(HHMMSSmmm,
                            "([0-9]{2}):([0-9]{2}):([0-9]{2}):([0-9]{3})")) {
      time_segs <- stringr::str_match(HHMMSSmmm,
                                      "([0-9]{2}):([0-9]{2}):([0-9]{2}):([0-9]{3})")
      as.numeric(time_segs[5]) + as.numeric(time_segs[4]) * 
        1000 + as.numeric(time_segs[3]) * 1000 * 60 +
        as.numeric(time_segs[2]) * 1000 * 60 * 60
    } else {
      NULL
    }
  }
  
  #----------------------------------------------------------------------------
  #' Show Databrary Release Levels
  #'
  #' @returns A data frame with Databrary's release levels.
  #'
  #' @inheritParams options_params
  #'
  #' @examples
  #' \donttest{
  #' get_release_levels()
  #' }
  #'
  #' @export
  get_release_levels <- function(vb = options::opt("vb")) {
  enums <- get_release_levels_enums()
  vapply(enums$levels, function(item) item$code, character(1))
  }
  
  #----------------------------------------------------------------------------
  #' Extracts File Types Supported by Databrary.
  #'
  #'
  #' @returns A data frame with the file types permitted on Databrary.
  #'
  #' @inheritParams options_params
  #'
  #' @examples
  #' \donttest{
  #' get_supported_file_types()
  #' }
  #'
  #' @export
  get_supported_file_types <- function(vb = options::opt("vb")) {
  constants <- assign_constants(vb = vb)
  constants$format_df |>
    dplyr::rename(
      asset_type = name,
      asset_type_id = id,
      asset_category = category
    )
  }
  
  #----------------------------------------------------------------------------
  #' Make Portable File Names
  #'
  #' @param fn Databrary party ID
  #' @param replace_regex A character string. A regular expression to capture
  #' the "non-portable" characters in fn.
  #' @param replacement_char A character string. The character(s) that will 
  #' replace the non-portable characters.
  #'
  #' @returns A "cleaned" portable file name
  #'
  #' @inheritParams options_params
  #'
  make_fn_portable <- function(fn,
                               vb = options::opt("vb"),
                               replace_regex = "[ &\\!\\)\\(\\}\\{\\[\\]\\+\\=@#\\$%\\^\\*]",
                               replacement_char = "_") {
    assertthat::is.string(fn)
    assertthat::assert_that(!is.numeric(fn))
    assertthat::assert_that(!is.logical(fn))
    assertthat::assert_that(length(fn) == 1)
    
    assertthat::assert_that(is.logical(vb))
    assertthat::assert_that(length(vb) == 1)
    
    assertthat::is.string(replace_regex)
    assertthat::assert_that(length(replace_regex) == 1)
    
    assertthat::is.string(replacement_char)
    assertthat::assert_that(length(replacement_char) == 1)
    
    if (vb) {
      non_portable_chars <- stringr::str_detect(fn, replace_regex)
      message("There are ", sum(non_portable_chars), " in ", fn)
    }
    new_fn <- stringr::str_replace_all(fn, replace_regex, replacement_char)
    new_fn
  }
  