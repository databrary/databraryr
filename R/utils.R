# Utility functions for the databraryr package.

utils::globalVariables(c("name", "id", "category"))

#------------------------------------------------------------------------------
#' @eval options::as_params()
#' @name options_params
#'
NULL


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
  validate_flag(vb, "vb")
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

  if (stringr::str_detect(HHMMSSmmm, "([0-9]{2}):([0-9]{2}):([0-9]{2}):([0-9]{3})")) {
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
  validate_flag(vb, "vb")
  enums <- get_release_levels_enums()
  vapply(enums$levels, function(item) {
    item$code
  }, character(1))
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
#' \dontrun{
#' get_supported_file_types()
#' }
#'
#' @export
get_supported_file_types <- function(vb = options::opt("vb")) {
  validate_flag(vb, "vb")
  constants <- assign_constants(vb = vb)
  if (is.null(constants)) {
    return(NULL)
  }
  df <- constants$format_df
  if (is.null(df) || !is.data.frame(df)) {
    return(NULL)
  }
  req_names <- c("name", "id", "category")
  if (length(setdiff(req_names, names(df))) > 0L) {
    return(NULL)
  }
  df |>
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
  assertthat::assert_that(assertthat::is.string(fn))
  assertthat::assert_that(!is.numeric(fn))
  assertthat::assert_that(!is.logical(fn))
  assertthat::assert_that(length(fn) == 1)

  validate_flag(vb, "vb")

  assertthat::assert_that(assertthat::is.string(replace_regex))
  assertthat::assert_that(length(replace_regex) == 1)

  assertthat::assert_that(assertthat::is.string(replacement_char))
  assertthat::assert_that(length(replacement_char) == 1)

  if (vb) {
    non_portable_chars <- stringr::str_detect(fn, replace_regex)
    message("There are ", sum(non_portable_chars), " in ", fn)
  }
  new_fn <- stringr::str_replace_all(fn, replace_regex, replacement_char)
  new_fn
}
