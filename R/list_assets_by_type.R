#' List Stored Assets (Files) By Type.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated and may be removed from a future version of 
#' the package. `list_volume_assets()` is preferred.
#'
#' @param vol_id Volume ID
#' @param type Data file type, e.g. "video", "pdf", or "csv"
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about assets in a volume, sorted by
#' type.
#' 
#' @examples
#' \donttest{
#' list_assets_by_type() # Lists assets in volume 2.
#' }
#' @export
list_assets_by_type <- function(vol_id = 2,
                                type = "video",
                                vb = FALSE) {

  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(type) == 1)
  assertthat::assert_that(is.character(type))

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  if (!(
    type %in% c(
      "video",
      "audio",
      "image",
      "text",
      "pdf",
      "msword",
      "ms-excel",
      "datavyu",
      "spss",
      "lena",
      "elan"
    )
  )) {
    stop("Invalid type.")
  }

  # Retrieve, process asset list --------------------------------------------
  va <- list_assets_in_volume(vol_id = vol_id, vb = vb)
  if (is.null(va)) {
    if (vb)
      message("Assets not available for volume ", vol_id, ".\n")
    return(NULL)
  }
  
  file_types <- get_supported_file_types(vb = vb)
  these_types <-
    file_types$mimetype[stringr::str_detect(file_types$mimetype, type)]
  if (is.null(these_types)) {
    stop(paste0("Invalid data type '", type))
  }
  if (vb) {
    message(paste0("Searching for files of type '", type))
  }
  
  files_of_given_type <-
    dplyr::filter(va, va$mimetype %in% these_types)
  
  if ((dim(files_of_given_type)[1] == 0) ||
      (is.null(files_of_given_type))) {
    if (vb)
      message(paste0("No supported files of type ", type, " found.\n"))
    NULL
  } else {
    # not all assets have name or sess.date...
    # l <- dplyr::select(files.of.given.type, vol_id, session.id, asset.id, format, duration,
    #                    permission, mimetype, extension)
    files_of_given_type
  }
}
