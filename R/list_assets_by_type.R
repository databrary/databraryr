#' Lists stored assets (files) by type.
#'
#' @param vol_id Volume ID
#' @param type Data file type, e.g. "video", "pdf", or "csv"
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return Data frame with the selected asset types.
#' @examples
#' list_assets_by_type()
#' @export
list_assets_by_type <- function(vol_id = 1, type = "video",
                                vb = FALSE) {
  # Error checking ----------------------------------------------------------
  if (!is.numeric(vol_id)) {
    stop("Volume must be numeric.")
  }
  if (vol_id < 1) {
    stop("Volume must be >= 1.")
  }
  if (!is.character(type)) {
    stop("Asset type must be character.")
  }
  if (!(type %in% c("video", "audio", "image", "text", "pdf", "msword",
                    "ms-excel", "datavyu", "spss", "lena", "elan"))) {
    stop("Invalid type.")
  }
  if (!is.logical(vb)) {
    stop("vb type must be logical.")
  }
  if (length(vb) > 1) {
    stop("vb must have length = 1.")
  }
  # Retrieve, process asset list --------------------------------------------
  va <- list_assets_in_volume(vol_id = vol_id, vb = vb)
  if (is.null(va)) {
    if (vb) message("Assets not available for volume ", vol_id, ".\n")
    return(NULL)
  }

  file.types <- get_supported_file_types(vb = vb)
  these.types <- file.types$mimetype[stringr::str_detect(file.types$mimetype, type)]
  if (is.null(these.types)) {
    stop(paste0("Invalid data type '", type))
  }
  if (vb) {
    message(paste0("Searching for files of type '", type))
  }

  files.of.given.type <- dplyr::filter(va, mimetype %in% these.types)

  if ((dim(files.of.given.type)[1] == 0) || (is.null(files.of.given.type))) {
    if (vb) message(paste0("No supported files of type ", type, " found.\n"))
    return (NULL)
  } else {
    # not all assets have name or sess.date...
    # l <- dplyr::select(files.of.given.type, vol_id, session.id, asset.id, format, duration,
    #                    permission, mimetype, extension)
    return(files.of.given.type)
  }
}
