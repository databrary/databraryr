#' Lists stored assets (files) by type.
#'
#' @param volume Volume ID
#' @param type Data file type, e.g. "video", "pdf", or "csv"
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return Data frame with the selected asset types.
#' @examples
#' list_assets_by_type()
#' @export
list_assets_by_type <- function(volume = 1, type = "video",
                                vb = FALSE) {
  # Error checking ----------------------------------------------------------
  if (!is.numeric(volume)) {
    stop("Volume must be numeric.")
  }
  if (volume < 1) {
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
  va <- list_assets_in_volume(volume = volume, vb = vb)
  if (is.null(va)) {
    if (vb) message("Assets not available for volume ", volume, ".\n")
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

  m_df <- dplyr::left_join(va, file.types, by = c("format" = "id"))
  files.of.given.type <- dplyr::filter(m_df, mimetype %in% these.types)

  if ((dim(files.of.given.type)[1] == 0) || (is.null(files.of.given.type))) {
    if (vb) message(paste0("No supported files of type ", type, " found.\n"))
    return (NULL)
  } else {
    # not all assets have name or sess.date...
    l <- dplyr::mutate(files.of.given.type, asset.id = id, session.id = sess.id)
    l <- dplyr::select(l, vol.id, session.id, asset.id, format, duration,
                       permission, mimetype, extension)
    return(l)
  }
#
#   l <- dplyr::mutate(files.of.given.type, asset.id = id, asset.name = name.x)
#   l <- dplyr::select(l, vol.id, sess.id, sess.date, sess.release,
#                      asset.id, asset.name, format, permission, size, duration,
#                      mimetype, extension)
#   # #l <- dplyr::filter(l, mimetype %in% these.types)
#   #
#   # if  ((dim(l)[1] == 0) || (is.null(l))) {
#   #   stop(paste0("No files of type '", type, "' found.\n"))
#   # }
#   return(l)
}
