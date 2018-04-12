#' Lists stored assets (files) by type.
#'
#' @param volume Volume ID
#' @param type Data file type, e.g. "video", "pdf", or "csv"
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return Data frame with the selected asset types.
#' @examples
#' list_assets_by_type()
list_assets_by_type <- function(volume = 1, type = "video",
                                vb = FALSE) {
  # Error checking
  if (!is.numeric(volume)) {
    stop("Volume must be numeric.")
  }
  if (volume < 1) {
    stop("Volume must be >= 1.")
  }
  if (!is.character(type)) {
    stop("Asset type must be character.")
  }

  va <- databraryapi::list_assets_in_volume(volume = volume, vb = vb)
  file.types <- databraryapi::get_supported_file_types(vb = vb)
  these.types <- file.types$mimetype[stringr::str_detect(file.types$mimetype, type)]
  if (is.null(these.types)) {
    stop(paste0("Invalid data type '", type))
  }
  if (vb) {
    message(paste0("Searching for files of type '", type))
  }

  va %>%
    dplyr::left_join(., file.types, by = c("format" = "id")) ->
    files.of.given.type

  if ((dim(files.of.given.type)[1] == 0) || (is.null(files.of.given.type))) {
    stop(paste0("No files of type '", type, "' found.\n"))
  }

  files.of.given.type %>%
    dplyr::mutate(., asset.id = id, asset.name = name.x) %>%
    dplyr::select(., vol.id, sess.id, sess.date, sess.release,
           asset.id, asset.name, format, permission, size, duration,
           mimetype, extension) %>%
    dplyr::filter(., mimetype %in% these.types) ->
    l

  if  ((dim(l)[1] == 0) || (is.null(l))) {
    stop(paste0("No files of type '", type, "' found.\n"))
  }
  return(l)
}
