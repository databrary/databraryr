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
  va <- list_assets_in_volume(volume = volume)
  file.types <- get_supported_file_types()
  these.types <- file.types$mimetype[stringr::str_detect(file.types$mimetype, type)]
  if (is.null(these.types)) {
    stop(paste0("Invalid data type '", type, "'\n"))
  }
  if (vb) {
    message(paste0("Searching for files of type '", type, "'\n"))
  }

  va %>%
    dplyr::left_join(., file.types, by = c("format" = "id")) ->
    files.of.given.type

  files.of.given.type %>%
    dplyr::mutate(., asset.id = id, asset.name = name.x) %>%
    dplyr::select(., vol.id, sess.id, sess.date, sess.release,
           asset.id, asset.name, format, permission, size, duration,
           mimetype, extension) %>%
    dplyr::filter(., mimetype %in% these.types) -> l

  if ((is.null(l)) ||
      (dim(l)[1] == 0)) {
    stop(paste0("No files of type '", type, "' found.\n"))
  }
  return(l)
}
