list_assets_by_type <- function(volume = 1, type = "video") {
  va <- list_assets_in_volume(volume = volume)
  file.types <- get_supported_file_types()
  these.types <- file.types$mimetype[stringr::str_detect(file.types$mimetype, type)]
  va %>%
    left_join(., file.types, by = c("format" = "id")) %>%
    mutate(., asset.id = id, asset.name = name.x) %>%
    select(., vol.id, sess.id, sess.name, sess.date, sess.release,
           asset.id, asset.name, format, permission, size, duration,
           mimetype, extension) %>%
    filter(., mimetype %in% these.types) -> l
  return(l)
}
