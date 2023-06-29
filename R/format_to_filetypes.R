#' Converts volume asset data frame to one with media types.
#'
#' @param vol_assets A data frame of volume assets.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return A data frame with information about the vol.id owner(s).
#' @examples
#' format_to_filetypes()
#' @export
format_to_filetypes <- function(vol_assets, vb = FALSE) {
  # Parameter checking -------------------------------------------------------------------
  if (is.null(vol_assets)) {
    stop("Must provide non-NULL data frame of volume assets")
  }
  if (!is.data.frame(vol_assets)) {
    stop("vol_assets must be a data frame.")
  }
  if (!("format" %in% names(vol_assets))) {
    stop("No 'format' field to match in volume assets.")
  }

  # Get file types -----------------------------------------------------------------------
  if (vb) message("Getting supported file types.")
  fts <- get_supported_file_types()
  if (is.null(fts)) {
    stop("List of supported file types not available.")
  }

  # Merge file types with assets in volume -----------------------------------------------
  if (vb) message("Matching file types to those in specified volume.")
  df <- dplyr::left_join(vol_assets, fts, by = c("format" = "id"))
  df <- dplyr::rename(df, 
                      vol_id = vol.id,
                      asset_name = name.x,
                      asset_type = name.y,
                      vol_id = vol.id,
                      session_id = session.id,
                      a)
  dplyr::select(df, vol.id, session_id, asset_name, classification, size, duration, mimetype,
                extension, transcodable)
}
