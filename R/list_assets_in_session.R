#========================================================================================
#' Lists assets in a given Databrary volume and session (slot).
#'
#' @param session_id Slot/session ID.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the assets in the selected volume and session.
#' @examples
#' list_assets_in_session()
#' @export
list_assets_in_session <- function(session_id = 9807, vb = FALSE) {
  # Parameter checking----------------------------------------------
  if (length(session_id) > 1) {
    stop("session_id must have length == 1.")
  }
  if ((!is.numeric(session_id)) || session_id <= 0 ) {
    stop("session_id must be > 0.")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }
  if (vb) message('list_assets_in_session()...')

  # Make URL, GET(), and handle response ---------------------------

  r <- GET_db_contents(URL_components = paste0('/api/slot/', session_id,
                                               '/-?assets'), vb = vb)
  if (!is.null(r)) {
    # if (vb) message("Making data frame from returned content.")
    if (is.data.frame(r$assets)) {
      df <- data.frame(r$assets)
      df <- dplyr::mutate(df, session_id = session_id)
      df <- dplyr::rename(df, asset_id = id,
                          asset_type_id = format)
      df <- format_to_filetypes(df, vb = vb)
    } else {
      df <- NULL
    }
    return(df)
  } else {
    if (vb) message('Download failed')
  }

}

#==================================================================
#' Lists assets in a given Databrary volume and session (slot).
#'
#' @param session_id Slot/session ID.
#' @media_type A string indicating what type of file.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the assets in the selected volume and session.
#' @examples
#' list_specified_assets_in_session()
#' @export
list_specified_assets_in_session <- function(session_id = 9807,
                                             media_type = 'MPEG-4 video',
                                             vb = FALSE) {

  # List all assets
  if (vb) message('list_specified_assets_in_session()...')
  al <- list_assets_in_session(session_id, vb = vb)
  dplyr::filter(al, asset_type == media_type)
}

#========================================================================================
#' Converts volume asset data frame to one with media types.
format_to_filetypes <- function(vol_assets, vb = FALSE) {
  # Parameter checking -------------------------------------------------------------------
  if (is.null(vol_assets)) {
    stop("Must provide non-NULL data frame of volume assets")
  }
  if (!is.data.frame(vol_assets)) {
    stop("vol_assets must be a data frame.")
  }

  if (vb) message('format_to_filetypes()...')
  # Get file types -----------------------------------------------------------------------
  if (vb) message("Getting supported file types.")
  fts <- get_supported_file_types()
  if (is.null(fts)) {
    stop("List of supported file types not available.")
  }

  # Merge file types with assets in volume -----------------------------------------------
  if (vb) message("Matching file types to those in specified volume.")
  df <- dplyr::left_join(vol_assets, fts, by = c('asset_type_id'))
  df
}
