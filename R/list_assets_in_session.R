#' Lists assets in a given Databrary volume and session (slot).
#'
#' @param vol_id Selected volume number.
#' @param session_id Slot/session ID.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the assets in the selected volume and session.
#' @examples
#' list_assets_in_session()
#' @export
list_assets_in_session <- function(vol_id = 1, session_id = 9807, vb = FALSE) {
  # Parameter checking----------------------------------------------
  if (length(vol_id) > 1) {
    stop("vol_id must have length == 1.")
  }
  if ((!is.numeric(vol_id)) || vol_id <= 0 ) {
    stop("vol_id must be > 0.")
  }
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
  session.url <- paste0("https://nyu.databrary.org/api/volume/", vol_id, "/slot/", session_id, "?assets")
  if (vb) {
    message(paste0("Sending GET to ", session.url))
  }
  g <- httr::GET(session.url)
  if (httr::status_code(g) == 200) {
    if (vb) message("Successful HTML call.")
    g.content <- httr::content(g, 'text', encoding = 'UTF-8')
    d.sess <- jsonlite::fromJSON(g.content)
    if (!is.null(d.sess)) {
      if (vb) message("Making data frame from returned content.")
      if (is.data.frame(d.sess$assets)) {
        df <- data.frame(d.sess$assets)
        df$vol_id <- vol_id
        df$session_id <- session_id
        # df <- dplyr::rename(df, asset.id = id,
        #                     asset.type.id = format,
        #                     asset.name = name)
        df <- dplyr::rename(df, asset_id = id,
                            asset_type_id = format)
        # df <- dplyr::select(df, vol_id, session_id, asset.id,
        #                     asset.type.id, classification, name, permission,
        #                     size, duration)
        df <- format_to_filetypes(df, vb = vb)
      } else {
        # Handle case of single value in assets field
        df <- NULL
      }
      return(df)
    } else {
      return(NULL)
    }
  } else if (vb) {
    message(paste0( 'Download Failed, HTTP status ', httr::status_code(g)))
  }
}

#==================================================================
#' Lists assets in a given Databrary volume and session (slot).
#'
#' @param vol_id Selected volume number.
#' @param session_id Slot/session ID.
#' @media.type A string indicating what type of file.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the assets in the selected volume and session.
#' @examples
#' list_specified_assets_in_session()
#' @export
list_specified_assets_in_session <- function(vol_id = 1,
                                             session_id = 9807,
                                             media.type = 'MPEG-4 video',
                                             vb = FALSE) {

  # List all assets
  if (vb) message('list_specified_assets_in_session()...')
  al <- list_assets_in_session(vol_id, session_id, vb = vb)
  dplyr::filter(al, asset.type == media.type)
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
  #df <- dplyr::left_join(vol_assets, fts, by = c("format" = "id"))
  df <- dplyr::left_join(vol_assets, fts, by = c('asset_type_id'))
  # Since the function is more commonly used by volume level functions, we don't resort here
  # dplyr::select(df, vol_id, session_id, asset.name, size, duration, mimetype,
  #               extension, transcodable)
  df
}
