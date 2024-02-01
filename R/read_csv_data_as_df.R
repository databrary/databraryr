#' Download Stored CSV As Data Frame.
#'
#' If a given Databrary session has a CSV file, this function will
#' try to download it as a data frame.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function has been deprecated and may be removed in a future release.
#' See `download_session_asset()` for more similar, but more general functionality.
#'
#' @param session_id Session ID within some volume.
#' @param asset_id Asset ID within the session.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' 
#' @returns A data frame with the CSV data.
#' 
#' @examples
#' \donttest{
#' read_csv_data_as_df() # Downloads a CSV depicting Databrary growth from volume 1
#' }
#' @export
read_csv_data_as_df <-
  function(session_id = 9807,
           asset_id = 153108,
           vb = FALSE) {
    
    # This is a spreadsheet from volume 1, session/slot 9807
    
    # Check parameters
    assertthat::assert_that(length(session_id) == 1)
    assertthat::assert_that(is.numeric(session_id))
    assertthat::assert_that(session_id > 0)
    
    assertthat::assert_that(length(asset_id) == 1)
    assertthat::assert_that(is.numeric(asset_id))
    assertthat::assert_that(asset_id > 0)
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    
    r <- GET_db_contents(
      base_URL = 'https://nyu.databrary.org/slot/',
      URL_components = paste0(session_id, '/0/asset/', asset_id,
                              '/download?inline=false'),
      vb = vb,
      convert_JSON = FALSE
    )
    
    if (!is.null(r)) {
      if (vb) message("Valid CSV downloaded.")
        as.data.frame(r)
    } else {
      if (vb)
        message("No CSV data returned for session_id ", 
                session_id, " and asset_id ", asset_id)
      r
    }
  }
