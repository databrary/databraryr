#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Assets in a Session from a Databrary volume.
#'
#'#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `list_volume_session_assets()` is a new name for the <v0.6.2 `list_session_assets()` as
#' function. There is a new `list_session_assets()` function that does not
#' requre the volume ID. The `list_volume_session_assets()` *requires* a volume
#' ID.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id The session number in the selected volume.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object.
#'
#' @returns A data frame with information about all assets in a volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_session_assets() # Defaults to session 11 in volume 2
#' }
#' }
#' @export
list_volume_session_assets <-
  function(vol_id = 2,
           session_id = 11,
           vb = options::opt("vb"),
           rq = NULL) {
    assertthat::assert_that(length(vol_id) == 1)
    assertthat::assert_that(is.numeric(vol_id))
    assertthat::assert_that(vol_id >= 1)
    
    assertthat::assert_that(length(session_id) == 1)
    assertthat::assert_that(is.numeric(session_id))
    assertthat::assert_that(session_id >= 1)
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    
    assertthat::assert_that(is.null(rq) |
                              ("httr2_request" %in% class(rq)))
    
    # Not needed in DB2 API. Delete.
    # if (is.null(rq)) {
    #   if (vb) {
    #     message("NULL request object. Will generate default.")
    #   }
    #  rq <- databraryr::make_default_request()
    # }
    
    session <- perform_api_get(
      path = sprintf(API_SESSION_DETAIL, vol_id, session_id),
      rq = rq,
      vb = vb
    )
    
    if (is.null(session)) {
      if (vb)
        message("No matching session_id: ", session_id)
      return(NULL)
    }
    
    files <- collect_paginated_get(
      path = sprintf(API_SESSION_FILES, vol_id, session_id),
      rq = rq,
      vb = vb
    )
    
    if (is.null(files) || length(files) == 0) {
      if (vb)
        message("No assets in vol_id ", vol_id, " session_id ", session_id)
      return(NULL)
    }
    if (vb)
      message("Found n = ",
              length(files),
              " assets in vol_id ",
              vol_id,
              " session_id ",
              session_id)
    
    asset_rows <- purrr::map(files, function(file) {
      format <- file$format
      uploader <- file$uploader
      
      tibble::tibble(
        asset_id = file$id,
        asset_name = file$name,
        asset_permission = file$release_level,
        asset_size = file$size,
        asset_mime_type = format$mimetype,
        asset_format_id = format$id,
        asset_format_name = format$name,
        asset_duration = file$duration,
        asset_created_at = file$created_at,
        asset_updated_at = file$updated_at,
        asset_uploader_id = uploader$id,
        asset_uploader_first_name = uploader$first_name,
        asset_uploader_last_name = uploader$last_name,
        asset_sha1 = file$sha1,
        asset_thumbnail_url = file$thumbnail_url,
        session_id = session$id,
        session_name = session$name,
        session_release = session$release_level
      )
    }) %>%
      purrr::list_rbind()
    
    asset_rows
  }
