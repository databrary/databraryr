#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Activity History in Databrary Session.
#'
#' @description For an accessible session, returns the logged history events associated with
#' the session. Requires authenticated access with sufficient permissions.
#'
#' @param vol_id Volume identifier (required by the Django API). Must be a positive integer.
#' @param session_id Session identifier. Must be a positive integer.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`. When `NULL`, a
#'   default request is generated, but this will only permit public information
#'   to be returned.
#'
#' @returns A tibble with the activity history for a session, or `NULL` when
#'   no data is available.
#'
#' @inheritParams options_params
#'
#' @examples
#' \\donttest{
#' \\dontrun{
#' list_session_activity(vol_id = 1892, session_id = 76113)
#' }
#' }
#' @export
list_session_activity <-
  function(vol_id = 1892,
           session_id = 76113,
           vb = options::opt("vb"),
           rq = NULL) {
    # Check parameters
    assertthat::assert_that(length(vol_id) == 1)
    assertthat::assert_that(is.numeric(vol_id))
    assertthat::assert_that(vol_id > 0)
    
    assertthat::assert_that(length(session_id) == 1)
    assertthat::assert_that(is.numeric(session_id))
    assertthat::assert_that(session_id > 0)
    
    assertthat::assert_that(length(vb) == 1)
    validate_flag(vb, "vb")
    
    assertthat::assert_that(is.null(rq) ||
                              inherits(rq, "httr2_request"))
    
    if (is.null(rq)) {
      rq <- databraryr::make_default_request()
    }
    rq <- httr2::req_timeout(rq, REQUEST_TIMEOUT_VERY_LONG)
    
    activities <- collect_paginated_get(
      path = sprintf(API_VOLUME_HISTORY, vol_id),
      rq = rq,
      vb = vb
    )
    
    if (is.null(activities) || length(activities) == 0) {
      if (vb) {
        message("No activity history available for volume ", vol_id)
      }
      return(NULL)
    }
    
    session_details <- databraryr::get_session_by_id(
      session_id = session_id,
      vol_id = vol_id,
      vb = vb,
      rq = rq
    )
    session_name <- NULL
    if (!is.null(session_details)) {
      session_name <- session_details$name
    }
    
    session_entries <- purrr::keep(activities, function(entry) {
      session_identifier <- entry$session_id
      if (is.null(session_identifier) && !is.null(entry$session)) {
        session_value <- entry$session
        if (is.list(session_value) && !is.null(session_value$id)) {
          session_identifier <- session_value$id
        } else {
          session_identifier <- session_value
        }
      }
      
      if (!is.null(session_identifier)) {
        return(isTRUE(session_identifier == session_id))
      }
      
      if (!is.null(session_name) && !is.null(entry$name)) {
        return(isTRUE(entry$name == session_name))
      }
      
      FALSE
    })
    
    if (length(session_entries) == 0) {
      if (vb) {
        message("No activity history for session ",
                session_id,
                " within volume ",
                vol_id)
      }
      return(NULL)
    }
    
    purrr::map_dfr(session_entries, function(entry) {
      history_user <- entry$history_user
      folder_id <- entry$folder_id
      if (is.null(folder_id) && !is.null(entry$folder)) {
        folder <- entry$folder
        if (is.list(folder) && !is.null(folder$id)) {
          folder_id <- folder$id
        } else {
          folder_id <- folder
        }
      }
      
      safe_int <- function(value) {
        if (is.null(value))
          NA_integer_
        else
          value
      }
      
      safe_chr <- function(value) {
        if (is.null(value))
          NA_character_
        else
          value
      }
      
      tibble::tibble(
        event_type = entry$type,
        event_timestamp = entry$timestamp,
        history_id = safe_int(entry$history_id),
        history_user_id = safe_int(history_user$id),
        history_user_email = safe_chr(history_user$email),
        history_user_first_name = safe_chr(history_user$first_name),
        history_user_last_name = safe_chr(history_user$last_name),
        ip_address = entry$ip_address,
        changed_fields = list(entry$changed_fields),
        changed_data = list(entry$changed_data),
        volume_id = vol_id,
        session_id = session_id,
        session_name = safe_chr(entry$name),
        folder_id = safe_int(folder_id),
        deleted_at = entry$deleted_at
      )
    })
  }
