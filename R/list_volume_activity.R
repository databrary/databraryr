#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Activity In A Databrary Volume
#'
#' If a user has access to a volume, this command lists the modification
#' history of the volume as a
#'
#' @param vol_id Selected volume number.
#' @param rq An `httr2` request object. Defaults to NULL.
#'
#' @returns A list with the activity history on a volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # The following will only return output if the user has write privileges
#' # on the volume.
#'
#' list_volume_activity(vol_id = 1892) # Activity on volume 1892.
#' }
#' }
#' @export
list_volume_activity <-
  function(vol_id = 1892,
           vb = options::opt("vb"),
           rq = NULL) {
    # Check parameters
    assertthat::assert_that(length(vol_id) == 1)
    assertthat::assert_that(is.numeric(vol_id))
    assertthat::assert_that(vol_id > 0)
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    if (vb)
      message('list_volume_activity()...')

    if (is.null(rq)) {
      rq <- databraryr::make_default_request()
    }
    rq <- httr2::req_timeout(rq, REQUEST_TIMEOUT_VERY_LONG)

    activities <- collect_paginated_get(
      path = sprintf(API_VOLUME_HISTORY, vol_id),
      rq = rq,
      vb = vb
    )

    if (is.null(activities)) {
      if (vb)
        message("Cannot access requested resource on Databrary. Exiting.")
      return(NULL)
    }

    purrr::map_dfr(activities, function(entry) {
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

      session_id <- entry$session_id
      if (is.null(session_id) && !is.null(entry$session)) {
        session <- entry$session
        if (is.list(session) && !is.null(session$id)) {
          session_id <- session$id
        } else {
          session_id <- session
        }
      }

      safe_int <- function(value) {
        if (is.null(value)) NA_integer_ else value
      }

      safe_chr <- function(value) {
        if (is.null(value)) NA_character_ else value
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
        session_id = safe_int(session_id),
        session_name = safe_chr(entry$name),
        folder_id = safe_int(folder_id),
        deleted_at = entry$deleted_at
      )
    })
  }
