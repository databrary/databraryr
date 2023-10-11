# R/utils.R
# 
# Utility functions.

#-------------------------------------------------------------------------------
#' Get Duration (In ms) Of A File.
#'
#' @param asset_id Asset number.
#' @param types_w_durations Asset types that have valid durations.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns Duration of a file in ms.
#' @examples
#' get_file_duration() # default is the test video from databrary.org/volume/1
#' @export
get_file_duration <- function(asset_id = 1,
                              types_w_durations = c("-600", "-800"),
                              vb = FALSE) {
  # Test parameters---------------------------------------------------------
  if (length(asset_id) > 1) {
    stop("asset_id must have length == 1.")
  }
  if (!is.numeric(asset_id)) {
    stop("asset_id must be numeric")
  }
  if (asset_id < 1) {
    stop("asset_id must be >= 1")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical")
  }
  
  r <-
    GET_db_contents(URL_components = paste0('/api/asset/', asset_id),
                    vb = vb)
  if (r$format %in% types_w_durations) {
    r$duration
  } else {
    if (vb)
      message("File type does not have a defined duration.")
    return(NULL)
  }
}

#-------------------------------------------------------------------------------
#' Get Time Range For An Asset.
#'
#' @param vol_id Volume ID
#' @param session_id Slot/session number.
#' @param asset_id Asset number.
#' @param convert_JSON A Boolean value. If TRUE, convert JSON to a data frame. Default is TRUE.
#' @param segment_only A Boolean value. If TRUE, returns only the segment values. Otherwise returns
#' a data frame with two fields, segment and permission. Default is TRUE.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns The time range (in ms) for an asset, if one is indicated.
#' @examples
#' get_asset_segment_range()
#' @export
get_asset_segment_range <- function(vol_id = 1,
                                    session_id = 9807,
                                    asset_id = 1,
                                    convert_JSON = TRUE,
                                    segment_only = TRUE,
                                    vb = FALSE) {
  # Test parameters--------------------------------------------------------
  if (length(vol_id) > 1) {
    stop("vol_id must have length == 1.")
  }
  if (!is.numeric(vol_id)) {
    stop("vol_id must be numeric")
  }
  if (vol_id < 1) {
    stop("vol_id must be >= 1")
  }
  if (length(session_id) > 1) {
    stop("session_id must have length == 1.")
  }
  if (!is.numeric(session_id)) {
    stop("session_id must be numeric")
  }
  if (length(asset_id) > 1) {
    stop("asset_id must have length == 1.")
  }
  if (session_id < 1) {
    stop("slot.id must be >= 1")
  }
  if (!is.numeric(asset_id)) {
    stop("asset_id must be numeric")
  }
  if (asset_id < 1) {
    stop("asset_id must be >= 1")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical")
  }
  
  r <-
    GET_db_contents(
      URL_components = paste0(
        '/api/volume/',
        vol_id,
        "/slot/",
        session_id,
        "/asset/",
        asset_id
      ),
      vb = vb,
      convert_JSON = convert_JSON
    )
  if (vb) {
    message(
      "Returning segment start & end times (in ms) from volume ",
      vol_id,
      ", session ",
      session_id,
      ", asset ",
      asset_id
    )
  }
  if (segment_only) {
    return(r$segment)
  } else {
    return(r)
  }
}

#-------------------------------------------------------------------------------
#' Extract Databrary Permission Levels.
#' 
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns An array with the permission levels that can be assigned to data.
#' @examples
#' get_permission_levels()
#' @export
get_permission_levels <- function(vb = FALSE) {
  c <- assign_constants(vb = vb)
  pl <- c$permission
  return(pl)
}

#-------------------------------------------------------------------------------
#' Convert Timestamp String To ms.
#'
#' @param HHMMSSmmm a string in the format "HH:MM:SS:mmm"
#' @returns A numeric value from the input string.
#' @examples
#' HHMMSSmmm_to_ms() # 01:01:01:333 in ms
#' @export
HHMMSSmmm_to_ms <- function(HHMMSSmmm = "01:01:01:333") {
  # Check parameters
  if (!is.character(HHMMSSmmm)) {
    stop("HHMMSSmmm must be a string.")
  }
  
  if (stringr::str_detect(HHMMSSmmm, "([0-9]{2}):([0-9]{2}):([0-9]{2}):([0-9]{3})")) {
    time_segs <- stringr::str_match(HHMMSSmmm, "([0-9]{2}):([0-9]{2}):([0-9]{2}):([0-9]{3})")
    as.numeric(time_segs[5]) + as.numeric(time_segs[4])*1000 + as.numeric(time_segs[3])*1000*60 +
      as.numeric(time_segs[2])*1000*60*60
  } else {
    NULL
  }
}

#-------------------------------------------------------------------------------
#' Show Databrary Release Levels
#'
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with Databrary's release levels.
#' @examples
#' get_release_levels()
#' @export
get_release_levels <- function(vb = FALSE) {
  c <- assign_constants(vb = vb)
  rl <- c$release
  return(rl)
}

#-------------------------------------------------------------------------------
#' Extracts File Types Supported by Databrary.
#' 
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with the file types permitted on Databrary.
#' @examples
#' get_supported_file_types()
#' @export
get_supported_file_types <- function(vb = FALSE) {
  c <- assign_constants(vb = vb)
  ft <- Reduce(function(x,y) merge(x, y, all=TRUE), c$format)
  ft <- dplyr::rename(ft, asset_type = "name", asset_type_id = "id")
  ft
}

#-------------------------------------------------------------------------------
#' Is This Party An Institution?
#'
#' @param party_id Databrary party ID
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns TRUE or FALSEl.
#' @examples
#' is_institution() # Is party 8 (NYU) an institution.
#' @export
is_institution <- function(party_id=8, vb = FALSE) {
  # Error handling
  if (length(party_id) > 1) {
    stop("party_id must be single value")
  }
  if ((!is.numeric(party_id)) || (party_id <= 0)) {
    stop("party_id must be an integer > 0")
  }
  
  # Process request-----------------------------------------------------
  r <- GET_db_contents(URL_components = paste0('/api/party/', party_id),
                       vb = vb)
  if (("institution" %in% names(r)) && (!is.null(r[['institution']]))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#-------------------------------------------------------------------------------
#' Is This Party A Person?
#'
#' @param party_id Databrary party ID
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns TRUE or FALSE.
#' @examples
#' is_person()
#' @export
is_person <- function(party_id = 7, vb = FALSE){
  return(!is_institution(party_id, vb = vb))
}
