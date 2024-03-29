#' List Info for a Databrary Volume.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' `list_volume()` has been deprecated in lieu of other, more specific,
#' volume-related functions. It will be dropped from a future release.
#'
#' @param vol_id Target volume number.
#' @param search_str The specific data to retrieve for the volume
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about a volume.
#' 
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume() # Default is volume 1
#' }
#' }
#' @export
list_volume <- function(vol_id = 1, search_str = "",
                      vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)

  assertthat::assert_that(length(search_str) == 1)
  assertthat::assert_that(is.character(search_str))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  if (vb)
    message(paste0("Getting data for volume ", vol_id, "."))
  vol_search_str <- make_vol_search_string(search_str, vb=vb)
  if (vb) message(vol_search_str)
  
  g <-
    databraryr::GET_db_contents(
      URL_components = paste0("/api/volume/", vol_id, vol_search_str),
      vb = vb
    )

  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for volume ", vol_id, "."))
      g
    } else {
    if (vb)
      message(paste0("No data for volume ", vol_id, "."))
    NULL
  }
}

search_str_element_valid <- function(search_str_element) {
  if (!(search_str_element %in% c("access", "citation",
                           "links", "funding", "top",
                           "tags", "excerpts",
                           "comments", "records",
                           "containers=all", "metrics",
                           "state"))) {
    FALSE
  } else {
    TRUE
  }
}

return_valid_search_str_elements <- function(search_str = "", default_str = "", vb) {
  if (!is.character(search_str)) {
    stop("'search_str' must be a string.")
  }
  if (!is.character(default_str)) {
    stop("'default_str' must be a string.")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be logical value.")
  }
  valid_terms <- purrr::map_lgl(search_str, search_str_element_valid)
  if (sum(valid_terms) <= 0) {
    if (vb) message(paste0("No valid search terms. Using default value: '", default_str, "'."))
    ""
  } else {
    if (vb) message(paste0("Returning valid search terms."))
    search_str[valid_terms]
  }
}

make_vol_search_string <- function(search_str = "", default_str = "",
                                   vb = FALSE) {
  if (vb) message("Making valid search string.")
  vse <- return_valid_search_str_elements(search_str,
                                          default_str = default_str,
                                          vb = vb)
  if (length(vse) == 1) {
    if (vb) message("Single valid search term.")
    paste0("?", vse)
  } else {
    if (vb) message("Multiple valid search terms.")
    paste0("?", paste(vse, collapse = "&"))
  }
}
