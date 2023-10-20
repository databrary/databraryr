#' List Volume Metadata.
#'
#' @param vol_id Selected volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with metadata about a volume.
#' @examples
#' \donttest{
#' list_volume_metadata() # List info about volume 1.
#' }
#' @export
list_volume_metadata <- function(vol_id = 1,
                                 vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # Declare helpers----------------------------------------------------------
  make_url_doi <- function(doi) {
    if (is.character(doi)) {
      paste0("https://doi.org/", doi)
    } else {
      paste("NA")
      #stop("DOI must be character string")
    }
  }
  
  flatten_names <- function(owners) {
    stringr::str_flatten(owners, collapse = "; ")
  }
  
  # Body of function--------------------------------------------------------------
  v <- list_containers_records(vol_id = vol_id, vb = vb)
  if (!(is.null(v))) {
    data.frame(
      vol_id = v$id,
      name = v$name,
      owners = flatten_names(v$owners[, 'name']),
      permission = v$permission,
      doi = make_url_doi(v$doi)
    )
  } else {
    if (vb) {
      message(paste0('No data in volume ', vol_id))
    }
    return(NULL)
  }
}
