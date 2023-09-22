#' List Volume Metadata.
#'
#' @param vol_id Selected volume number.
#' @param write_header A Boolean value. If TRUE writes a comma-separated header.
#' @param data_frame A Boolean value. If TRUE writes a data frame as output.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with metadata about a volume.
#' @examples
#' list_volume_metadata() # List info about volume 1.
#' @export
list_volume_metadata <- function(vol_id = 1,
                                 write_header = FALSE,
                                 data_frame = TRUE,
                                 vb = FALSE) {

  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)

  assertthat::assert_that(length(write_header) == 1)
  assertthat::assert_that(is.logical(write_header))
  
  assertthat::assert_that(length(data_frame) == 1)
  assertthat::assert_that(is.logical(data_frame))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  # Declare helpers----------------------------------------------------------
  surround_w_quotes <- function(s) {
    paste0('"', s, '"')
  }

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
  # v <- download_containers_records(vol_id = vol_id, vb = vb)
  v <- list_containers_records(vol_id = vol_id, vb = vb)
  if (!(is.null(v))){
    if (write_header) {
      cat(paste("vol_id", "volume.name", "owners", "permission",
                "doi\n", sep=","))
    }
    if (data_frame) {
      data.frame(vol_id = v$id, name = v$name,
                 owners = flatten_names(v$owners[,'name']),
                 permission = v$permission,
                 doi = make_url_doi(v$doi))
    } else {
      cat(paste(v$id, surround_w_quotes(v$name[1]),
                surround_w_quotes(flatten_names(v$owners[,'name'])),
                v$permission,
                paste0(make_url_doi(v$doi), "\n"), sep = ","))
    }
  } else {
    if (vb) {
      message(paste0('No data in volume ', vol_id))
    }
    return(NULL)
  }
}
