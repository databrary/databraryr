#' List volume metadata.
#'
#' @param vol_id Selected volume number.
#' @param write_header A Boolean value. If TRUE writes a comma-separated header.
#' @param data_frame A Boolean value. If TRUE writes a data frame as output.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with information about the volume.
#' @examples
#' list_volume_metadata()
#' @export
list_volume_metadata <- function(vol_id = 2,
                                 write_header = FALSE,
                                 data_frame = TRUE,
                                 vb = FALSE) {

  # Error-checking----------------------------------------------------------
  if (!(is.numeric(vol_id))) {
    stop("Volume must be a number.")
  }
  if (vol_id <= 0) {
    stop("Volume must be > 0.")
  }
  if (!is.logical(write_header)) {
    stop("write_header must be type logical.")
  }
  if (length(write_header) > 1) {
    stop("write_header must have length == 1")
  }
  if (!is.logical(data_frame)) {
    stop("data_frame must be type logical.")
  }
  if (length(data_frame) > 1) {
    stop("data_frame must have length == 1")
  }
  if (!is.logical(vb)) {
    stop("vb must be type logical.")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1")
  }

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
  v <- download_containers_records(vol_id = vol_id, vb = vb)
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
