#' List volume metadata.
#'
#' @param volume Selected volume number.
#' @param write.header A Boolean value. If TRUE writes a comma-separated header.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with information about the volume.
#' @examples
#' list_volume_metadata()
#' @export
list_volume_metadata <- function(volume = 2,
                                 write.header = FALSE,
                                 data.frame = TRUE,
                                 vb = FALSE) {

  # Error-checking
  if (!(is.numeric(volume))) {
    stop("Volume must be a number.")
  }
  if (volume <= 0) {
    stop("Volume must be > 0.")
  }

  # Declare helpers
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

  # Body of function
  v <- download_containers_records(volume = volume, vb = vb)
  if (!(is.null(v))){
    if (write.header) {
      cat(paste("volume.id", "volume.name", "owners", "permission",
                "doi\n", sep=","))
    }
    if (data.frame) {
      data.frame(id = v$id, name = v$name,
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
      message(paste0('No data in volume ', volume))
    }
    return(NULL)
  }
}
