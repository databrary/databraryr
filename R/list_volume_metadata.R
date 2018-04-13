#' List volume metadata.
#'
#' @param volume Selected volume number.
#' @param write.header A Boolean value. If TRUE writes a comma-separated header.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with information about the volume owner.
#' @examples
#' list_volume_metadata()
list_volume_metadata <- function(volume = 2,
                                 write.header = FALSE,
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
      stop("DOI must be character string")
    }
  }

  v <- download_containers_records(volume = volume, vb = vb)
  if (!(is.null(v))){
    if (write.header) {
      cat(paste("volume.id", "volume.name", "permission",
                "doi\n", sep=","))
    }
    cat(paste(v$id, surround_w_quotes(v$name[1]), v$owners[1],
              v$permission,
              paste0(make_url_doi(v$doi), "\n"), sep = ","))
  } else {
    message(paste0('No data in volume ', volume, "\n."))
    return(NULL)
  }
}
