#' Check SSL certificates for nyu.databary.org.
#' 
#' @param host Target URL. Defaults to 'nyu.databrary.org'.
#' @examples
#' check_ssl_certs()
#' @export
check_ssl_certs <- function(host = "nyu.databrary.org") {
  stopifnot(is.character(host))
  
  message(paste0('Checking SSL certificates for host: ', host))
  x <- openssl::download_ssl_cert(host)
  validity_dates <- lapply(x, `[[`, "validity")
  issuer <- lapply(x, `[[`, "issuer")
  
  df <- data.frame(issuer = unlist(issuer),
                   start_date = unlist(lapply(validity_dates, '[[', 1)),
                   exp_date = unlist(lapply(validity_dates, '[[', 2)))
  
  df
}
