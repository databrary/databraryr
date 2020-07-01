#' Checks the SSL certificates for nyu.databary.org.
#' @param url Target URL. Defaults to 'nyu.databrary.org'.
#' @examples
#' check_databrary_ssl_certs()
#' @export
check_databrary_ssl_certs <- function(url = "nyu.databrary.org") {
  if (!is.character(url)) {
    stop('URL must be character string.')
  }
  x <- openssl::download_ssl_cert("nyu.databrary.org")
  validity_dates <- lapply(x, `[[`, "validity")
  issuer <- lapply(x, `[[`, "issuer")
  df <- data.frame(issuer = unlist(issuer),
                   start_date = unlist(lapply(validity_dates, '[[', 1)),
                   exp_date = unlist(lapply(validity_dates, '[[', 2)))
  df
}
