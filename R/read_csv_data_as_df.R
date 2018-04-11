#' Download a stored CSV data file as a data.frame.
#'
#' @param session Session ID within some volume.
#' @param asset Asset ID within the session.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' read_csv_data_as_df() # Loads CSV depicting Databrary growth from volume 1
read_csv_data_as_df <- function(session = 9807, asset = 116888,
                                vb = FALSE) {
  if (!is.numeric(session)) {
    stop("Session ID must be numeric.")
  }
  if (!is.numeric(asset)) {
    stop("Asset ID must be numeric.")
  }
  asset.url <- paste0(databrary.url, "/slot/", session, "/-/asset/", asset, "/download?inline=false")
  if (vb) message(paste0("Sending GET to ", asset.url))
  g <- httr::GET(url = asset.url)
  if (httr::status_code(g) == 200) {
    if (vb) message(paste0("Successful HTML GET query."))
    if (g$headers$`content-type` == "text/csv") {
      df <- read.table(header = T,
                       text = httr::content(g, type = "text"),
                       sep = ",", stringsAsFactors = FALSE)
      return(df)
    } else {
      stop("Invalid file type at URL.")
    }
  } else {
    if (vb) cat(paste0('Download Failed, HTTP status ', httr::status_code(g), "\n."))
    return(NULL)
  }
}
