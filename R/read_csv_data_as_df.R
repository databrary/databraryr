#' Download a stored CSV data file as a data.frame.
#'
#' @param session_id Session ID within some volume.
#' @param asset_id Asset ID within the session.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' read_csv_data_as_df() # Loads CSV depicting Databrary growth from volume 1
#' @export
read_csv_data_as_df <- function(session_id = 9807, asset_id = 116888,
                                vb = FALSE) {
  if (length(session_id) > 1) {
    stop("session_id must have length == 1.")
  }
  if (!is.numeric(session_id)) {
    stop("session_id must be numeric.")
  }
  if (session_id < 1) {
    stop("session_id must be > 1")
  }
  if (length(asset_id) > 1) {
    stop("asset_id must have length == 1.")
  }
  if (!is.numeric(asset_id)) {
    stop("asset_id must be numeric.")
  }
  if (asset_id < 1) {
    stop("asset_id must be > 1")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be numeric.")
  }

  asset.url <- paste0("https://nyu.databrary.org/slot/", session_id, "/0/asset/", asset_id, "/download?inline=false")
  if (vb) message(paste0("Sending GET to ", asset.url))
  g <- httr::GET(url = asset.url)
  if (httr::status_code(g) == 200) {
    if (vb) message(paste0("Successful HTML GET query."))
    if (g$headers$`content-type` == "text/csv") {
      df <- utils::read.table(header = T,
                       text = httr::content(g, type = "text"),
                       sep = ",", stringsAsFactors = FALSE)
      return(df)
    } else {
      stop("Invalid file type at URL.")
    }
  } else {
    if (vb) cat(paste0('Download Failed, HTTP status ', httr::status_code(g), "\n"))
    return(NULL)
  }
}
