#' Download Stored CSV As Data Frame.
#' 
#' If a given Databrary session has a CSV file, this function will
#' try to download it as a data frame.
#'
#' @param session_id Session ID within some volume.
#' @param asset_id Asset ID within the session.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with the CSV data.
#' @examples
#' read_csv_data_as_df() # Loads CSV depicting Databrary growth from volume 1
#' @export
read_csv_data_as_df <- function(session_id = 9807, asset_id = 153108,
                                vb = FALSE) {
  # This is a spreadsheet in volume 1, slot 9807
  # Parameter checking----------------------------------------------------------
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

  # Handle request---------------------------------------------------------------
  r <- GET_db_contents(base_URL = 'https://nyu.databrary.org/slot/',
                       URL_components = paste0(session_id, '/0/asset/', asset_id,
                                               '/download?inline=false'),
                       vb = vb,
                       convert_JSON = FALSE)

  df <- utils::read.csv(text = r)
  if (is(df, "data.frame")) {
    return(df)
  } else {
    if (vb) message("Can't coerce to data frame. Skipping.\n")
    return(NULL)
  }
}
