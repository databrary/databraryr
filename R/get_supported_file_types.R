#' Extracts Databrary supported file types.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the file types permitted on Databrary.
#' @examples
#' get_supported_file_types()
#' @export
get_supported_file_types <- function(vb = FALSE) {
  c <- assign_constants(vb = vb)
  ft <- Reduce(function(x,y) merge(x, y, all=TRUE), c$format)
  ft <- dplyr::rename(ft, asset_type = name, asset_type_id = id)
  ft
}
