#' Extracts Databrary supported file types.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the file types.
#' @examples
#' get_supported_file_types()
get_supported_file_types <- function(vb = FALSE) {
  c <- assign_constants(vb = vb)
  ft <- Reduce(function(x,y) merge(x, y, all=TRUE), c$format)
  return(ft)
}
