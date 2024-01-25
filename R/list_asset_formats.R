#' List Stored Assets (Files) By Type.
#'
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about the data formats Databrary
#' supports.
#' @examples
#' \donttest{
#' list_asset_formats()
#' }
#' @export
list_asset_formats <- function(vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  db_constants <- assign_constants()
  if (!is.null(db_constants$format)) {
    purrr::map(db_constants$format, as.data.frame) |>
      purrr::list_rbind() |>
      dplyr::rename(format_id = id,
                    format_mimetype = mimetype,
                    format_extension = extension,
                    format_name = name,
                    format_transcodable = transcodable)
  } else {
    if (vb)
      message("No format information retrieved.")
    return(NULL)
  }
}
