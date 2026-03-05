#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Download Databrary Constants From API.
#'
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to NULL.
#'
#' @returns A data frame with the constants.
#'
#' @inheritParams options_params
#'
#' @examples
#' \dontrun{
#' assign_constants()
#' }
#' @export
assign_constants <- function(vb = options::opt("vb"), rq = NULL) {
  validate_flag(vb, "vb")
  if (vb) {
    message("Retrieving grouped formats and static enums.")
  }

  grouped <- perform_api_get(
    path = API_GROUPED_FORMATS,
    rq = rq,
    vb = vb,
    normalize = TRUE
  )

  if (is.null(grouped)) {
    message("Unable to load grouped format metadata from Databrary.")
    return(NULL)
  }

  lists <- grouped$root
  if (is.null(lists)) {
    lists <- grouped
  }

  format_entries <- purrr::imap(lists, function(items, category) {
    purrr::map(items, function(item) {
      item$category <- category
      item
    })
  }) |>
    purrr::list_c()

  formats_df <- purrr::map(format_entries, tibble::as_tibble) |>
    purrr::list_rbind()

  list(
    format = format_entries,
    format_df = formats_df,
    permission = get_permission_levels_enums(),
    release = get_release_levels_enums()
  )
}
