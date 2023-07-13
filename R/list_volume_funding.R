#' Lists Funders Associated With a Databrary Volume.
#'
#' @param vol_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A tibble (data.frame) with the requested data.
#' @examples
#' list_volume_funding()
#' @export
list_volume_funding <- function(vol_id = 1, vb = FALSE) {
  if (length(vol_id) > 1) {
    stop("'vol_id' must have length == 1.")
  }
  if (!is.numeric(vol_id)) {
    stop("'vol_id' must be an integer.")
  }
  if (vol_id < 0) {
    stop("'vol_id' must be > 0.")
  }

  if (length(vb) > 1) {
    stop("'vb' must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be a Boolean.")
  }

  g <-
    databraryr::GET_db_contents(
      URL_components = paste0("/api/volume/", vol_id,
                              "?funding=all"),
      vb = vb
    )
  if (is.data.frame(g$funding)) {
    f_df <- g$funding
    f_df <- normalize_funder_dataframe(f_df)
    f_df$vol_id <- vol_id
    f_df[c("vol_id", "funder_id", "funder_name", "award")]
  } else {
    if (vb) {
      message("'funding' is NULL.")
    }
    data.frame(`vol_id` = vol_id, `funder_id` = NA, `funder_name` = NA, `award` = NA)
  }
}

replace_empty_award_vals <- function(f) {
  # Replace empty funder award numbers
  r <- f$awards
  n_awards <- length(r)
  for (f in 1:n_awards) {
    if (length(r[[f]]) == 0)
      r[[f]] <- NA
  }
  r
}

normalize_funder_dataframe <- function(x) {
  # Coerce weird output when there are more than one award for a source
  if (is.data.frame(x)) {
    f <- x$funder
    f$award <- replace_empty_award_vals(x)
    f <- tidyr::unnest(f, "award")
    dplyr::rename(f, funder_id = "id", funder_name = "name")
  }
}
