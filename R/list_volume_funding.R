#' Lists Funders Associated With a Databrary Volume.
#'
#' @param vol_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2` request object.
#' @returns A data frame with funder information for the volume.
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_funding() # Funding for volume 1
#' 
#' list_volume_funding(vol_id = c(1:10))
#' }
#' }
#' @export
list_volume_funding <- function(vol_id = 1, vb = FALSE, rq = NULL) {
  
  # Check parameters
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(sum(vol_id >= 1) == length(vol_id))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  
  #------------------------------------------------------------
  if (vb) message("Summarizing funding for n=", length(vol_id), " volumes.")
  purrr::map(vol_id,
             list_single_volume_funding,
             vb = vb,
             rq = rq,
             .progress = "Volume funding: ") |>
    purrr::list_rbind()
}


#-------------------------------------------------------------------------------
# Helper function for handling lists
list_single_volume_funding <- function(vol_id = NULL, vb = NULL, rq) {
  # g <-
  #   databraryr::GET_db_contents(
  #     URL_components = paste0("/api/volume/", vol_id,
  #                             "?funding=all"),
  #     vb = vb
  #   )
  # 
  
  if (is.null(rq)) {
    rq <- make_default_request()
  }
  rq <- rq |>
    httr2::req_url(sprintf(GET_VOLUME_FUNDING, vol_id))
  
  resp <- tryCatch(
    httr2::req_perform(rq),
    httr2_error = function(cnd) {
      NULL
    }
  )
  
  if (!is.null(resp)) {
    res <- httr2::resp_body_json(resp)
    if (!(is.null(res))) {
      purrr::map(res$funding, normalize_funder_dataframe, vol_id) |>
        purrr::list_rbind()
    }
  } else {
    resp
  }
  
  # if (is.data.frame(g$funding)) {
  #   f_df <- g$funding
  #   f_df <- normalize_funder_dataframe(f_df)
  #   f_df$vol_id <- vol_id
  #   f_df[c("vol_id", "funder_id", "funder_name", "award")]
  # } else {
  #   if (vb) {
  #     message("'funding' is NULL.")
  #   }
  #   NULL
  # }
}


#-------------------------------------------------------------------------------
extract_funder_info <- function(vol_funder_list_item) {
  funder_id <- vol_funder_list_item$funder$id
  funder_name <- vol_funder_list_item$funder$name
}

#-------------------------------------------------------------------------------
normalize_funder_dataframe <- function(x, vol_id) {
  # Coerce weird output when there are more than one award for a source
  x <- as.data.frame(x)
  if (is.data.frame(x)) {
    f <- data.frame(funder_id = x$funder.id, funder_name = x$funder.name,
                    funder_award = x[,3], vol_id = vol_id)
    # f <- x$funder
    # f$award <- replace_empty_award_vals(x)
    # f <- tidyr::unnest(f, "award")
    # dplyr::rename(f, funder_id = "id", funder_name = "name", funder_award = x[,3])
  }
  f
}

#-------------------------------------------------------------------------------
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
