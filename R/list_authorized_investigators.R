#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List authorized investigators for an institution
#'
#' @description Lists the authorized investigators at an institution.
#'
#' @param institution_id Institution identifier. Must be a positive integer. Default is 12.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @inheritParams list_institution_affiliates
#'
#' @return Tibble of investigators; NULL if none.
#' @export
list_authorized_investigators <- function(institution_id = 12,
                                          vb = options::opt("vb"),
                                          rq = NULL) {
  assertthat::assert_that(is.numeric(institution_id),
                          length(institution_id) == 1,
                          institution_id > 0)
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  affiliates <- list_institution_affiliates(institution_id, vb = vb, rq = rq)
  if (is.null(affiliates)) {
    return(NULL)
  }
  
  investigators <- affiliates |> dplyr::filter(.data$role == "investigator")
  if (nrow(investigators) == 0) {
    return(NULL)
  }
  investigators
}
