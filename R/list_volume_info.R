#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Basic Volume Info.
#'
#' @param vol_id Target volume number.
#' @param rq An `httr2` request object. If NULL (the default)
#' a request will be generated, but this will only permit public information
#' to be returned.
#'
#' @returns A data frame with basic information about a volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_info() # Sessions in Volume 1
#' }
#' }
#' 
#' @export
list_volume_info <-
  function(vol_id = 1,
           vb = options::opt("vb"),
           rq = NULL) {
    # Check parameters
    assertthat::assert_that(length(vol_id) == 1)
    assertthat::assert_that(is.numeric(vol_id))
    assertthat::assert_that(vol_id >= 1)
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    
    assertthat::assert_that(is.null(rq) |
                              ("httr2_request" %in% class(rq)))
    
    # Handle NULL rq
    if (is.null(rq)) {
      if (vb) {
        message("\nNULL request object. Will generate default.")
        message("Not logged in. Only public information will be returned.")
      }
      rq <- databraryr::make_default_request()
    }
    
    # Make character array of "release" constants to decode release index
    constants <- databraryr::assign_constants()
    release_levels <- constants$release |>
      as.character()
    
    vol_list <- databraryr::get_volume_by_id(vol_id = vol_id, vb = vb, rq = rq)
    if (is.null(vol_list)) {
      return(NULL)
    } else {
      # Extract owner info
      if (vb) message("Extracting owner info...")
      #vol_owners <- databraryr::list_volume_owners(vol_id = vol_id, vb = vb, rq = rq)
      vol_owners <- purrr::map(vol_list$owners, tibble::as_tibble) %>%
        purrr::list_rbind() %>%
        dplyr::rename(party_id = id, owner_name = name) %>%
        dplyr::filter(!(stringr::str_detect(owner_name, "Databrary")))
      
      vol_owners_str <- stringr::str_flatten(vol_owners$owner_name,
                                             collapse = "; ")
      
      # Extract session info
      if (vb) message("Extracting session info...")
      #vol_sessions <- list_volume_sessions(vol_id = vol_id, vb = vb, rq = rq)
      vol_sessions <- purrr::map(vol_list$containers, get_info_from_session, 
                       release_levels = release_levels) %>%
        purrr::list_rbind()
      if (is.null(vol_sessions)) {
        n_vol_sessions <- 0      
      } else {
        n_vol_sessions <- dim(vol_sessions)[1]      
      }
      
      # Extract funder info
      if (vb) message("Extracting funder info...")
      #vol_funders <- list_volume_funding(vol_id = vol_id, vb = vb, rq = rq)
      vol_funders <- purrr::map(vol_list$funding, extract_funder_info) %>%
        purrr::list_rbind()
      if (is.null(vol_funders)) {
        n_vol_funders <- 0
      } else {
        n_vol_funders <- dim(vol_funders)[1]
      }
      
      # Extract asset info
      vol_assets <- list_volume_assets(vol_id = vol_id, vb = vb, rq = rq)
      
      if (is.null(vol_assets)) {
        n_vol_assets <- 0
        tot_vol_size_mb <- 0
        tot_vol_dur_hrs <- 0
      } else {
        n_vol_assets <- dim(vol_assets)[1]
        tot_vol_size_mb <- round(sum(na.omit(vol_assets$asset_size))/(1024*1024), 3)
        tot_vol_dur_hrs <- round(sum(na.omit(vol_assets$asset_duration))/(1000*60*60), 3)
      }
      
      # Create output data frame/tibble
      tibble::tibble(
        vol_id = vol_list$id,
        vol_name = vol_list$name,
        vol_doi = vol_list$doi,
        vol_desc = vol_list$body,
        vol_creation = vol_list$creation,
        vol_publicaccess = vol_list$publicaccess,
        vol_owners = vol_owners_str,
        vol_n_sessions = n_vol_sessions,
        vol_n_assets = n_vol_assets,
        vol_tot_size_mb = tot_vol_size_mb,
        vol_tot_dur_hrs = tot_vol_dur_hrs,
        vol_n_funders = n_vol_funders
      )      
    }
  }
