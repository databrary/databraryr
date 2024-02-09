#' List Affiliates for A Databrary Party.
#' 
#' #' @description
#' `r lifecycle::badge("superseded")`
#' 
#' This function has been superseded by `list_party_affiliates()`.
#'
#' @param party_id Target volume number.
#' @param report_target_party Report data about the target party. Default is FALSE.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' 
#' @returns A data frame with information about a party's affiliates (sponsored
#' investigators).
#' 
#' @examples
#' \donttest{
#' list_affiliates() # Lists Rick Gilmore's affiliates
#' #' }
#' 
#' @export
list_affiliates <-
  function(party_id = 6,
           report_target_party = FALSE,
           vb = FALSE) {
    
    #------------------------------------------------------------
    # Check parameters
    
    assertthat::assert_that(is.numeric(party_id))
    assertthat::assert_that(sum(party_id >= 1) == length(party_id))
    
    
    assertthat::assert_that(length(report_target_party) == 1)
    assertthat::assert_that(is.logical(report_target_party))
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    
    #------------------------------------------------------------
    # Helper function for handling lists
    
    list_single_party_affiliates <-
      function(party_id = NULL,
               report_target_party = NULL,
               vb = NULL) {
        if (!databraryr::is_person(party_id)) {
          if (vb)
            message("Party is not a person: ", party_id)
          return(NULL)
        }
        
        g <-
          databraryr::GET_db_contents(
            URL_components = paste0("/api/party/", party_id,
                                    "?parents&children&access"),
            vb = vb
          )
        
        # Process retrieved
        if (!is.null(g)) {
          if (vb)
            message(paste0("Retrieving data for party ", party_id, "."))
          if (report_target_party) {
            message(
              "Affiliates for party ",
              party_id,
              ", ",
              paste0(g$prename, " ", g$sortname),
              ", ",
              g$affiliation,
              ":"
            )
          }
          p <- g$children$party
          if (!is.null(p)) {
            
            id <- NULL
            prename <- NULL
            sortname <- NULL
            pi_id <- NULL
            pi_first <- NULL
            pi_last <- NULL
            pi_affiliation <- NULL
            affiliate_id <- NULL
            last_name <- NULL
            first_name <- NULL
            affiliation <- NULL
            
            p %>%
              dplyr::mutate(
                pi_id = party_id,
                pi_first = g$prename,
                pi_last = g$sortname,
                pi_affiliation = g$affiliation
              ) %>%
              dplyr::rename(
                affiliate_id = id,
                last_name = sortname,
                first_name = prename
              ) %>%
              dplyr::select(
                pi_id,
                pi_first,
                pi_last,
                pi_affiliation,
                affiliate_id,
                last_name,
                first_name,
                affiliation
              )
          } else {
            NULL
          }
        } else {
          if (vb)
            message(paste0("No data for party ", party_id, "."))
          NULL
        }
      }
    
    #------------------------------------------------------------
    # Map across parties
    
    if (vb)
      message("Summarizing affiliates for n=", length(party_id), " parties.")
    
    purrr::map(
      party_id,
      list_single_party_affiliates,
      report_target_party = report_target_party,
      vb = vb,
      .progress = "Party affiliates: "
    ) %>%
      purrr::list_rbind()
    
  }
