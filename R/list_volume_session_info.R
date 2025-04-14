# list_volume_session_info <- function(vol_sessions = NULL,
#                                      vb = options::opt("vb")) {
#   assertthat::assert_that(is.list(vol_sessions))
#   assertthat::has_attr(vol_sessions, "count")
#   
#   assertthat::assert_that(length(vb) == 1)
#   assertthat::assert_that(is.logical(vb))
#   
#   if (vb)
#     message("There are n=", vol_sessions$count, " sessions.")
#   
#   session_results <- vol_sessions$results
#   purrr::map(session_results, extract_session_info,
#              .progress = TRUE) |>
#     purrr::list_rbind()
# }
# 
# # Helper function
# extract_session_info <- function(x) {
#   df = data.frame(vol_id = x$volume,
#                   session_id = x$id,
#                   session_name = x$name,
#                   session_date = x$sourceDate,
#                   release_lvl = x$releaseLevel,
#                   n_files = x$fileCount)
#   df
# }