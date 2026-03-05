#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Session File Data From A Databrary Volume
#' 
#' @description
#' Databrary volumes have folders where study or collection-wide files 
#' can be stored and shared. `get_folder_file()` returns metadata about
#' specific files stored in a volume folder.
#' 
#' @param vol_id An integer indicating the volume identifier. Default is 1.
#' @param folder_id An integer indicating a valid folder identifier
#' linked to a volume. Default value is 9807, the materials folder for volume 1.
#' @param file_id An integer indicating the file identifier. Default is 1.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An httr2 request object.
#'
#' @returns A JSON blob with the file data. If the user has previously logged
#' in to Databrary via `login_db()`, then files that have restricted access
#' can be downloaded, subject to the sharing release levels on those files.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' get_folder_file() # Data about file_id 1 from folder_id 9807 in Volume 1.
#' get_folder_file(vol_id = 2, folder_id = 9819, file_id = 16) # A PDF from Volume 2.
#' }
#' }
#' @export
get_folder_file <-
  function(vol_id = 1,
           folder_id = 9807,
           file_id = 1,
           vb = options::opt("vb"),
           rq = NULL) {
    assertthat::assert_that(is.numeric(vol_id))
    assertthat::assert_that(vol_id > 0)
    assertthat::assert_that(length(vol_id) == 1)
    
    assertthat::assert_that(is.numeric(folder_id))
    assertthat::assert_that(folder_id > 0)
    assertthat::assert_that(length(folder_id) == 1)
    
    assertthat::assert_that(is.numeric(file_id))
    assertthat::assert_that(file_id > 0)
    assertthat::assert_that(length(file_id) == 1)
    
    validate_flag(vb, "vb")
    
    assertthat::assert_that(is.null(rq) ||
                              inherits(rq, "httr2_request"))
    
    file <- perform_api_get(
      path = sprintf(API_FOLDER_FILES_DETAIL, vol_id, folder_id, file_id),
      rq = rq,
      vb = vb
    )
    
    if (is.null(file)) {
      if (vb) {
        message(
          "Cannot access requested file ",
          file_id,
          " in folder ",
          folder_id,
          " of volume ",
          vol_id
        )
      }
      return(NULL)
    }
    
    file
  }
