#' Plot summary of volume's participant characteristics.
#'
#' @param vol_id Selected volume number.
#' @param return_df A boolean value.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return Status code if successful.
#' @examples
#' summarize_demog()
#' @export
summarize_demog <- function(vol_id = 4, return_df = FALSE,
                           vb = FALSE) {

  # Error handling ----------------------------------------------------------
  if (length(vol_id) > 1) {
    stop("vol_id must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("vol_id must be an integer > 0.")
  }

  # Retrieve session data ----------------------------------------------------
  if (vb) message(paste0("Downloading session spreadsheet from volume ", vol_id))
  df <- download_session_csv(vol_id = vol_id, vb=vb)
  if (is.null(df)) {
    stop("Download failed.")
  }

  if (vb) message(paste0("Preparing session spreadsheet for plotting."))
  if("session.date" %in% names(df)) {
    df <- df[complete.cases(df$session.date),]
    df$session.date <- as.Date(df$session.date)
  }

  if("participant.birthdate" %in% names(df)) {
    df$participant.birthdate <- as.Date(df$participant.birthdate)
    df$age.weeks <- as.numeric( (df$session.date - df$participant.birthdate )/7 )
  }

  unreported <- (df$participant.race == "")
  df$participant.race[unreported] = "Unknown or not reported"

  # Drop NAs
  df <- df[complete.cases(df[,c('age.weeks', 'participant.gender',
                          'participant.race')]),]

  # Prepare summary plot -----------------------------------------------------
  demog.theme <- ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(size = ggplot2::rel(1)),
                       axis.title = ggplot2::element_text(size = ggplot2::rel(1.5))
                       )
  if (vb) message(paste0("Plotting session demographic data."))
  p <- ggplot2::ggplot(data = df) +
    ggplot2::aes(x = participant.gender, y = age.weeks, color = participant.race) +
    ggplot2::geom_boxplot() +
    demog.theme
  p

  if (return_df) {
    return(df)
  } else {
    return(p)
  }
}
