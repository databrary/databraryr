#' Plot summary of volume's participant characteristics.
#'
#' @param volume Selected volume number.
#' @param return.df A boolean value.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return Status code if successful.
#' @examples
#' summarize_demog()
#' @export
summarize_demog <- function(volume = 4, return.df = FALSE,
                           vb = FALSE) {

  # Error handling ----------------------------------------------------------
  if (length(volume) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(volume)) || (volume <= 0)) {
    stop("Volume must be an integer > 0.")
  }

  # Retrieve session data ----------------------------------------------------
  if (vb) message(paste0("Downloading session spreadsheet from volume ", volume))
  df <- download_session_csv(volume = volume, vb=vb)
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

  if (return.df) {
    return(df)
  } else {
    return(p)
  }
}
