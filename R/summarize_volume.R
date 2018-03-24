#' Plot summary of volume's participant characteristics.
#'
#' @param volume Selected volume number.
#' @param plot.style Defaults to 'ggplot'
#' @param return.df A Boolean value. If TRUE returns a data frame.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return Status code if successful.
#' @examples
#' summarize_volume()
summarize_volume <- function(volume = 4, plot.style = "ggplot",
                             return.df = FALSE, vb = FALSE) {
  # Downloads volume CSV and plots summary data for participant race, ethnicity, and age
  #
  # Args:
  #  volume: Databrary volume (integer). Default is 4.
  #  plot.style: Type of plotting commands to use. Default is 'ggplot'.
  #  return.df: Flag indicating whether or not to return the spreadsheet as a data frame. Default is
  #    FALSE.
  #  vb: Flag specifying whether to provide vb status messages. Default is FALSE.

  # Error handling
  if (length(volume) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(volume)) || (volume <= 0)) {
    stop("Volume must be an integer > 0.")
  }

  df <- download_csv(volume=volume, vb=vb)
  if (is.null(df)) {
    stop("Download failed.")
  }

  # Drop empty sessions, session.date is empty
  df <- df[!(is.na(df$session.date)),]

  # Convert dates to ages in days
  if("session.date" %in% names(df)) {
    df$session.date <- as.Date(df$session.date)
  }
  if("participant.birthdate" %in% names(df)) {
    df$participant.birthdate <- as.Date(df$participant.birthdate)
    df$age.weeks <- as.numeric( (df$session.date - df$participant.birthdate )/7 )
  }

  if (("participant.gender" %in% names(df)) & ("participant.race" %in% names(df))) {
    if (plot.style=="ggplot"){
      p <- ggplot2::qplot(data=df, y = age.weeks,
                          x=participant.gender, geom=c("boxplot"),
                          color=participant.race) +
        ggplot2::ggtitle(paste("Participant Characteristics for Databrary Volume ", volume, sep=""))
      if (return.df) {
        return(list("data.frame"=df,"plot"=p))
      } else {
        return(p)
      }
    } else {
      par(mfrow=c(1,2))
      plot(df$participant.gender ~ df$participant.race, xlab="", ylab="Race")
      hist(na.omit(df$age.days), xlab="Age (days)", main="")
    }
  } else if (vb) cat("Nothing to plot.\n")
  # TODO(someone): Add support for other types of plotting, e.g. base graphics
}
