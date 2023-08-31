#' Plot waveform
#'
#' Function for plotting waveforms called by [praatpicture]. Instead of using
#' this function directly, just use
#' `praatpicture('my_sound_file', frames='sound')`.
#'
#' @param sig Numeric vector corresponding to a sound signal.
#' @param t Numeric vector giving times corresponding to the signal.
#' @param tgbool Logical; should dotted lines be plotted corresponding to
#' locations in a TextGrid? Default is `FALSE`.
#' @param lines Numeric vector giving locations in seconds of locations from
#' a TextGrid to be plotted with dotted lines. Default is `NULL`.
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param nframe Integer giving the number of plot components. Default is `NULL`.
#' @param start_end_only Logical; should there only be ticks on the x-axis
#' for start and end times? Default is `TRUE`.
#'
#' @export
#'
#' @examples
#' #dont use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' praatpicture(paste0(datapath, '/1.wav'), frames='sound')
waveplot <- function(sig, t, tgbool=FALSE, lines=NULL,
                     ind=NULL, nframe=NULL, start_end_only=TRUE) {
  if (ind==nframe) {
    if (!start_end_only) {
      xax <- 's'
    } else {
      xax <- 'n'
      xtix <- c(round(min(t), 3), round(max(t), 3), 0)
    }
  } else {
    xax <- 'n'
  }

  if (ind != 1) {
    ytix <- grDevices::axisTicks(c(min(sig), max(sig)), log=F)
    ytix <- ytix[-length(ytix)]
    yax <- 'n'
  } else {
    yax <- 's'
  }

  plot(t, sig, type='l', xlab='', xaxt=xax, ylab='', yaxt=yax)
  if (ind == nframe & start_end_only) graphics::axis(1, at=xtix)
  if (ind != 1) graphics::axis(2, at=ytix)
  if (tgbool) graphics::abline(v=lines, lty='dotted')
}
