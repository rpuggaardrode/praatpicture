#' Plot intensity object
#'
#' Function for plotting intensity objects called by [praatpicture]. Instead of
#' using this function directly, just use
#' `praatpicture('my_sound_file', frames='intensity')`.
#'
#' @param it IntensityTier object loaded using [rPraat::it.read]
#' @param start Start time (in seconds) of desired plotted area.
#' @param end End time (in seconds) of desired plotted area.
#' @param tfrom0 Logical; should time on the x-axis run from 0 or from the
#' original time? Default is `TRUE`.
#' @param tgbool Logical; should dotted lines be plotted corresponding to
#' locations in a TextGrid? Default is `FALSE`.
#' @param lines Numeric vector giving locations in seconds of locations from
#' a TextGrid to be plotted with dotted lines. Default is `NULL`.
#' @param intensityrange Vector of two integers giving the intensity range to be
#' used for producing intensity plots. Default is `NULL`, in which case the
#' range is simply the minimum and maximum levels in the curve.
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param nframe Integer giving the number of plot components. Default is `NULL`.
#'
#' @export
#'
#' @examples
#' #dont use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' praatpicture(paste0(datapath, '/1.wav'), frames='intensity')
intensityplot <- function(it, start, end, tfrom0=TRUE, tgbool=FALSE, lines=NULL,
                          intensityrange=NULL, ind=NULL, nframe=NULL) {

  if (ind==nframe) {
    xax <- 's'
  } else {
    xax <- 'n'
  }

  if (is.null(intensityrange)) {
    intensityrange <- c(min(it$i), max(it$i))
  }

  if (ind != 1) {
    ytix <- grDevices::axisTicks(intensityrange, log=FALSE)
    ytix <- ytix[-length(ytix)]
    yax <- 'n'
  } else {
    yax <- 's'
  }

  if (tfrom0) {
    it$t <- it$t - start
    start <- 0
  }

  plot(it$t, it$i, xlim=c(start, end+start), xaxt=xax,
       ylim=intensityrange, yaxt=yax, type='l')
  if (ind != 1) graphics::axis(2, at=ytix)
  if (tgbool) graphics::abline(v=lines, lty='dotted')
  graphics::mtext('Intensity (dB)', side=2, line=3, cex=0.8)

}
