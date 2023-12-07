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
#' @param range Vector of two integers giving the intensity range to be
#' used for producing intensity plots. Default is `NULL`, in which case the
#' range is simply the minimum and maximum levels in the curve.
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param nframe Integer giving the number of plot components. Default is `NULL`.
#' @param start_end_only Logical; should there only be ticks on the x-axis
#' for start and end times? Default is `TRUE`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
#' @param axisLabel String giving the name of the label to print along
#' the y-axis when plotting intensity. Default is `Intensity (dB)`.
#'
#' @export
#'
#' @examples
#' #dont use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' praatpicture(paste0(datapath, '/1.wav'), frames='intensity')
intensityplot <- function(it, start, end, tfrom0=TRUE, tgbool=FALSE, lines=NULL,
                          range=NULL, ind=NULL, nframe=NULL,
                          start_end_only=TRUE, min_max_only=TRUE,
                          axisLabel='Intensity (dB)') {

  if (is.null(range)) {
    range <- c(min(it$i), max(it$i))
  }

  if (!min_max_only[ind]) {
    if (ind == 1) {
      yax <- 's'
    } else {
      ytix <- grDevices::axisTicks(range, log=F)
      ytix <- ytix[-length(ytix)]
      yax <- 'n'
    }
  } else {
    yax <- 'n'
    ytix <- round(range, 0)
  }

  if (tfrom0) {
    it$t <- it$t - start
    start <- 0
  }

  if (ind==nframe) {
    if (!start_end_only) {
      xax <- 's'
    } else {
      xax <- 'n'
      xtix <- c(round(start, 3), round(end, 3), 0)
    }
  } else {
    xax <- 'n'
  }

  plot(it$t, it$i, xlim=c(start, end+start), xaxt=xax,
       ylim=range, yaxt=yax, type='l')
  if (ind == nframe & start_end_only) graphics::axis(1, at=xtix)
  if (!min_max_only[ind] & ind != 1) graphics::axis(2, at=ytix)
  if (min_max_only[ind]) graphics::axis(2, at=ytix, padj=c(0,1), las=2,
                                        tick=F)
  if (tgbool) graphics::abline(v=lines, lty='dotted')
  graphics::mtext(axisLabel, side=2, line=3.5, cex=0.8)

}
