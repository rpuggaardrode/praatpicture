#' Plot intensity object
#'
#' Function for plotting intensity objects called by [praatpicture]. Instead of
#' using this function directly, just use
#' `praatpicture('my_sound_file', frames='intensity')`.
#'
#' @param it IntensityTier object loaded using [rPraat::it.read] or other object
#' formatted in a similar way, i.e. a `list` object containing the elements
#' `t` (a vector of time values) and `i` (a vector of intensity values) of
#' identical length.
#' @param start Start time (in seconds) of desired plotted area.
#' @param end End time (in seconds) of desired plotted area.
#' @param tfrom0 Logical; should time on the x-axis run from 0 or from the
#' original time? Default is `TRUE`.
#' @param tgbool Logical; should dotted lines be plotted corresponding to
#' locations in a TextGrid? Default is `FALSE`.
#' @param lines Numeric vector giving locations in seconds of locations from
#' a TextGrid to be plotted with dotted lines. Default is `NULL`.
#' @param focusTierColor String or vector of strings giving the color(s) to
#' use for plotting focus tier lines. If multiple tiers are focused, a vector
#' of the same length can be passed, and the nth tier will be plotted in the
#' nth color. Default is `'black'`.
#' @param focusTierLineType String or vector of strings giving the line
#' type(s) for plotting focus tier lines. If multiple tiers are focused, a
#' vector of the same length can be passed, and the nth tier will be plotted in
#' the nth line type. Default is `'dotted'`.
#' @param range Vector of two integers giving the intensity range to be
#' used for producing intensity plots. Default is `NULL`, in which case the
#' range is simply the minimum and maximum levels in the curve.
#' @param color String giving the name of the color to be used for
#' plotting intensity. Default is `'black'`.
#' @param ind Integer indexing current plot frame relative to other plot
#' components. Default is `NULL`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
#' @param highlight Named list giving parameters for differential
#' highlighting of the intensity contour based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the optional arguments
#' `color` (string or vector of strings, see `color`) and
#' `drawSize` (integer), and `background`
#' (a string specifying a background color).
#' @param axisLabel String giving the name of the label to print along
#' the y-axis when plotting intensity. Default is `Intensity (dB)`.
#' @param drawSize Number indicating the line width of the intensity contour.
#' Default is `1`. Controls the `lwd` argument of [graphics::lines].
#'
#' @return No return values, called internally by [praatpicture] and sibling
#' functions.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' praatpicture(soundFile, frames='intensity')
intensityplot <- function(it, start, end, tfrom0=TRUE, tgbool=FALSE, lines=NULL,
                          focusTierColor='black', focusTierLineType='dotted',
                          range=NULL, color='black', ind=NULL, min_max_only=TRUE,
                          highlight=NULL,
                          axisLabel='Intensity (dB)', drawSize=1) {

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

  if (!is.null(highlight)) {
    highlight_t <- c()
    highlight_i <- c()
    for (int in 1:length(highlight$start)) {
      times <- which(it$t > highlight$start[int] & it$t < highlight$end[int])
      if (highlight$start[int] < start) {
        extrema <- zoo::na.approx(c(NA, it$i),
                                  c(highlight$end[int], it$t))
        highlight_t <- c(highlight_t,
                         it$t[times], highlight$end[int],
                         max(it$t[times] + 0.0001))
        highlight_i <- c(highlight_i, it$i[times], extrema[1], NA)
      } else {
        extrema <- zoo::na.approx(c(NA, NA, it$i),
                                  c(highlight$start[int], highlight$end[int],
                                    it$t))[1:2]
        highlight_t <- c(highlight_t, highlight$start[int],
                         it$t[times], highlight$end[int],
                         max(it$t[times] + 0.0001))
        highlight_i <- c(highlight_i, extrema[1], it$i[times], extrema[2], NA)
      }
    }
    if (any(highlight$start < start)) {
      highlight$start[which(highlight$start < start)] <- start
    }
    if (any(highlight$end > end)) {
      highlight$end[which(highlight$end > end)] <- end
    }
    if (!'color' %in% names(highlight)) highlight$color <- color
    if (!'drawSize' %in% names(highlight)) highlight$drawSize <- drawSize
  }

  plot(it$t, it$i, xlim=c(start, end+start), xaxt='n',
       ylim=range, yaxt=yax, type='l', col=color, lwd=drawSize)
  if (!min_max_only[ind] & ind != 1) graphics::axis(2, at=ytix)
  if (min_max_only[ind]) graphics::axis(2, at=ytix, padj=c(0,1), las=2,
                                        tick=F)

  if (!is.null(highlight)) {
    if ('background' %in% names(highlight)) {
      graphics::rect(highlight$start,
                     range[1] - range[2] * 2,
                     highlight$end,
                     range[2] + range[2] * 2,
                     col = highlight$background, border = NA)
    }
    graphics::lines(highlight_t, highlight_i,
                    col=highlight$color,
                    lwd=highlight$drawSize)
  }

  if (tgbool) {
    for (i in 1:length(lines)) {
      graphics::abline(v=lines[[i]], col=focusTierColor[i],
                       lty=focusTierLineType[i])
    }
  }

  graphics::mtext(axisLabel, side=2, line=3.5, cex=0.8)

}
