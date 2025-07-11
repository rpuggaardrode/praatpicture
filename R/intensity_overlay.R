#' Overlay intensity on plot frame
#'
#' Function for overlaying intensity contour on another plot frame, viz. the
#' waveform or spectrogram. Instead of using this function directly, just use
#' `praatpicture('my_sound_file')` with `intensity_plotOnSpec` or
#' `pitch_plotOnWave` set to `TRUE`.
#'
#' @param it IntensityTier object loaded using [rPraat::it.read] or other object
#' formatted in a similar way, i.e. a `list` object containing the elements
#' `t` (a vector of time values) and `i` (a vector of intensity values) of
#' identical length.
#' @param bottomRange Bottom y-axis range of the plot frame that intensity
#' is plotted on.
#' @param topRange Top y-axis range of the plot frame that intensity is
#' plotted on.
#' @param start Start time (in seconds) of desired plotted area.
#' @param org_start Start time (in seconds) of desired plotted area in the
#' original sound file.
#' @param tfrom0 Logical; should time on the x-axis run from 0 or from the
#' original time? Default is `TRUE`.
#' @param range Vector of two integers giving the intensity range to be
#' used for producing intensity plots. Default is `NULL`, in which case the
#' range is simply the minimum and maximum levels in the curve.
#' @param color String giving the name of the color to be used for
#' plotting intensity. Default is `'black'`.
#' @param ind Integer indexing intensity relative to other plot components.
#' Default is `NULL`.
#' @param drawSize Number indicating the line width of the intensity contour.
#' Default is `1`. Controls the `lwd` argument of [graphics::lines].
#' @param axisLabel String giving the name of the label to print along
#' the y-axis when plotting intensity. Default is `Intensity (dB)`.
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
#' @param pitch_overlay Logical; is pitch also overlaid on the same
#' plot frame? Default is `FALSE`.
#'
#' @return No return values, called internally by [praatpicture] and sibling
#' functions.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' praatpicture(soundFile, frames = 'spectrogram',
#' intensity_plotOnSpec = TRUE)
intensity_overlay <- function(it, bottomRange, topRange, start, org_start=0,
                              tfrom0=TRUE, range=NULL, color='black', ind=NULL,
                              drawSize=1, axisLabel='Intensity (dB)',
                              min_max_only=TRUE, highlight=NULL,
                              pitch_overlay=FALSE) {
  if (tfrom0) it$t <- it$t - org_start
  sRan <- topRange - bottomRange
  iRan <- range[2] - range[1]
  multiplier <- sRan / iRan
  it$i <- bottomRange + (it$i - range[1]) * multiplier

  if (!is.null(highlight)) {
    highlight_t <- c()
    highlight_i <- c()
    for (int in 1:length(highlight$start)) {
      times <- which(it$t > highlight$start[int] &
                       it$t < highlight$end[int])
      if (highlight$start[int] < start) {
        extrema <- zoo::na.approx(c(NA, it$i),
                                  c(highlight$end[int], it$t))
        highlight_t <- c(highlight_t,
                         it$t[times], highlight$end[int],
                         max(it$t[times] + 0.0001))
        highlight_i <- c(highlight_i, it$i[times], extrema[1], NA)
      } else {
        extrema <- zoo::na.approx(c(NA, NA, it$i),
                                  c(highlight$start[int],
                                    highlight$end[int],
                                    it$t))[1:2]
        highlight_t <- c(highlight_t, highlight$start[int],
                         it$t[times], highlight$end[int],
                         max(it$t[times] + 0.0001))
        highlight_i <- c(highlight_i, extrema[1], it$i[times],
                         extrema[2], NA)
      }
    }
    if (!'color' %in% names(highlight)) highlight$color <- color
    if (!'drawSize' %in% names(highlight)) highlight$drawSize <- drawSize
  }

  if (length(color) == 2) graphics::lines(it$t, it$i,
                                                    lwd=drawSize+2,
                                                    col=color[2])
  graphics::lines(it$t, it$i, col=color[1], lwd=drawSize)
  if (!is.null(highlight)) {
    if (length(highlight$color) == 2) {
      graphics::lines(highlight_t, highlight_i,
                      lwd=highlight$drawSize+2,
                      col=highlight$color[2])
    }
    graphics::lines(highlight_t, highlight_i,
                    col=highlight$color,
                    lwd=highlight$drawSize)
  }

  iline <- c(3.5,1)
  if (pitch_overlay) iline <- c(3.5,3.5)

  graphics::mtext(axisLabel, side=4, line=iline[1], cex=0.8, las=3,
                  col=color[1])

  if (min_max_only[ind]) {
    graphics::mtext(round(range[1], 0),
                    side=4, line=iline[2], at=bottomRange,
                    padj=0, las=2, cex=0.7, col=color[1])
    graphics::mtext(round(range[2]),
                    side=4, line=iline[2], at=topRange,
                    padj=1, las=2, cex=0.7, col=color[1])
  } else {
    rtix <- pretty(range[1]:range[2])
    graphics::axis(4, at=(rtix-range[1])*multiplier, labels=rtix,
                   col.ticks=color[1],
                   col.axis=color[1])
  }
}
