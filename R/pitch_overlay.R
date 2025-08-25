#' Overlay pitch on plot frame
#'
#' Function for overlaying pitch contour on another plot frame, viz. the
#' waveform or spectrogram. Instead of using this function directly, just use
#' `praatpicture('my_sound_file')` with `pitch_plotOnSpec` or `pitch_plotOnWave`
#' set to `TRUE`.
#'
#' @param pt PitchTier object loaded using [rPraat::pt.read] or other object
#' formatted in a similar way, i.e. a `list` object containing the elements
#' `t` (a vector of time values) and `f` (a vector of frequency values) of
#' identical length.
#' @param bottomRange Bottom y-axis range of the plot frame that pitch
#' is plotted on.
#' @param topRange Top y-axis range of the plot frame that pitch is plotted on.
#' @param start Start time (in seconds) of desired plotted area.
#' @param org_start Start time (in seconds) of desired plotted area in the
#' original sound file.
#' @param tfrom0 Logical; should time on the x-axis run from 0 or from the
#' original time? Default is `TRUE`.
#' @param freqRange Vector of two integers giving the frequency range to be
#' used for producing pitch plots. Default is `NULL`, in which case the pitch
#' range is automatically reset to `c(-12,30)` for the `semitones` scale,
#' `c(0,10)` for the `erb` scale, and `c(50,500)` for the Hz-based scales,
#' following Praat defaults.
#' @param plotType String giving the type of pitch plot to produce; default
#' is `draw` (a line plot), the only other option is `speckle` (a point plot).
#' Alternatively a vector `c('draw','speckle')` can be passed, in which case
#' both are used.
#' @param scale String giving the frequency scale to use when producing
#' pitch plots. Default is `hz`; other options are `logarithmic` (also in Hz),
#' `semitones`, `erb`, and `mel`.
#' @param color String giving the name of the color to be used for
#' plotting pitch. Default is `'black'`.
#' @param ind Integer indexing current plot frame relative to other plot
#' components. Default is `NULL`.
#' @param drawSize Number indicating the line width if
#' `plotType` is `'draw'`. Default is `1`. Controls the `lwd` argument of
#' [graphics::lines].
#' @param speckleSize Number indicating the point size of if `_plotType` is
#' `'speckle'`. Default is `1`. Controls the `cex` arguments of
#' [graphics::points].
#' @param axisLabel String giving the name of the label to print along the
#' y-axis when printing a pitch track. Default is `NULL`, in which case the
#' axis label will depend on the scale.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
#' @param highlight Named list giving parameters for differential
#' highlighting of pitch based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the optional arguments
#' `color` (string or vector of strings, see `color`),
#' `drawSize` or `speckleSize` (both numeric), and `background`
#' (a string specifying a background color).
#' @param intensity_overlay Logical; is intensity also overlaid on the same
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
#' praatpicture(soundFile, frames = 'spectrogram', pitch_plotOnSpec = TRUE)
pitch_overlay <- function(pt, bottomRange, topRange, start, org_start=0,
                          tfrom0=TRUE, freqRange=NULL, plotType='draw',
                          scale='hz', color='black', ind=NULL, drawSize=1,
                          speckleSize=1, axisLabel=NULL, min_max_only=TRUE,
                          highlight=NULL, intensity_overlay=FALSE) {

  pfr <- freqRange
  if (tfrom0) pt$t <- pt$t - org_start
  if (scale == 'logarithmic') {
    pt$f <- log(pt$f)
    pfr <- log(freqRange)
  }
  sRan <- topRange - bottomRange
  pRan <- pfr[2] - pfr[1]
  multiplier <- sRan / pRan
  pt$f <- bottomRange + (pt$f - pfr[1]) * multiplier

  if (!is.null(highlight)) {
    highlight_tSpeckle <- c()
    highlight_fSpeckle <- c()
    highlight_tDraw <- c()
    highlight_fDraw <- c()
    for (int in 1:length(highlight$start)) {
      times <- which(pt$t > highlight$start[int] &
                       pt$t < highlight$end[int])
      highlight_tSpeckle <- c(highlight_tSpeckle, pt$t[times],
                              max(pt$t[times] + 0.0001))
      highlight_fSpeckle <- c(highlight_fSpeckle, pt$f[times], NA)
      if (highlight$start[int] < start) {
        extrema <- zoo::na.approx(c(NA, pt$f),
                                  c(highlight$end[int], pt$t))
        highlight_tDraw <- c(highlight_tDraw,
                             pt$t[times], highlight$end[int],
                             max(pt$t[times] + 0.0001))
        highlight_fDraw <- c(highlight_fDraw, pt$f[times], extrema[1], NA)
      } else {
        extrema <- zoo::na.approx(c(NA, NA, pt$f),
                                  c(highlight$start[int],
                                    highlight$end[int],
                                    pt$t))[1:2]
        highlight_tDraw <- c(highlight_tDraw, highlight$start[int],
                             pt$t[times], highlight$end[int],
                             max(pt$t[times] + 0.0001))
        highlight_fDraw <- c(highlight_fDraw, extrema[1], pt$f[times],
                             extrema[2], NA)
      }
    }

    if (!'color' %in% names(highlight)) highlight$color <-
      color
    if (!'drawSize' %in% names(highlight)) highlight$drawSize <-
      drawSize
    if (!'speckleSize' %in% names(highlight)) highlight$speckleSize <-
      speckleSize
  }

  if ('draw' %in% plotType) {
    if (length(color) == 2) graphics::lines(pt$t, pt$f,
                                            lwd=drawSize+2, col=color[2])
    graphics::lines(pt$t, pt$f, lwd=drawSize, col=color[1])
    if (!is.null(highlight)) {
      if (length(highlight$color) == 2) {
        graphics::lines(highlight_tDraw, highlight_fDraw,
                        lwd=highlight$drawSize+2,
                        col=highlight$color[2])
      }
      graphics::lines(highlight_tDraw, highlight_fDraw,
                      col=highlight$color[1],
                      lwd=highlight$drawSize)
    }
  }
  if ('speckle' %in% plotType) {
    if (length(color) == 2) graphics::points(pt$t, pt$f, lwd=3,
                                             pch=20, col=color[2],
                                             cex=speckleSize)
    graphics::points(pt$t, pt$f, pch=20, col=color[1],
                     cex=speckleSize)
    if (!is.null(highlight)) {
      if (length(highlight$color == 2)) {
        graphics::points(highlight_tSpeckle, highlight_fSpeckle,
                         lwd=3, pch=20,
                         col=highlight$color[2],
                         cex=highlight$speckleSize)
      }
      graphics::points(highlight_tSpeckle, highlight_fSpeckle, pch=20,
                       col=highlight$color[1],
                       cex=highlight$speckleSize)
    }
  }

  pline <- c(3.5,1)
  if (intensity_overlay) pline <- c(1,1)

  graphics::mtext(axisLabel, side=4, line=pline[1], cex=0.8, las=3,
                  col=color[1])

  if (min_max_only[ind]) {
    graphics::mtext(freqRange[1], side=4, line=pline[2],
                    at=bottomRange, padj=0, las=2, cex=0.7,
                    col=color[1])
    graphics::mtext(freqRange[2], side=4, line=pline[2],
                    at=topRange, padj=1, las=2, cex=0.7,
                    col=color[1])
  } else {
    rtix <- pretty(pfr[1]:pfr[2])
    graphics::axis(4, at=(rtix-pfr[1])*multiplier, labels=rtix,
                   col.ticks=color[1], col.axis=color[1])
  }
}
