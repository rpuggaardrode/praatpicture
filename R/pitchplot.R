#' Plot pitch object
#'
#' Function for plotting pitch objects called by [praatpicture]. Instead of
#' using this function directly, just use
#' `praatpicture('my_sound_file', frames='pitch')`.
#'
#' @param pt PitchTier object loaded using [rPraat::pt.read] or other object
#' formatted in a similar way, i.e. a `list` object containing the elements
#' `t` (a vector of time values) and `f` (a vector of frequency values) of
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
#' @param plotType String giving the type of pitch plot to produce; default
#' is `draw` (a line plot), the only other option is `speckle` (a point plot).
#' Alternatively a vector `c('draw','speckle')` can be passed, in which case
#' both are used.
#' @param scale String giving the frequency scale to use when producing
#' pitch plots. Default is `hz`; other options are `logarithmic` (also in Hz),
#' `semitones`, `erb`, and `mel`.
#' @param freqRange Vector of two integers giving the frequency range to be
#' used for producing pitch plots. Default is `NULL`, in which case the pitch
#' range is automatically reset to `c(-12,30)` for the `semitones` scale,
#' `c(0,10)` for the `erb` scale, and `c(50,500)` for the Hz-based scales,
#' following Praat defaults.
#' @param semitonesRe Frequency in Hz giving the reference level for converting
#' pitch frequency to semitones. Default is `100`.
#' @param color String giving the name of the color to be used for
#' plotting pitch. Default is `'black'`.
#' @param ind Integer indexing pitch relative to other plot components.
#' Default is `NULL`.
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
#' (a string specifying a background color). Can be nested lists to highlight
#' different parts of a figure in different ways.
#' @param axisLabel String giving the name of the label to print along the
#' y-axis when printing a pitch track. Default is `NULL`, in which case the
#' axis label will depend on the scale.
#' @param drawSize Number indicating the line width if
#' `plotType` is `'draw'`. Default is `1`. Controls the `lwd` argument of
#' [graphics::lines].
#' @param speckleSize Number indicating the point size of if `_plotType` is
#' `'speckle'`. Default is `1`. Controls the `cex` arguments of
#' [graphics::points].
#' @param suppressAxes Should all axes and the box around plots be suppressed?
#' Default is `FALSE`. Generally not recommended.
#'
#' @return No return values, called internally by [praatpicture] and sibling
#' functions.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' praatpicture(soundFile, frames='pitch')
pitchplot <- function(pt, start, end, tfrom0=TRUE, tgbool=FALSE, lines=NULL,
                      focusTierColor='black', focusTierLineType='dotted',
                      plotType='draw', scale='hz', freqRange=NULL,
                      semitonesRe=100, color='black', ind=NULL,
                      min_max_only=TRUE, highlight=NULL,
                      axisLabel=NULL, drawSize=1, speckleSize=1,
                      suppressAxes=FALSE) {

  if (scale == 'logarithmic') {
    logsc <- 'y'
  } else {
    logsc <- ''
  }

  if (!min_max_only[ind]) {
    if (ind == 1 | scale == 'logarithmic') {
      yax <- 's'
    } else {
      ytix <- grDevices::axisTicks(freqRange, log=F)
      ytix <- ytix[-length(ytix)]
      yax <- 'n'
    }
  } else {
    yax <- 'n'
    ytix <- freqRange
  }

  if (tfrom0) {
    pt$t <- pt$t - start
    start <- 0
  }

  create_highlighting <- function(highlight, pt) {
    tSpeckle <- c()
    fSpeckle <- c()
    tDraw <- c()
    fDraw <- c()
    for (int in 1:length(highlight$start)) {
      times <- which(pt$t > highlight$start[int] & pt$t < highlight$end[int])
      tSpeckle <- c(tSpeckle, pt$t[times], max(pt$t[times] + 0.0001))
      fSpeckle <- c(fSpeckle, pt$f[times], NA)
      if (highlight$start[int] < start) {
        extrema <- zoo::na.approx(c(NA, pt$f),
                                  c(highlight$end[int], pt$t))
        tDraw <- c(tDraw, pt$t[times], highlight$end[int],
                   max(pt$t[times] + 0.0001))
        fDraw <- c(fDraw, pt$f[times], extrema[1], NA)
      } else {
        extrema <- zoo::na.approx(c(NA, NA, pt$f),
                                  c(highlight$start[int], highlight$end[int],
                                    pt$t))[1:2]
        tDraw <- c(tDraw, highlight$start[int], pt$t[times], highlight$end[int],
                   max(pt$t[times] + 0.0001))
        fDraw <- c(fDraw, extrema[1], pt$f[times], extrema[2], NA)
      }
      if (any(highlight$start < start)) {
        highlight$start[which(highlight$start < start)] <- start
      }
      if (any(highlight$end > end)) {
        highlight$end[which(highlight$end > end)] <- end
      }
    }

    if (!'color' %in% names(highlight)) highlight$color <- color
    if (!'drawSize' %in% names(highlight)) highlight$drawSize <- drawSize
    if (!'speckleSize' %in% names(highlight)) highlight$speckleSize <-
      speckleSize

    return(list(params = highlight, tSpeckle = tSpeckle, fSpeckle = fSpeckle,
                tDraw = tDraw, fDraw = fDraw))
  }

  if ('draw' %in% plotType) {
    plot(pt$t, pt$f, xlim=c(start, end+start), xaxt='n', ylim=freqRange,
         yaxt=yax, type='l', log=logsc, col=color, lwd=drawSize)
    if (!suppressAxes) {
      if (ind != 1 & scale != 'logarithmic' & !min_max_only[ind]) {
        graphics::axis(2, at=ytix)
      }
      if (min_max_only[ind]) graphics::axis(2, at=ytix, las=2, padj=c(0,1),
                                            tick=F)
    }

    if ('speckle' %in% plotType) {
      graphics::points(pt$t, pt$f, pch=20, col=color, cex=speckleSize)
    }

    if (!is.null(highlight)) {
      if (any(sapply(highlight, is.list))) {
        highlightLoopDepth <- sum(sapply(highlight, is.list))
      } else {
        highlightLoopDepth <- 1
      }
      for (i in 1:highlightLoopDepth) {
        if (any(sapply(highlight, is.list))) {
          thisHL <- create_highlighting(highlight[[i]], pt)
        } else {
          thisHL <- create_highlighting(highlight, pt)
        }
        if ('background' %in% names(thisHL$params)) {
          graphics::rect(thisHL$params$start,
                         freqRange[1] - freqRange[2] * 2,
                         thisHL$params$end,
                         freqRange[2] + freqRange[2] * 2,
                         col = thisHL$params$background, border = NA)
        }
        graphics::lines(thisHL$tDraw, thisHL$fDraw,
                        col=thisHL$params$color,
                        lwd=thisHL$params$drawSize)
        if ('speckle' %in% plotType) {
          graphics::points(thisHL$tSpeckle, thisHL$fSpeckle, pch=20,
                           col=thisHL$params$color,
                           cex=thisHL$params$speckleSize)
        }
      }
    }

    if (tgbool) {
      for (i in 1:length(lines)) {
        graphics::abline(v=lines[[i]], col=focusTierColor[i],
                         lty=focusTierLineType[i])
      }
    }
  }

  if (length(plotType) == 1) {
    if (plotType == 'speckle') {
      plot(pt$t, pt$f, xlim=c(start, end+start), xaxt='n', ylim=freqRange,
           yaxt=yax, type='p', pch=20, log=logsc, col=color, cex=speckleSize)
      if (!suppressAxes) {
        if (ind != 1 & scale != 'logarithmic' & !min_max_only[ind]) {
          graphics::axis(2, at=ytix)
        }
        if (min_max_only[ind]) graphics::axis(2, at=ytix, las=2, padj=c(0,1),
                                              tick=F)
      }

      if (!is.null(highlight)) {
        if (any(sapply(highlight, is.list))) {
          highlightLoopDepth <- sum(sapply(highlight, is.list))
        } else {
          highlightLoopDepth <- 1
        }
        for (i in 1:highlightLoopDepth) {
          if (any(sapply(highlight, is.list))) {
            thisHL <- create_highlighting(highlight[[i]], pt)
          } else {
            thisHL <- create_highlighting(highlight, pt)
          }
          if ('background' %in% names(thisHL$params)) {
            graphics::rect(thisHL$params$start,
                           freqRange[1] - freqRange[2] * 2,
                           thisHL$params$end,
                           freqRange[2] + freqRange[2] * 2,
                           col = thisHL$params$background, border = NA)
          }
          graphics::points(thisHL$tSpeckle, thisHL$fSpeckle,
                           col=thisHL$params$color, pch=20,
                           cex=thisHL$params$speckleSize)
          if ('draw' %in% plotType) {
            graphics::lines(thisHL$tDraw, thisHL$fDraw, col=thisHL$params$color,
                            lwd=thisHL$params$drawSize)
          }
        }
      }

      if (tgbool) {
        for (i in 1:length(lines)) {
          graphics::abline(v=lines[[i]], col=focusTierColor[i],
                           lty=focusTierLineType[i])
        }
      }
    }
  }

  if (!suppressAxes) graphics::mtext(axisLabel, side=2, line=3.5, cex=0.8)

}
