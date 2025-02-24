#' Plot formant object
#'
#' Function for plotting formant objects called by [praatpicture]. Instead of
#' using this function directly, just use
#' `praatpicture('my_sound_file', frames='formant')`.
#'
#' @param fm Formant object loaded using [rPraat::formant.read]
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
#' @param dynamicRange Dynamic range in dB for producing formant plots.
#' When a formant plot of `plotType='speckle'` is drawn, no formants are
#' shown in frames with intensity level `dynamicRange` below the maximum
#' intensity. Default is `30`. If set to `0`, all formants are shown.
#' @param freqRange Vector of two integers giving the frequency range to be
#' used for producing formant plots. Default is `c(0,5500)`.
#' @param plotType String giving the type of formant plot to produce;
#' default is `speckle` (a point plot), the only other option is `draw` (a line
#' plot). Alternatively a vector `c('draw','speckle')` can be passed, in which
#' case both are used.
#' @param color String or vector of strings giving the name(s) of
#' colors to be used for plotting formants. If one color is provided, all
#' formants will be plotted in this color. If multiple colors are provided,
#' different formants will be shown in different colors. Default is `'black'`.
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `FALSE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
#' @param highlight Named list giving parameters for differential
#' highlighting of formants based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the optional arguments
#' `color` (string or vector of strings, see `color`),
#' `drawSize` or `speckleSize` (both numeric), and `background`
#' (a string specifying a background color).
#' @param axisLabel String giving the name of the label to print along the
#' y-axis when plotting formants. Default is `Frequency (Hz)`.
#' @param drawSize Number indicating the line width if
#' `plotType` is `'draw'`. Default is `1`. Controls the `lwd` argument of
#' [graphics::lines].
#' @param speckleSize Number indicating the point size of if `_plotType` is
#' `'speckle'`. Default is `1`. Controls the `cex` arguments of
#' [graphics::points].
#'
#' @return No return values, called internally by [praatpicture] and sibling
#' functions.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' praatpicture(soundFile, frames='formant')
formantplot <- function(fm, start, end, tfrom0=TRUE, tgbool=FALSE, lines=NULL,
                        focusTierColor='black', focusTierLineType='dotted',
                        dynamicRange=30, freqRange=c(0,5500),
                        plotType='speckle', color='black',
                        ind=NULL, min_max_only=FALSE, highlight=NULL,
                        axisLabel='Frequency (Hz)', drawSize=1, speckleSize=1) {

  if (!min_max_only[ind]) {
    if (ind == 1) {
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

  nf <- fm$maxnFormants
  if (length(color) == 1) color <- rep(color, nf)

  if (tfrom0) {
    fm$t <- fm$t - start
    start <- 0
  }

  if (fm$conv2db) {
    db <- gsignal::pow2db(fm$intensityVector)
  } else {
    db <- fm$intensityVector
  }
  if (dynamicRange != 0) {
    subdr <- which(db < max(db)-dynamicRange)
    if (length(subdr) == 0) subdr <- 1
  } else {
    subdr <- 1
  }

  if (!is.null(highlight)) {
    highlight_t <- 0
    highlight_f <- array(rep(NA, nf), dim = c(nf,1))
    highlight_i <- c()
    for (int in 1:length(highlight$start)) {
      times <- which(fm$t > highlight$start[int] & fm$t < highlight$end[int])
      highlight_t <- c(highlight_t, fm$t[times], max(fm$t[times] + 0.0001))
      highlight_f <- cbind(highlight_f, fm$frequencyArray[,times], rep(NA, nf))
      highlight_i <- c(highlight_i, db[times], 0)
    }
    if (any(highlight$start < start)) {
      highlight$start[which(highlight$start < start)] <- start
    }
    if (any(highlight$end > end)) {
      highlight$end[which(highlight$end > end)] <- end
    }

    if (dynamicRange != 0) {
      hsubdr <- which(highlight_i < max(db)-dynamicRange)
      if (length(hsubdr) == 0) hsubdr <- 1
    } else {
      hsubdr <- 1
    }

    if (!'color' %in% names(highlight)) highlight$color <- color
    if (!'drawSize' %in% names(highlight)) highlight$drawSize <- drawSize
    if (!'speckleSize' %in% names(highlight)) highlight$speckleSize <-
      speckleSize
    if (length(highlight$color) == 1) highlight$color <-
      rep(highlight$color, nf)
  }

  s <- freqRange[1]:freqRange[2]
  freql <- s[s %% 1000 == 0]

  if ('draw' %in% plotType) {
    plot(fm$t, fm$frequencyArray[1,], xlim=c(start, end+start),
         xaxt='n', ylim=freqRange, yaxt=yax, type='l', col=color[1],
         lwd=drawSize)
    for (i in 2:nf) {
      graphics::lines(fm$t, fm$frequencyArray[i,], col=color[i], lwd=drawSize)
    }
    if (!min_max_only[ind] & ind != 1) graphics::axis(2, at=ytix)
    if (min_max_only[ind]) graphics::axis(2, at=ytix, padj=c(0,1), las=2,
                                          tick=F)

    if ('speckle' %in% plotType) {
      graphics::points(fm$t[-subdr], fm$frequencyArray[1,-subdr], pch=20,
                       col=color[1], cex=speckleSize)
      for (i in 2:nf) {
        graphics::points(fm$t[-subdr], fm$frequencyArray[i,-subdr], pch=20,
                         col=color[i], cex=speckleSize)
      }
    }

    if (!is.null(highlight)) {
      if ('background' %in% names(highlight)) {
        graphics::rect(highlight$start,
                       freqRange[1] - freqRange[2] * 2,
                       highlight$end,
                       freqRange[2] + freqRange[2] * 2,
                       col = highlight$background, border = NA)
      }
      graphics::lines(highlight_t, highlight_f[1,], col=highlight$color[1],
                      lwd=highlight$drawSize)
      for (i in 2:nf) {
        graphics::lines(highlight_t, highlight_f[i,],
                        col=highlight$color[i], lwd=highlight$drawSize)
      }
      if ('speckle' %in% plotType) {
        graphics::points(highlight_t[-hsubdr], highlight_f[1,-hsubdr], pch=20,
                         col=highlight$color[1], cex=highlight$speckleSize)
        for (i in 2:nf) {
          graphics::points(highlight_t[-hsubdr], highlight_f[i,-hsubdr], pch=20,
                           col=highlight$color[i], cex=highlight$speckleSize)
        }
      }
    }

    if (tgbool) {
      for (i in 1:length(lines)) {
        graphics::abline(v=lines[[i]], col=focusTierColor[i],
                         lty=focusTierLineType[i])
      }
    }

    graphics::mtext(axisLabel, side=2, line=3.5, cex=0.8)
  }

  if (length(plotType) == 1) {
    if (plotType == 'speckle') {
      plot(fm$t[-subdr], fm$frequencyArray[1,-subdr], pch=20,
           xlim=c(start, end+start), xaxt='n',
           ylim=freqRange, yaxt=yax, col=color[1], cex=speckleSize)
      for (i in 2:nf) {
        graphics::points(fm$t[-subdr], fm$frequencyArray[i,-subdr], pch=20,
                         col=color[i], cex=speckleSize)
      }
      if (!min_max_only[ind] & ind != 1) graphics::axis(2, at=ytix)
      if (min_max_only[ind]) graphics::axis(2, at=ytix, padj=c(0,1), las=2,
                                            tick=F)

      if (!is.null(highlight)) {
        if ('background' %in% names(highlight)) {
          graphics::rect(highlight$start,
                         freqRange[1] - freqRange[2] * 2,
                         highlight$end,
                         freqRange[2] + freqRange[2] * 2,
                         col = highlight$background, border = NA)
        }
        graphics::points(highlight_t[-hsubdr], highlight_f[1,-hsubdr], pch=20,
                         col=highlight$color[1], cex=highlight$speckleSize)
        for (i in 2:nf) {
          graphics::points(highlight_t[-hsubdr], highlight_f[i,-hsubdr], pch=20,
                           col=highlight$color[i], cex=highlight$speckleSize)
        }
      }

      if (tgbool) {
        for (i in 1:length(lines)) {
          graphics::abline(v=lines[[i]], col=focusTierColor[i],
                           lty=focusTierLineType[i])
        }
      }

      graphics::mtext(axisLabel, side=2, line=3.5, cex=0.8)
    }
  }

}
