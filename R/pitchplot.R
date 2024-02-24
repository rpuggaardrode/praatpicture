#' Plot pitch object
#'
#' Function for plotting pitch objects called by [praatpicture]. Instead of
#' using this function directly, just use
#' `praatpicture('my_sound_file', frames='pitch')`.
#'
#' @param pt PitchTier object loaded using [rPraat::pt.read]
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
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
#' @param axisLabel String giving the name of the label to print along the
#' y-axis when printing a pitch track. Default is `NULL`, in which case the
#' axis label will depend on the scale.
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
                      min_max_only=TRUE,
                      axisLabel=NULL) {

  if (!plotType %in% c('draw', 'speckle')) {
    stop('Please select either draw or speckle as the pitch plot type')
  }

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

  if (plotType == 'draw') {
    plot(pt$t, pt$f, xlim=c(start, end+start), xaxt='n', ylim=freqRange,
         yaxt=yax, type='l', log=logsc, col=color)
    if (ind != 1 & scale != 'logarithmic' & !min_max_only[ind]) {
      graphics::axis(2, at=ytix)
    }
    if (min_max_only[ind]) graphics::axis(2, at=ytix, las=2, padj=c(0,1),
                                          tick=F)
    if (tgbool) {
      for (i in 1:length(lines)) {
        graphics::abline(v=lines[[i]], col=focusTierColor[i],
                         lty=focusTierLineType[i])
      }
    }
    graphics::mtext(axisLabel, side=2, line=3.5, cex=0.8)
  }

  if (plotType == 'speckle') {
    plot(pt$t, pt$f, xlim=c(start, end+start), xaxt='n', ylim=freqRange,
         yaxt=yax, type='p', pch=20, log=logsc, col=color)
    if (ind != 1 & scale != 'logarithmic' & !min_max_only[ind]) {
      graphics::axis(2, at=ytix)
    }
    if (min_max_only[ind]) graphics::axis(2, at=ytix, las=2, padj=c(0,1),
                                          tick=F)
    if (tgbool) {
      for (i in 1:length(lines)) {
        graphics::abline(v=lines[[i]], col=focusTierColor[i],
                         lty=focusTierLineType[i])
      }
    }
    graphics::mtext(axisLabel, side=2, line=3.5, cex=0.8)
  }
}
