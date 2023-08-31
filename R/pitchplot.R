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
#' @param type String giving the type of pitch plot to produce; default
#' is `draw` (a line plot), the only other option is `speckle` (a point plot).
#' @param scale String giving the frequency scale to use when producing
#' pitch plots. Default is `hz`; other options are `logarithmic` (also in Hz),
#' `semitones`, `erb`, and `mel`.
#' @param pitchrange Vector of two integers giving the frequency range to be
#' used for producing pitch plots. Default is `c(50,500)`. If the frequency
#' scales `semitones` or `erb` are used, the pitch range is automatically reset
#' to the Praat defaults for these scales (`c(-12,30)` and `c(0,10)`,
#' respectively).
#' @param semitones_re Frequency in Hz giving the reference level for converting
#' pitch frequency to semitones. Default is `100`.
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
#' praatpicture(paste0(datapath, '/1.wav'), frames='pitch')
pitchplot <- function(pt, start, end, tfrom0=TRUE, tgbool=FALSE, lines=NULL,
                      type='draw', scale='hz', pitchrange=c(50,500),
                      semitones_re=100, ind=NULL, nframe=NULL,
                      start_end_only=TRUE) {

  if (!type %in% c('draw', 'speckle')) {
    stop('Please select either draw or speckle as the pitch plot type')
  }

  legal_scales <- c('hz', 'logarithmic', 'semitones', 'erb', 'mel')
  if (!scale %in% legal_scales) {
    stop('Possible pitch scales are hz, logarithmic, semitones, erb, and mel')
  }

  axlab <- 'Frequency (Hz)'

  if (scale == 'logarithmic') {
    logsc <- 'y'
  } else {
    logsc <- ''
  }

  if (scale == 'semitones') {
    pt <- rPraat::pt.Hz2ST(pt, ref=semitones_re)
    pitchrange <- c(-12,30)
    axlab <- paste('Frequency (semitones re ', semitones_re, ')')
  }

  if (scale == 'mel') {
    pt$f <- emuR::mel(pt$f)
    axlab <- 'Frequency (mel)'
  }

  if (scale == 'erb') {
    pt$f <- soundgen::HzToERB(pt$f)
    pitchrange <- c(0,10)
    axlab <- 'Frequency (ERB)'
  }

  if (ind != 1 & scale != 'logarithmic') {
    ytix <- grDevices::axisTicks(pitchrange, log=FALSE)
    ytix <- ytix[-length(ytix)]
    yax <- 'n'
  } else {
    yax <- 's'
  }

  if (tfrom0) {
    pt$t <- pt$t - start
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

  if (type == 'draw') {
    diffs <- diff(pt$t) - min(diff(pt$t))
    gaps <- which(diffs > min(diff(pt$t)))
    gaps <- c(gaps, length(pt$t))
    sep_lines_t <- list()
    sep_lines_f <- list()
    i <- 1
    for (g in 1:length(gaps)) {
      sep_lines_t[[g]] <- pt$t[i:gaps[g]]
      sep_lines_f[[g]] <- pt$f[i:gaps[g]]
      i <- 1 + gaps[g]
    }

    plot(sep_lines_t[[1]], sep_lines_f[[1]], xlim=c(start, end+start), xaxt=xax,
         ylim=pitchrange, yaxt=yax, type='l', log=logsc)
    if (ind != 1 & scale != 'logarithmic') graphics::axis(2, at=ytix)
    if (ind == nframe & start_end_only) graphics::axis(1, at=xtix)
    if (length(sep_lines_t) > 1) {
      for (i in 2:length(sep_lines_t)) {
        graphics::lines(sep_lines_t[[i]], sep_lines_f[[i]])
      }
    }
    if (tgbool) graphics::abline(v=lines, lty='dotted')
    graphics::mtext(axlab, side=2, line=3, cex=0.8)
  }

  if (type == 'speckle') {
    plot(pt$t, pt$f, xlim=c(start, end+start), xaxt=xax, ylim=pitchrange,
         yaxt=yax, type='p', pch=20, log=logsc)
    if (ind != 1 & scale != 'logarithmic') graphics::axis(2, at=ytix)
    if (ind == nframe & start_end_only) graphics::axis(1, at=xtix)
    if (tgbool) graphics::abline(v=lines, lty='dotted')
    graphics::mtext(axlab, side=2, line=3, cex=0.8)
  }
}
