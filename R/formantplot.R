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
#' @param dynamicrange Dynamic range in dB for producing formant plots.
#' When a formant plot of `formanttype='speckle'` is drawn, no formants are
#' shown in frames with intensity level `formant_dynrange` below the maximum
#' intensity. Default is `30`. If set to `0`, all formants are shown.
#' @param formantrange Vector of two integers giving the frequency range to be
#' used for producing formant plots. Default is `c(0,5500)`.
#' @param type String giving the type of formant plot to produce;
#' default is `speckle` (a point plot), the only other option is `draw` (a line
#' plot).
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param nframe Integer giving the number of plot components. Default is `NULL`.
#'
#' @export
#'
#' @examples
#' #dont use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' praatpicture(paste0(datapath, '/1.wav'), frames='formant')
formantplot <- function(fm, start, end, tfrom0=TRUE, tgbool=FALSE, lines=NULL,
                        dynamicrange=30, formantrange=c(0,5500),
                        type='speckle', ind=NULL, nframe=NULL) {

  if (!type %in% c('draw', 'speckle')) {
    stop('Please select either draw or speckle as the formant plot type')
  }

  if (ind==nframe) {
    xax <- 's'
  } else {
    xax <- 'n'
  }

  if (ind != 1) {
    ytix <- grDevices::axisTicks(formantrange, log=F)
    ytix <- ytix[-length(ytix)]
    yax <- 'n'
  } else {
    yax <- 's'
  }

  nf <- fm$maxnFormants
  fm <- rPraat::formant.toArray(fm)

  if (tfrom0) {
    fm$t <- fm$t - start
    start <- 0
  }

  db <- gsignal::pow2db(fm$intensityVector)
  if (dynamicrange != 0) {
    subdr <- which(db < max(db)-dynamicrange)
    if (length(subdr) == 0) subdr <- 1
  } else {
    subdr <- 1
  }

  s <- formantrange[1]:formantrange[2]
  freql <- s[s %% 1000 == 0]

  if (type == 'draw') {
    plot(fm$t, fm$frequencyArray[1,], xlim=c(start, end+start),
         xaxt=xax, ylim=formantrange, yaxt=yax, type='l')
    for (i in 2:nf) {
      graphics::lines(fm$t, fm$frequencyArray[i,])
    }
    if (ind != 1) graphics::axis(2, at=ytix)
    graphics::abline(h=freql, lty='dotted')
    if (tgbool) graphics::abline(v=lines, lty='dotted')
    graphics::mtext('Frequency (Hz)', side=2, line=3, cex=0.8)
  }

  if (type == 'speckle') {
    plot(fm$t[-subdr], fm$frequencyArray[1,-subdr], pch=20,
         xlim=c(start, end+start), xaxt=xax,
         ylim=formantrange, yaxt=yax)
    for (i in 2:nf) {
      graphics::points(fm$t[-subdr], fm$frequencyArray[i,-subdr], pch=20)
    }
    if (ind != 1) graphics::axis(2, at=ytix)
    graphics::abline(h=freql, lty='dotted')
    if (tgbool) graphics::abline(v=lines, lty='dotted')
    graphics::mtext('Frequency (Hz)', side=2, line=3, cex=0.8)
  }

}
