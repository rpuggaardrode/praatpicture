#' Plot spectrogram
#'
#' Function for plotting spectrograms called by [praatpicture]. Instead of using
#' this function directly, just use
#' `praatpicture('my_sound_file', frames='spectrogram')`.
#'
#' @param sig Numeric vector corresponding to a sound signal.
#' @param sr Integer giving the sampling rate of the signal.
#' @param t Numeric vector giving times corresponding to the signal.
#' @param start Start time (in seconds) of desired plotted area.
#' @param end End time (in seconds) of desired plotted area.
#' @param tfrom0 Logical; should time on the x-axis run from 0 or from the
#' original time? Default is `TRUE`.
#' @param freqRange Vector of two integers giving the frequency range to be
#' used for plotting spectrograms. Default is `c(0,5000)`.
#' @param windowLength Window length in seconds for generating spectrograms.
#' Default is `0.005`.
#' @param dynamicRange Dynamic range in dB for generating spectrograms. The
#' maximum intensity minus `dynamicRange` will all be printed in white. Default
#' is `50`.
#' @param timeStep How many time steps should be calculated for spectrograms?
#' Default is `1000`. Note that this takes a while to plot, so for fiddling with
#' plotting parameters it is a good idea to choose a smaller value.
#' @param windowShape String giving the name of the window shape to be applied
#' to the signal when generating spectrograms. Default is `Gaussian`; other
#' options are `square`, `Hamming`, `Bartlett`, or `Hanning`.
#' @param formant_plotOnSpec Boolean; should formants be plotted on top of
#' spectrogram? Default is `FALSE`.
#' @param fm Formant object loaded using [rPraat::formant.read]
#' @param formant_plotType String giving the type of formant plot to produce;
#' default is `speckle` (a point plot), the only other option is `draw` (a line
#' plot).
#' @param formant_dynamicRange Dynamic range in dB for producing formant plots.
#' When a formant plot of `formant_plotType='speckle'` is drawn, no formants are
#' shown in frames with intensity level `formant_dynamicRange` below the maximum
#' intensity. Default is `30`. If set to `0`, all formants are shown.
#' @param tgbool Logical; should dotted lines be plotted corresponding to
#' locations in a TextGrid? Default is `FALSE`.
#' @param lines Numeric vector giving locations in seconds of locations from
#' a TextGrid to be plotted with dotted lines. Default is `NULL`.
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param nframe Integer giving the number of plot components. Default is `NULL`.
#' @param start_end_only Logical; should there only be ticks on the x-axis
#' for start and end times? Default is `TRUE`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
#'
#' @export
#'
#' @examples
#' #dont use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' praatpicture(paste0(datapath, '/1.wav'), frames='spectrogram')
specplot <- function(sig, sr, t, start, end, tfrom0=TRUE, freqRange=c(0,5000),
                     windowLength=0.005, dynamicRange=60, timeStep=1000,
                     windowShape='Gaussian', formant_plotOnSpec=FALSE, fm=NULL,
                     formant_plotType='draw', formant_dynamicRange=30,
                     tgbool=FALSE, lines=NULL, ind=NULL, nframe=NULL,
                     start_end_only=TRUE, min_max_only=TRUE) {

  wl <- windowLength*1000
  ts <- -timeStep

  legal_ws <- c('square', 'Hamming', 'Bartlett', 'Hanning', 'Gaussian')
  if (!windowShape %in% legal_ws) {
    stop('Possible window shapes are square, Hamming, Bartlett, Hanning, or ',
         'Gaussian.')
  }

  if (tfrom0) {
    org_start <- start
    start <- 0
  }

  if (windowShape == 'square') ws <- 'rectangular'
  if (windowShape == 'Hamming') ws <- 'hamming'
  if (windowShape == 'Bartlett') ws <- 'bartlett'
  if (windowShape == 'Hanning') ws <- 'hann'
  if (windowShape == 'Gaussian') ws <- 'gaussian'

  if (ind==nframe) {
    if (!start_end_only) {
      xax <- 's'
    } else {
      xax <- 'n'
      xtix <- c(round(min(t), 3), round(max(t), 3), 0)
    }
  } else {
    xax <- 'n'
  }

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

  spec <- phonTools::spectrogram(sig, sr, colors=F, show=F, timestep=ts,
                                 windowlength=wl, window=ws,
                                 dynamicrange=dynamicRange)

  time_s <- as.numeric(rownames(spec$spectrogram))/1000
  if (!tfrom0) time_s <- time_s + start
  rownames(spec$spectrogram) <- time_s

  plot(NULL, xaxt=xax, xlim=c(start, end+start),
       ylim=freqRange, yaxs='i', yaxt=yax)
  if (ind == nframe & start_end_only) graphics::axis(1, at=xtix)
  if (!min_max_only[ind] & ind != 1) graphics::axis(2, at=ytix)
  if (min_max_only[ind]) graphics::axis(2, at=ytix, padj=c(0,1), las=2,
                                        tick=F)
  graphics::mtext('Frequency (Hz)', side=2, line=3.5, cex=0.8)

  plot(spec, add=T)
  if (tgbool) graphics::abline(v=lines, lty='dotted')

  if (formant_plotOnSpec) {
    nf <- fm$maxnFormants
    if (tfrom0) fm$t <- fm$t - org_start
    if (fm$conv2db) {
      db <- gsignal::pow2db(fm$intensityVector)
    } else {
      db <- fm$intensityVector
    }
    if (formant_dynamicRange != 0) {
      subdr <- which(db < max(db)-formant_dynamicRange)
      if (length(subdr) == 0) subdr <- 1
    } else {
      subdr <- 1
    }
    if (formant_plotType == 'draw') {
      graphics::lines(fm$t, fm$frequencyArray[1,])
      for (i in 2:nf) {
        graphics::lines(fm$t, fm$frequencyArray[i,])
      }
    }
    if (formant_plotType == 'speckle') {
      graphics::points(fm$t[-subdr], fm$frequencyArray[1,-subdr], pch=20)
      for (i in 2:nf) {
        graphics::points(fm$t[-subdr], fm$frequencyArray[i,-subdr], pch=20)
      }
    }
  }
}
