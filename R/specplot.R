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
#' @param freqrange Vector of two integers giving the frequency range to be
#' used for plotting spectrograms. Default is `c(0,5000)`.
#' @param windowlength Window length in seconds for generating spectrograms.
#' Default is `0.005`.
#' @param dynamicrange Dynamic range in dB for generating spectrograms. The
#' maximum intensity minus `dynamicrange` will all be printed in white. Default
#' is `50`.
#' @param timestep How many time steps should be calculated for spectrograms?
#' Default is `1000`. Note that this takes a while to plot, so for fiddling with
#' plotting parameters it is a good idea to choose a smaller value.
#' @param windowshape String giving the name of the window shape to be applied
#' to the signal when generating spectrograms. Default is `Gaussian`; other
#' options are `square`, `Hamming`, `Bartlett`, or `Hanning`.
#' @param tgbool Logical; should dotted lines be plotted corresponding to
#' locations in a TextGrid? Default is `FALSE`.
#' @param lines Numeric vector giving locations in seconds of locations from
#' a TextGrid to be plotted with dotted lines. Default is `NULL`.
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param nframe Integer giving the number of plot components. Default is `NULL`.
#'
#' @export
#'
#' @examples
#' #dont use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' praatpicture(paste0(datapath, '/1.wav'), frames='spectrogram')
specplot <- function(sig, sr, t, start, end, tfrom0=TRUE, freqrange=c(0,5000),
                     windowlength=0.005, dynamicrange=60, timestep=1000,
                     windowshape='Gaussian', tgbool=FALSE,
                     lines=NULL, ind=NULL, nframe=NULL) {

  wl <- windowlength*1000
  ts <- -timestep

  legal_ws <- c('square', 'Hamming', 'Bartlett', 'Hanning', 'Gaussian')
  if (!windowshape %in% legal_ws) {
    stop('Possible window shapes are square, Hamming, Bartlett, Hanning, or ',
         'Gaussian.')
  }

  if (tfrom0) {
    start <- 0
  }

  if (windowshape == 'square') ws <- 'rectangular'
  if (windowshape == 'Hamming') ws <- 'hamming'
  if (windowshape == 'Bartlett') ws <- 'bartlett'
  if (windowshape == 'Hanning') ws <- 'hann'
  if (windowshape == 'Gaussian') ws <- 'gaussian'

  if (ind==nframe) {
    xax <- 's'
  } else {
    xax <- 'n'
  }

  if (ind != 1) {
    ytix <- grDevices::axisTicks(freqrange, log=F)
    ytix <- ytix[-length(ytix)]
    yax <- 'n'
  } else {
    yax <- 's'
  }

  spec <- phonTools::spectrogram(sig, sr, colors=F, show=F, timestep=ts,
                                 windowlength=wl, window=ws)
  time_s <- as.numeric(rownames(spec$spectrogram))/1000
  if (!tfrom0) time_s <- time_s + start

  rownames(spec$spectrogram) <- time_s

  plot(NULL, xaxt=xax, xlim=c(start, end+start),
       ylim=freqrange, yaxs='i', yaxt=yax)
  if (ind != 1) graphics::axis(2, at=ytix)
  graphics::mtext('Frequency (Hz)', side=2, line=3, cex=0.8)
  plot(spec, add=T)
  if (tgbool) graphics::abline(v=lines, lty='dotted')
}
