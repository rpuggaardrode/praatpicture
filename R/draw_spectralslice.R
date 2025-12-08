#' Draw spectral slice
#'
#' Generate and plot spectral slice from window around a specified time point
#' of a sound file.
#'
#' @param sound String giving the file name of a sound file with the .wav
#' extension.
#' @param time Time (in seconds) specifying the center of the window from which
#' to estimate spectrum.
#' @param channel Numeric giving the channel that should be used to
#' generate the spectrogram. Default is `1`.
#' @param freqRange Vector of two numbers giving the frequency range to be
#' used for plotting spectrograms. Default is `NULL`, in which case the whole
#' spectrum is plotted using the Nyquist frequency for the upper limit.
#' @param energyRange Either numeric or vector of two numbers used to specify
#' the y-axis range in units of dB/Hz. If a single number is passed, this is
#' interpreted as the desired dynamic range of the y-axis. If a vector of
#' two numbers is passed, these are used to exactly delimit the y-axis.
#' Default is `60`, i.e. a dynamic range of 60 dB/Hz relative to the global
#' maximum.
#' @param scale String giving the frequency scale to use. Default is `hz`.
#' Alternatives are `erb` (for the equivalent
#' rectangular bandwidth scale), `mel` (for the Mel scale, using 1000 Hz
#' as the corner frequency, following Fant 1968), or `logarithmic` (for
#' log Hz).
#' @param method String specifying the spectral estimation. Default is `fft`
#' (for the fast discrete Fourier transform). The only other option is
#' `multitaper`, in which case [multitaper::spec.mtm] is used to generate the
#' spectrum.
#' @param multitaper_args Optional named list of arguments passed on to
#' [multitaper::spec.mtm] if `method='multitaper'`.
#' @param windowLength Window length in seconds for generating spectra.
#' Default is `0.005`.
#' @param windowShape String giving the name of the window shape to be
#' applied to the signal when generating spectrograms. Default is `Gaussian`;
#' other options are `square`, `Hamming`, `Bartlett`, `Hanning`, `Blackman`, or
#' `Kaiser`.Note that the Gaussian window function provided by the `phonTools`
#' package and used for this function does not have the same properties as the
#' Gaussian window function used for spectral estimation in Praat.
#' @param color String giving the name of the color to be used for plotting
#' the spectrum. Default is `'black'`.
#' @param lineWidth Number giving the line width to use for plotting
#' the spectrum. Default is `1`.
#' @param freq_axisLabel String giving the name of the label to print along the
#' y-axis when plotting a spectrogram. Default is `NULL`, in which case the
#' axis label will depend on the scale.
#' @param energy_axisLabel String giving the name of the label to print along
#' the y-axis. Default is `Sound pressure level (dB/Hz)`.
#' @param mainTitle String giving a title to print at the top of the plot.
#' The default is an empty string, i.e. no title.
#' @param mainTitleAlignment Number indicating the vertical alignment of the
#' plot title, where `0` (default) indicates left-alignment, `1` indicates
#' right-alignment, `0.5` indicates central alignment, etc, following the
#' conventions of the `adj` argument of [graphics::mtext].
#' @param highlight Named list giving parameters for differential highlighting
#' of part of the plot based on the frequency domain. This list should contain
#' information about which parts of the plot to highlight, done with the
#' `start` and `end` arguments which must be numbers or numeric vectors.
#' Further contains the optional arguments `color`
#' (a string), `drawSize` (numeric), and `background`
#' (a string specifying a background color).
#' @param draw_lines Use for drawing straight lines. Takes
#' an argument of type `list` which should contain arguments to pass on to
#' [graphics::abline]. Should have a named argument `h` for horizontal lines,
#' or `v` for vertical lines, or `a`,`b` for the intercept and slope of the
#' line otherwise. Alternatively a nested list can be passed if more (sets of)
#' lines should be drawn. Default is `NULL`.
#' @param draw_rectangle Use for drawing rectangles. Should be a named
#' list containing arguments to pass on to [graphics::rect]. Can also be
#' multiple nested lists, if more rectangles should be drawn.
#' @param draw_arrow Use for drawing arrows on plot components. Should be a
#' named list containing arguments to pass on to [graphics::arrows].
#' Can also be multiple nested lists, if more rectangles should be drawn.
#' @param annotate Use for annotating plot components. Should be a
#' named list containing arguments to pass on to[graphics::text].
#' Can also be multiple nested lists, if more annotations should be added.
#' @param ... Further global plotting arguments passed on to `par()`.
#'
#' @return No return value, produces a figure.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' draw_spectralslice(soundFile, time = 0.75)
draw_spectralslice <- function(sound, time,
                               channel = 1, freqRange = NULL, energyRange = 60,
                               scale = 'hz', method = 'fft',
                               multitaper_args = NULL,
                               windowLength = 0.005, windowShape = 'Gaussian',
                               color = 'black', lineWidth = 1,
                               freq_axisLabel = NULL,
                               energy_axisLabel = 'Sound pressure level (dB/Hz)',
                               mainTitle = '', mainTitleAlignment = 0,
                               highlight = NULL, draw_lines = NULL,
                               draw_rectangle = NULL, draw_arrow = NULL,
                               annotate = NULL, ...) {

  p <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(p))

  if (!method %in% c('fft', 'multitaper')) stop(
    'method should be either fft or multitaper')

  if (scale == 'log') scale <- 'logarithmic'
  legal_scales <- c('hz', 'logarithmic', 'erb', 'mel')
  if (!scale %in% legal_scales) {
    stop('Possible scales are hz, logarithmic, erb, and mel')
  }
  if (is.null(freq_axisLabel)) {
    if (scale == 'hz') freq_axisLabel <- 'Frequency (Hz)'
    if (scale == 'logarithmic') freq_axisLabel <- 'Frequency (log Hz)'
    if (scale == 'mel') freq_axisLabel <- 'Frequency (mel)'
    if (scale == 'erb') freq_axisLabel <- 'Frequency (ERB)'
  }

  legal_ws <- c('square', 'Hamming', 'Bartlett', 'Hanning', 'Gaussian',
                'Blackman', 'Kaiser')
  if (!windowShape %in% legal_ws) {
    stop('Possible window shapes are square, Hamming, Bartlett, Hanning, ',
         'Gaussian, Kaiser, or Blackman.')
  }
  if (windowShape == 'square') {
    ws <- 'rectangular'
  } else if (windowShape == 'Hanning') {
    ws <- 'hann'
  } else {
    ws <- tolower(windowShape)
  }

  if (ws == 'kaiser') {
    winparam <- 3
  } else if (ws == 'gaussian') {
    winparam <- 0.4
  } else {
    winparam <- -1
  }

  log <- ifelse(scale == 'logarithmic', 'x', '')

  start <- time - (windowLength / 2)
  end <- time + (windowLength / 2)

  snd <- tuneR::readWave(sound, from=start, to=end, units='seconds', toWaveMC=T)

  sr <- snd@samp.rate
  bit <- snd@bit
  sig <- snd@.Data[,channel]

  if (scale == 'erb') {
    maxFreq <- soundgen::HzToERB(sr/2)
  } else if (scale == 'mel') {
    maxFreq <- 1000 / log(2) * log(1 + (sr/2) / 1000)
  } else {
    maxFreq <- sr / 2
  }

  if (is.null(freqRange)) {
    freqRange <- c(0, maxFreq)
  }

  if (method == 'fft') {
    sig <- sig / (2^(bit - 1) - 1)
    hz <- seq(0, sr, length.out = (length(sig)*2))
    hz <- hz[hz <= sr / 2]
    windowed_sig <- sig * phonTools::windowfunc(length(sig), ws, winparam)
    padded_sig <- c(rep(0, length(sig)/2), windowed_sig, rep(0, length(sig)/2))
    spec <- stats::fft(padded_sig)
    psd <- (Mod(spec)^2) / (length(sig) * sr)
    psd <- psd[1:length(sig)]
    spec <- data.frame(hz = hz,
                       dB = 10 * log10(psd / 0.00002^2))
  } else {
    multitaper_args$timeSeries <- sig
    multitaper_args$plot <- FALSE
    multitaper_args$deltat <- 1 / sr
    multitaper_args$log <- 'dB'
    spec <- do.call(multitaper::spec.mtm, multitaper_args)
    spec <- data.frame(hz = spec$freq,
                       dB = 10 * log10(spec$spec / 0.00002))
  }

  if (scale == 'erb') {
    spec$freq <- soundgen::HzToERB(spec$hz)
  } else if (scale == 'mel') {
    spec$freq <- 1000 / log(2) * log(1 + spec$hz / 1000)
  } else {
    spec$freq <- spec$hz
  }

  spec <- spec[which(spec$freq > freqRange[1] & spec$freq < freqRange[2]),]

  graphics::par(...)

  if (!is.null(highlight)) {
    highlight_f <- c()
    highlight_p <- c()
    for (int in 1:length(highlight$start)) {
      freqs <- which(spec$freq > highlight$start[int] &
                       spec$freq < highlight$end[int])
      extrema <- zoo::na.approx(c(NA, NA, spec$dB),
                                c(highlight$start[int], highlight$end[int],
                                  spec$hz))[1:2]
      highlight_f <- c(highlight_f, highlight$start[int],
                       spec$freq[freqs], highlight$end[int],
                       max(spec$freq[freqs] + 0.0001))
      highlight_p <- c(highlight_p, extrema[1], spec$dB[freqs],
                       extrema[2], NA)
    }
    if (!'color' %in% names(highlight)) highlight$color <- color
  }

  if (length(energyRange) == 1) {
    energyRange <- c(max(spec$dB) - energyRange, max(spec$dB))
  }

  plot(spec$freq, spec$dB, type = 'l', xlab = freq_axisLabel,
       ylab = energy_axisLabel, log = log, ylim = energyRange,
       col = color, lwd = lineWidth)
  graphics::mtext(mainTitle, side=3, line=2, adj=mainTitleAlignment)

  if (!is.null(highlight)) {
    if ('background' %in% names(highlight)) {
      graphics::rect(highlight$start,
                     energyRange[1] - energyRange[2] * 2,
                     highlight$end,
                     energyRange[2] + energyRange[2] * 2,
                     col = highlight$background, border = NA)
    }
    graphics::lines(highlight_f, highlight_p, col=highlight$color,
                    lwd=highlight$drawSize)
  }

  if (!is.null(draw_arrow)) {
    if (is.list(draw_arrow[[1]])) {
      for (i in 1:length(draw_arrow)) do.call(graphics::arrows, draw_arrow[[i]])
    } else {
      do.call(graphics::arrows, as.list(draw_arrow))
    }
  }

  if (!is.null(draw_rectangle)) {
    if (is.list(draw_rectangle[[1]])) {
      for (i in 1:length(draw_rectangle)) do.call(graphics::rect,
                                                  draw_rectangle[[i]])
    } else {
      do.call(graphics::rect, as.list(draw_rectangle))
    }
  }

  if (!is.null(draw_lines)) {
    if (is.list(draw_lines[[1]])) {
      for (i in 1:length(draw_lines)) do.call(graphics::abline, draw_lines[[i]])
    } else {
      do.call(graphics::abline, as.list(draw_lines))
    }
  }

  if (!is.null(annotate)) {
    if (is.list(annotate[[1]])) {
      for (i in 1:length(annotate)) do.call(graphics::text, annotate[[i]])
    } else {
      do.call(graphics::text, as.list(annotate))
    }
  }

  graphics::box()

}
