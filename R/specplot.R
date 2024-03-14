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
#' @param colors Vector of strings giving the names of colors to be used
#' for plotting the spectrogram; default is `c('white', 'black')`. The first
#' value is used for plotting the lowest visible amplitude, and the last for
#' plotting the highest visible amplitude. Vectors with more than two color
#' names can be used for plotting values in between in different colors.
#' @param pitch_plotOnSpec Boolean; should pitch be plotted on top of
#' spectrogram? Default is `FALSE`.
#' @param pt Pitch object loaded using [rPraat::pt.read] or similar object.
#' @param pitch_plotType String giving the type of pitch plot to produce; default
#' is `draw` (a line plot), the only other option is `speckle` (a point plot).
#' Alternatively a vector `c('draw','speckle')` can be passed, in which case
#' both are used.
#' @param pitch_scale String giving the frequency scale to use when producing
#' pitch plots. Default is `hz`; other options are `logarithmic` (also in Hz),
#' `semitones`, `erb`, and `mel`.
#' @param pitch_freqRange Vector of two integers giving the frequency range to be
#' used for producing pitch plots. Default is `NULL`, in which case the pitch
#' range is automatically reset to `c(-12,30)` for the `semitones` scale,
#' `c(0,10)` for the `erb` scale, and `c(50,500)` for the Hz-based scales,
#' following Praat defaults.
#' @param pitch_axisLabel String giving the name of the label to print along the
#' y-axis when printing a pitch track. Default is `NULL`, in which case the
#' axis label will depend on the scale.
#' @param pitch_color String or vector of strings giving the name of the color
#' to be used for plotting pitch. Default is `'black'`. If a vector of two
#' strings is passed, the second color will be used for background highlighting.
#' @param formant_plotOnSpec Boolean; should formants be plotted on top of
#' spectrogram? Default is `FALSE`.
#' @param fm Formant object loaded using [rPraat::formant.read] or similar
#' object.
#' @param formant_plotType String giving the type of formant plot to produce;
#' default is `speckle` (a point plot), the only other option is `draw` (a line
#' plot). Alternatively a vector `c('draw','speckle')` can be passed, in which
#' case both are used.
#' @param formant_dynamicRange Dynamic range in dB for producing formant plots.
#' When a formant plot of `formant_plotType='speckle'` is drawn, no formants are
#' shown in frames with intensity level `formant_dynamicRange` below the maximum
#' intensity. Default is `30`. If set to `0`, all formants are shown.
#' @param formant_color String or vector of strings giving the name(s) of
#' colors to be used for plotting formants. If one color is provided, all
#' formants will be plotted in this color. If multiple colors are provided,
#' different formants will be shown in different colors. Default is `'black'`.
#' If the length of this vector twice the number of formants plotted, the first
#' half of strings will be used for the formants' primary colors and the second
#' half will be used for background highlighting. If the length of this vector
#' is one more than the number of formants plotted, the last string will
#' be used for background highlighting.
#' @param intensity_plotOnSpec Boolean; should intensity be plotted on top of
#' spectrogram? Default is `FALSE`.
#' @param it Intensity object loaded using [rPraat::it.read] or similar object.
#' @param intensity_range Vector of two integers giving the intensity range to be
#' used for producing intensity plots. Default is `NULL`, in which case the
#' range is simply the minimum and maximum levels in the curve.
#' @param intensity_axisLabel String giving the name of the label to print along
#' the y-axis when plotting intensity. Default is `Intensity (dB)`.
#' @param intensity_color String or vector of strings giving the name of the
#' color to be used for plotting intensity. Default is `'black'`. If a vector of
#' two strings is passed, the second color will be used for background
#' highlighting.
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
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
#' @param axisLabel String giving the name of the label to print along the
#' y-axis when plotting a spectrogram. Default is `Frequency (Hz)`.
#'
#' @return No return values, called internally by [praatpicture] and sibling
#' functions.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' praatpicture(soundFile, frames='spectrogram')
specplot <- function(sig, sr, t, start, end, tfrom0=TRUE, freqRange=c(0,5000),
                     windowLength=0.005, dynamicRange=60, timeStep=1000,
                     windowShape='Gaussian', colors=c('white', 'black'),
                     pitch_plotOnSpec=FALSE, pt=NULL,
                     pitch_plotType='draw', pitch_scale='hz',
                     pitch_freqRange=NULL, pitch_axisLabel=NULL,
                     pitch_color='black',
                     formant_plotOnSpec=FALSE, fm=NULL,
                     formant_plotType='speckle', formant_dynamicRange=30,
                     formant_color='black',
                     intensity_plotOnSpec=FALSE, it=NULL, intensity_range=NULL,
                     intensity_axisLabel='Intensity (dB)',
                     intensity_color='black',
                     tgbool=FALSE, lines=NULL, focusTierColor='black',
                     focusTierLineType='dotted', ind=NULL,
                     min_max_only=TRUE,
                     axisLabel='Frequency (Hz)') {

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
                                 dynamicrange=dynamicRange,
                                 windowparameter=0.4)

  freqdom <- as.numeric(unlist(dimnames(spec$spectrogram)[2]))
  within_freqran <- which(freqdom < freqRange[2] & freqdom > freqRange[1])
  spec$spectrogram <- spec$spectrogram[,within_freqran]

  time_s <- as.numeric(rownames(spec$spectrogram))/1000
  if (!tfrom0) time_s <- time_s + start
  rownames(spec$spectrogram) <- time_s

  if (grDevices::dev.capabilities()$rasterImage == 'yes') {
    useRaster <- TRUE
  } else {
    useRaster <- FALSE
  }

  plot(NULL, xaxt='n', xlim=c(start, end+start),
       ylim=freqRange, yaxs='i', yaxt=yax)
  if (!min_max_only[ind] & ind != 1) graphics::axis(2, at=ytix)
  if (min_max_only[ind]) graphics::axis(2, at=ytix, padj=c(0,1), las=2,
                                        tick=F)
  graphics::mtext(axisLabel, side=2, line=3.5, cex=0.8)

  fillCol <- grDevices::colorRampPalette(colors)
  fillRan <- c(-dynamicRange, 0)
  nlevels <- abs(-dynamicRange - 0) * 1.2
  levels <- pretty(fillRan, nlevels)
  fillCol <- fillCol(length(levels) - 1)
  subdr <- which(spec$spectrogram < -dynamicRange)
  spec$spectrogram[subdr] <- -dynamicRange

  specDims <- dim(spec$spectrogram)
  xVals <- seq(min(time_s), max(time_s), length.out=specDims[1])
  yVals <- seq(freqRange[1], freqRange[2], length.out=specDims[2])

  graphics::image(x=xVals, y=yVals, z=spec$spectrogram,
                  col=fillCol, useRaster=useRaster, add=TRUE)

  if (formant_plotOnSpec) {
    nf <- fm$maxnFormants
    if (length(formant_color) == 1) formant_color <- rep(formant_color, nf)
    if (length(formant_color) == 2 & nf > 2) formant_color <- c(
      rep(formant_color[1], nf), rep(formant_color[2], nf))
    if (length(formant_color) == nf+1) formant_color <- c(
      formant_color, rep(formant_color[nf+1], nf-1))
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
    if ('draw' %in% formant_plotType) {
      if (length(formant_color) == nf*2) {
        graphics::lines(fm$t, fm$frequencyArray[1,], col=formant_color[nf+1],
                         lwd=3)
      }
      graphics::lines(fm$t, fm$frequencyArray[1,], col=formant_color[1])
      for (i in 2:nf) {
        if (length(formant_color) == nf*2) {
          graphics::lines(fm$t, fm$frequencyArray[i,], lwd=3,
                          col=formant_color[nf+i])
        }
        graphics::lines(fm$t, fm$frequencyArray[i,], col=formant_color[i])
      }
    }
    if ('speckle' %in% formant_plotType) {
      if (length(formant_color) == nf*2) {
        graphics::points(fm$t[-subdr], fm$frequencyArray[1,-subdr], pch=20,
                         lwd=3, col=formant_color[nf+1])
      }
      graphics::points(fm$t[-subdr], fm$frequencyArray[1,-subdr], pch=20,
                       col=formant_color[1])
      for (i in 2:nf) {
        if (length(formant_color) == nf*2) {
          graphics::points(fm$t[-subdr], fm$frequencyArray[i,-subdr], pch=20,
                           lwd=3, col=formant_color[nf+i])
        }
        graphics::points(fm$t[-subdr], fm$frequencyArray[i,-subdr], pch=20,
                         col=formant_color[i])
      }
    }
  }

  if (pitch_plotOnSpec) {
    pfr <- pitch_freqRange
    if (tfrom0) pt$t <- pt$t - org_start
    if (pitch_scale == 'logarithmic') {
      pt$f <- log(pt$f)
      pfr <- log(pitch_freqRange)
    }
    sRan <- freqRange[2] - freqRange[1]
    pRan <- pfr[2] - pfr[1]
    multiplier <- sRan / pRan
    pt$f <- (pt$f - pfr[1]) * multiplier

    if ('draw' %in% pitch_plotType) {
      if (length(pitch_color) == 2) graphics::lines(pt$t, pt$f, lwd=3,
                                                    col=pitch_color[2])
      graphics::lines(pt$t, pt$f, lwd=1, col=pitch_color[1])
    }
    if ('speckle' %in% pitch_plotType) {
      if (length(pitch_color) == 2) graphics::points(pt$t, pt$f, lwd=3, pch=20,
                                                     col=pitch_color[2])
      graphics::points(pt$t, pt$f, pch=20, col=pitch_color[1])
    }

    pline <- c(3.5,1)
    if (intensity_plotOnSpec) pline <- c(1,1)

    graphics::mtext(pitch_axisLabel, side=4, line=pline[1], cex=0.8, las=3,
                    col=pitch_color[1])

    if (min_max_only[ind]) {
      graphics::mtext(pitch_freqRange[1], side=4, line=pline[2],
                      at=freqRange[1], padj=0, las=2, cex=0.7,
                      col=pitch_color[1])
      graphics::mtext(pitch_freqRange[2], side=4, line=pline[2],
                      at=freqRange[2], padj=1, las=2, cex=0.7,
                      col=pitch_color[1])
    } else {
      rtix <- pretty(pfr[1]:pfr[2])
      graphics::axis(4, at=(rtix-pfr[1])*multiplier, labels=rtix,
                     col.ticks=pitch_color[1], col.axis=pitch_color[1])
    }
  }

  if (intensity_plotOnSpec) {
    if (tfrom0) it$t <- it$t - org_start
    sRan <- freqRange[2] - freqRange[1]
    iRan <- intensity_range[2] - intensity_range[1]
    multiplier <- sRan / iRan
    it$i <- (it$i - intensity_range[1]) * multiplier

    if (length(intensity_color) == 2) graphics::lines(it$t, it$i, lwd=3,
                                                      col=intensity_color[2])
    graphics::lines(it$t, it$i, col=intensity_color[1])
    iline <- c(3.5,1)
    if (pitch_plotOnSpec) iline <- c(3.5,3.5)

    graphics::mtext(intensity_axisLabel, side=4, line=iline[1], cex=0.8, las=3,
                    col=intensity_color[1])

    if (min_max_only[ind]) {
      graphics::mtext(round(intensity_range[1], 0),
                      side=4, line=iline[2], at=freqRange[1],
                      padj=0, las=2, cex=0.7, col=intensity_color[1])
      graphics::mtext(round(intensity_range[2]),
                      side=4, line=iline[2], at=freqRange[2],
                      padj=1, las=2, cex=0.7, col=intensity_color[1])
    } else {
      rtix <- pretty(intensity_range[1]:intensity_range[2])
      graphics::axis(4, at=(rtix-intensity_range[1])*multiplier, labels=rtix,
                     col.ticks=intensity_color[1], col.axis=intensity_color[1])
    }
  }

  if (tgbool) {
    for (i in 1:length(lines)) {
      graphics::abline(v=lines[[i]], col=focusTierColor[i],
                       lty=focusTierLineType[i])
    }
  }

}
