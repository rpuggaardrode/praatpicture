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
#' options are `square`, `Hamming`, `Bartlett`, `Hanning`, or `Blackman`.
#' Note that the Gaussian window function provided by the `phonTools` package
#' and used in `praatpicture()` does not have the same properties as the
#' Gaussian window function used for spectral estimation in Praat; plotting
#' a simple sine wave with high dynamic range will produce sidelobes in
#' `praatpicture()` but not in Praat. It's recommended to use Blackman windows
#' instead if you have this problem.
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
#' @param pitch_highlight Named list giving parameters for differential
#' highlighting of pitch based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the optional arguments
#' `color` (string or vector of strings, see `pitch_color`),
#' `drawSize` or `speckleSize` (both numeric).
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
#' @param formant_highlight Named list giving parameters for differential
#' highlighting of formants based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the optional arguments
#' `color` (string or vector of strings, see `formant_color`),
#' `drawSize` or `speckleSize` (both numeric).
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
#' @param intensity_highlight Named list giving parameters for differential
#' highlighting of the intensity contour based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the optional arguments
#' `color` (string or vector of strings, see `intensity_color`) and
#' `drawSize` (integer).
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
#' @param highlight Named list giving parameters for differential
#' highlighting of the spectrogram based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the argument
#' `colors` (vector of strings, see `colors`).
#' @param axisLabel String giving the name of the label to print along the
#' y-axis when plotting a spectrogram. Default is `Frequency (Hz)`.
#' @param drawSize Number indicating the line width of plot components where
#' the `_plotType` is `'draw'` (i.e., pitch, formants, or intensity rendered as
#' line plots). Default is `1`. Controls the `lwd` argument of
#' [graphics::lines].
#' @param speckleSize Number indicating the point size of plot components where
#' the `_plotType` is `'speckle'` (i.e. pitch or formants rendered as point
#' plots). Default is `1`. Controls the `cex` arguments of [graphics::points].
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
                     pitch_color='black', pitch_highlight=NULL,
                     formant_plotOnSpec=FALSE, fm=NULL,
                     formant_plotType='speckle', formant_dynamicRange=30,
                     formant_color='black', formant_highlight=NULL,
                     intensity_plotOnSpec=FALSE, it=NULL, intensity_range=NULL,
                     intensity_axisLabel='Intensity (dB)',
                     intensity_color='black', intensity_highlight=NULL,
                     tgbool=FALSE, lines=NULL, focusTierColor='black',
                     focusTierLineType='dotted', ind=NULL,
                     min_max_only=TRUE, highlight=NULL,
                     axisLabel='Frequency (Hz)', drawSize=1, speckleSize=1) {

  wl <- windowLength*1000
  ts <- -timeStep

  legal_ws <- c('square', 'Hamming', 'Bartlett', 'Hanning', 'Gaussian',
                'Blackman')
  if (!windowShape %in% legal_ws) {
    stop('Possible window shapes are square, Hamming, Bartlett, Hanning, ',
         'Gaussian, or Blackman.')
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
  if (windowShape == 'Blackman') ws <- 'blackman'

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

  if (!is.null(highlight)) {
    hlFillCol <- grDevices::colorRampPalette(highlight$colors)
    hlFillCol <- hlFillCol(length(levels) - 1)
    for (int in 1:length(highlight$start)) {
      times <- which(xVals > highlight$start[int] & xVals < highlight$end[int])
      highlight_t <- xVals[times]
      highlight_spec <- spec$spectrogram[times,]
      graphics::image(x=highlight_t, y=yVals, z=highlight_spec,
                      col=hlFillCol, useRaster=useRaster, add=TRUE)
    }
  }

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

    if (!is.null(formant_highlight)) {
      times <- which(fm$t > formant_highlight$start &
                       fm$t < formant_highlight$end)
      highlight_t <- fm$t[times]
      highlight_f <- fm$frequencyArray[,times]
      highlight_i <- db[times]
      if (formant_dynamicRange != 0) {
        hsubdr <- which(highlight_i < max(db)-formant_dynamicRange)
        if (length(hsubdr) == 0) hsubdr <- 1
      } else {
        hsubdr <- 1
      }

      if (!'color' %in% names(formant_highlight)) formant_highlight$color <-
        formant_color
      if (!'drawSize' %in% names(formant_highlight)) {
        formant_highlight$drawSize <- drawSize
      }
      if (!'speckleSize' %in% names(formant_highlight)) {
        formant_highlight$speckleSize <- speckleSize
      }
      if (length(formant_highlight$color) == 1) formant_highlight$color <-
        rep(formant_highlight$color, nf)
      if (length(formant_highlight$color) == 2 & nf > 2) {
        formant_highlight$color <- c(
          rep(formant_highlight$color[1], nf),
          rep(formant_highlight$color[2], nf))
      }
      if (length(formant_highlight$color) == nf+1) formant_highlight$color <- c(
        formant_highlight$color, rep(formant_highlight$color[nf+1], nf-1))
    }

    if ('draw' %in% formant_plotType) {
      if (length(formant_color) == nf*2) {
        graphics::lines(fm$t, fm$frequencyArray[1,], col=formant_color[nf+1],
                         lwd=drawSize+2)
      }
      graphics::lines(fm$t, fm$frequencyArray[1,], col=formant_color[1],
                      lwd=drawSize)
      for (i in 2:nf) {
        if (length(formant_color) == nf*2) {
          graphics::lines(fm$t, fm$frequencyArray[i,], lwd=drawSize+2,
                          col=formant_color[nf+i])
        }
        graphics::lines(fm$t, fm$frequencyArray[i,], col=formant_color[i],
                        lwd=drawSize)
      }
      if (!is.null(formant_highlight)) {
        if (length(formant_highlight$color) == nf*2) {
          graphics::lines(highlight_t, highlight_f[1,],
                          col=formant_highlight$color[nf+1],
                          lwd=formant_highlight$drawSize+2)
        }
        graphics::lines(highlight_t, highlight_f[1,],
                        col=formant_highlight$color[1],
                        lwd=formant_highlight$drawSize)
        for (i in 2:nf) {
          if (length(formant_highlight$color) == nf*2) {
            graphics::lines(highlight_t, highlight_f[i,],
                            lwd=formant_highlight$drawSize+2,
                            col=formant_highlight$color[nf+i])
          }
          graphics::lines(highlight_t, highlight_f[i,],
                          col=formant_highlight$color[i],
                          lwd=formant_highlight$drawSize)
        }
      }
    }
    if ('speckle' %in% formant_plotType) {
      if (length(formant_color) == nf*2) {
        graphics::points(fm$t[-subdr], fm$frequencyArray[1,-subdr], pch=20,
                         cex=speckleSize, lwd=3,
                         col=formant_color[nf+1])
      }
      graphics::points(fm$t[-subdr], fm$frequencyArray[1,-subdr], pch=20,
                       col=formant_color[1], cex=speckleSize)
      for (i in 2:nf) {
        if (length(formant_color) == nf*2) {
          graphics::points(fm$t[-subdr], fm$frequencyArray[i,-subdr], pch=20,
                           lwd=3, col=formant_color[nf+i], cex=speckleSize)
        }
        graphics::points(fm$t[-subdr], fm$frequencyArray[i,-subdr], pch=20,
                         col=formant_color[i], cex=speckleSize)
      }
      if (!is.null(formant_highlight)) {
        if (length(formant_highlight$color) == nf*2) {
          graphics::points(highlight_t[-hsubdr], highlight_f[1,-hsubdr],
                          col=formant_highlight$color[nf+1], pch=20,
                          lwd=3, cex=formant_highlight$speckleSize)
        }
        graphics::points(highlight_t[-hsubdr], highlight_f[1,-hsubdr],
                        col=formant_highlight$color[1], pch=20,
                        cex=formant_highlight$speckleSize)
        for (i in 2:nf) {
          if (length(formant_highlight$color) == nf*2) {
            graphics::points(highlight_t[-hsubdr], highlight_f[i,-hsubdr],
                             col=formant_highlight$color[nf+i], pch=20,
                            lwd=3, cex=formant_highlight$speckleSize)
          }
          graphics::points(highlight_t[-hsubdr], highlight_f[i,-hsubdr],
                          col=formant_highlight$color[i], pch=20,
                          cex=formant_highlight$speckleSize)
        }
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
    pt$f <- freqRange[1] + (pt$f - pfr[1]) * multiplier

    if (!is.null(pitch_highlight)) {
      highlight_tSpeckle <- c()
      highlight_fSpeckle <- c()
      highlight_tDraw <- c()
      highlight_fDraw <- c()
      for (int in 1:length(pitch_highlight$start)) {
        times <- which(pt$t > pitch_highlight$start[int] &
                         pt$t < pitch_highlight$end[int])
        highlight_tSpeckle <- c(highlight_tSpeckle, pt$t[times],
                                max(pt$t[times] + 0.0001))
        highlight_fSpeckle <- c(highlight_fSpeckle, pt$f[times], NA)
        if (pitch_highlight$start[int] < start) {
          extrema <- zoo::na.approx(c(NA, pt$f),
                                    c(pitch_highlight$end[int], pt$t))
          highlight_tDraw <- c(highlight_tDraw,
                               pt$t[times], pitch_highlight$end[int],
                               max(pt$t[times] + 0.0001))
          highlight_fDraw <- c(highlight_fDraw, pt$f[times], extrema[1], NA)
        } else {
          extrema <- zoo::na.approx(c(NA, NA, pt$f),
                                    c(pitch_highlight$start[int],
                                      pitch_highlight$end[int],
                                      pt$t))[1:2]
          highlight_tDraw <- c(highlight_tDraw, pitch_highlight$start[int],
                               pt$t[times], pitch_highlight$end[int],
                               max(pt$t[times] + 0.0001))
          highlight_fDraw <- c(highlight_fDraw, extrema[1], pt$f[times],
                               extrema[2], NA)
        }
      }

      if (!'color' %in% names(pitch_highlight)) pitch_highlight$color <-
        pitch_color
      if (!'drawSize' %in% names(pitch_highlight)) pitch_highlight$drawSize <-
        drawSize
      if (!'speckleSize' %in% names(pitch_highlight)) {
        pitch_highlight$speckleSize <- speckleSize
      }
    }

    if ('draw' %in% pitch_plotType) {
      if (length(pitch_color) == 2) graphics::lines(pt$t, pt$f, lwd=drawSize+2,
                                                    col=pitch_color[2])
      graphics::lines(pt$t, pt$f, lwd=drawSize, col=pitch_color[1])
      if (!is.null(pitch_highlight)) {
        if (length(pitch_highlight$color) == 2) {
          graphics::lines(highlight_tDraw, highlight_fDraw,
                          lwd=pitch_highlight$drawSize+2,
                          col=pitch_highlight$color[2])
        }
        graphics::lines(highlight_tDraw, highlight_fDraw,
                        col=pitch_highlight$color[1],
                        lwd=pitch_highlight$drawSize)
      }
    }
    if ('speckle' %in% pitch_plotType) {
      if (length(pitch_color) == 2) graphics::points(pt$t, pt$f, lwd=3, pch=20,
                                                     col=pitch_color[2],
                                                     cex=speckleSize)
      graphics::points(pt$t, pt$f, pch=20, col=pitch_color[1], cex=speckleSize)
      if (!is.null(pitch_highlight)) {
        if (length(pitch_highlight$color == 2)) {
          graphics::points(highlight_tSpeckle, highlight_fSpeckle,
                           lwd=3, pch=20,
                           col=pitch_highlight$color[2],
                           cex=pitch_highlight$speckleSize)
        }
        graphics::points(highlight_tSpeckle, highlight_fSpeckle, pch=20,
                         col=pitch_highlight$color[1],
                         cex=pitch_highlight$speckleSize)
      }
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
    it$i <- freqRange[1] + (it$i - intensity_range[1]) * multiplier

    if (!is.null(intensity_highlight)) {
      highlight_t <- c()
      highlight_i <- c()
      for (int in 1:length(intensity_highlight$start)) {
        times <- which(it$t > intensity_highlight$start[int] &
                         it$t < intensity_highlight$end[int])
        if (intensity_highlight$start[int] < start) {
          extrema <- zoo::na.approx(c(NA, it$i),
                                    c(intensity_highlight$end[int], it$t))
          highlight_t <- c(highlight_t,
                           it$t[times], intensity_highlight$end[int],
                           max(it$t[times] + 0.0001))
          highlight_i <- c(highlight_i, it$i[times], extrema[1], NA)
        } else {
          extrema <- zoo::na.approx(c(NA, NA, it$i),
                                    c(intensity_highlight$start[int],
                                      intensity_highlight$end[int],
                                      it$t))[1:2]
          highlight_t <- c(highlight_t, intensity_highlight$start[int],
                           it$t[times], intensity_highlight$end[int],
                           max(it$t[times] + 0.0001))
          highlight_i <- c(highlight_i, extrema[1], it$i[times], extrema[2], NA)
        }
      }
      if (!'color' %in% names(intensity_highlight)) intensity_highlight$color <-
        intensity_color
      if (!'drawSize' %in% names(intensity_highlight)) {
        intensity_highlight$drawSize <- drawSize
      }
    }

    if (length(intensity_color) == 2) graphics::lines(it$t, it$i,
                                                      lwd=drawSize+2,
                                                      col=intensity_color[2])
    graphics::lines(it$t, it$i, col=intensity_color[1], lwd=drawSize)
    if (!is.null(intensity_highlight)) {
      if (length(intensity_highlight$color) == 2) {
        graphics::lines(highlight_t, highlight_i,
                        lwd=intensity_highlight$drawSize+2,
                        col=intensity_highlight$color[2])
      }
      graphics::lines(highlight_t, highlight_i,
                      col=intensity_highlight$color,
                      lwd=intensity_highlight$drawSize)
    }

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
