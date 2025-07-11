#' Plot waveform
#'
#' Function for plotting waveforms called by [praatpicture]. Instead of using
#' this function directly, just use
#' `praatpicture('my_sound_file', frames='sound')`.
#'
#' @param sig Numeric vector corresponding to a sound signal.
#' @param bit Numeric; will generally be grabbed from a loaded `WaveMC` object.
#' @param t Numeric vector giving times corresponding to the signal.
#' @param start Start time (in seconds) of desired plotted area.
#' @param tfrom0 Logical; should time on the x-axis run from 0 or from the
#' original time? Default is `TRUE`.
#' @param nchan Numeric; how many channels will be plotted? Default is `1`.
#' @param color String giving the name of the color to be used for plotting
#' the waveform. Default is `'black'`. Alternatively, a vector of colors, if
#' different channels should be plotted with different colors.
#' @param pitch_plotOnWave Boolean; should pitch be plotted on top of
#' waveform? Default is `FALSE`.
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
#' @param intensity_plotOnWave Boolean; should intensity be plotted on top of
#' waveform? Default is `FALSE`.
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
#' @param line_comp Vector of strings or numbers giving plot components to draw
#' straight lines on. Default is `NULL`.
#' @param rect_comp Vector of strings or numbers giving plot components to draw
#' rectangles on. Default is `NULL`.
#' @param arr_comp Vector of strings of numbers giving plot components to draw
#' arrows on. Default is `NULL`.
#' @param annot_comp Vector of strings of numbers giving plot components to
#' annotate. Default is `NULL`.
#' @param draw_lines List of arguments for drawing straight lines passed from
#' `praatpicture()`. Default is `NULL`.
#' @param draw_rectangle List of arguments for drawing rectangles passed from
#' `praatpicture()`. Default is `NULL`.
#' @param draw_arrow List of arguments for drawing arrows passed from
#' `praatpicture()`. Default is `NULL`.
#' @param annotate List of arguments for annotating passed from
#' `praatpicture()`. Default is `NULL`.
#' @param channelNames Logical; should names of audio channels be printed on
#' the y-axis? Default is `FALSE`.
#' @param axisDigits Numeric giving the number of digits to print for
#' values along the y-axis of the waveform. Default is `3`. If `0` is passed,
#' the y-axis is suppressed. Note that this only applies when
#' `min_max_only = TRUE`, as otherwise the look of the y-axis is determined
#' entirely using `grDevices::axisTicks()`.
#' @param lineWidth Number giving the line width to use for plotting
#' the waveform. Default is `1`.
#' @param cn Vector of strings with channel names to be printed on the y-axis
#' if `channelNames` is `TRUE`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
#' @param highlight Named list giving parameters for differential
#' highlighting of the waveform based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the argument
#' `color` (string, see `color`), and `background`
#' (a string specifying a background color).
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
#' praatpicture(soundFile, frames='sound')
waveplot <- function(sig, bit, t, start, tfrom0=TRUE, nchan=1, color='black',
                     pitch_plotOnWave=FALSE, pt=NULL,
                     pitch_plotType='draw', pitch_scale='hz',
                     pitch_freqRange=NULL, pitch_axisLabel=NULL,
                     pitch_color='black', pitch_highlight=NULL,
                     intensity_plotOnWave=FALSE, it=NULL, intensity_range=NULL,
                     intensity_axisLabel='Intensity (dB)',
                     intensity_color='black', intensity_highlight=NULL,
                     tgbool=FALSE,
                     lines=NULL, focusTierColor='black',
                     focusTierLineType='dotted',
                     ind=NULL, line_comp=NULL,
                     rect_comp=NULL, arr_comp=NULL, annot_comp=NULL,
                     draw_lines=NULL, draw_rectangle=NULL, draw_arrow=NULL,
                     annotate=NULL, channelNames=FALSE, axisDigits=3,
                     lineWidth=1, cn=NULL, min_max_only=TRUE, highlight=NULL,
                     drawSize=1, speckleSize=1) {

  if (length(color) != nchan) color <- rep(color, nchan)

  if (tfrom0) {
    org_start <- start
    start <- 0
  }

  for (j in 1:nchan) {

    sig[,j] <- sig[,j] / (2^(bit - 1) - 1)

    if (!min_max_only[ind]) {
      if (j == 1 & ind == 1) {
        yax <- 's'
      } else {
        ytix <- grDevices::axisTicks(c(min(sig[,j]), max(sig[,j])), log=F)
        ytix <- ytix[-length(ytix)]
        yax <- 'n'
      }
    } else {
      yax <- 'n'
      ytix <- c(min(sig[,j]), 0, max(sig[,j]))
      if (axisDigits == 0) {
        ytixLabs <- rep('', 3)
      } else {
        ytixLabs <- c(round(min(sig[,j]), axisDigits), 0,
                      round(max(sig[,j]), axisDigits))
      }
    }

    if (!is.null(highlight)) {
      highlight_t <- c()
      highlight_w <- c()
      for (int in 1:length(highlight$start)) {
        times <- which(t > highlight$start[int] & t < highlight$end[int])
        highlight_t <- c(highlight_t, t[times[-length(times)]+1],
                         max(t[times] + 0.0001))
        highlight_w <- c(highlight_w, sig[times[-length(times)],j], NA)
      }
      if (any(highlight$start < t[1])) {
        highlight$start[which(highlight$start < t[1])] <- t[1]
      }
      if (any(highlight$end > max(t))) {
        highlight$end[which(highlight$end > max(t))] <- max(t)
      }
      if (!'color' %in% names(highlight)) highlight$color <- color
    }

    plot(t[-1], sig[,j], type='l', xlab='', xaxt='n', ylab='', yaxt=yax,
         col=color[j], lwd=lineWidth)

    if (!is.null(highlight)) {
      if ('background' %in% names(highlight)) {
        graphics::rect(highlight$start,
                       -abs(min(sig[,j]) * 2),
                       highlight$end,
                       max(sig[,j]) * 2,
                       col = highlight$background, border = NA)
      }
      graphics::lines(highlight_t,
                      highlight_w,
                      col=highlight$color,
                      lwd=lineWidth)
    }

    if (yax == 'n' & !min_max_only[ind]) graphics::axis(2, at=ytix)
    if (min_max_only[ind]) graphics::axis(2, at=ytix, las=2, padj=c(0,0.5,1),
                                          tick=F, labels=ytixLabs)
    if (channelNames) graphics::mtext(cn[j], side=2, las=2, line=3.5, cex=0.8)
    if (tgbool) {
      for (k in 1:length(lines)) {
        graphics::abline(v=lines[[k]], col=focusTierColor[k],
                       lty=focusTierLineType[k])
      }
    }

    if (as.numeric(pitch_plotOnWave) == j) {
      pitch_overlay(pt, min(sig[,j]), max(sig[,j]), start, org_start, tfrom0,
                    pitch_freqRange, pitch_plotType, pitch_scale, pitch_color,
                    ind, drawSize, speckleSize, pitch_axisLabel, min_max_only,
                    pitch_highlight, intensity_plotOnWave == j)
    }

    if (as.numeric(intensity_plotOnWave) == j) {
      intensity_overlay(it, min(sig[,j]), max(sig[,j]), start, org_start,
                        tfrom0, intensity_range, intensity_color, ind, drawSize,
                        intensity_axisLabel, min_max_only, intensity_highlight,
                        pitch_plotOnWave == j)
    }

    if ('sound' %in% rect_comp) draw_rectangle('sound', draw_rectangle)
    if ('sound' %in% arr_comp) draw_arrow('sound', draw_arrow)
    if ('sound' %in% annot_comp) make_annot('sound', annotate)
    if ('sound' %in% line_comp) draw_lines('sound', draw_lines)

    if (j %in% rect_comp) draw_rectangle(j, draw_rectangle)
    if (j %in% arr_comp) draw_arrow(j, draw_arrow)
    if (j %in% annot_comp) make_annot(j, annotate)
    if (j %in% line_comp) draw_lines(j, draw_lines)

    graphics::box()
  }
}
