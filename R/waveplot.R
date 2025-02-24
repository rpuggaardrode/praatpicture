#' Plot waveform
#'
#' Function for plotting waveforms called by [praatpicture]. Instead of using
#' this function directly, just use
#' `praatpicture('my_sound_file', frames='sound')`.
#'
#' @param sig Numeric vector corresponding to a sound signal.
#' @param bit Numeric; will generally be grabbed from a loaded `WaveMC` object.
#' @param t Numeric vector giving times corresponding to the signal.
#' @param nchan Numeric; how many channels will be plotted? Default is `1`.
#' @param color String giving the name of the color to be used for plotting
#' the waveform. Default is `'black'`. Alternatively, a vector of colors, if
#' different channels should be plotted with different colors.
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
waveplot <- function(sig, bit, t, nchan=1, color='black', tgbool=FALSE,
                     lines=NULL, focusTierColor='black',
                     focusTierLineType='dotted', ind=NULL, line_comp=NULL,
                     rect_comp=NULL, arr_comp=NULL, annot_comp=NULL,
                     draw_lines=NULL, draw_rectangle=NULL, draw_arrow=NULL,
                     annotate=NULL, channelNames=FALSE, lineWidth=1, cn=NULL,
                     min_max_only=TRUE, highlight=NULL) {

  if (length(color) != nchan) color <- rep(color, nchan)

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
      ytix <- c(round(min(sig[,j]), 3), 0, round(max(sig[,j]), 2))
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
                                          tick=F)
    if (channelNames) graphics::mtext(cn[j], side=2, las=2, line=3.5, cex=0.8)
    if (tgbool) {
      for (k in 1:length(lines)) {
        graphics::abline(v=lines[[k]], col=focusTierColor[k],
                       lty=focusTierLineType[k])
      }
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
