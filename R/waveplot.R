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
#' the waveform. Default is `'black'`.
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
#' @param rect_comp Vector of strings or numbers giving plot components to draw
#' rectangles on. Default is `NULL`.
#' @param arr_comp Vector of strings of numbers giving plot components to draw
#' arrows on. Default is `NULL`.
#' @param annot_comp Vector of strings of numbers giving plot components to
#' annotate. Default is `NULL`.
#' @param draw_rectangle List of arguments for drawing rectangles passed from
#' `praatpicture()`. Default is `NULL`.
#' @param draw_arrow List of arguments for drawing arrows passed from
#' `praatpicture()`. Default is `NULL`.
#' @param annotate List of arguments for annotating passed from
#' `praatpicture()`. Default is `NULL`.
#' @param channelNames Logical; should names of audio channels be printed on
#' the y-axis? Default is `FALSE`.
#' @param cn Vector of strings with channel names to be printed on the y-axis
#' if `channelNames` is `TRUE`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
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
                     focusTierLineType='dotted', ind=NULL,
                     rect_comp=NULL, arr_comp=NULL, annot_comp=NULL,
                     draw_rectangle=NULL, draw_arrow=NULL, annotate=NULL,
                     channelNames=FALSE, cn=NULL, min_max_only=TRUE) {
  for (i in 1:nchan) {

    sig[,i] <- sig[,i] / (2^(bit - 1) - 1)

    if (!min_max_only[ind]) {
      if (i == 1 & ind == 1) {
        yax <- 's'
      } else {
        ytix <- grDevices::axisTicks(c(min(sig[,i]), max(sig[,i])), log=F)
        ytix <- ytix[-length(ytix)]
        yax <- 'n'
      }
    } else {
      yax <- 'n'
      ytix <- c(round(min(sig[,i]), 3), 0, round(max(sig[,i]), 2))
    }

    plot(t[-1], sig[,i], type='l', xlab='', xaxt='n', ylab='', yaxt=yax,
         col=color)

    if (yax == 'n' & !min_max_only[ind]) graphics::axis(2, at=ytix)
    if (min_max_only[ind]) graphics::axis(2, at=ytix, las=2, padj=c(0,0.5,1),
                                          tick=F)
    if (channelNames) graphics::mtext(cn[i], side=2, las=2, line=3.5, cex=0.8)
    if (tgbool) {
      for (i in 1:length(lines)) {
        graphics::abline(v=lines[[i]], col=focusTierColor[i],
                       lty=focusTierLineType[i])
      }
    }

    if ('sound' %in% rect_comp) draw_rectangle('sound', draw_rectangle)
    if ('sound' %in% arr_comp) draw_arrow('sound', draw_arrow)
    if ('sound' %in% annot_comp) make_annot('sound', annotate)

    if (i %in% rect_comp) draw_rectangle(i, draw_rectangle)
    if (i %in% arr_comp) draw_arrow(i, draw_arrow)
    if (i %in% annot_comp) make_annot(i, annotate)
  }
}
