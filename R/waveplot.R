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
#' @param tgbool Logical; should dotted lines be plotted corresponding to
#' locations in a TextGrid? Default is `FALSE`.
#' @param lines Numeric vector giving locations in seconds of locations from
#' a TextGrid to be plotted with dotted lines. Default is `NULL`.
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param nframe Integer giving the number of plot components. Default is `NULL`.
#' @param rect_comp Vector of strings or numbers giving plot components to draw
#' rectangles on.
#' @param arr_comp Vector of strings of numbers giving plot components to draw
#' arrows on.
#' @param draw_rectangle List of arguments for drawing rectangles passed from
#' `praatpicture()`.
#' @param draw_arrow List of arguments for drawing arrows passed from
#' `praatpicture()`.
#' @param channel_names Logical; should names of audio channels be printed on
#' the y-axis? Default is `FALSE`.
#' @param cn Vector of strings with channel names to be printed on the y-axis
#' if `channel_names` is `TRUE`.
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
#' praatpicture(paste0(datapath, '/1.wav'), frames='sound')
waveplot <- function(sig, bit, t, nchan=1, tgbool=FALSE, lines=NULL,
                     ind=NULL, nframe=NULL,
                     rect_comp=NULL, arr_comp=NULL,
                     draw_rectangle=NULL, draw_arrow=NULL,
                     channel_names=FALSE, cn=NULL,
                     start_end_only=TRUE, min_max_only=TRUE) {
  for (i in 1:nchan) {

    if (ind==nframe & i==nchan) {
      if (!start_end_only) {
        xax <- 's'
      } else {
        xax <- 'n'
        xtix <- c(round(min(t), 3), round(max(t), 3), 0)
      }
    } else {
      xax <- 'n'
    }

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

    plot(t, sig[,i], type='l', xlab='', xaxt=xax, ylab='', yaxt=yax)

    if (ind == nframe & i == nchan & start_end_only) graphics::axis(1, at=xtix)
    if (yax == 'n' & !min_max_only[ind]) graphics::axis(2, at=ytix)
    if (min_max_only[ind]) graphics::axis(2, at=ytix, las=2, padj=c(0,0.5,1),
                                          tick=F)
    if (channel_names) graphics::mtext(cn[i], side=2, las=2, line=3.5, cex=0.8)
    if (tgbool) graphics::abline(v=lines, lty='dotted')

    if ('sound' %in% rect_comp) draw_rectangle('sound', draw_rectangle)
    if ('sound' %in% arr_comp) draw_arrow('sound', draw_arrow)

    if (i %in% rect_comp) draw_rectangle(i, draw_rectangle)
    if (i %in% arr_comp) draw_arrow(i, draw_arrow)
  }
}
