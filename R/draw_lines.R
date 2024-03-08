#' Draw straight lines on plot component
#'
#' Helper function for adding straight lines to plot components made with
#' praatpicture. Do not use directly, instead use [praatpicture] with the
#' `draw_lines` argument.
#'
#' @param plot_component String giving the name of the plot component to
#' draw on.
#' @param args A list of vectors giving arguments used for drawing
#' straight lines. See [praatpicture] documentation.
#'
#' @return No return values, called internally by [praatpicture] and sibling
#' functions.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' praatpicture(soundFile, draw_lines=c('spectrogram',
#' h=seq(0,5000,by=1000), lty='dashed'))
draw_lines <- function(plot_component, args) {
  r <- which(sapply(args, '[[', 1) == plot_component)
  for (i in r) {
    params <- args[[i]]
    params <- as.list(params[2:length(params)])
    do.call(eval(parse(text='graphics::abline')), params)
  }
}
