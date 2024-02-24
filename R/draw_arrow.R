#' Draw arrow on praatpicture plot component
#'
#' Helper function for drawing arrows on plot components made with
#' praatpicture. Do not use directly, instead use [praatpicture] with the
#' `draw_arrow` argument.
#'
#' @param plot_component String giving the name of the plot component to
#' draw on.
#' @param args A list of vectors giving arguments used for drawing
#' arrows. See [praatpicture] documentation.
#'
#' @return No return values, called internally by [praatpicture] and sibling
#' functions.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' praatpicture(soundFile, draw_arrow=c('spectrogram', 0.1, 500, 0.4, 2000))
draw_arrow <- function(plot_component, args) {
  r <- which(sapply(args, '[[', 1) == plot_component)
  for (i in r) {
    params <- args[[i]]
    params <- as.list(params[2:length(params)])
    for (p in 1:4) params[[p]] <- as.numeric(params[[p]])
    do.call(eval(parse(text='graphics::arrows')), params)
  }
}
