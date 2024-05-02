#' Annotate praatpicture plot component
#'
#' Helper function for annotating plot components made with
#' praatpicture. Do not use directly, instead use [praatpicture] with the
#' `annotate` argument.
#'
#' @param plot_component String giving the name of the plot component to
#' annotate.
#' @param args A list of vectors giving arguments used for annotating.
#' See [praatpicture] documentation.
#'
#' @return No return values, called internally by [praatpicture] and sibling
#' functions.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' praatpicture(soundFile, annotate=c('spectrogram', 0.25, 1500,
#' 'An annotation'))
make_annot <- function(plot_component, args) {
  r <- which(sapply(args, '[[', 1) == plot_component)
  for (i in r) {
    params <- args[[i]]
    params <- as.list(params[2:length(params)])
    for (p in 1:2) params[[p]] <- as.numeric(params[[p]])
    for (p in c('adj', 'pos', 'offset', 'cex', 'font')) {
      params[which(names(params) == p)] <-
        as.numeric(params[which(names(params) == p)])
    }
    do.call(eval(parse(text='graphics::text')), params)
  }
}
