% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_rectangle.R
\name{draw_rectangle}
\alias{draw_rectangle}
\title{Draw rectangle on praatpicture plot component}
\usage{
draw_rectangle(plot_component, args)
}
\arguments{
\item{plot_component}{String giving the name of the plot component to
draw on.}

\item{args}{A list of vectors giving arguments used for drawing
rectangles. See \link{praatpicture} documentation.}
}
\value{
No return values, called internally by \link{praatpicture} and sibling
functions.
}
\description{
Helper function for drawing rectangles on plot components made with
praatpicture. Do not use directly, instead use \link{praatpicture} with the
\code{draw_rectangle} argument.
}
\examples{
# Don't use directly
datapath <- system.file('extdata', package='praatpicture')
soundFile <- paste0(datapath, '/1.wav')
praatpicture(soundFile, draw_rectangle=c('spectrogram', 0.1, 500, 0.4, 2000))
}
