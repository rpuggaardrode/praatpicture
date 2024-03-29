% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/talking_praatpicture.R
\name{talking_praatpicture}
\alias{talking_praatpicture}
\title{Make Praat Picture style plots of acoustic data with embedded audio}
\usage{
talking_praatpicture(
  sound,
  start = 0,
  end = 0,
  audio_start = start,
  audio_end = end,
  width = 1080,
  height = 720,
  pointsize = 25,
  outputFile = "praatvid.mp4",
  useViewer = TRUE,
  ...
)
}
\arguments{
\item{sound}{String giving the file name of a sound file with the .wav
extension.}

\item{start}{Start time (in seconds) of desired plotted area. Default is \code{0}.}

\item{end}{End time (in seconds) of desired plotted area. Default is \code{0}
(= the entire file).}

\item{audio_start}{Start time (in seconds) of embedded audio. By default
it is the same as \code{start}, i.e. the embedded audio is the portion of the
sound file that is being plotted.}

\item{audio_end}{End time (in seconds) of embedded audio. By default it is
the same as \code{end}, i.e. the embedded audio is the portion of the sound
that is being plotted.}

\item{width}{Number giving the desired width of the resulting animation in
pixels; default is \code{1080}.}

\item{height}{Number giving the desired height of the resulting animation in
pixels; default is \code{720}.}

\item{pointsize}{Number; which point size should be used for text in the
animation? Default is \code{25}. See \code{\link[grDevices:png]{grDevices::png()}} for more details.}

\item{outputFile}{String giving the desired file name. Default is
\code{praatvid.mp4}.}

\item{useViewer}{Logical; should the video be shown in the Viewer pane in
RStudio? Default is \code{TRUE}; if true, the video is oSnly saved in a
temporary directory, but can be downloaded from a browser.}

\item{...}{Further arguments passed to \code{praatpicture}.}
}
\value{
No return value, produces a video file.
}
\description{
Generate simple MP4 video files with Praat Picture style plots of acoustic
data with time-aligned transcriptions and embedded audio to use in
presentations etc.
}
\examples{
\dontrun{
datapath <- system.file('extdata', package='praatpicture')
soundFile <- paste0(datapath, '/1.wav')
talking_praatpicture(soundFile)
}
}
\seealso{
This function is a wrapper for \code{\link[av:capturing]{av::av_capture_graphics()}} used to
produce plots similar to those made with \code{\link[=praatpicture]{praatpicture()}} with embedded
audio. For more detail on your
options, see the \code{praatpicture()} help file.
}
