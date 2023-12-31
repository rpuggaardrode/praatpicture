% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specplot.R
\name{specplot}
\alias{specplot}
\title{Plot spectrogram}
\usage{
specplot(
  sig,
  sr,
  t,
  start,
  end,
  tfrom0 = TRUE,
  freqRange = c(0, 5000),
  windowLength = 0.005,
  dynamicRange = 60,
  timeStep = 1000,
  windowShape = "Gaussian",
  colors = c("white", "black"),
  formant_plotOnSpec = FALSE,
  fm = NULL,
  formant_plotType = "draw",
  formant_dynamicRange = 30,
  formant_color = "black",
  tgbool = FALSE,
  lines = NULL,
  ind = NULL,
  nframe = NULL,
  start_end_only = TRUE,
  min_max_only = TRUE,
  axisLabel = "Frequency (Hz)"
)
}
\arguments{
\item{sig}{Numeric vector corresponding to a sound signal.}

\item{sr}{Integer giving the sampling rate of the signal.}

\item{t}{Numeric vector giving times corresponding to the signal.}

\item{start}{Start time (in seconds) of desired plotted area.}

\item{end}{End time (in seconds) of desired plotted area.}

\item{tfrom0}{Logical; should time on the x-axis run from 0 or from the
original time? Default is \code{TRUE}.}

\item{freqRange}{Vector of two integers giving the frequency range to be
used for plotting spectrograms. Default is \code{c(0,5000)}.}

\item{windowLength}{Window length in seconds for generating spectrograms.
Default is \code{0.005}.}

\item{dynamicRange}{Dynamic range in dB for generating spectrograms. The
maximum intensity minus \code{dynamicRange} will all be printed in white. Default
is \code{50}.}

\item{timeStep}{How many time steps should be calculated for spectrograms?
Default is \code{1000}. Note that this takes a while to plot, so for fiddling with
plotting parameters it is a good idea to choose a smaller value.}

\item{windowShape}{String giving the name of the window shape to be applied
to the signal when generating spectrograms. Default is \code{Gaussian}; other
options are \code{square}, \code{Hamming}, \code{Bartlett}, or \code{Hanning}.}

\item{colors}{Vector of strings giving the names of colors to be used
for plotting the spectrogram; default is \code{c('white', 'black')}. The first
value is used for plotting the lowest visible amplitude, and the last for
plotting the highest visible amplitude. Vectors with more than two color
names can be used for plotting values in between in different colors.}

\item{formant_plotOnSpec}{Boolean; should formants be plotted on top of
spectrogram? Default is \code{FALSE}.}

\item{fm}{Formant object loaded using \link[rPraat:formant.read]{rPraat::formant.read}}

\item{formant_plotType}{String giving the type of formant plot to produce;
default is \code{speckle} (a point plot), the only other option is \code{draw} (a line
plot).}

\item{formant_dynamicRange}{Dynamic range in dB for producing formant plots.
When a formant plot of \code{formant_plotType='speckle'} is drawn, no formants are
shown in frames with intensity level \code{formant_dynamicRange} below the maximum
intensity. Default is \code{30}. If set to \code{0}, all formants are shown.}

\item{formant_color}{String or vector of strings giving the name(s) of
colors to be used for plotting formants. If one color is provided, all
formants will be plotted in this color. If multiple colors are provided,
different formants will be shown in different colors. Default is \code{'black'}.}

\item{tgbool}{Logical; should dotted lines be plotted corresponding to
locations in a TextGrid? Default is \code{FALSE}.}

\item{lines}{Numeric vector giving locations in seconds of locations from
a TextGrid to be plotted with dotted lines. Default is \code{NULL}.}

\item{ind}{Integer indexing waveform relative to other plot components.
Default is \code{NULL}.}

\item{nframe}{Integer giving the number of plot components. Default is \code{NULL}.}

\item{start_end_only}{Logical; should there only be ticks on the x-axis
for start and end times? Default is \code{TRUE}.}

\item{min_max_only}{Logical; should only minimum and maximum values be given
on the y-axis? Default is \code{TRUE}. Can also be a logical vector if some but
not all plot components should have minimum and maximum values on the y-axis.
Ignored for TextGrid component.}

\item{axisLabel}{String giving the name of the label to print along the
y-axis when plotting a spectrogram. Default is \code{Frequency (Hz)}.}
}
\description{
Function for plotting spectrograms called by \link{praatpicture}. Instead of using
this function directly, just use
\code{praatpicture('my_sound_file', frames='spectrogram')}.
}
\examples{
#dont use directly
datapath <- system.file('extdata', package='praatpicture')
praatpicture(paste0(datapath, '/1.wav'), frames='spectrogram')
}
