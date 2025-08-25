#' Make Praat Picture style plots of acoustic data
#'
#' Generate plots of acoustic data aligned with transcriptions similar
#' to those made with Praat Picture. The default is to produce a plot with a
#' relatively small waveform, somewhat larger spectrogram, and the first tier
#' of a TextGrid.
#'
#' @param sound String giving the file name of a sound file with the .wav
#' extension.
#' @param start Start time (in seconds) of desired plotted area. Default is `0`.
#' @param end End time (in seconds) of desired plotted area. Default is `0`
#' (= the entire file).
#' @param tfrom0 Logical; should time on the x-axis run from 0 or from the
#' original time? Default is `TRUE`.
#' @param tUnit String giving the unit of time to print along the x-axis.
#' Possible options are `'s'` (default) for seconds and `'ms'` for milliseconds.
#' @param frames String or vector of strings giving the frames that the plot
#' should consist of. Default is `sound`, `spectrogram`, `TextGrid`. This
#' requires a file with the extension `.TextGrid` and the same base name as the
#' sound file. Other options are `pitch`, `formant`, and `intensity`. See
#' details for more information.
#' @param proportion Integer or vector of integers of the same size as `frames`
#' giving the proportion in percents of the plotting area to be taken up by the
#' individual frames. Default is `c(30,50,20)`. If more or less than three
#' frames are plotted and no proportions are given, frames will be of equal
#' size.
#' @param mainTitle String giving a title to print at the top of the plot.
#' The default is an empty string, i.e. no title.
#' @param mainTitleAlignment Number indicating the vertical alignment of the
#' plot title, where `0` (default) indicates left-alignment, `1` indicates
#' right-alignment, `0.5` indicates central alignment, etc, following the
#' conventions of the `adj` argument of [graphics::mtext].
#' @param start_end_only Logical; should there only be ticks on the x-axis
#' for start and end times? Default is `TRUE`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
#' @param drawSize Number indicating the line width of plot components where
#' the `_plotType` is `'draw'` (i.e., pitch, formants, or intensity rendered as
#' line plots). Default is `1`. Controls the `lwd` argument of
#' [graphics::lines].
#' @param speckleSize Number indicating the point size of plot components where
#' the `_plotType` is `'speckle'` (i.e. pitch or formants rendered as point
#' plots). Default is `1`. Controls the `cex` arguments of [graphics::points].
#' @param wave_channels Vector of numbers or strings giving either numeric
#' identifiers of audio channels to plot of the names of audio channels to plot.
#' Also understands `'all'`, which plots all channels and is the default.
#' @param wave_channelNames Should names of audio channels be printed
#' on the y-axis? If `TRUE`, names will be grabbed from the audio metadata if
#' available. Alternatively, if two channels are available, they will be named
#' `left` and `right`. If more or less than two channels are available,
#' channels are named `Cn`, where `n` is the number of the channel. Alternatvely,
#' a vector of strings can be provided with channel names. Default is `FALSE`.
#' @param wave_energyRange Numeric vector of length 2 giving the desired energy
#' range (y-axis range) of waveform(s). Default is `NULL`, in which case the
#' y-axis range is set to the lowest and highest value for each wave.
#' @param wave_axisDigits Numeric giving the number of digits to print for
#' values along the y-axis of the waveform. Default is `3`. If `0` is passed,
#' the y-axis is suppressed. Note that this only applies when
#' `min_max_only = TRUE`, as otherwise the look of the y-axis is determined
#' entirely using `grDevices::axisTicks()`.
#' @param wave_color String giving the name of the color to be used for plotting
#' the waveform. Default is `'black'`. Alternatively a vector of strings, if
#' different colors should be used for different channels.
#' @param wave_lineWidth Number giving the line width to use for plotting
#' the waveform. Default is `1`.
#' @param wave_highlight Named list giving parameters for differential
#' highlighting of the waveform based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the argument
#' `color` (string, see `wave_color`), and `background`
#' (a string specifying a background color).
#' @param tg_file Path of file to be used for plotting TextGrid. Default is
#' `NULL`, in which case the function searches for a TextGrid sharing the same
#' base name as `sound` with the `.TextGrid` extension.
#' @param tg_obj A TextGrid object returned by the [make_TextGrid()] function.
#' @param tg_tiers Vector of numbers or strings giving either numeric identifiers
#' of TextGrid tiers to plot or the names of TextGrid tiers to plot. Also
#' understands `'all'`, which plots all tiers and is the default.
#' @param tg_focusTier For which tier(s) should lines be shown on all
#' acoustic plots giving the locations of boundaries? Vector of number or
#' strings giving either numeric identifiers
#' of TextGrid tiers or the names of TextGrid tiers to plot. Default is
#' `tg_tiers[1]`, i.e. the first tier given in the `tg_tiers` argument.
#' Additionally accepts the string `none`,
#' in which case no lines are shown on acoustic plots, and `all`, in
#' which case lines from all tiers are shown on acoustic plots.
#' @param tg_focusTierColor String or vector of strings giving the color(s) to
#' use for plotting focus tier lines. If multiple tiers are focused, a vector
#' of the same length can be passed, and the nth tier will be plotted in the
#' nth color. Default is `'black'`.
#' @param tg_focusTierLineType String or vector of strings giving the line
#' type(s) for plotting focus tier lines. If multiple tiers are focused, a
#' vector of the same length can be passed, and the nth tier will be plotted in
#' the nth line type. Default is `'dotted'`.
#' @param tg_alignment String giving the desired alignment of text in the
#' TextGrids. Default is `central`; other options are `left` and `right`.
#' Alternatively, a vector of strings if different alignments are needed.
#' @param tg_edgeLabels String specifying how to handle TextGrid labels in
#' interval tiers that fall partially before `start` or partially after `end`.
#' Default is `'keep'`, where labels are kept at the center of the interval.
#' Other options are `'center'`, where labels are recentered to the visible
#' part of the interval, or `'discard'`, where such labels are ignored.
#' @param tg_specialChar Logical; should Praat typesetting for special font types
#' such as italic, bold, and small caps be converted into corresponding
#' R-readable special font types. Default is `FALSE`, since special characters
#' can create unfortunate text alignment artefacts.
#' See https://www.fon.hum.uva.nl/praat/manual/Text_styles.html.
#' @param tg_tierNames Logical; should TextGrid tier names be printed along the
#' y-axis? Default is `TRUE`.
#' @param tg_color String or vector of strings giving the name of the color(s)
#' to be used for the text in TextGrids. Default is `'black'`. If a vector is
#' provided, different colors are used for different tiers.
#' @param tg_highlight Named list giving parameters for differential
#' highlighting of TextGrid intervals. This list
#' should contain information about which intervals to highlight, using the
#' `tier` and `label`. Further contains the argument
#' `color`, and `background`
#' (a string specifying a background color).
#' @param spec_channel Numeric giving the channel that should be used to
#' generate the spectrogram. Default is `1`. Generating spectrograms from
#' multiple channels is not currently possible with `praatpicture`.
#' @param spec_freqRange Vector of two integers giving the frequency range to be
#' used for plotting spectrograms. Default is `c(0,5000)`.
#' @param spec_windowLength Window length in seconds for generating spectrograms.
#' Default is `0.005`.
#' @param spec_dynamicRange Dynamic range in dB for generating spectrograms. The
#' maximum intensity minus `spec_dynamicRange` will all be printed in white.
#' Default is `50`.
#' @param spec_timeStep How many time steps should be calculated for spectrograms?
#' Default is `1000`.
#' @param spec_windowShape String giving the name of the window shape to be
#' applied to the signal when generating spectrograms. Default is `Gaussian`;
#' other options are `square`, `Hamming`, `Bartlett`, `Hanning`, or `Blackman`.
#' Note that the Gaussian window function provided by the `phonTools` package
#' and used in `praatpicture()` does not have the same properties as the
#' Gaussian window function used for spectral estimation in Praat; plotting
#' a simple sine wave with high dynamic range will produce sidelobes in
#' `praatpicture()` but not in Praat. It's recommended to use Blackman windows
#' instead if you have this problem.
#' @param spec_colors Vector of strings giving the names of colors to be used
#' for plotting the spectrogram; default is `c('white', 'black')`. The first
#' value is used for plotting the lowest visible amplitude, and the last for
#' plotting the highest visible amplitude. Vectors with more than two color
#' names can be used for plotting values in between in different colors.
#' @param spec_axisLabel String giving the name of the label to print along the
#' y-axis when plotting a spectrogram. Default is `Frequency (Hz)`.
#' @param spec_highlight Named list giving parameters for differential
#' highlighting of the spectrogram based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the argument
#' `colors` (vector of strings, see `spec_colors`).
#' @param pitch_timeStep Measurement interval in seconds for tracking pitch.
#' Default is `NULL`, in which case the measurement interval is equal to
#' 0.75 / `pitch_floor`.
#' @param pitch_floor Frequency in Hz; no pitch candidates considered below
#' this frequency. Default is `75`.
#' @param pitch_ceiling Frequency in Hz; no pitch candidates considered above
#' this frequency. Default is `600`.
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
#' @param pitch_semitonesRe Frequency in Hz giving the reference level for
#' converting pitch frequency to semitones. Default is `100`.
#' @param pitch_color String giving the name of the color to be used for
#' plotting pitch. Default is `'black'`. If `pitch_plotOnSpec=TRUE`, axes will
#' follow the same color scheme. Also if `pitch_plotOnSpec=TRUE`, a vector of
#' two strings can be passed, in which case the second color is used for
#' background highlighting.
#' @param pitch_plotOnSpec Boolean; should pitch be plotted on top of
#' spectrogram? Default is `FALSE`.
#' @param pitch_plotOnWave Boolean; should pitch be plotted on top of
#' waveform? Default is `FALSE`.
#' @param pitch_ssff An object of class `AsspDataObj` containing a pitch track.
#' Default is `NULL`.
#' @param pitch_axisLabel String giving the name of the label to print along the
#' y-axis when printing a pitch track. Default is `NULL`, in which case the
#' axis label will depend on the scale. If `pitch_plotOnSpec=TRUE`, this label
#' will be printed on the right-hand y-axis label.
#' @param pitch_highlight Named list giving parameters for differential
#' highlighting of pitch based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the optional arguments
#' `color` (string or vector of strings, see `pitch_color`),
#' `drawSize` or `speckleSize` (both numeric), and `background`
#' (a string specifying a background color).
#' @param formant_timeStep Measurement interval in seconds for tracking formants.
#' Default is `NULL`, in which case the measurement interval is equal to
#' `formant_windowLength` / 4.
#' @param formant_maxN Integer giving the maximum number of formants to track.
#' Default is `5`.
#' @param formant_windowLength The effective duration of the analysis window
#' used for tracking formants in seconds; the actual duration of the
#' analysis window is twice this value.
#' @param formant_dynamicRange Dynamic range in dB for producing formant plots.
#' When a formant plot of `formant_plotType='speckle'` is drawn, no formants are
#' shown in frames with intensity level `formant_dynamicRange` below the maximum
#' intensity. Default is `30`. If set to `0`, all formants are shown.
#' @param formant_freqRange Vector of two integers giving the frequency range to
#' be used for producing formant plots. Default is `c(0,5500)`.
#' @param formant_number Number of formants to plot. Default is `NULL`, in
#' which case all available formants are plotted.
#' @param formant_plotType String giving the type of formant plot to produce;
#' default is `speckle` (a point plot), the only other option is `draw` (a line
#' plot). Alternatively a vector `c('draw','speckle')` can be passed, in which
#' case both are used.
#' @param formant_color String or vector of strings giving the name(s) of
#' colors to be used for plotting formants. If one color is provided, all
#' formants will be plotted in this color. If multiple colors are provided,
#' different formants will be shown in different colors. Default is `'black'`.
#' If `formant_plotOnSpec=TRUE` and the length of this vector twice the number
#' of formants plotted, the first
#' half of strings will be used for the formants' primary colors and the second
#' half will be used for background highlighting. If the length of this vector
#' is one more than the number of formants plotted, the last string will
#' be used for background highlighting.
#' @param formant_plotOnSpec Boolean; should formants be plotted on top of
#' spectrogram? Default is `FALSE`.
#' @param formant_ssff An object of class `AsspDataObj` containing formant tracks.
#' Default is `NULL`.
#' @param formant_axisLabel String giving the name of the label to print along the
#' y-axis when plotting formants. Default is `Frequency (Hz)`.
#' @param formant_highlight Named list giving parameters for differential
#' highlighting of formants based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the optional arguments
#' `color` (string or vector of strings, see `formant_color`),
#' `drawSize` or `speckleSize` (both numeric), and `background`
#' (a string specifying a background color).
#' @param intensity_timeStep Measurement interval in seconds for tracking
#' intensity. Default is `NULL`, in which case the measurement interval is
#' equal to 0.8 * `intensity_minPitch`.
#' @param intensity_minPitch Lowest pitch in Hz used when calculating
#' intensity; default is `100`
#' @param intensity_range Vector of two integers giving the intensity range to be
#' used for producing intensity plots. Default is `NULL`, in which case the
#' range is simply the minimum and maximum levels in the curve.
#' @param intensity_color String giving the name of the color to be used for
#' plotting intensity. Default is `'black'`. If `intensity_plotOnSpec=TRUE`,
#' axes will follow the same color scheme. Also if `intensity_plotOnSpec=TRUE`,
#' a vector of two strings can be passed, in which case the second color is used
#' for background highlighting.
#' @param intensity_plotOnSpec Boolean; should intensity be plotted on top of
#' spectrogram? Default is `FALSE`.
#' @param intensity_plotOnWave Boolean; should intensity be plotted on top of
#' waveform? Default is `FALSE`.
#' @param intensity_ssff An object of class `AsspDataObj` containing intensity
#' tracks. Default is `NULL`.
#' @param intensity_axisLabel String giving the name of the label to print along
#' the y-axis when plotting intensity. Default is `Intensity (dB)`.
#' If `intensity_plotOnSpec=TRUE`, this label
#' will be printed on the right-hand y-axis label.
#' @param intensity_highlight Named list giving parameters for differential
#' highlighting of the intensity contour based on the time domain. This list
#' should contain information about which parts of the plot to highlight, either
#' done with the `start` and `end` arguments which must be numbers or numeric
#' vectors, or using the `tier` and `label` arguments to highlight based on
#' information in a plotted TextGrid. Further contains the optional arguments
#' `color` (string or vector of strings, see `intensity_color`) and
#' `drawSize` (integer), and `background`
#' (a string specifying a background color).
#' @param time_axisLabel String giving the name of the label to print along
#' the x-axis. Default is `NULL`, in which case `Time (s)` is printed if
#' `tUnit = 's'` and `Time (ms)` is printed if `tUnit = 'ms'`.
#' @param highlight Named list giving parameters for differential highlighting
#' of part of the plot based on the time domain. This list should contain
#' information about which parts of the plot to highlight, either done with the
#' `start` and `end` arguments which must be numbers or numeric vectors, or
#' using the `tier` and `label` arguments to highlight based on information in
#' a plotted TextGrid. Further contains the optional arguments `color`
#' (a string), `drawSize` and `speckleSize` (both numeric), and `background`
#' (a string specifying a background color). This argument is
#' used to highlight all plot components, use the `*_highlight` arguments for
#' highlighting individuals plot components.
#' @param draw_lines Use for drawing straight lines on plot components. Takes
#' an argument of type `list` which should contain a) a string giving the plot
#' component to draw straight lines on, and b) arguments to pass on to
#' [graphics::abline]. Should have a named argument `h` for horizontal lines,
#' or `v` for vertical lines, or `a`,`b` for the intercept and slope of the
#' line otherwise. Alternatively a nested list can be passed if more (sets of)
#' lines should be drawn. If multiple audio
#' channels are plotted and lines should be added to one of these,
#' use the channel identifier instead of a string giving the frame to draw on.
#' The default value is `list('formant', h=seq(0,10000,by=1000), lty='dotted')`.
#' According to Praat defaults, this means that if formants are plotted in a
#' separate frame, horizontal dotted lines (`lty`) are shown at 1000 Hz
#' intervals. To override this behavior, simply pass `draw_lines=NULL`.
#' @param draw_rectangle Use for drawing rectangles on plot components. A
#' vector containing a) a string giving the plot component to draw a rectangle
#' on, and b) arguments to pass on to [graphics::rect]. Alternatively a list
#' of such vectors, if more rectangles should be drawn. If multiple audio
#' channels are plotted and a rectangle should be added to one of these,
#' use the channel identifier instead of a string giving the frame to draw on.
#' @param draw_arrow Use for drawing arrows on plot components. A vector
#' containing a) a string giving the plot component to draw an arrow on, and
#' b) arguments to pass on to [graphics::arrows]. Alternatively a list of
#' such vectors, if more arrows should be drawn. If multiple audio
#' channels are plotted and an arrow should be added to one of these,
#' use the channel identifier instead of a string giving the frame to draw on.
#' @param annotate Use for annotating plot components. A vector containing
#' a) a string giving the plot component to annotate, and
#' b) arguments to pass on to [graphics::text]. Alternatively a list of
#' such vectors, if more annotations should be made. If multiple audio
#' channels are plotted and annotations should be added to one of these,
#' use the channel identifier instead of a string giving the frame to draw on.
#' @param gender String indicating the gender of the speaker; default is
#' `u` for unknown, other legal values are `m` and `f`. Used to tweak pitch
#' and formant tracking parameters.
#' @param ... Further global plotting arguments passed on to `par()`.
#'
#' @details When available, pitch, formant, and intensity tracks are loaded
#' from Praat files with the same base name as `sound`; i.e., if your sound
#' file is called `1.wav` and there is a Praat file called `1.Formant` in the
#' same directory, this file is used for plotting formants. Pitch files should
#' have either the `PitchTier` or `Pitch` extension, and intensity files should
#' have the `IntensityTier` extension.
#'
#' If no such files are available, the signal processing tools in the `wrassp`
#' package are used; pitch is tracked with the function [wrassp::ksvF0],
#' formants are tracked with [wrassp::forest], and intensity is tracked with
#' [wrassp::rmsana]. Parameters are set to mimic Praat as closely as possible,
#' e.g. using a Gaussian-like window shape `KAISER2_0`, but results will differ
#' from Praat simply because the tracking algorithms differ; as far as I know,
#' the Burg algorithm used by Praat for tracking formants isn't implemented in R,
#' nor is the autocorrelation method for tracking pitch.
#'
#' Spectrograms are generated with the function [phonTools::spectrogram]. The
#' code portion that actually adds the spectrogram to a plot is based on
#' [phonTools::plot.spectrogram] but rewritten to use a bitmap raster for
#' rendering the image if the graphics device allows for it, which
#' significantly speeds up rendering the spectrogram.
#'
#' @return No return value, produces a figure.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' praatpicture(soundFile)
praatpicture <- function(sound, start=0, end=0, tfrom0=TRUE, tUnit='s',
                         frames=c('sound', 'spectrogram', 'TextGrid'),
                         proportion=c(30,50,20), mainTitle='',
                         mainTitleAlignment = 0,
                         start_end_only=TRUE, min_max_only=TRUE,
                         drawSize = 1, speckleSize = 1,
                         wave_channels='all', wave_channelNames=FALSE,
                         wave_energyRange=NULL, wave_axisDigits=3,
                         wave_color='black', wave_lineWidth=1,
                         wave_highlight=NULL,
                         tg_obj=NULL, tg_file=NULL, tg_tiers='all',
                         tg_focusTier=tg_tiers[1], tg_focusTierColor='black',
                         tg_focusTierLineType='dotted', tg_tierNames=TRUE,
                         tg_alignment='central', tg_edgeLabels='keep',
                         tg_specialChar=FALSE,
                         tg_color='black', tg_highlight=NULL,
                         spec_channel=NULL, spec_freqRange=c(0,5000),
                         spec_windowLength=0.005, spec_dynamicRange=50,
                         spec_timeStep=1000, spec_windowShape='Gaussian',
                         spec_colors=c('white', 'black'),
                         spec_axisLabel='Frequency (Hz)',
                         spec_highlight=NULL,
                         pitch_timeStep=NULL, pitch_floor=75, pitch_ceiling=600,
                         pitch_plotType='draw', pitch_scale='hz',
                         pitch_freqRange=NULL, pitch_semitonesRe=100,
                         pitch_color='black', pitch_plotOnSpec=FALSE,
                         pitch_plotOnWave=FALSE, pitch_ssff=NULL,
                         pitch_axisLabel=NULL, pitch_highlight=NULL,
                         formant_timeStep=NULL, formant_maxN=5,
                         formant_windowLength=0.025, formant_dynamicRange=30,
                         formant_freqRange=c(50, 5500),
                         formant_number=NULL,
                         formant_plotType='speckle', formant_color='black',
                         formant_plotOnSpec=FALSE,
                         formant_ssff=NULL, formant_axisLabel='Frequency (Hz)',
                         formant_highlight=NULL,
                         intensity_timeStep=NULL, intensity_minPitch=100,
                         intensity_range=NULL, intensity_color='black',
                         intensity_plotOnSpec=FALSE, intensity_plotOnWave=FALSE,
                         intensity_ssff=NULL,
                         intensity_axisLabel='Intensity (dB)',
                         intensity_highlight=NULL,
                         time_axisLabel=NULL,
                         highlight=NULL,
                         draw_lines=list('formant', h=seq(0,10000,by=1000),
                                         lty='dotted'),
                         draw_rectangle=NULL, draw_arrow=NULL, annotate=NULL,
                         gender='u', ...) {

  p <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(p))

  legal_frames <- c('sound', 'TextGrid', 'spectrogram', 'pitch', 'formant',
                    'intensity')
  if (any(!frames %in% legal_frames)) {
    stop('Currently available frames are sound, TextGrid, spectrogram, formant, ',
         'pitch, and intensity')
  }
  if (pitch_scale == 'log') pitch_scale <- 'logarithmic'
  legal_scales <- c('hz', 'logarithmic', 'semitones', 'erb', 'mel')
  if (!pitch_scale %in% legal_scales) {
    stop('Possible pitch scales are hz, logarithmic, semitones, erb, and mel')
  }
  if (!any(pitch_plotType %in% c('draw', 'speckle'))) {
    stop('Please select either draw or speckle as the pitch plot type')
  }
  if (!tUnit %in% c('s', 'ms')) {
    stop('Please select either s or ms as the time unit')
  }
  if (!tg_edgeLabels %in% c('keep', 'center', 'discard')) {
    stop('tg_edgeLabels should be either keep, center, or discard')
  }

  if (is.null(time_axisLabel)) {
    time_axisLabel <- ifelse(tUnit == 's', 'Time (s)', 'Time (ms)')
  }
  if (is.null(formant_number)) {
    formant_number <- formant_maxN
  }

  proportion <- round((proportion / sum(proportion)) * 100)

  nframe <- length(frames)
  if (nframe != 3 & length(proportion) != nframe) {
    proportion <- rep(round(100/nframe), nframe)
  }
  if (nframe != length(proportion)) stop(paste('The proportion argument must',
                                               'be the same length as the',
                                               'frames argument'))
  if (nframe > length(min_max_only)) min_max_only <- rep(min_max_only, nframe)

  if (is.character(draw_lines[[1]])) draw_lines <- list(draw_lines)
  line_comp <- sapply(draw_lines, '[[', 1)

  if (!is.list(draw_rectangle)) draw_rectangle <- list(draw_rectangle)
  rect_comp <- sapply(draw_rectangle, '[[', 1)

  if (!is.list(draw_arrow)) draw_arrow <- list(draw_arrow)
  arr_comp <- sapply(draw_arrow, '[[', 1)

  if (!is.list(annotate)) annotate <- list(annotate)
  annot_comp <- sapply(annotate, '[[', 1)

  if (end == 0) {
    tend <- Inf
  } else {
    tend <- end
  }
  snd <- tuneR::readWave(sound, from=start, to=tend, units='seconds', toWaveMC=T)

  sr <- snd@samp.rate
  bit <- snd@bit
  nsamp <- snd@dim[1]
  nchan <- snd@dim[2]
  if (any(wave_channels == 'all')) {
    wave_channels <- 1:nchan
  }

  if (any(is.null(spec_channel))) {
    if (any(wave_channels == 'all')) {
      spec_channel <- 1
    } else {
      spec_channel <- wave_channels[1]
    }
  }

  sig <- snd@.Data[,wave_channels]
  if (any(class(sig) %in% c('integer', 'numeric'))) sig <- as.matrix(sig)
  nchan <- dim(sig)[2]

  if (is.logical(wave_channelNames) & isTRUE(wave_channelNames)) {
    if (length(colnames(sig)) > 0) {
      cn <- colnames(sig)
    } else if (nchan == 2) {
      cn <- c('left', 'right')
    } else {
      cn <- paste0('C', 1:nchan)
    }
  } else if (is.character(wave_channelNames)) {
    cn <- wave_channelNames
    wave_channelNames <- TRUE
  } else {
    cn <- NULL
  }

  if (start == 0) {
    tstart <- 0 + (1/sr)
  } else {
    tstart <- start
  }
  tseq <- seq(tstart, tstart+nsamp/sr, by=1/sr)

  spl <- unlist(strsplit(sound, '[.]'))
  fn <- paste(spl[1:(length(spl)-1)], collapse='.')

  if ('TextGrid' %in% frames) {
    if (!is.null(tg_file)) {
      tgfn <- tg_file
    } else {
      tgfn <- paste0(fn, '.TextGrid')
    }
    if (!is.null(tg_obj)) {
      tg <- tg_obj
    } else if (file.exists(tgfn)) {
      tg <- rPraat::tg.read(tgfn, encoding='auto')
    } else {
      stop(paste('There is no TextGrid file available for this sound file.',
                 'Either create the plot without a TextGrid by using the',
                 'frames argument, create a TextGrid object using the',
                 'make_TextGrid() function and pass it to the tg_obj argument,',
                 'or create a TextGrid using Praat',
                 'or the functionality available in the rPraat package.'))
    }

    tgind <- which(frames == 'TextGrid')
    if (any(tg_tiers == 'all')) tg_tiers <- 1:length(tg)
    if (length(tg_tiers) > 1) {
      ntiers <- length(tg_tiers)
      tgprop <- proportion[tgind]
      proportion[tgind] <- round(tgprop / ntiers)
      if (tgind == nframe) {
        proportion[(nframe+1):(nframe+ntiers-1)] <-
          round(tgprop / ntiers)
      } else {
        proportion[(tgind+ntiers):(nframe+ntiers-1)] <-
          proportion[(tgind+1):nframe]
        proportion[(tgind+1):(tgind+ntiers-1)] <-
          round(tgprop / ntiers)
      }
    }

    if (any(tg_focusTier == 'all')) tg_focusTier <- tg_tiers
    if (length(tg_focusTierColor) == 1) {
      tg_focusTierColor <- rep(tg_focusTierColor, length(tg_focusTier))
    }
    if (length(tg_focusTierLineType) == 1) {
      tg_focusTierLineType <- rep(tg_focusTierLineType, length(tg_focusTier))
    }
    if (any(tg_focusTier != 'none')) {
      tgbool <- TRUE
      focus_linevec <- list()
      j <- 1
      for (i in tg_focusTier) {
        if (tfrom0) {
          if (tg[[i]]$type == 'point') focus_linevec[[j]] <- tg[[i]]$t - start
          if (tg[[i]]$type == 'interval') focus_linevec[[j]] <-
              tg[[i]]$t1[-1] - start
        } else {
          if (tg[[i]]$type == 'point') focus_linevec[[j]] <- tg[[i]]$t
          if (tg[[i]]$type == 'interval') focus_linevec[[j]] <- tg[[i]]$t1[-1]
        }
        j <- j + 1
      }
    } else {
      tgbool <- FALSE
      focus_linevec <- NULL
    }
  } else {
    tgbool <- FALSE
    focus_linevec <- NULL
  }

  findStartEndVals <- function(listobj) {
    if ('tier' %in% names(listobj)) {
      tierName <- listobj$tier
      matches <- which(grepl(listobj$label, tg[[tierName]]$label, perl=T))
      if (length(matches) == 0) stop(paste('There is no match for the',
                                            'highlighting conditions'))
      listobj$start <- tg[[tierName]]$t1[matches]
      listobj$end <- tg[[tierName]]$t2[matches]
      if (end != 0) {
        discard <- which(listobj$end < start | listobj$start > end)
      } else {
        discard <- which(listobj$end < start)
      }
      if (length(discard) > 0) {
        listobj$start <- listobj$start[-discard]
        listobj$end <- listobj$end[-discard]
      }
      if (start > 0 & tfrom0) {
        listobj$start <- listobj$start - start
        listobj$end <- listobj$end - start
      }
    }
    return(listobj)
  }

  if (!is.null(highlight)) {
    if (is.null(wave_highlight)) wave_highlight <- highlight
    if (is.null(pitch_highlight)) pitch_highlight <- highlight
    if (is.null(formant_highlight)) formant_highlight <- highlight
    if (is.null(intensity_highlight)) intensity_highlight <- highlight
    if (is.null(spec_highlight)) {
      spec_highlight <- highlight
      spec_highlight$colors <- spec_colors
      if ('color' %in% names(highlight)) spec_highlight$colors <-
          c(spec_highlight$colors[1], highlight$color)
      if ('background' %in% names(highlight)) spec_highlight$colors <-
          c(highlight$background, spec_highlight$colors[2])
    }
    if (is.null(tg_highlight)) tg_highlight <- highlight
  }

  wave_highlight <- findStartEndVals(wave_highlight)
  pitch_highlight <- findStartEndVals(pitch_highlight)
  formant_highlight <- findStartEndVals(formant_highlight)
  intensity_highlight <- findStartEndVals(intensity_highlight)
  spec_highlight <- findStartEndVals(spec_highlight)

  if ('pitch' %in% frames | pitch_plotOnSpec | pitch_plotOnWave) {
    if (is.null(pitch_freqRange)) {
      if (pitch_scale == 'erb') {
        pitch_freqRange <- c(0,10)
      } else if (pitch_scale == 'semitones') {
        pitch_freqRange <- c(-12,30)
      } else {
        pitch_freqRange <- c(50,500)
      }
    }
    if (is.null(pitch_axisLabel)) {
      if (pitch_scale == 'hz') pitch_axisLabel <- 'Frequency (Hz)'
      if (pitch_scale == 'logarithmic') pitch_axisLabel <- 'Frequency (log Hz)'
      if (pitch_scale == 'mel') pitch_axisLabel <- 'Frequency (mel)'
      if (pitch_scale == 'erb') pitch_axisLabel <- 'Frequency (ERB)'
      if (pitch_scale == 'semitones') pitch_axisLabel <-
          paste0('Frequency (semitones re ', pitch_semitonesRe, ')')
    }
    if (file.exists(paste0(fn, '.PitchTier'))) {
      ptfn <- paste0(fn, '.PitchTier')
      pt <- rPraat::pt.read(ptfn)
    } else if (file.exists(paste0(fn, '.Pitch'))) {
      ptfn <- paste0(fn, '.Pitch')
      pt <- rPraat::pitch.read(ptfn)
      f <- c()
      for (i in 1:length(pt$frame)) f[i] <- pt$frame[[i]]$frequency[1]
      t <- pt$t
      f[which(f==0)] <- NA
      pt <- list(t = t, f = f)
    } else {
      if (!is.null(pitch_ssff)) {
        wpt <- pitch_ssff
      } else {
        if (is.null(pitch_timeStep)) {
          pitch_timeStep <- (0.75 / pitch_floor) * 1000
        } else {
          pitch_timeStep <- pitch_timeStep * 1000
        }
        wpt <- wrassp::ksvF0(paste0(fn, '.wav'), toFile=F,
                             beginTime=start, endTime=end,
                             windowShift=pitch_timeStep,
                             maxF=pitch_ceiling, minF=pitch_floor,
                             gender=gender)
      }
      f <- wpt[[1]][,1]
      a <- attributes(wpt)
      t <- seq(a$startRecord/a$sampleRate, a$endRecord/a$sampleRate,
               by=1/a$sampleRate)
      f[which(f==0)] <- NA
      pt <- list(t = t, f = f)
    }
    if (pitch_scale == 'semitones') pt <-
        rPraat::pt.Hz2ST(pt, ref=pitch_semitonesRe)
    if (pitch_scale == 'mel') pt$f <- emuR::mel(pt$f)
    if (pitch_scale == 'erb') pt$f <- soundgen::HzToERB(pt$f)
  }

  if ('formant' %in% frames | formant_plotOnSpec) {
    if (file.exists(paste0(fn, '.Formant'))) {
      fmfn <- paste0(fn, '.Formant')
      fm <- rPraat::formant.read(fmfn)
      fm <- rPraat::formant.toArray(fm)
      fm$conv2db <- TRUE
      mnf <- dim(fm$frequencyArray)[1]
      if (formant_number < mnf) {
        fm$frequencyArray <- fm$frequencyArray[1:formant_number,]
        fm$maxnFormants <- formant_number
      }
      if (formant_number > mnf) stop(paste('formant_number is greater than the',
                                           'number of formants available'))
    } else {
      if (!is.null(formant_ssff)) {
        wfm <- formant_ssff
        a <- attributes(wfm)
        it_time <- (1 / a$sampleRate) * 1000
        wit <- wrassp::rmsana(paste0(fn, '.wav'), toFile=F,
                              windowShift=it_time,
                              windowSize=(3.2/intensity_minPitch)*1000,
                              window='KAISER2_0')
      } else {
        if (is.null(formant_timeStep)) {
          formant_timeStep <- (0.25 * formant_windowLength) * 1000
        } else {
          formant_timeStep <- formant_timeStep * 1000
        }
        wfm <- wrassp::forest(paste0(fn, '.wav'), toFile=F,
                              beginTime=start, endTime=end,
                              windowShift = formant_timeStep,
                              windowSize = formant_windowLength*2000,
                              numFormants = formant_maxN, estimate=TRUE,
                              window='KAISER2_0', gender=gender)
        wit <- wrassp::rmsana(paste0(fn, '.wav'), toFile=F,
                              beginTime=start, endTime=end,
                              windowShift=formant_timeStep,
                              windowSize=(3.2/intensity_minPitch)*1000,
                              window='KAISER2_0')
      }
      wfm$fm[which(wfm$fm==0)] <- NA
      a <- attributes(wfm)
      t <- seq(a$startTime, a$endRecord/a$sampleRate, by=1/a$sampleRate)
      fArray <- t(wfm[[1]])
      mnf <- dim(fArray)[1]
      if (formant_number < mnf) fArray <- fArray[1:formant_number,]
      if (formant_number > mnf) stop(paste('formant_number is greater than the',
                                           'number of formants available'))
      fm <- list(t = t, frequencyArray = fArray, maxnFormants = formant_number,
                 intensityVector = wit$rms, conv2db = FALSE)
    }
  } else {
    fm <- NULL
  }

  if ('intensity' %in% frames | intensity_plotOnSpec | intensity_plotOnWave) {
    if (file.exists(paste0(fn, '.IntensityTier'))) {
      itfn <- paste0(fn, '.IntensityTier')
      it <- rPraat::it.read(itfn)
    } else {
      if (!is.null(intensity_ssff)) {
        wit <- intensity_ssff
      } else {
        if (is.null(intensity_timeStep)) {
          intensity_timeStep <- (0.8 / intensity_minPitch) * 1000
        } else {
          intensity_timeStep <- intensity_timeStep * 1000
        }
        wit <- wrassp::rmsana(paste0(fn, '.wav'), toFile=F,
                              beginTime=start, endTime=end,
                              windowShift=intensity_timeStep,
                              windowSize=(3.2/intensity_minPitch)*1000,
                              window='KAISER2_0')
      }
      db <- wit[[1]][,1]
      a <- attributes(wit)
      t <- seq(a$startTime, a$endRecord/a$sampleRate, by=1/a$sampleRate)
      it <- list(t = t, i = db)
    }
    if (is.null(intensity_range)) intensity_range <-
        c(min(it$i), max(it$i))
  }

  if (tfrom0) {
    t <- tseq - start
  } else {
    t <- tseq
  }

  if (nchan > 1) {
    nf <- nframe + length(tg_tiers) - 1
    wavind <- which(frames=='sound')
    if ('TextGrid' %in% frames) {
      if (length(tg_tiers) > 1 & wavind > tgind) wavind <-
          wavind + length(tg_tiers) - 1
    }
    wavprop <- proportion[wavind]
    proportion[wavind] <- round(wavprop / nchan)
    if (wavind == nf) {
      proportion[(nf+1):(nf+nchan-1)] <-
        round(wavprop / nchan)
    } else {
      proportion[(wavind+nchan):(nf+nchan-1)] <-
        proportion[(wavind+1):nf]
      proportion[(wavind+1):(wavind+nchan-1)] <-
        round(wavprop / nchan)
    }
  }

  plot_matrix <- rep(1:length(proportion), proportion)
  graphics::par(mar=c(0,2,0,2), oma=c(5,5,5,5), ...)
  graphics::layout(matrix(plot_matrix, nrow=length(plot_matrix)))

  for (i in 1:nframe) {
    if (frames[i] == 'sound') {
      ind <- which(frames == 'sound')
      waveplot(sig, bit, t, start, tfrom0, nchan, wave_energyRange, wave_color,
               pitch_plotOnWave, pt, pitch_plotType, pitch_scale,
               pitch_freqRange, pitch_axisLabel, pitch_color, pitch_highlight,
               intensity_plotOnWave, it,
               intensity_range, intensity_axisLabel, intensity_color,
               intensity_highlight,
               tgbool, focus_linevec,
               tg_focusTierColor, tg_focusTierLineType, ind,
               line_comp, rect_comp, arr_comp, annot_comp,
               draw_lines, draw_rectangle, draw_arrow, annotate,
               wave_channelNames, wave_axisDigits, wave_lineWidth,
               cn, min_max_only, wave_highlight, drawSize, speckleSize)
    } else if (frames[i] == 'spectrogram') {
      ind <- which(frames == 'spectrogram')
      specplot(sig[,which(wave_channels==spec_channel)], sr, t, start,
               max(tseq)-start, tfrom0,
               spec_freqRange, spec_windowLength, spec_dynamicRange,
               spec_timeStep, spec_windowShape, spec_colors,
               pitch_plotOnSpec, pt, pitch_plotType, pitch_scale,
               pitch_freqRange, pitch_axisLabel, pitch_color, pitch_highlight,
               formant_plotOnSpec, fm, formant_plotType, formant_dynamicRange,
               formant_color, formant_highlight, intensity_plotOnSpec, it,
               intensity_range, intensity_axisLabel, intensity_color,
               intensity_highlight,
               tgbool, focus_linevec, tg_focusTierColor,
               tg_focusTierLineType, ind, min_max_only,
               spec_highlight, spec_axisLabel, drawSize, speckleSize)
      if ('spectrogram' %in% rect_comp) draw_rectangle('spectrogram',
                                                       draw_rectangle)
      if ('spectrogram' %in% arr_comp) draw_arrow('spectrogram', draw_arrow)
      if ('spectrogram' %in% annot_comp) make_annot('spectrogram', annotate)
      if ('spectrogram' %in% line_comp) draw_lines('spectrogram', draw_lines)
      graphics::box()
    } else if (frames[i] == 'TextGrid') {
      ind <- which(frames == 'TextGrid')
      tgplot(tg, t, sr, start, end, tg_tiers, tfrom0, tg_tierNames,
             tg_alignment, tg_edgeLabels, tg_specialChar,
             tg_color, tg_highlight)
    } else if (frames[i] == 'pitch') {
      ind <- which(frames == 'pitch')
      pitchplot(pt, start, max(tseq)-start, tfrom0, tgbool, focus_linevec,
                tg_focusTierColor, tg_focusTierLineType,
                pitch_plotType, pitch_scale, pitch_freqRange,
                pitch_semitonesRe, pitch_color, ind,
                min_max_only, pitch_highlight, pitch_axisLabel,
                drawSize, speckleSize)
      if ('pitch' %in% rect_comp) draw_rectangle('pitch', draw_rectangle)
      if ('pitch' %in% arr_comp) draw_arrow('pitch', draw_arrow)
      if ('pitch' %in% annot_comp) make_annot('pitch', annotate)
      if ('pitch' %in% line_comp) draw_lines('pitch', draw_lines)
      graphics::box()
    } else if (frames[i] == 'formant') {
      ind <- which(frames == 'formant')
      formantplot(fm, start, max(tseq)-start, tfrom0, tgbool, focus_linevec,
                  tg_focusTierColor, tg_focusTierLineType,
                  formant_dynamicRange, formant_freqRange, formant_plotType,
                  formant_color, ind, min_max_only, formant_highlight,
                  formant_axisLabel, drawSize, speckleSize)
      if ('formant' %in% rect_comp) draw_rectangle('formant', draw_rectangle)
      if ('formant' %in% arr_comp) draw_arrow('formant', draw_arrow)
      if ('formant' %in% annot_comp) make_annot('formant', annotate)
      if ('formant' %in% line_comp) draw_lines('formant', draw_lines)
      graphics::box()
    } else if (frames[i] == 'intensity') {
      ind <- which(frames == 'intensity')
      intensityplot(it, start, max(tseq)-start, tfrom0, tgbool, focus_linevec,
                    tg_focusTierColor, tg_focusTierLineType,
                    intensity_range, intensity_color, ind,
                    min_max_only, intensity_highlight,
                    intensity_axisLabel, drawSize)
      if ('intensity' %in% rect_comp) draw_rectangle('intensity',
                                                     draw_rectangle)
      if ('intensity' %in% arr_comp) draw_arrow('intensity', draw_arrow)
      if ('intensity' %in% annot_comp) make_annot('intensity', annotate)
      if ('intensity' %in% line_comp) draw_lines('intensity', draw_lines)
      graphics::box()
    }
  }

  if (!start_end_only) {
    xtix <- grDevices::axisTicks(c(round(min(t), 3),
                                   round(max(t), 3)), log=F)
    if (tUnit == 's') {
      xtixLab <- xtix
    } else {
      xtixLab <- xtix*1000
    }
    graphics::axis(1, at=xtix, labels=xtixLab, ...)
  } else {
    xtix <- c(round(min(t), 3), round(max(t), 3), 0)
    if (tUnit == 's') {
      xtixLab <- xtix
    } else {
      xtixLab <- xtix*1000
    }
    graphics::axis(1, at=xtix, labels=xtixLab, ...)
  }

  graphics::mtext(time_axisLabel, side=1, line=3, outer=T, cex=0.8)
  graphics::mtext(mainTitle, side=3, line=2, adj=mainTitleAlignment, outer=T)

  graphics::par(p)
}

