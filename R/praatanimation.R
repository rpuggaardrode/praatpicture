#' Make animations from Praat Picture-style plots of acoustic data
#'
#' Animate some aspect of a Praat Picture-style plot
#' of acoustic data, potentially aligned with transcriptions.
#'
#' @param sound String giving the file name of a sound file with the .wav
#' extension.
#' @param width Number giving the desired width of the resulting animation in
#' pixels; default is `1080`.
#' @param height Number giving the desired height of the resulting animation in
#' pixels; default is `720`.
#' @param frameRate Number giving the desired frame rate of the resulting
#' animation in Hz; default is `24`, i.e. 24 frames per second.
#' @param n_frames Number giving the desired number of frames of the resulting
#' animation; default is `50`.
#' @param loop Logical; should the animation be looped? Default is `TRUE`.
#' Ignored when `outputType` is `mp4`.
#' @param outputFile String giving the desired file name of the animation.
#' Default is `NULL`, in which case GIF files are named `praatgif.gif` and
#' MP4 files are named `praatvid.mp4`. If you choose a different name, make
#' sure that the file extension matches the selected `outputType`.
#' @param outputFormat String giving the desired file type; default is `gif`,
#' the only other option is `mp4`.S
#' @param useViewer Logical; should the animation be shown in the Viewer pane in
#' RStudio? Default is `TRUE`; if true, the animation is only saved in a
#' temporary directory, but can be downloaded from a browser.
#' @param verbose Logical; should status messages be printed in the console as
#' figures are being generated? Default is `TRUE`.
#' @param pointsize Number; which point size should be used for text in the
#' animation? Default is `25`. See [grDevices::png()] for more details.
#' @param start Start time (in seconds) of desired plotted area. Default is `0`.
#' Alternatively, a vector giving the first and last start time in the
#' animation.
#' @param end End time (in seconds) of desired plotted area. Default is `0`
#' (= the entire file). Alternatively, a vector giving the first and last
#' end time in the animation.
#' @param spec_freqRange Vector of two integers giving the frequency range to be
#' used for plotting spectrograms. Default is `c(0,5000)`. Alternatively, a
#' vector of four integers giving the first and last lowest frequency, followed
#' by the first and last highest frequency in the animation; i.e.,
#' `c(0,0,5000,10000)` will produce an animation where the upper frequency
#' boundary gradually increases from 5000 Hz to 10,000 Hz.
#' @param spec_windowLength Window length in seconds for generating spectrograms.
#' Default is `0.005`. Alternatively, a vector giving the first and last
#' window lengths in the animation.
#' @param spec_dynamicRange Dynamic range in dB for generating spectrograms. The
#' maximum intensity minus `spec_dynamicRange` will all be printed in white.
#' Default is `50`. Alternatively, a vector giving the first and last
#' dynamic range values in the animation.
#' @param spec_timeStep How many time steps should be calculated for spectrograms?
#' Default is `1000`. Alternatively, a vector giving the first and last
#' time step values in the animation.
#' @param pitch_timeStep Measurement interval in seconds for tracking pitch.
#' Default is `NULL`, in which case the measurement interval is equal to
#' 0.75 / `pitch_floor`. Alternatively, a vector giving the first and last
#' measurement intervals in the animation.
#' @param pitch_floor Frequency in Hz; no pitch candidates considered below
#' this frequency. Default is `75`. Alternatively, a vector giving the first and
#' last pitch floors to be used in the animation.
#' @param pitch_ceiling Frequency in Hz; no pitch candidates considered above
#' this frequency. Default is `600`. Alternatively, a vector giving the first and
#' last pitch ceilings to be used in the animation.
#' @param pitch_freqRange Vector of two integers giving the frequency range to be
#' used for producing pitch plots. Default is `c(50,500)`. If the frequency
#' scales `semitones` or `erb` are used, the pitch range is automatically reset
#' to the Praat defaults for these scales (`c(-12,30)` and `c(0,10)`,
#' respectively). Alternatively, a
#' vector of four integers giving the first and last lowest frequency, followed
#' by the first and last highest frequency in the animation
#' (see `spec_freqRange` for usage details).
#' @param pitch_semitonesRe Frequency in Hz giving the reference level for
#' converting pitch frequency to semitones. Default is `100`.
#' Alternatively, a vector giving the first and
#' last semitone reference levels to be used in the animation.
#' @param formant_timeStep Measurement interval in seconds for tracking formants.
#' Default is `NULL`, in which case the measurement interval is equal to
#' `formant_windowLength` / 4. Alternatively, a vector giving the first and
#' last measurement intervals to be used in the animation.
#' @param formant_windowLength The effective duration of the analysis window
#' used for tracking formants in seconds; the actual duration of the
#' analysis window is twice this value. Alternatively, a vector giving the first
#' and last window lengths to be used in the animation.
#' @param formant_dynamicRange Dynamic range in dB for producing formant plots.
#' When a formant plot of `formant_plotType='speckle'` is drawn, no formants are
#' shown in frames with intensity level `formant_dynamicRange` below the maximum
#' intensity. Default is `30`. If set to `0`, all formants are shown.
#' Alternatively, a vector giving the first
#' and last dynamic range levels to be used in the animation.
#' @param formant_freqRange Vector of two integers giving the frequency range to
#' be used for producing formant plots. Default is `c(0,5500)`. Alternatively, a
#' vector of four integers giving the first and last lowest frequency, followed
#' by the first and last highest frequency in the animation
#' (see `spec_freqRange` for usage details).
#' @param intensity_timeStep Measurement interval in seconds for tracking
#' intensity. Default is `NULL`, in which case the measurement interval is
#' equal to 0.8 * `intensity_minPitch`. Alternatively, a vector giving the first
#' and last measurement intervals to be used in the animation.
#' @param intensity_minPitch Lowest pitch in Hz used when calculating
#' intensity; default is `100`. Alternatively, a vector giving the first
#' and last minimum pitch levels to be used in the animation.
#' @param intensity_range Vector of two integers giving the intensity range to be
#' used for producing intensity plots. Default is `NULL`, in which case the
#' range is simply the minimum and maximum levels in the curve. Alternatively, a
#' vector of four integers giving the first and last lowest level, followed
#' by the first and last highest level in the animation
#' (see `spec_freqRange` for usage details).
#' @param ... Further arguments passed to `praatpicture`.
#'
#' @seealso This function is a wrapper for either `gifski::save_gif()`
#' or `av::av_capture_graphics()` used to
#' produce animations based on `praatpicture()`. For more detail on your
#' options, see the `praatpicture()` help file.
#' @return No return value, produces an animated figure.
#' @export
#'
#' @examples
#' \dontrun{
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#'
#' # Show increasing frequency range
#' praatanimation(soundFile, spec_freqRange=c(0,0,4000,12000))
#'
#' # Transition from narrowband to broadband spectrogram
#' praatanimation(soundFile, spec_windowLength=c(0.005,0.03))
#'
#' # Etc.
#' }
praatanimation <- function(sound, width=1080, height=720, frameRate=24,
                           n_frames=50, loop=TRUE, outputFile=NULL,
                           outputFormat='gif', useViewer=TRUE,
                           verbose=TRUE, pointsize=25, start=0, end=0,
                           spec_freqRange=c(0,5000), spec_windowLength=0.005,
                           spec_dynamicRange=50, spec_timeStep=1000,
                           pitch_timeStep=NULL, pitch_floor=50,
                           pitch_ceiling=600, pitch_freqRange=c(50,500),
                           pitch_semitonesRe=100, formant_timeStep=NULL,
                           formant_windowLength=0.025, formant_dynamicRange=30,
                           formant_freqRange=c(50,5500), intensity_timeStep=NULL,
                           intensity_minPitch=100, intensity_range=NULL, ...) {

  if (is.null(outputFile)) {
    if (outputFormat == 'gif') outputFile <- 'praatgif.gif'
    if (outputFormat == 'mp4') outputFile <- 'praatvid.mp4'
  }

  makeseq <- function(vec, n_frames) {
    newseq <- seq(utils::head(vec, 1), utils::tail(vec, 1), length.out=n_frames)
    return(newseq)
  }

  start <- makeseq(start,n_frames); end <- makeseq(end,n_frames)
  spec_windowLength <- makeseq(spec_windowLength,n_frames)
  spec_dynamicRange <- makeseq(spec_dynamicRange,n_frames)
  spec_timeStep <- makeseq(spec_timeStep,n_frames)
  if (!is.null(pitch_timeStep)) pitch_timeStep <-
    makeseq(pitch_timeStep,n_frames)
  pitch_floor <- makeseq(pitch_floor,n_frames)
  pitch_ceiling <- makeseq(pitch_ceiling,n_frames)
  pitch_semitonesRe <- makeseq(pitch_semitonesRe,n_frames)
  if (!is.null(formant_timeStep)) formant_timeStep <-
    makeseq(formant_timeStep,n_frames)
  formant_windowLength <- makeseq(formant_windowLength,n_frames)
  formant_dynamicRange <- makeseq(formant_dynamicRange,n_frames)
  if (!is.null(intensity_timeStep)) intensity_timeStep <-
    makeseq(intensity_timeStep,n_frames)
  intensity_minPitch <- makeseq(intensity_minPitch,n_frames)

  if (length(spec_freqRange) == 4) {
    spec_freqRangeLow <- makeseq(spec_freqRange[1:2],n_frames)
    spec_freqRangeUpp <- makeseq(spec_freqRange[3:4],n_frames)
  } else {
    spec_freqRangeLow <- makeseq(spec_freqRange[1],n_frames)
    spec_freqRangeUpp <- makeseq(spec_freqRange[2],n_frames)
  }
  if (length(pitch_freqRange) == 4) {
    pitch_freqRangeLow <- makeseq(pitch_freqRange[1:2],n_frames)
    pitch_freqRangeUpp <- makeseq(pitch_freqRange[3:4],n_frames)
  } else {
    pitch_freqRangeLow <- makeseq(pitch_freqRange[1],n_frames)
    pitch_freqRangeUpp <- makeseq(pitch_freqRange[2],n_frames)
  }
  if (length(formant_freqRange) == 4) {
    formant_freqRangeLow <- makeseq(formant_freqRange[1:2],n_frames)
    formant_freqRangeUpp <- makeseq(formant_freqRange[3:4],n_frames)
  } else {
    formant_freqRangeLow <- makeseq(formant_freqRange[1],n_frames)
    formant_freqRangeUpp <- makeseq(formant_freqRange[2],n_frames)
  }
  if (!is.null(intensity_range)) {
    if (length(intensity_range == 4)) {
      intensity_rangeLow <- makeseq(intensity_range[1:2],n_frames)
      intensity_rangeUpp <- makeseq(intensity_range[3:4],n_frames)
    } else {
      intensity_rangeLow <- makeseq(intensity_range[1],n_frames)
      intensity_rangeUpp <- makeseq(intensity_range[2],n_frames)
    }
  } else {
    intensity_rangeLow <- NULL; intensity_rangeUpp <- NULL
  }

  if (useViewer) {
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, 'index.html')
    outputFile <- file.path(tempDir, outputFile)
  }

  if (outputFormat == 'gif') {
    gifski::save_gif(
      for (i in 1:n_frames) {
        praatpicture(sound, start=start[i], end=end[i],
                     spec_freqRange=c(spec_freqRangeLow[i], spec_freqRangeUpp[i]),
                     spec_windowLength=spec_windowLength[i],
                     spec_dynamicRange=spec_dynamicRange[i],
                     spec_timeStep=spec_timeStep[i],
                     pitch_timeStep=pitch_timeStep[i],
                     pitch_floor=pitch_floor[i],
                     pitch_ceiling=pitch_ceiling[i],
                     pitch_freqRange=c(
                       pitch_freqRangeLow[i], pitch_freqRangeUpp[i]),
                     pitch_semitonesRe=pitch_semitonesRe[i],
                     formant_timeStep=formant_timeStep[i],
                     formant_windowLength=formant_windowLength[i],
                     formant_dynamicRange=formant_dynamicRange[i],
                     formant_freqRange=c(
                       formant_freqRangeLow[i], formant_freqRangeUpp[i]),
                     intensity_timeStep=intensity_timeStep[i],
                     intensity_minPitch=intensity_minPitch[i],
                     intensity_range=c(
                       intensity_rangeLow[i], intensity_rangeUpp[i]),
                     ...)
        if (verbose) print(paste('Frame', i, 'generated'))
      }, gif_file=outputFile, width=width, height=height,
      delay=1/frameRate, loop=loop, pointsize=pointsize
    )
    if (useViewer) {
      writeLines(text='<img width="600" height="400" src="praatgif.gif">',
                 con=htmlFile)
      rstudioapi::viewer(htmlFile)
    }
  }

  if (outputFormat == 'mp4') {
    av::av_capture_graphics(
      for (i in 1:n_frames) {
        praatpicture(sound, start=start[i], end=end[i],
                     spec_freqRange=c(spec_freqRangeLow[i], spec_freqRangeUpp[i]),
                     spec_windowLength=spec_windowLength[i],
                     spec_dynamicRange=spec_dynamicRange[i],
                     spec_timeStep=spec_timeStep[i],
                     pitch_timeStep=pitch_timeStep[i],
                     pitch_floor=pitch_floor[i],
                     pitch_ceiling=pitch_ceiling[i],
                     pitch_freqRange=c(
                       pitch_freqRangeLow[i], pitch_freqRangeUpp[i]),
                     pitch_semitonesRe=pitch_semitonesRe[i],
                     formant_timeStep=formant_timeStep[i],
                     formant_windowLength=formant_windowLength[i],
                     formant_dynamicRange=formant_dynamicRange[i],
                     formant_freqRange=c(
                       formant_freqRangeLow[i], formant_freqRangeUpp[i]),
                     intensity_timeStep=intensity_timeStep[i],
                     intensity_minPitch=intensity_minPitch[i],
                     intensity_range=c(
                       intensity_rangeLow[i], intensity_rangeUpp[i]),
                     ...)
        if (verbose) print(paste('Frame', i, 'generated'))
      }, output=outputFile, width=width, height=height, framerate=frameRate,
      verbose=verbose, pointsize=pointsize
    )
    if (useViewer) {
      writeLines(text=paste0('<video width="550" height="400" controls>\n',
      '<source src="praatvid.mp4">\n</video>'), con=htmlFile)
      rstudioapi::viewer(htmlFile)
    }
  }
}
