#' Make animations from Praat Picture-style plots of acoustic data
#'
#' Generate GIF-files that animate some aspect of a Praat Picture-style plot
#' of acoustic data, potentially aligned with transcriptions.
#'
#' @param sound String giving the file name of a sound file with the .wav
#' extension.
#' @param width Number giving the desired width of the resulting GIF-file in
#' pixels; default is `800`.
#' @param height Number giving the desired height of the resulting GIF-file in
#' pixels; default is `500`.
#' @param frameRate Number giving the desired frame rate of the resulting
#' GIF-file in Hz; default is `20`, i.e. 20 frames per second.
#' @param frames Number giving the desired number of frames of the resulting
#' GIF-file; default is `100`.
#' @param loop Logical; should the GIF be looped? Default is `TRUE`.
#' @param outputFile String giving the desired name of the GIF-file.
#' Default is `'praatgif.gif'`.
#' @param verbose Logical; should status messages be printed in the console as
#' figures are being generated? Default is `TRUE`.
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
#' @seealso This function is a wrapper for `gifski::save_gif()` used to
#' produce animations based on `praatpicture()`. For more detail on your
#' options, see the `praatpicture()` help file.
#' @export
#'
#' @examples
#' #not now
praatgif <- function(sound, width=800, height=500, frameRate=20, frames=100,
                     loop=TRUE, outputFile='praatgif.gif', verbose=TRUE,
                     start=0, end=0, spec_freqRange=c(0,5000),
                     spec_windowLength=0.0025, spec_dynamicRange=50,
                     spec_timeStep=1000, pitch_timeStep=NULL, pitch_floor=50,
                     pitch_ceiling=600, pitch_freqRange=c(50,500),
                     pitch_semitonesRe=100, formant_timeStep=NULL,
                     formant_windowLength=0.025, formant_dynamicRange=30,
                     formant_freqRange=c(50,5500), intensity_timeStep=NULL,
                     intensity_minPitch=100, intensity_range=NULL, ...) {

  makeseq <- function(vec, frames) {
    newseq <- seq(head(vec, 1), tail(vec, 1), length.out=frames)
    return(newseq)
  }

  start <- makeseq(start, frames); end <- makeseq(end, frames)
  spec_windowLength <- makeseq(spec_windowLength, frames)
  spec_dynamicRange <- makeseq(spec_dynamicRange, frames)
  spec_timeStep <- makeseq(spec_timeStep, frames)
  if (!is.null(pitch_timeStep)) pitch_timeStep <-
    makeseq(pitch_timeStep, frames)
  pitch_floor <- makeseq(pitch_floor, frames)
  pitch_ceiling <- makeseq(pitch_ceiling, frames)
  pitch_semitonesRe <- makeseq(pitch_semitonesRe, frames)
  if (!is.null(formant_timeStep)) formant_timeStep <-
    makeseq(formant_timeStep, frames)
  formant_windowLength <- makeseq(formant_windowLength, frames)
  formant_dynamicRange <- makeseq(formant_dynamicRange, frames)
  if (!is.null(intensity_timeStep)) intensity_timeStep <-
    makeseq(intensity_timeStep, frames)
  intensity_minPitch <- makeseq(intensity_minPitch, frames)

  if (length(spec_freqRange) == 4) {
    spec_freqRangeLow <- makeseq(spec_freqRange[1:2], frames)
    spec_freqRangeUpp <- makeseq(spec_freqRange[3:4], frames)
  } else {
    spec_freqRangeLow <- makeseq(spec_freqRange[1], frames)
    spec_freqRangeUpp <- makeseq(spec_freqRange[2], frames)
  }
  if (length(pitch_freqRange) == 4) {
    pitch_freqRangeLow <- makeseq(pitch_freqRange[1:2], frames)
    pitch_freqRangeUpp <- makeseq(pitch_freqRange[3:4], frames)
  } else {
    pitch_freqRangeLow <- makeseq(pitch_freqRange[1], frames)
    pitch_freqRangeUpp <- makeseq(pitch_freqRange[2], frames)
  }
  if (length(formant_freqRange) == 4) {
    formant_freqRangeLow <- makeseq(formant_freqRange[1:2], frames)
    formant_freqRangeUpp <- makeseq(formant_freqRange[3:4], frames)
  } else {
    formant_freqRangeLow <- makeseq(formant_freqRange[1], frames)
    formant_freqRangeUpp <- makeseq(formant_freqRange[2], frames)
  }
  if (!is.null(intensity_range)) {
    if (length(intensity_range == 4)) {
      intensity_rangeLow <- makeseq(intensity_range[1:2], frames)
      intensity_rangeUpp <- makeseq(intensity_range[3:4], frames)
    } else {
      intensity_rangeLow <- makeseq(intensity_range[1], frames)
      intensity_rangeUpp <- makeseq(intensity_range[2], frames)
    }
  } else {
    intensity_rangeLow <- NULL; intensity_rangeUpp <- NULL
  }


  gifski::save_gif(
    for (i in 1:frames) {
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
    delay=1/frameRate, loop=loop, pointsize=18
  )

}
