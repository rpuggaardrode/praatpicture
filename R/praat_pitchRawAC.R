#' Estimate pitch using Praat's raw autocorrelation function
#'
#' @param sound String giving the file name of a sound file with the .wav
#' extension.
#' @param output String giving the output format. Should be either `ssff`
#' for the SSFF-style format used by `wrassp` (default) or `df` to output a
#' data frame.
#' @param timeStep Measurement interval in seconds.
#' Default is `NULL`, in which case the measurement interval is equal to
#' 0.75 / `pitch_floor`.
#' @param floor Frequency in Hz; no pitch candidates considered below
#' this frequency. Default is `75`.
#' @param ceiling Frequency in Hz; no pitch candidates considered above
#' this frequency. Default is `600`.
#' @param maxNoCandidates Integer giving the maximum number of pitch candidates
#' to estimate. Default is `15`.
#' @param gaussianWindow Logical; should Gaussian windows be used for
#' estimating pitch? Default is `FALSE`, in which case Hanning windows are used.
#' @param silenceThreshold Numeric giving the silence threshold when estimating
#' pitch in terms of amplitude relative to the global maximum. Default is
#' `0.01`.
#' @param voicingThreshold Numeric giving the voicing threshold when estimating
#' pitch in terms of fractional strength in the autocorrelation function.
#' Default is `0.14`.
#' @param octaveCost Numeric specifying how much high frequency pitch candidates
#' should be favored in terms of fractional strength in the autocorrelation
#' function. Default is `0.01`.
#' @param octaveJumpCost Numeric specifying how much pitch changes should be
#' disfavored in terms of fractional strength in the autocorrelation function.
#' Default is `0.35`.
#' @param voicedUnvoicedCost Numeric specifying how much transitions in voicing
#' value should be disfavored in terms of fractional strength in the
#' autocorelation function. Default is `0.14`.
#'
#' @returns List in the SSFF format or data frame with estimated pitch.
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
praat_pitchRawAC <- function(sound, output = 'ssff',
                             timeStep = NULL,
                             floor = 75, ceiling = 600,
                             maxNoCandidates = 15, gaussianWindow = FALSE,
                             silenceThreshold = 0.03, voicingThreshold = 0.45,
                             octaveCost = 0.01, octaveJumpCost = 0.35,
                             voicedUnvoicedCost = 0.14) {

  praat <- reticulate::import('parselmouth')
  pit <- praat$Sound(sound)$to_pitch_ac(
    time_step = timeStep, pitch_floor = floor,
    max_number_of_candidates = as.integer(maxNoCandidates),
    very_accurate = gaussianWindow, silence_threshold = silenceThreshold,
    voicing_threshold = voicingThreshold, octave_cost = octaveCost,
    octave_jump_cost = octaveJumpCost,
    voiced_unvoiced_cost = voicedUnvoicedCost, pitch_ceiling = ceiling)

  if (output == 'ssff') {
    ado <- list()
    attr(ado, 'sampleRate') <- 1 / pit$time_step
    attr(ado, 'origFreq') <- 0
    attr(ado, 'startTime') <- pit$x1
    attr(ado, 'startRecord') <- 1
    attr(ado, 'endRecord') <- pit$get_number_of_frames()
    class(ado) <- 'AsspDataObj'
    wrassp::AsspFileFormat(ado) <- 'SSFF'
    wrassp::AsspDataFormat(ado) <- as.integer(2)
    ado <- wrassp::addTrack(ado, 'F0', as.matrix(pit$to_matrix()$values[1,]),
                            format='REAL32')
    attr(ado, 'trackFormats') <- 'REAL32'
    return(ado)
  } else if (output == 'df') {
    fn <- rep(sound, pit$get_number_of_frames())
    t <- pit$ts()
    val <- pit$to_matrix()$values[1,]
    out <- data.frame(file = fn, t = t, f0 = val)
    return(out)
  }
}
