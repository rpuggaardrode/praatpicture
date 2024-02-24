#' Make Praat Picture style plots of acoustic data with embedded audio
#'
#' Generate simple MP4 video files with Praat Picture style plots of acoustic
#' data with time-aligned transcriptions and embedded audio to use in
#' presentations etc.
#'
#' @param sound String giving the file name of a sound file with the .wav
#' extension.
#' @param start Start time (in seconds) of desired plotted area. Default is `0`.
#' @param end End time (in seconds) of desired plotted area. Default is `0`
#' (= the entire file).
#' @param audio_start Start time (in seconds) of embedded audio. By default
#' it is the same as `start`, i.e. the embedded audio is the portion of the
#' sound file that is being plotted.
#' @param audio_end End time (in seconds) of embedded audio. By default it is
#' the same as `end`, i.e. the embedded audio is the portion of the sound
#' that is being plotted.
#' @param width Number giving the desired width of the resulting animation in
#' pixels; default is `1080`.
#' @param height Number giving the desired height of the resulting animation in
#' pixels; default is `720`.
#' @param pointsize Number; which point size should be used for text in the
#' animation? Default is `25`. See [grDevices::png()] for more details.
#' @param outputFile String giving the desired file name. Default is
#' `praatvid.mp4`.
#' @param useViewer Logical; should the video be shown in the Viewer pane in
#' RStudio? Default is `TRUE`; if true, the video is oSnly saved in a
#' temporary directory, but can be downloaded from a browser.
#' @param ... Further arguments passed to `praatpicture`.
#'
#' @seealso This function is a wrapper for [av::av_capture_graphics()] used to
#' produce plots similar to those made with [praatpicture()] with embedded
#' audio. For more detail on your
#' options, see the `praatpicture()` help file.
#' @return No return value, produces a video file.
#' @export
#'
#' @examples
#' \dontrun{
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#' talking_praatpicture(soundFile)
#' }
talking_praatpicture <- function(sound, start=0, end=0,
                                 audio_start=start, audio_end=end,
                                 width=1080, height=720, pointsize=25,
                                 outputFile='praatvid.mp4', useViewer=TRUE,
                                 ...) {

  aend <- audio_end
  if (aend == 0) aend <- Inf

  if (audio_start != 0 | audio_end != 0) {
    snd <- tuneR::readWave(sound, from=audio_start, to=aend, units='seconds')
    sr <- snd@samp.rate
    nsamp <- length(snd@left)
    dur <- nsamp / sr
    tuneR::writeWave(snd, filename='tmp.wav', extensible=FALSE)
    audioFile <- 'tmp.wav'
  } else {
    audioFile <- sound
    snd <- tuneR::readWave(sound, from=0, units='seconds', toWaveMC=T)
    sr <- snd@samp.rate
    nsamp <- snd@dim[1]
    dur <- nsamp / sr
  }

  if (useViewer) {
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, 'index.html')
    outputFile <- file.path(tempDir, 'output.mp4')
  }

  av::av_capture_graphics(praatpicture(sound, start, end, ...),
                          output=outputFile, audio=audioFile, verbose=FALSE,
                          width=width, height=height, pointsize=pointsize,
                          framerate=1/dur)

  if (audio_start != start | audio_end != end) unlink('tmp.wav', force=TRUE)

  if (useViewer) {
    writeLines(text='<video width="550" height="400" controls>\n<source src="output.mp4">\n</video>', con=htmlFile)
    rstudioapi::viewer(htmlFile)
  }

}
