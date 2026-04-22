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
#' @param frames String or vector of strings giving the frames that the plot
#' should consist of. Default is `sound`, `spectrogram`, `TextGrid`. This
#' requires a file with the extension `.TextGrid` and the same base name as the
#' sound file. Other options are `pitch`, `formant`, and `intensity`. See
#' details for more information.
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
#' The argument is ignored if `cursor = TRUE`.
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
#' @param cursor Logical; should a moving cursor show the current position in
#' the sound file? Default is `FALSE`.
#' @param cursor_frameRate Number giving the desired number of frames per second
#' for moving cursor. Default is `50`.
#' @param cursor_color String giving the color of the moving cursor. Default is
#' `black`.
#' @param cursor_lwd Number giving the line width of the moving cursor. Default
#' is `2`.
#' @param verbose Logical; should status messages be printed in the console as
#' figures are being generated? Default is `TRUE`.
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
                                 frames = c('sound', 'spectrogram', 'TextGrid'),
                                 draw_lines=list('formant',
                                                 h=seq(0,10000,by=1000),
                                                 lty='dotted'),
                                 audio_start=start, audio_end=end,
                                 width=1080, height=720, pointsize=25,
                                 cursor=FALSE, cursor_frameRate=50,
                                 cursor_color = 'black', cursor_lwd = 2,
                                 verbose=TRUE,
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

  if (cursor) {
    frameShift <- 1 / cursor_frameRate
    numFrames <- dur / frameShift
    cursorPositions <- seq(audio_start, audio_start + dur,
                           length.out = numFrames)
    av::av_capture_graphics(
      for (i in 1:numFrames) {
        cursorLines <- list()
        for (frame in frames) cursorLines[[frame]] <- list(
          frame, v = cursorPositions[i], col = cursor_color, lwd = cursor_lwd)
        praatpicture(sound, start, end, frames = frames,
                     draw_lines = cursorLines, tfrom0 = FALSE,
                     ...)
        if (verbose) print(paste('Frame', i, 'generated'))
        },
      output=outputFile, audio=audioFile, verbose=FALSE,
      width=width, height=height, pointsize=pointsize,
      framerate=cursor_frameRate)
  } else {
    av::av_capture_graphics(praatpicture(sound, start, end, frames = frames,
                                         draw_lines = draw_lines, ...),
                            output=outputFile, audio=audioFile, verbose=FALSE,
                            width=width, height=height, pointsize=pointsize,
                            framerate=1/dur)
  }

  if (audio_start != start | audio_end != end) unlink('tmp.wav', force=TRUE)

  if (useViewer) {
    writeLines(text='<video width="550" height="400" controls>\n<source src="output.mp4">\n</video>', con=htmlFile)
    rstudioapi::viewer(htmlFile)
  }

}
