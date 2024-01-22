#' Interactively create a TextGrid object
#'
#' Annotate a sound file by interacting with waveform or spectrogram plots,
#' resulting in a TextGrid object which can be used for creating various
#' acoustic plots with time-aligned annotations with [praatpicture()].
#'
#' @param sound String giving the file name of a sound file with the .wav
#' extension.
#' @param tierNames String or vector of strings giving the name(s) of tiers in
#' the new TextGrid object.
#' @param start Start time (in seconds) of desired plotted area. Default is `0`.
#' @param end End time (in seconds) of desired plotted area. Default is `0`
#' (= the entire file).
#' @param show String giving the type of plot to show. Default is `wave`,
#' another option is `spectrogram`. Note that spectrogram plotting is relatively
#' slow within this function.
#' @param channel Number indicating which audio channel to show. Default is `1`.
#'
#' @details Running this function will show either a waveform or a spectrogram
#' in a separate `X11` graphics device window. Click on this figure in the
#' locations where you want to add boundaries to your TextGrid objects. This
#' should be done sequentially, starting with the first boundary along the
#' time axis and ending with the last. It does not matter where on the y-axis
#' you click.
#'
#' Once you have indicated all the desired boundaries, you will be prompted
#' in the R console to say whether the tier is an interval tier or a point tier
#' by typing `y` (for interval tier) or `n` (for point tier). Subsequently
#' you will be prompted in the console to write labels corresponding to each
#' interval or point.
#'
#' If you are creating a TextGrid with multiple tiers (i.e., if `tierNames`
#' is longer than 1), this process will be repeated for all tiers.
#'
#' @return A list object identical to those created by [rPraat::tg.read()] when
#' loading TextGrid objects into R. This object can be passed to the `tg_obj`
#' argument when using `praatpicture`.
#' @seealso `make_TextGrid()` is largely a wrapper around the function
#' [tg_createTier()] which does most of the work.
#' @export
#'
#' @examples
#' #not now
make_TextGrid <- function(sound, tierNames, start=0, end=0,
                          show='wave', channel=1) {
  tg <- list()
  for (t in tierNames) {
    tg[[t]] <- tg_createTier(sound, t, start, end, show, channel)
  }
  return(tg)
}
