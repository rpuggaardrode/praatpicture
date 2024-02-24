#' Interactively create a TextGrid tier
#'
#' Function for creating TextGrid tiers called by [make_TextGrid]. Instead of
#' using this function directly, use [make_TextGrid].
#'
#' @param sound String giving the file name of a sound file with the .wav
#' extension.
#' @param tierName String giving the name of the tier.
#' @param start Start time (in seconds) of desired plotted area. Default is `0`.
#' @param end End time (in seconds) of desired plotted area. Default is `0`
#' (= the entire file).
#' @param show String giving the type of plot to show. Default is `wave`,
#' another option is `spectrogram`. Note that spectrogram plotting is relatively
#' slow within this function.
#' @param channel Number indicating which audio channel to show. Default is `1`.
#' @param sampa2ipa Logical; should SAMPA transcriptions be converted to IPA?
#' Default is `FALSE`.
#'
#' @return A list object identical to a single tier created by
#' [rPraat::tg.read()] when
#' loading TextGrid objects into R.
#' @export
#'
#' @examples
#' \dontrun{
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/2.wav')
#' tg <- make_TextGrid(soundFile, tierNames='Mary')
#' # Follow the steps shown in the console
#'
#' praatpicture(soundFile, tg_obj=tg)
#' }
tg_createTier <- function(sound, tierName, start=0, end=0, show='wave',
                          channel=1, sampa2ipa=FALSE) {

  if (end == 0) end <- Inf
  snd <- tuneR::readWave(sound, from=start, to=end, units='seconds', toWaveMC=T)
  sig <- snd@.Data[,channel]
  sr <- snd@samp.rate

  grDevices::X11()
  dur <- length(sig) / sr

  if (show == 'wave') {
    plot(y=sig, x=seq(0, dur, length.out=length(sig)), type='l',
         ylim=c(min(sig), max(sig)*1.1),
         xlab='Time (s)', ylab='',
         main='Click Stop -> Stop locator when you are done')
  }

  if (show == 'spectrogram') {
    phonTools::spectrogram(sig, sr, colors=FALSE, window='hamming')
    dur <- dur * 1000
  }

  message(crayon::blue(paste('Navigate to the graphics device window and click',
              'in the',
              'positions where you want TextGrid boundaries for tier',
              tierName)))
  l <- graphics::locator()
  t0 <- l$x
  graphics::abline(v=t0, lty='dotted')
  tier <- list()
  tier$name <- tierName
  intbool <- readline('Is this an interval tier? [y/n] ')
  if (intbool == 'y') {
    tier$type <- 'interval'
    tier$t1 <- c(0, t0)
    tier$t2 <- c(t0, dur)
    for (i in 1:(length(tier$t1)-2)) {
      tier$label[i] <- readline(paste0('Label ', i, ' of ',
                                       length(tier$t1)-2, ': '))
    }
    tier$label <- c('', tier$label, '')
    if (sampa2ipa) tier$label <- ipa::sampa(tier$label)
    if (show == 'wave') graphics::text(tier$t1+(tier$t2-tier$t1)/2, max(sig)*1.1,
                                       tier$label, font=2)
    if (show == 'spectrogram') graphics::text(tier$t1+(tier$t2-tier$t1)/2, 4500,
                                              tier$label, font=2, col='blue')
    if (show == 'spectrogram') {
      tier$t1 <- tier$t1 / 1000
      tier$t2 <- tier$t2 / 1000
    }
    tier$t1 <- start + tier$t1
    tier$t2 <- start + tier$t2
  } else {
    tier$type <- 'point'
    tier$t <- t0
    for (i in 1:length(tier$t)) {
      tier$label[i] <- readline(paste0('Label ', i, ' of ',
                                       length(tier$t), ': '))
    }
    if (sampa2ipa) tier$label <- ipa::sampa(tier$label)
    if (show == 'wave') graphics::text(tier$t, max(sig)*1.1, tier$label, font=2)
    if (show == 'spectrogram') graphics::text(tier$t, 4500, tier$label, font=2,
                                              col='blue')
    if (show == 'spectrogram') tier$t <- tier$t / 1000
    tier$t <- start + tier$t
  }
  dummy <- readline('Check the resulting annotation and type enter! ')
  grDevices::dev.off()
  return(tier)
}
