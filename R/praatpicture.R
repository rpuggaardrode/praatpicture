#' Make Praat Picture style plots of acoustic data
#'
#' Generate plots of acoustic data aligned with transcriptions similar
#' to those made with Praat Picture. The default is to produce a plot with a
#' relatively small waveform, somewhat larger spectrogram, and the first tier
#' of a TextGrid.
#'
#' @param sound String giving the file name of a sound file with the .wav or
#' .mp3 extension.
#' @param start Start time (in seconds) of desired plotted area. Default is `0`.
#' @param end End time (in seconds) of desired plotted area. Default is `Inf`
#' (= the entire file).
#' @param tfrom0 Logical; should time on the x-axis run from 0 or from the
#' original time? Default is `TRUE`.
#' @param frames String or vector of strings giving the frames that the plot
#' should consist of. Default is `sound`, `spectrogram`, `TextGrid`. This
#' requires a file with the extension `.TextGrid` and the same base name as the
#' sound file. Other options are `pitch`, `formant`, and `intensity`. For these,
#' files with the same base name as the sound file and the extensions
#' `.PitchTier`, `.Formant`, and `.IntensityTier` are required, respectively.
#' These will be plotted in the given order from top to bottom.
#' @param proportion Integer or vector of integers of the same size as `frames`
#' giving the proportion in percents of the plotting area to be taken up by the
#' individual frames. The numbers should sum up to 100.
#' Default is `c(30,50,20)`. If more or less than three
#' frames are plotted and no proportions are given, frames will be of equal
#' size.
#' @param tiers Vector of number or strings giving either numeric identifiers
#' of TextGrid tiers to plot or the names of TextGrid tiers to plot. Default is
#' `1`, which plots just the first tier.
#' @param focus_tier For which tier(s) should dotted lines be shown on all
#' acoustic plots giving the locations of boundaries? Vector of number or
#' strings giving either numeric identifiers
#' of TextGrid tiers or the names of TextGrid tiers to plot. Default is
#' `tiers[1]`, i.e. the first tier given in the `tiers` argument. Additionally
#' accepts the string `none`,
#' in which case no dotted lines are shown on acoustic plots, and `all`, in
#' which case dotted lines from all tiers are shown on acoustic plots.
#' @param tier_names Logical; should TextGrid tier names be printed along the
#' y-axis? Default is `TRUE`.
#' @param freqrange Vector of two integers giving the frequency range to be
#' used for plotting spectrograms. Default is `c(0,5000)`.
#' @param windowlength Window length in seconds for generating spectrograms.
#' Default is `0.005`.
#' @param dynamicrange Dynamic range in dB for generating spectrograms. The
#' maximum intensity minus `dynamicrange` will all be printed in white. Default
#' is `50`.
#' @param timestep How many time steps should be calculated for spectrograms?
#' Default is `1000`. Note that this takes a while to plot, so for fiddling with
#' plotting parameters it is a good idea to choose a smaller value.
#' @param windowshape String giving the name of the window shape to be applied
#' to the signal when generating spectrograms. Default is `Gaussian`; other
#' options are `square`, `Hamming`, `Bartlett`, or `Hanning`.
#' @param pitchtype String giving the type of pitch plot to produce; default
#' is `draw` (a line plot), the only other option is `speckle` (a point plot).
#' @param pitchscale String giving the frequency scale to use when producing
#' pitch plots. Default is `hz`; other options are `logarithmic` (also in Hz),
#' `semitones`, `erb`, and `mel`.
#' @param pitchrange Vector of two integers giving the frequency range to be
#' used for producing pitch plots. Default is `c(50,500)`. If the frequency
#' scales `semitones` or `erb` are used, the pitch range is automatically reset
#' to the Praat defaults for these scales (`c(-12,30)` and `c(0,10)`,
#' respectively).
#' @param semitones_re Frequency in Hz giving the reference level for converting
#' pitch frequency to semitones. Default is `100`.
#' @param formant_dynrange Dynamic range in dB for producing formant plots.
#' When a formant plot of `formanttype='speckle'` is drawn, no formants are
#' shown in frames with intensity level `formant_dynrange` below the maximum
#' intensity. Default is `30`. If set to `0`, all formants are shown.
#' @param formantrange Vector of two integers giving the frequency range to be
#' used for producing formant plots. Default is `c(0,5500)`.
#' @param formanttype String giving the type of formant plot to produce;
#' default is `speckle` (a point plot), the only other option is `draw` (a line
#' plot).
#' @param intensityrange Vector of two integers giving the intensity range to be
#' used for producing intensity plots. Default is `NULL`, in which case the
#' range is simply the minimum and maximum levels in the curve.
#' @param ... Further global plotting arguments passed on to `par()`.
#'
#' @seealso Functions from `rPraat` are used to load in files created with
#' Praat. Spectrograms are generated using [phonTools::spectrogram].
#' @export
#'
#' @examples
#' praatpicture('inst/extdata/1.wav')
praatpicture <- function(sound, start=0, end=Inf, tfrom0=TRUE,
                         frames=c('sound', 'spectrogram', 'TextGrid'),
                         proportion=c(30,50,20), tiers=1,
                         focus_tier=tiers[1], tier_names=TRUE,
                         freqrange=c(0,5000), windowlength=0.005,
                         dynamicrange=50, timestep=1000, windowshape='Gaussian',
                         pitchtype='draw', pitchscale='hz', pitchrange=c(50,500),
                         semitones_re=100, formant_dynrange=30,
                         formantrange=c(50, 5500), formanttype='speckle',
                         intensityrange=NULL, ...) {

  legal_frames <- c('sound', 'TextGrid', 'spectrogram', 'pitch', 'formant',
                    'intensity')
  if (any(!frames %in% legal_frames)) {
    stop('Currently available frames are sound, TextGrid, spectrogram, formant, ',
         'pitch, and intensity')
  }

  if (sum(proportion) != 100) {
    stop('The numbers in proportion should sum up to 100')
  }

  nframe <- length(frames)
  if (nframe != 3 & length(proportion) != nframe) {
    proportion <- rep(round(100/nframe), nframe)
  }

  snd <- rPraat::snd.read(sound, from=start, to=end, units='seconds')
  sig <- snd$sig[,1]
  sr <- snd$fs
  dur <- max(snd$t)

  fn <- unlist(strsplit(sound, '[.]'))[1]

  if ('TextGrid' %in% frames) {
    tgfn <- paste0(fn, '.TextGrid')
    tg <- rPraat::tg.read(tgfn)
    if (any(focus_tier == 'all')) focus_tier <- tiers
    if (any(focus_tier != 'none')) {
      tgbool <- TRUE
      focus_linevec <- c()
      for (i in focus_tier) {
        if (tfrom0) {
          if (tg[[i]]$type == 'point') focus_linevec <-
              c(focus_linevec, tg[[i]]$t - start)
          if (tg[[i]]$type == 'interval') focus_linevec <-
              c(focus_linevec, tg[[i]]$t1[-1] - start)
        } else {
          if (tg[[i]]$type == 'point') focus_linevec <-
              c(focus_linevec, tg[[i]]$t)
          if (tg[[i]]$type == 'interval') focus_linevec <-
              focus_linevec <- c(focus_linevec, tg[[i]]$t1[-1])
        }
      }
    } else {
      tgbool <- FALSE
      focus_linevec <- NULL
    }
  } else {
    tgbool <- FALSE
    focus_linevec <- NULL
  }

  if ('pitch' %in% frames) {
    ptfn <- paste0(fn, '.PitchTier')
    pt <- rPraat::pt.read(ptfn)
  }

  if ('formant' %in% frames) {
    fmfn <- paste0(fn, '.Formant')
    fm <- rPraat::formant.read(fmfn)
  }

  if ('intensity' %in% frames) {
    itfn <- paste0(fn, '.IntensityTier')
    it <- rPraat::it.read(itfn)
  }

  if (tfrom0) {
    t <- snd$t - start
  } else {
    t <- snd$t
  }

  if (length(tiers) > 1) {
    ntiers <- length(tiers)
    tgind <- which(frames == 'TextGrid')
    tgprop <- proportion[tgind]
    proportion[tgind] <- round(tgprop / ntiers)
    if (tgind == nframe) {
      proportion[(nframe+1):(nframe+ntiers-1)] <-
        round(tgprop / ntiers)
    } else {
      proportion[(tgind+ntiers-1):(nframe+ntiers-1)] <-
        proportion[(tgind+1):nframe]
      proportion[(tgind+1):(tgind+ntiers-1)] <-
        round(tgprop / ntiers)
    }
  }

  plot_matrix <- rep(1:length(proportion), proportion)

  graphics::par(mar=c(0,2,0,2), oma=c(5,5,5,5), ...)
  graphics::layout(matrix(plot_matrix, nrow=length(plot_matrix)))

  for (i in 1:nframe) {

    if (frames[i] == 'sound') {
      ind <- which(frames == 'sound')
      waveplot(sig, t, tgbool, focus_linevec, ind, nframe)
    } else if (frames[i] == 'spectrogram') {
      ind <- which(frames == 'spectrogram')
      specplot(sig, sr, t, start, max(snd$t)-start, tfrom0, freqrange, windowlength,
               dynamicrange, timestep,
               windowshape, tgbool, focus_linevec, ind, nframe)
    } else if (frames[i] == 'TextGrid') {
      ind <- which(frames == 'TextGrid')
      tgplot(tg, t, sr, start, tiers, tfrom0, tier_names, ind, nframe)
    } else if (frames[i] == 'pitch') {
      ind <- which(frames == 'pitch')
      pitchplot(pt, start, max(snd$t)-start, tfrom0, tgbool, focus_linevec,
                pitchtype, pitchscale, pitchrange,
                semitones_re, ind, nframe)
    } else if (frames[i] == 'formant') {
      ind <- which(frames == 'formant')
      formantplot(fm, start, max(snd$t)-start, tfrom0, tgbool, focus_linevec,
                  formant_dynrange, formantrange, formanttype, ind, nframe)
    } else if (frames[i] == 'intensity') {
      ind <- which(frames == 'intensity')
      intensityplot(it, start, max(snd$t)-start, tfrom0, tgbool, focus_linevec,
                    intensityrange, ind, nframe)
    }
  }

  graphics::mtext('Time (s)', side=1, line=3, outer=T, cex=0.8)

}
