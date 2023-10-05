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
#' @param channels Vector of numbers or strings giving either numeric identifiers
#' of audio channels to plot of the names of audio channels to plot. Also
#' understands `'all'`, which plots all channels and is the default.
#' @param channel_names Should names of audio channels be printed
#' on the y-axis? If `TRUE`, names will be grabbed from the audio metadata if
#' available. Alternatively, if two channels are available, they will be named
#' `left` and `right`. If more or less than two channels are available,
#' channels are named `Cn`, where `n` is the number of the channel. Alternatvely,
#' a vector of strings can be provided with channel names. Default is `FALSE`.
#' @param tiers Vector of numbers or strings giving either numeric identifiers
#' of TextGrid tiers to plot or the names of TextGrid tiers to plot. Also
#' understands `'all'`, which plots all tiers and is the default.
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
#' @param spec_channel Numeric giving the channel that should be used to
#' generate the spectrogram. Default is `1`. Generating spectrograms from
#' multiple channels is not currently possible with `praatpicture`.
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
#' @param formants_on_spec Boolean; should formants be plotted on top of
#' spectrogram? Default is `FALSE`.
#' @param intensityrange Vector of two integers giving the intensity range to be
#' used for producing intensity plots. Default is `NULL`, in which case the
#' range is simply the minimum and maximum levels in the curve.
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
#' @param tg_alignment String giving the desired alignment of text in the
#' TextGrids. Default is `central`; other options are `left` and `right`.
#' Alternatively, a vector of strings if different alignments are needed.
#' @param tg_specialchar Logical; should Praat typesetting for special font types
#' such as italic, bold, and small caps be converted into corresponding
#' R-readable special font types. Default is `TRUE`.
#' See [https://www.fon.hum.uva.nl/praat/manual/Text_styles.html].
#' @param start_end_only Logical; should there only be ticks on the x-axis
#' for start and end times? Default is `TRUE`.
#' @param min_max_only Logical; should only minimum and maximum values be given
#' on the y-axis? Default is `TRUE`. Can also be a logical vector if some but
#' not all plot components should have minimum and maximum values on the y-axis.
#' Ignored for TextGrid component.
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
                         proportion=c(30,50,20),
                         channels='all', channel_names=FALSE,
                         tiers='all',focus_tier=tiers[1], tier_names=TRUE,
                         spec_channel=1, freqrange=c(0,5000), windowlength=0.005,
                         dynamicrange=50, timestep=1000, windowshape='Gaussian',
                         pitchtype='draw', pitchscale='hz', pitchrange=c(50,500),
                         semitones_re=100, formant_dynrange=30,
                         formantrange=c(50, 5500), formanttype='speckle',
                         formants_on_spec=FALSE, intensityrange=NULL,
                         draw_rectangle=NULL, draw_arrow=NULL,
                         tg_alignment='central', tg_specialchar=TRUE,
                         start_end_only=TRUE, min_max_only=TRUE, ...) {

  p <- par(no.readonly=TRUE)

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
  if (nframe > length(min_max_only)) min_max_only <- rep(min_max_only, nframe)

  if (class(draw_rectangle) != 'list') draw_rectangle <- list(draw_rectangle)
  rect_comp <- sapply(draw_rectangle, '[[', 1)

  if (class(draw_arrow) != 'list') draw_arrow <- list(draw_arrow)
  arr_comp <- sapply(draw_arrow, '[[', 1)

  snd <- tuneR::readWave(sound, from=start, to=end, units='seconds', toWaveMC=T)

  sr <- snd@samp.rate
  bit <- snd@bit
  nsamp <- snd@dim[1]
  nchan <- snd@dim[2]
  if (any(channels == 'all')) channels <- 1:nchan
  sig <- snd@.Data[,channels]
  if (any(class(sig) == 'integer')) sig <- as.matrix(sig)
  nchan <- dim(sig)[2]

  if (class(channel_names) == 'logical' & isTRUE(channel_names)) {
    if (length(colnames(sig)) > 0) {
      cn <- colnames(sig)
    } else if (nchan == 2) {
      cn <- c('left', 'right')
    } else {
      cn <- paste0('C', 1:nchan)
    }
  } else if (class(channel_names) == 'character') {
    cn <- channel_names
    channel_names <- TRUE
  } else {
    cn <- NULL
  }

  if (start == 0) {
    tstart <- 0 + (1/sr)
  } else {
    tstart <- start
  }
  tseq <- seq(tstart, tstart+nsamp/sr, by=1/sr)

  fn <- unlist(strsplit(sound, '[.]'))[1]

  if ('TextGrid' %in% frames) {
    tgfn <- paste0(fn, '.TextGrid')
    tg <- rPraat::tg.read(tgfn)
    if (any(tiers == 'all')) tiers <- 1:length(tg)
    if (length(tiers) > 1) {
      ntiers <- length(tiers)
      tgind <- which(frames == 'TextGrid')
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

  if ('formant' %in% frames | formants_on_spec) {
    fmfn <- paste0(fn, '.Formant')
    fm <- rPraat::formant.read(fmfn)
  } else {
    fm <- NULL
  }

  if ('intensity' %in% frames) {
    itfn <- paste0(fn, '.IntensityTier')
    it <- rPraat::it.read(itfn)
  }

  if (tfrom0) {
    t <- tseq - start
  } else {
    t <- tseq
  }

  if (nchan > 1) {
    nf <- nframe + length(tiers) - 1
    wavind <- which(frames=='sound')
    if (length(tiers) > 1 & wavind > tgind) wavind <- wavind + length(tiers) - 1
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
      waveplot(sig, bit, t, nchan, tgbool, focus_linevec, ind, nframe,
               rect_comp, arr_comp, draw_rectangle, draw_arrow,
               channel_names, cn, start_end_only, min_max_only)
    } else if (frames[i] == 'spectrogram') {
      ind <- which(frames == 'spectrogram')
      specplot(sig[,which(channels==spec_channel)], sr, t, start,
               max(tseq)-start, tfrom0,
               freqrange, windowlength, dynamicrange, timestep, windowshape,
               formants_on_spec, fm, formanttype, formant_dynrange,
               tgbool, focus_linevec, ind, nframe, start_end_only, min_max_only)
      if ('spectrogram' %in% rect_comp) draw_rectangle('spectrogram', draw_rectangle)
      if ('spectrogram' %in% arr_comp) draw_arrow('spectrogram', draw_arrow)
    } else if (frames[i] == 'TextGrid') {
      ind <- which(frames == 'TextGrid')
      tgplot(tg, t, sr, start, tiers, tfrom0, tier_names, ind, nframe,
             tg_alignment, tg_specialchar, start_end_only)
      if ('TextGrid' %in% rect_comp) draw_rectangle('TextGrid', draw_rectangle)
      if ('TextGrid' %in% arr_comp) draw_arrow('TextGrid', draw_arrow)
    } else if (frames[i] == 'pitch') {
      ind <- which(frames == 'pitch')
      pitchplot(pt, start, max(tseq)-start, tfrom0, tgbool, focus_linevec,
                pitchtype, pitchscale, pitchrange,
                semitones_re, ind, nframe, start_end_only, min_max_only)
      if ('pitch' %in% rect_comp) draw_rectangle('pitch', draw_rectangle)
      if ('pitch' %in% arr_comp) draw_arrow('pitch', draw_arrow)
    } else if (frames[i] == 'formant') {
      ind <- which(frames == 'formant')
      formantplot(fm, start, max(tseq)-start, tfrom0, tgbool, focus_linevec,
                  formant_dynrange, formantrange, formanttype, ind, nframe,
                  start_end_only, min_max_only)
      if ('formant' %in% rect_comp) draw_rectangle('formant', draw_rectangle)
      if ('formant' %in% arr_comp) draw_arrow('formant', draw_arrow)
    } else if (frames[i] == 'intensity') {
      ind <- which(frames == 'intensity')
      intensityplot(it, start, max(tseq)-start, tfrom0, tgbool, focus_linevec,
                    intensityrange, ind, nframe, start_end_only, min_max_only)
      if ('intensity' %in% rect_comp) draw_rectangle('intensity', draw_rectangle)
      if ('intensity' %in% arr_comp) draw_arrow('intensity', draw_arrow)
    }
  }
  graphics::mtext('Time (s)', side=1, line=3, outer=T, cex=0.8)

  par(p)
}
