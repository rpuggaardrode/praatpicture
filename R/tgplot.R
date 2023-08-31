#' Plot TextGrid
#'
#' Function for plotting TextGrids called by [praatpicture]. Instead of using
#' this function directly, just use
#' `praatpicture('my_sound_file', frames='TextGrid')`.
#'
#' @param tg TextGrid object loaded using [rPraat::tg.read]
#' @param t Numeric vector giving times corresponding to the signal.
#' @param sr Integer giving the sampling rate of the signal.
#' @param start Start time (in seconds) of desired plotted area.
#' @param tiers Vector of number or strings giving either numeric identifiers
#' of TextGrid tiers to plot or the names of TextGrid tiers to plot. Default is
#' `1`, which plots just the first tier.
#' @param tfrom0 Logical; should time on the x-axis run from 0 or from the
#' original time? Default is `TRUE`.
#' @param tier_names Logical; should TextGrid tier names be printed along the
#' y-axis? Default is `TRUE`.
#' @param ind Integer indexing waveform relative to other plot components.
#' Default is `NULL`.
#' @param nframe Integer giving the number of plot components. Default is `NULL`.
#' @param tg_alignment String giving the desired alignment of text in the
#' TextGrids. Default is `central`; other options are `left` and `right`.
#' Alternatively, a vector of strings if different alignments are needed.
#' @param tg_specialchar Logical; should Praat typesetting for special font types
#' such as italic, bold, and small caps be converted into corresponding
#' R-readable special font types. Default is `TRUE`.
#' See [https://www.fon.hum.uva.nl/praat/manual/Text_styles.html].
#' @param start_end_only Logical; should there only be ticks on the x-axis
#' for start and end times? Default is `TRUE`.
#'
#' @export
#'
#' @examples
#' #dont use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' praatpicture(paste0(datapath, '/1.wav'), frames='TextGrid')
tgplot <- function(tg, t, sr, start, tiers=1, tfrom0=TRUE, tier_names=TRUE,
                   ind=NULL, nframe=NULL, tg_alignment='central',
                   tg_specialchar=TRUE, start_end_only=TRUE) {

  if (length(tg_alignment) != length(tiers)) {
    tg_alignment <- rep(tg_alignment, length(tiers))
  }

  i <- 0
  for (tier in tiers) {
    i <- i+1
    lab <- tg[[tier]]$label
    tname <- tg[[tier]]$name

    if (tg[[tier]]$type == 'interval') {
      t1 <- tg[[tier]]$t1
      t2 <- tg[[tier]]$t2
      if (tfrom0) {
        t1 <- t1 - start
        t2 <- t2 - start
      }
      line_vec <- t1[-1]
    } else {
      t0 <- tg[[tier]]$t
      if (tfrom0) t0 <- t0 - start
      line_vec <- t0
    }

    if (tfrom0) {
      x1 <- 0
      x2 <- length(t)/sr
    } else {
      x1 <- start
      x2 <- (length(t)/sr) + start
    }

    if (ind == nframe) {
      if (tier == utils::tail(tiers, n=1)) {
        if (!start_end_only) {
          xax <- 's'
        } else {
          xax <- 'n'
          xtix <- c(round(min(t), 3), round(max(t), 3), 0)
        }
      } else {
        xax <- 'n'
      }
    } else {
      xax <- 'n'
    }

    if (tg_specialchar) lab <- tg_stylize(lab)

    plot(1, type='n', ylab='',
         xlim=c(x1, x2), xaxt=xax,
         ylim=c(0,10), yaxt='n', yaxs='i')
    if (tg[[tier]]$type == 'interval') {
      graphics::abline(v=line_vec)
      if (tg_alignment[[i]]=='central') graphics::text(
        t1+(t2-t1)/2, 5, as.expression(lab))
      if (tg_alignment[[i]]=='left') graphics::text(
        t1, 5, as.expression(lab), pos=4)
      if (tg_alignment[[i]]=='right') graphics::text(
        t2, 5, as.expression(lab), pos=2)
    } else {
      graphics::segments(x0=line_vec, x1=line_vec, y0=0, y1=2)
      graphics::segments(x0=line_vec, x1=line_vec, y0=8, y1=10)
      graphics::text(t0, 5, as.expression(lab), adj=0.5)
    }
    if (ind == nframe & start_end_only & tier == utils::tail(tiers, n=1)) {
      graphics::axis(1, at=xtix)
    }
    if (tier_names) graphics::mtext(tname, side=2, las=2, line=0.6, cex=0.8)
  }

}
