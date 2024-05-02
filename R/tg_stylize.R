#' Convert Praat font styles to R font styles
#'
#' Helper function for converting Praat font styles such as italics, bold,
#' and small caps into expressions that can be read by base R plots.
#' Instead of using
#' this function directly, just use
#' `praatpicture('my_sound_file', frames='TextGrid', tg_specialChar=TRUE)`.
#'
#' @param lab A string or vector of strings with labels from a TextGrid.
#'
#' @return A list with elements of class `expression`.
#' @export
#'
#' @examples
#' # Don't use directly
#' datapath <- system.file('extdata', package='praatpicture')
#' soundFile <- paste0(datapath, '/1.wav')
#'
#' # With stylized text
#' praatpicture(soundFile, frames='TextGrid')
#'
#' # Without stylized text
#' praatpicture(soundFile, frames='TextGrid', tg_specialChar=FALSE)
tg_stylize <- function(lab) {
  x <- list()

  for (l in 1:length(lab)) {
    if (lab[l] != '') {
      if (grepl('\\\\s', lab[l])) {
        s <- c()
        splsc <- unlist(strsplit(lab[l], split='\\\\s'))
        for (i in 2:length(splsc)) {
          tmp <- unlist(strsplit(splsc[i], split='}', perl=T))
          tmp[1] <- conv2sc(tmp[1])
          tmp[1] <- sub('{', '', tmp[1], perl=T)
          s <- c(s, paste(tmp, collapse=''))
        }
        s <- paste(s, collapse='')
        lab[l] <- paste(splsc[1], s, collapse='', sep='')
      }

      spl <- unlist(strsplit(lab[l], split='(?<=.)(?=[\\\\#%^_]{2})', perl=T))
      s <- c()

      for (i in 1:length(spl)) {
        ch1 <- substr(spl[i], 1, 1)
        if (!is.na(spl[i]) & grepl('[#%^_]{2}', spl[i])) {
          if (substr(spl[i], 1, 2) != '#%') {
            tmp <- unlist(strsplit(spl[i], split=paste0('(?<=.)(?=', ch1, ')'), perl=T))
            tmp[1] <- paste0(tmp[1], tmp[2])
            tmp <- tmp[-2]
            if (length(tmp) > 1) tmp[2] <- sub(ch1, '', tmp[2])
            s <- c(s, tmp)
          } else {
            s <- c(s, spl[i])
          }
        } else if (!is.na(ch1) & ch1 == '\\') {
          tmp <- unlist(strsplit(spl[i], split='(?=.)(?=[#%^_])', perl=T))
          tmp[1] <- paste0(tmp[1], tmp[2])
          tmp <- tmp[-2]
          s <- c(s, tmp)
        } else {
          s <- c(s, spl[i])
        }
      }

      spl <- s
      s <- c()
      for (i in 1:length(spl)) {
        if (!grepl('[\\\\#%^_]{2}', spl[i]) | substr(spl[i], 1, 2) == '#%') {
          tmp <- unlist(strsplit(spl[i], split=''))
          sch <- rev(grep('[%#^_]', tmp))
          for (j in sch) {
            tmp[j] <- paste0(tmp[j], tmp[j+1])
            tmp <- tmp[-(j+1)]
          }
          s <- c(s, tmp)
        } else if (substr(spl[i], 1, 1) == '\\') {
          s <- c(s, spl[i])
        } else {
          tmp <- substr(spl[i], 2, nchar(spl[i]))
          s <- c(s, tmp)
        }
      }
      spl <- s

      print_text <- NULL
      for (i in 1:length(spl)) {
        if (!grepl('[%#^_]', spl[i])) pr <- bquote(.(spl[i]))
        clean <- sub('[%#^_]*', '', spl[i])
        if (grepl('#%', spl[i])) {
          pr <- bquote(bolditalic(.(clean)))
        } else if (grepl('%', spl[i])) {
          pr <- bquote(italic(.(clean)))
        } else if (grepl('#', spl[i])) {
          pr <- bquote(bold(.(clean)))
        }
        if (grepl('\\^', spl[i])) pr <- bquote(''^.(clean))
        if (grepl('_', spl[i])) pr <- bquote(''[.(clean)])
        if (grepl('\\\\', spl[i])) {
          pr <- bquote(.(substr(spl[i], nchar(spl[i]), nchar(spl[i]))))
        }
        print_text <- bquote(.(print_text)*.(pr))
      }

      x[[l]] <- print_text

    } else {
      x[[l]] <- ''
    }
  }


  return(x)
}

