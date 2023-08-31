#' Convert capital letters to Unicode small caps
#'
#' Hacky helper function to convert capital letters into Unicode small caps.
#' May not work for all font families. Note that there's no Unicode small cap
#' 'X'.
#'
#' @param x A string where all capital letters should be converted to small
#' caps.
#'
#' @return A string where all capital letters have been converted to small caps.
#' @export
#'
#' @examples
#' my_string <- 'ABCDEFGHIJKLM'
#' conv2sc(my_string)
conv2sc <- function(x) {
  x <- gsub('A', 'ᴀ', x)
  x <- gsub('B', 'ʙ', x)
  x <- gsub('C', 'ᴄ', x)
  x <- gsub('D', 'ᴅ', x)
  x <- gsub('E', 'ᴇ', x)
  x <- gsub('F', 'ғ', x)
  x <- gsub('G', 'ɢ', x)
  x <- gsub('H', 'ʜ', x)
  x <- gsub('I', 'ɪ', x)
  x <- gsub('J', 'ᴊ', x)
  x <- gsub('K', 'ᴋ', x)
  x <- gsub('L', 'ʟ', x)
  x <- gsub('M', 'ᴍ', x)
  x <- gsub('N', 'ɴ', x)
  x <- gsub('O', 'ᴏ', x)
  x <- gsub('P', 'ᴘ', x)
  x <- gsub('Q', 'ǫ', x)
  x <- gsub('R', 'ʀ', x)
  x <- gsub('S', 's', x)
  x <- gsub('T', 'ᴛ', x)
  x <- gsub('U', 'ᴜ', x)
  x <- gsub('V', 'ᴠ', x)
  x <- gsub('W', 'ᴡ', x)
  x <- gsub('Y', 'ʏ', x)
  x <- gsub('Z', 'ᴢ', x)
  return(x)
}
