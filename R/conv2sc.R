#' Convert capital letters to Unicode small caps
#'
#' Helper function to convert capital letters into Unicode small caps.
#' May not work for all font families. Note that there's no Unicode small cap
#' 'X', so 'X' will just be converted to 'x'.
#'
#' @param x A string where all capital letters should be converted to small
#' caps.
#'
#' @return A string where all capital letters have been converted to small caps.
#' @export
#'
#' @examples
#' my_string <- 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
#' conv2sc(my_string)
conv2sc <- function(x) {
  x <- gsub('A', '\u1d00', x)
  x <- gsub('B', '\u0299', x)
  x <- gsub('C', '\u1d04', x)
  x <- gsub('D', '\u1d05', x)
  x <- gsub('E', '\u1d07', x)
  x <- gsub('F', '\u0493', x)
  x <- gsub('G', '\u0262', x)
  x <- gsub('H', '\u029c', x)
  x <- gsub('I', '\u026a', x)
  x <- gsub('J', '\u1d0a', x)
  x <- gsub('K', '\u1d0b', x)
  x <- gsub('L', '\u029f', x)
  x <- gsub('M', '\u1d0d', x)
  x <- gsub('N', '\u0274', x)
  x <- gsub('O', '\u1d0f', x)
  x <- gsub('P', '\u1d18', x)
  x <- gsub('Q', '\u01eb', x)
  x <- gsub('R', '\u0280', x)
  x <- gsub('S', '\ua731', x)
  x <- gsub('T', '\u1d1b', x)
  x <- gsub('U', '\u1d1c', x)
  x <- gsub('V', '\u1d20', x)
  x <- gsub('W', '\u1d21', x)
  x <- gsub('X', 'x', x)
  x <- gsub('Y', '\u028f', x)
  x <- gsub('Z', '\u1d22', x)
  return(x)
}
