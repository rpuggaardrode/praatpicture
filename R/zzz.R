.onLoad <- function(...) {
  Sys.setenv('RETICULATE_PYTHON' = 'managed')
  reticulate::py_require('praat-parselmouth')
}
