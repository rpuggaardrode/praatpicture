#' Make Praat Picture style plots in EMU
#'
#' Generate plots in the style of Praat Pictures from sound files and annotations
#' in an EMU database.
#'
#' @param db_handle The handle of an EMU database loaded into R.
#' @param session String giving the name of the session where the sound file
#' to plot is located.
#' Default is `0000`.
#' @param bundle String giving the name of the bundle with the sound file to plot.
#' @param pitch_ssffExt String giving the file extension for an SSFF track with
#' pitch data to plot. Default is `NULL`.
#' @param formant_ssffExt String giving the file extension for an SSFF track
#' with formant data to plot. Default is `NULL`.
#' @param intensity_ssffExt
#' String giving the file extension for an SSFF track with intensity data to
#' plot. Default is `NULL`.
#' @param talking Boolean; should a video be created with embedded audio, as
#' when using [talking_praatpicture]? Default is `FALSE`.
#' @param ... Further arguments passed to [praatpicture] (or
#' [talking_praatpicture] if `talking = TRUE`).
#'
#' @seealso See [praatpicture] for more details on how to customize plots and
#' [talking_praatpicture] for more details on how to customize videos.
#' @return No return value, produces a plot or a video.
#' @export
#'
#' @examples
#' \donttest{
#' # Create demo data and load demo database
#' emuR::create_emuRdemoData(tempdir())
#' db_path <- paste0(tempdir(), '/emuR_demoData/ae_emuDB')
#' db <- emuR::load_emuDB(db_path)
#'
#' emuR::list_bundles(db)
#' emupicture(db, bundle='msajc003', tg_tiers=c('Text', 'Tone'))
#'
#' # Plot SSFF track data
#'
#' emuR::list_ssffTrackDefinitions(db)
#' emupicture(db, bundle='msajc003', frames=c('sound', 'formant'),
#' proportion=c(30,70), formant_ssffExt='fms', formant_number=4)
#' }
emupicture <- function(db_handle, session='0000', bundle,
                       pitch_ssffExt=NULL, formant_ssffExt=NULL,
                       intensity_ssffExt=NULL, talking=FALSE, ...) {
  tg <- FALSE
  args <- list(...)
  if ('frames' %in% names(args) & 'TextGrid' %in% args$frames) tg <- TRUE
  if (!'frames' %in% names(args)) tg <- TRUE
  if ('tg_tiers' %in% names(args)) {
    attrNames <- args$tg_tiers
  } else {
    attrNames <- NULL
  }
  if (tg) {
    emuR::export_TextGridCollection(db_handle, targetDir='.',
                                    sessionPattern=session,
                                    bundlePattern=bundle,
                                    attributeDefinitionNames = attrNames,
                                    verbose=F)
    fn <- paste0(session, '/', bundle, '.wav')
  } else {
    fn <- paste0(db_handle$basePath, '/', session, '_ses/', bundle, '_bndl/',
                 bundle, '.wav')
  }

  if (!is.null(pitch_ssffExt)) {
    pfn <- paste0(db_handle$basePath, '/', session, '_ses/', bundle, '_bndl/',
                  bundle, '.', pitch_ssffExt)
    wpt <- wrassp::read.AsspDataObj(pfn)
  } else {
    wpt <- NULL
  }
  if (!is.null(formant_ssffExt)) {
    ffn <- paste0(db_handle$basePath, '/', session, '_ses/', bundle, '_bndl/',
                  bundle, '.', formant_ssffExt)
    wft <- wrassp::read.AsspDataObj(ffn)
  } else {
    wft <- NULL
  }
  if (!is.null(intensity_ssffExt)) {
    ifn <- paste0(db_handle$basePath, '/', session, '_ses/', bundle, '_bndl/',
                  bundle, '.', intensity_ssffExt)
    wit <- wrassp::read.AsspDataObj(ifn)
  } else {
    wit <- NULL
  }

  if (talking) {
    talking_praatpicture(fn, tg_specialChar=FALSE,
                         pitch_ssff=wpt, formant_ssff=wft, intensity_ssff=wit,
                         ...)
  } else {
    praatpicture(fn, tg_specialChar=FALSE,
                 pitch_ssff=wpt, formant_ssff=wft, intensity_ssff=wit, ...)
  }

  if (tg) unlink(session, recursive=T)
}
