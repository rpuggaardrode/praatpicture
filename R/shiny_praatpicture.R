#' Run praatpicture as Shiny app
#'
#' Interactive version of [praatpicture]
#'
#' @return No return values
#' @export
#'
#' @examples
#' \dontrun{
#' shiny_praatpicture()
#' }
shiny_praatpicture <- function() {
  appDir <- system.file('shiny', 'shiny_praatpicture', package='praatpicture')
  shiny::runApp(appDir, display.mode = 'normal')
}
