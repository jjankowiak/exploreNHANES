#' Run shiny application to explore NHANES data
#'
#' @examples
#' \dontrun{
#' exploreNHANES::run_app()
#' }
#' @export
run_app <- function() {
  appDir <- system.file("shiny-app", package = "exploreNHANES")
  if (appDir == "") {
    stop("Could not find shiny-app. Try re-installing `exploreNHANES`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
