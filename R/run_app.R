#' Run the Shiny Application
#'
#' This function launches the Shiny application for performing comprehensive factor analysis.
#' The application allows users to upload datasets, check assumptions, manipulate data, perform
#' exploratory and confirmatory factor analysis, and conduct reliability analysis and item weighting.
#'
#' The application supports multiple languages. By default, it runs in English. However, it can
#' be switched to Turkish by setting the `lang` parameter to "tr".
#'
#' @param lang A character string specifying the language of the application. Options are "eng"
#' (default) for English and "tr" for Turkish.
#'
#' @return This function does not return a value. It launches the Shiny application interface.
#'
#' @param onStart A function to execute when the app starts.
#' @param options A list of options to be passed to `shiny::shinyApp`.
#' @param enableBookmarking Enable bookmarking in the app.
#' @param uiPattern A pattern to be matched for the app's UI.
#' @param ... Additional arguments to pass to `golem_opts`.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#'
#'
run_app <- function(
    lang = "eng",
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  if (lang == "eng") {
    with_golem_options(
      app = shinyApp(
        ui = app_ui,
        server = app_server,
        onStart = onStart,
        options = options,
        enableBookmarking = enableBookmarking,
        uiPattern = uiPattern
      ),
      golem_opts = list(...)
    )
  } else {
    with_golem_options(
      app = shinyApp(
        ui = app_ui_tr,
        server = app_server_tr,
        onStart = onStart,
        options = options,
        enableBookmarking = enableBookmarking,
        uiPattern = uiPattern
      ),
      golem_opts = list(lang = lang, ...)
    )
  }
}
