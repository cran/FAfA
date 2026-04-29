#' Assumptions Server Logic (Modern)
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @export
assumptions_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    results_rv <- reactiveValues(desc = NULL, multi = NULL, norm = NULL)

    observeEvent(input$run_descriptives_button, {
      req(data())
      res <- assumptions(data())
      results_rv$desc <- res$descriptives
    })

    observeEvent(input$run_collinearity_button, {
      req(data())
      res <- assumptions(data())
      results_rv$multi <- res$multicollinearity
    })

    observeEvent(input$run_normality_tests_button, {
      req(data())
      progress_id <- showNotification("Running normality tests...",
                                      duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        res            <- assumptions(data())
        norm_df        <- res$mvn_table

        # Format p-values for display
        norm_df[["p-value"]] <- sapply(norm_df[["p-value"]], function(p) {
          if (is.na(p)) return(NA_character_)
          if (p < 0.001) "< .001" else as.character(round(p, 3))
        })

        results_rv$norm <- norm_df
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 8)
      })
    })

    output$descriptives_table_output          <- renderTable({ results_rv$desc  }, rownames = TRUE)
    output$collinearity_table_output          <- renderTable({ results_rv$multi })
    output$multivariate_normality_table_output <- renderTable({
      validate(need(results_rv$norm, "Click 'Run Normality Tests' to compute results."))
      results_rv$norm
    }, striped = TRUE, bordered = TRUE, na = "-")

    output$download_descriptives_button <- downloadHandler(
      filename = "descriptives.csv",
      content  = function(f) write.csv(results_rv$desc, f)
    )
  })
}
