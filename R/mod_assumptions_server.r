#' Assumptions Server Logic
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @noRd
assumptions_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    results_rv <- reactiveValues(desc = NULL, multi = NULL, norm = NULL)

    # Cache assumptions() result; descriptives, collinearity, and
    # normality all read from the same computation.
    cached_results <- reactiveVal(NULL)

    get_assumptions <- function() {
      if (is.null(cached_results())) {
        cached_results(assumptions(data()))
      }
      cached_results()
    }

    # Invalidate cache when upstream data changes
    observeEvent(data(), {
      cached_results(NULL)
      results_rv$desc <- NULL
      results_rv$multi <- NULL
      results_rv$norm  <- NULL
    }, ignoreNULL = TRUE)

    observeEvent(input$run_descriptives_button, {
      req(data())
      res <- get_assumptions()
      results_rv$desc <- res$descriptives
    })

    observeEvent(input$run_collinearity_button, {
      req(data())
      res <- get_assumptions()
      results_rv$multi <- res$multicollinearity
    })

    observeEvent(input$run_normality_tests_button, {
      req(data())
      progress_id <- showNotification("Running normality tests...",
                                      duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        res     <- get_assumptions()
        norm_df <- res$mvn_table

        # Format p-values
        norm_df[["p-value"]] <- sapply(norm_df[["p-value"]], function(p) {
          if (is.na(p)) return(NA_character_)
          if (p < 0.001) "< .001" else as.character(round(p, 3))
        })

        results_rv$norm <- norm_df
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 8)
      })
    })

    output$descriptives_table_output           <- renderTable({ results_rv$desc  }, rownames = TRUE)
    output$collinearity_table_output           <- renderTable({ results_rv$multi })
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
