#' ItemRest Analysis Server Module
#'
#' @param id Module namespace ID.
#' @param data Reactive containing the input dataset.
#' @import shiny
#' @importFrom stats na.omit
#' @importFrom utils capture.output
#' @export
mod_itemrest_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Store results: 'res' is the object, 'console_output' is the text printed to console
    analysis_results <- reactiveVal(list(res = NULL, console_output = NULL))

    observeEvent(input$run_itemrest, {
      req(data())

      # Check if ItemRest package is available
      if (!requireNamespace("ItemRest", quietly = TRUE)) {
        showNotification("Package 'ItemRest' is required but not installed.", type = "error")
        return()
      }

      # Data preparation
      df <- data()
      if(!all(sapply(df, is.numeric))) {
        showNotification("ItemRest requires all variables to be numeric.", type = "error")
        return()
      }
      df_clean <- stats::na.omit(df)

      # Prepare arguments
      num_factors_arg <- if(is.na(input$n_factors)) NULL else input$n_factors

      showNotification("Running ItemRest automation strategies...", type = "message", duration = NULL, id = "ir_progress")

      tryCatch({
        # capture.output ile konsola basılan her şeyi yakalıyoruz
        captured_txt <- utils::capture.output({
          res <- ItemRest::itemrest(
            data = df_clean,
            cor_method = input$cor_method,
            n_factors = num_factors_arg,
            extract = input$extraction_method,
            rotate = input$rotation_method
          )
        })

        # Hem nesneyi hem de yakalanan metni sakla
        analysis_results(list(res = res, console_output = captured_txt))

        removeNotification("ir_progress")
        showNotification("Analysis Complete!", type = "message")

      }, error = function(e) {
        removeNotification("ir_progress")
        showNotification(paste("Analysis Failed:", e$message), type = "error", duration = 10)
      })
    })

    # Output 1: Optimal Strategy Text
    output$optimal_strategy_text <- renderPrint({
      req(analysis_results())
      out_data <- analysis_results()
      res <- out_data$res
      txt <- out_data$console_output

      if (!is.null(res) && "optimal_strategy" %in% names(res) && !is.null(res$optimal_strategy)) {
        print(res$optimal_strategy)
      }
      else if (!is.null(txt) && length(txt) > 0) {
        cat(paste(txt, collapse = "\n"))
      }
      else {
        cat("Optimal strategy details not found in result object or console output.")
      }
    })

    # Output 2: Removal Summary Table
    output$removal_summary_table <- renderTable({
      req(analysis_results())
      out_data <- analysis_results()
      res <- out_data$res

      if (!is.null(res) && "removal_summary" %in% names(res) && !is.null(res$removal_summary)) {
        return(res$removal_summary)
      } else {
        return(data.frame(Message = "Summary table not returned. Please check the text output above."))
      }
    }, striped = TRUE, hover = TRUE, digits = 3)

  })
}
