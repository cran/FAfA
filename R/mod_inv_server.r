#' Measurement Invariance Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive)
#' @export
inv_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

    analysis_results_rv <- reactiveValues(
      fit_measures_df = NULL,
      model_comparison_df = NULL
    )


    observe({
      req(data())
      current_data <- data()
      col_names <- names(current_data)


      updateSelectizeInput(session, "builder_items", choices = col_names)
      updateSelectizeInput(session, "builder_cov_items", choices = col_names)

      is_cat <- sapply(current_data, function(x) is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(stats::na.omit(x))) <= 10))
      updateSelectInput(session, "grouping_variable_select", choices = c("", col_names[is_cat]))
    })

    append_syntax <- function(new_text) {
      current <- input$inv_model_syntax
      if (is.null(current) || current == "") {
        updateTextAreaInput(session, "inv_model_syntax", value = new_text)
      } else {
        updateTextAreaInput(session, "inv_model_syntax", value = paste(current, new_text, sep = "\n"))
      }
    }

    observeEvent(input$btn_add_to_model, {
      req(input$builder_factor_name, input$builder_items)
      f_name <- trimws(input$builder_factor_name)
      items_str <- paste(input$builder_items, collapse = " + ")
      new_line <- paste0(f_name, " =~ ", items_str)
      append_syntax(new_line)

      updateTextInput(session, "builder_factor_name", value = "")
      updateSelectizeInput(session, "builder_items", selected = character(0))
    })

    observeEvent(input$btn_add_cov, {
      req(input$builder_cov_items)
      if(length(input$builder_cov_items) != 2) {
        showNotification("Select exactly 2 variables for covariance.", type = "warning")
        return()
      }
      new_line <- paste0(input$builder_cov_items[1], " ~~ ", input$builder_cov_items[2])
      append_syntax(new_line)

      updateSelectizeInput(session, "builder_cov_items", selected = character(0))
    })

    observeEvent(input$run_invariance_button, {
      validate(
        need(data(), "Upload data."),
        need(input$inv_model_syntax, "Define model."),
        need(input$grouping_variable_select, "Select grouping var."),
        need(length(input$invariance_levels_checkbox) > 0, "Select levels.")
      )

      clean_res <- clean_missing_data(data())
      dat <- clean_res$cleaned_data

      grp <- input$grouping_variable_select
      dat[[grp]] <- as.factor(dat[[grp]])

      is_poly <- if(!is.null(input$correlation_matrix_type)) (input$correlation_matrix_type == "poly") else FALSE
      est <- if(isTRUE(is_poly)) "WLSMV" else "ML"
      ordered_arg <- if(isTRUE(is_poly)) names(dat)[sapply(dat, is.numeric)] else FALSE

      showNotification("Running Analysis...", type="message")

      tryCatch({
        mods <- list()
        levels <- input$invariance_levels_checkbox

        run_mod <- function(lvl, eq=NULL) {
          tryCatch(lavaan::cfa(input$inv_model_syntax, data=dat, group=grp, group.equal=eq, estimator=est, ordered=ordered_arg), error=function(e) NULL)
        }

        if("configural" %in% levels) mods$configural <- run_mod("Configural")
        if("metric" %in% levels)     mods$metric     <- run_mod("Metric", "loadings")
        if("scalar" %in% levels)     mods$scalar     <- run_mod("Scalar", c("loadings", "intercepts"))
        if("strict" %in% levels)     mods$strict     <- run_mod("Strict", c("loadings", "intercepts", "residuals"))

        # Fit Measures Table
        res_list <- lapply(names(mods), function(n) {
          m <- mods[[n]]
          if(is.null(m)) return(NULL)
          fm <- lavaan::fitMeasures(m)
          data.frame(Model=n, CFI=round(fm["cfi"],3), RMSEA=round(fm["rmsea"],3), ChiSq=round(fm["chisq"],2), df=fm["df"])
        })
        analysis_results_rv$fit_measures_df <- do.call(rbind, res_list)

        # Comparison
        valid_mods <- Filter(Negate(is.null), mods)
        if(length(valid_mods) > 1) {
          analysis_results_rv$model_comparison_df <- as.data.frame(lavaan::lavTestLRT(object=valid_mods[[1]], A=valid_mods[-1]))
        }

      }, error = function(e) showNotification(e$message, type="error"))
    })

    output$invariance_fit_measures_table <- renderTable({ analysis_results_rv$fit_measures_df }, striped=TRUE)
    output$model_comparison_table <- renderTable({ analysis_results_rv$model_comparison_df }, rownames=TRUE, striped=TRUE)

    output$download_fit_measures_button <- downloadHandler(
      filename="inv_fit.csv", content=function(f) write.csv(analysis_results_rv$fit_measures_df, f)
    )
    output$download_model_comparison_button <- downloadHandler(
      filename="inv_comp.csv", content=function(f) write.csv(analysis_results_rv$model_comparison_df, f)
    )
  })
}
