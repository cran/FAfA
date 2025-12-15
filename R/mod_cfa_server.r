#' Confirmatory Factor Analysis (CFA) Server Module
#'
#' @param id Module namespace ID.
#' @param data Reactive containing the input dataset.
#' @importFrom utils head
#' @export
cfa_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Helper for safe NULL/NA access
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

    cfa_analysis_results_rv <- reactiveValues(
      lavaan_object = NULL,
      fit_measures_df = NULL,
      factor_loadings_df = NULL,
      modification_indices_df = NULL
    )

    # --- 1. MODEL BUILDER LOGIC ---

    observe({
      req(data())
      updateSelectizeInput(session, "builder_items", choices = names(data()))
      updateSelectizeInput(session, "builder_cov_items", choices = names(data()))
    })

    # Helper to append text
    append_syntax <- function(new_text) {
      current <- input$cfa_model_syntax_input
      if (is.null(current) || current == "") {
        updateTextAreaInput(session, "cfa_model_syntax_input", value = new_text)
      } else {
        updateTextAreaInput(session, "cfa_model_syntax_input", value = paste(current, new_text, sep = "\n"))
      }
    }

    # Add Factor (=~)
    observeEvent(input$btn_add_to_model, {
      req(input$builder_factor_name, input$builder_items)
      f_name <- trimws(input$builder_factor_name)
      items_str <- paste(input$builder_items, collapse = " + ")
      new_line <- paste0(f_name, " =~ ", items_str)
      append_syntax(new_line)
      updateTextInput(session, "builder_factor_name", value = "")
      updateSelectizeInput(session, "builder_items", selected = character(0))
    })

    # Add Covariance (~~)
    observeEvent(input$btn_add_cov, {
      req(input$builder_cov_items)
      if(length(input$builder_cov_items) != 2) {
        showNotification("Select exactly 2 variables.", type = "warning")
        return()
      }
      new_line <- paste0(input$builder_cov_items[1], " ~~ ", input$builder_cov_items[2])
      append_syntax(new_line)
      updateSelectizeInput(session, "builder_cov_items", selected = character(0))
    })

    # --- 2. DYNAMIC ESTIMATOR SELECTION ---
    observeEvent(input$cfa_correlation_type_radio, {
      req(input$cfa_correlation_type_radio)
      estimator_choices <- if (input$cfa_correlation_type_radio == "pea") {
        c("Default (MLR)" = "default", "MLR", "ML", "GLS")
      } else {
        c("Default (WLSMV)" = "default", "WLSMV", "ULSMV", "DWLS")
      }
      updateSelectInput(session, "cfa_estimator_select", choices = estimator_choices, selected = "default")
    })

    # --- 3. RUN ANALYSIS (Mplus Compatible) ---
    observeEvent(input$run_cfa_button, {
      validate(
        need(data(), "Upload data."),
        need(input$cfa_model_syntax_input, "Define model.")
      )

      current_data <- data()
      model_syntax <- input$cfa_model_syntax_input
      corr_type <- input$cfa_correlation_type_radio
      est_sel <- input$cfa_estimator_select

      final_estimator <- est_sel
      if (est_sel == "default") {
        final_estimator <- if (corr_type == "poly") "WLSMV" else "MLR"
      }

      manifest_vars <- unique(unlist(lavaan::lavaanify(model_syntax)$rhs[lavaan::lavaanify(model_syntax)$op == "=~"]))
      ordered_arg <- if (corr_type == "poly") manifest_vars else FALSE

      progress_id <- showNotification("Running CFA...", duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        fit <- lavaan::cfa(
          model = model_syntax,
          data = current_data,
          ordered = ordered_arg,
          estimator = final_estimator,
          missing = "listwise",
          mimic = "Mplus" # Critical for Mplus match
        )
        cfa_analysis_results_rv$lavaan_object <- fit

        # Fit Measures
        fm_raw <- tryCatch(lavaan::fitMeasures(fit), error = function(e) NULL)
        if(!is.null(fm_raw)) {
          chi <- fm_raw["chisq.scaled"] %||% fm_raw["chisq"]
          df  <- fm_raw["df.scaled"] %||% fm_raw["df"]
          p   <- fm_raw["pvalue.scaled"] %||% fm_raw["pvalue"]
          cfi <- fm_raw["cfi.scaled"] %||% fm_raw["cfi.robust"] %||% fm_raw["cfi"]
          tli <- fm_raw["tli.scaled"] %||% fm_raw["tli.robust"] %||% fm_raw["tli"]
          rmsea     <- fm_raw["rmsea.scaled"] %||% fm_raw["rmsea.robust"] %||% fm_raw["rmsea"]
          rmsea_low <- fm_raw["rmsea.ci.lower.scaled"] %||% fm_raw["rmsea.ci.lower.robust"] %||% fm_raw["rmsea.ci.lower"]
          rmsea_upp <- fm_raw["rmsea.ci.upper.scaled"] %||% fm_raw["rmsea.ci.upper.robust"] %||% fm_raw["rmsea.ci.upper"]
          srmr <- fm_raw["srmr"] %||% fm_raw["srmr_bentler"]

          cfa_analysis_results_rv$fit_measures_df <- data.frame(
            Measure = c("Chi-Square", "Degrees of Freedom (df)", "p-value",
                        "CFI", "TLI (NNFI)", "RMSEA", "RMSEA 90% CI Lower", "RMSEA 90% CI Upper", "SRMR"),
            Value = round(c(chi, df, p, cfi, tli, rmsea, rmsea_low, rmsea_upp, srmr), 3)
          )
          if(!is.na(p)) cfa_analysis_results_rv$fit_measures_df$Value[3] <- format.pval(p, digits=3, eps=0.001)
        }

        # Loadings
        std_sol <- lavaan::standardizedSolution(fit)
        loadings <- std_sol[std_sol$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
        colnames(loadings) <- c("Factor", "Item", "Std. Estimate", "SE", "p-value")
        loadings[,3:4] <- round(loadings[,3:4], 3)
        loadings[,5]   <- format.pval(loadings[,5], digits=3, eps=0.001)
        cfa_analysis_results_rv$factor_loadings_df <- loadings

        # Mod Indices
        mod_ind <- lavaan::modificationIndices(fit, sort. = TRUE, minimum.value = 3.84)
        cfa_analysis_results_rv$modification_indices_df <- head(mod_ind[, c("lhs","op","rhs","mi","epc")], 20)

        showNotification("CFA Analysis Complete!", type = "message")

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        cfa_analysis_results_rv$lavaan_object <- NULL
      })
    })

    # --- 4. RENDER OUTPUTS (FIXED GRAPHIC) ---

    output$cfa_path_diagram_output <- renderPlot({
      req(cfa_analysis_results_rv$lavaan_object)

      # UI Controls
      selected_layout <- input$plot_layout %||% "tree"
      rotation_val    <- input$plot_rotation %||% 2
      box_width       <- input$plot_man_size %||% 10
      label_cex       <- input$plot_edge_label_cex %||% 0.8

      if(selected_layout == "tree2") {
        selected_layout <- "tree"
        rotation_val <- 2
      }

      what_labels <- if(!is.null(input$plot_show_labels) && input$plot_show_labels) "std" else "hide"

      # Fix for Ordinal Data "Thick Arrows"
      # If ordinal, max residual is 1.0 (very thick). We reduce edge width scaling.
      is_poly <- (input$cfa_correlation_type_radio == "poly")
      custom_edge_width <- if(is_poly) 0.5 else 1.2

      semPlot::semPaths(
        object = cfa_analysis_results_rv$lavaan_object,
        what = "std",
        whatLabels = what_labels,
        layout = selected_layout,
        rotation = rotation_val,

        # --- FIXED GRAPHICS SETTINGS ---
        shapeMan = "rectangle",
        sizeMan = box_width,
        sizeMan2 = box_width / 2, # Proportional height
        sizeLat = box_width,
        sizeLat2 = box_width / 2,

        label.cex = 1.2,
        edge.label.cex = label_cex,
        edge.width = custom_edge_width, # DYNAMIC WIDTH FIX
        edge.color = "black",
        style = "lisrel",

        intercepts = FALSE,
        thresholds = FALSE,      # FIX: Hides lines inside boxes for ordinal data
        residuals = TRUE,
        residScale = 15,         # Keeps arrows small

        reorder = FALSE,
        optimizeLatRes = TRUE,
        curve = 2.5,
        mar = c(5,5,5,5),
        nCharNodes = 0,
        theme = "gray"
      )
    })

    output$cfa_fit_measures_table <- renderTable({ cfa_analysis_results_rv$fit_measures_df }, striped = TRUE, bordered = TRUE)
    output$cfa_factor_loadings_table <- renderTable({ cfa_analysis_results_rv$factor_loadings_df }, striped = TRUE)
    output$cfa_modification_indices_table <- renderTable({ cfa_analysis_results_rv$modification_indices_df }, striped = TRUE)

    # Downloads
    output$download_fit_measures_button <- downloadHandler(
      filename = "cfa_fit_measures.csv", content = function(file) write.csv(cfa_analysis_results_rv$fit_measures_df, file)
    )
    output$download_factor_loadings_button <- downloadHandler(
      filename = "cfa_factor_loadings.csv", content = function(file) write.csv(cfa_analysis_results_rv$factor_loadings_df, file)
    )
    output$download_path_diagram_button <- downloadHandler(
      filename = "cfa_path_diagram.svg",
      content = function(file) {
        svg(file, width = 12, height = 8)

        selected_layout <- input$plot_layout %||% "tree"
        rotation_val <- input$plot_rotation %||% 2
        box_width <- input$plot_man_size %||% 10
        if(selected_layout == "tree2") { selected_layout <- "tree"; rotation_val <- 2 }
        is_poly <- (input$cfa_correlation_type_radio == "poly")
        custom_edge_width <- if(is_poly) 0.5 else 1.2

        semPlot::semPaths(
          object = cfa_analysis_results_rv$lavaan_object,
          what = "std", whatLabels = "std",
          layout = selected_layout, rotation = rotation_val,
          shapeMan = "rectangle", sizeMan = box_width, sizeMan2 = box_width/2,
          edge.width = custom_edge_width,
          thresholds = FALSE, # Also for download
          reorder = FALSE, residScale = 15,
          edge.color = "black", style = "lisrel", intercepts = FALSE, residuals = TRUE, nCharNodes = 0
        )
        dev.off()
      }
    )
  })
}
