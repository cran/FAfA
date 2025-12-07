# Confirmatory Factor Analysis (CFA) Server Module
 
#' Coalesce for NULL/NA or empty values (Local Helper)
#' @param x The primary value.
#' @param y The fallback value.
#' @return x if valid, otherwise y.
#' @noRd
# Helper function for safe access (coalesce for NULL/NA/empty)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

#' Confirmatory Factor Analysis (CFA) Server Module Logic
#'
#' Handles the server-side logic for performing CFA, including dynamic UI updates,
#' running the analysis, and rendering results like fit measures, factor loadings,
#' modification indices, and path diagrams.
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param data A reactive expression returning the current dataset.
#'
#' @import shiny
#' @importFrom lavaan lavaanify
#' @importFrom lavaan cfa
#' @importFrom lavaan fitMeasures
#' @importFrom lavaan standardizedSolution
#' @importFrom lavaan modificationIndices
#' @importFrom semPlot semPaths
#' @importFrom utils write.csv
#' @importFrom grDevices svg dev.off
#' @importFrom graphics plot text title
#' @importFrom stats na.omit
#' @noRd
cfa_server <- function(input, output, session, data) {
  # reactiveValues to store CFA results
  cfa_analysis_results_rv <- reactiveValues(
    lavaan_object = NULL,
    fit_measures_df = NULL,
    factor_loadings_df = NULL,
    modification_indices_df = NULL,
    path_diagram_plot = NULL
  )

  # Dynamically update estimator choices based on selected correlation matrix type
  observeEvent(input$cfa_correlation_type_radio, {
    req(input$cfa_correlation_type_radio) # Ensure selection is made

    estimator_choices_list <- if (input$cfa_correlation_type_radio == "pea") { # Pearson (continuous data)
      c("Default (ML if complete, MLR if missing)" = "default", "ML", "MLR", "MLM", "MLMV", "GLS")
    } else { # Polychoric (ordinal data)
      c("Default (WLSMV if categorical)" = "default", "WLSMV", "ULSMV", "DWLS")
    }
    # Default estimator selection logic
    selected_estimator_val <- if (input$cfa_correlation_type_radio == "pea") "default" else "default"

    updateSelectInput(session, "cfa_estimator_select",
                      choices = estimator_choices_list,
                      selected = selected_estimator_val)
  }, ignoreNULL = FALSE, ignoreInit = FALSE) # ignoreInit=FALSE to run on startup with default "pea"

  # Main observer for running the CFA
  observeEvent(input$run_cfa_button, {
    # --- Validations ---
    validate(
      need(data(), "Please upload your dataset to run CFA."),
      need(input$cfa_model_syntax_input, "Please define the factor structure model using lavaan syntax."),
      need(input$cfa_correlation_type_radio, "Please select a data type / correlation matrix."),
      need(input$cfa_estimator_select, "Please select an estimator.")
    )

    current_data <- data() # Get the reactive data (assumed to be cleaned by app.R)
    model_syntax <- input$cfa_model_syntax_input
    correlation_type <- input$cfa_correlation_type_radio
    chosen_estimator <- input$cfa_estimator_select

    # Handle "default" estimator selection
    if (chosen_estimator == "default") {
        if (correlation_type == "poly") {
            chosen_estimator <- "WLSMV"
        } else {
            # For continuous, lavaan defaults to ML if data is complete.
            # Our clean_missing_data in app.R aims for complete cases via listwise deletion.
            chosen_estimator <- "ML"
        }
    }

    # Specific validation for estimator and correlation type compatibility
    if (correlation_type == "pea" && chosen_estimator %in% c("WLSMV", "ULSMV", "DWLS")) {
      showNotification(
        paste0("Error: The estimator '", chosen_estimator, "' is typically used with ordinal data (Polychoric correlations). ",
               "For Pearson correlations, please select an estimator like ML, MLR, or GLS."),
        type = "error", duration = 10
      )
      return() # Stop execution
    }
    if (correlation_type == "poly" && chosen_estimator %in% c("ML", "GLS")) {
        # ML can be used with polychoric if using full data and `missing="FIML"` but our setup uses listwise deletion.
        # GLS is generally for continuous.
         showNotification(
            paste0("Warning: The estimator '", chosen_estimator, "' with Polychoric correlations might be suboptimal or require specific data conditions. ",
                   "Consider WLSMV, ULSMV, or DWLS for ordinal data."),
            type = "warning", duration = 10
        )
    }


    # Show a notification that analysis is starting
    progress_id <- showNotification("Running Confirmatory Factor Analysis...", duration = NULL, type = "message")
    on.exit(removeNotification(progress_id), add = TRUE)

    # Determine 'ordered' argument for lavaan based on correlation type
    # If "poly", all manifest variables in the model are treated as ordered.
    manifest_vars <- unique(unlist(lavaan::lavaanify(model_syntax)$rhs[lavaan::lavaanify(model_syntax)$op == "=~"]))
    ordered_arg_lavaan <- if (correlation_type == "poly" && length(manifest_vars) > 0) manifest_vars else FALSE


    # --- Perform CFA ---
    tryCatch({
      cfa_fit_object <- lavaan::cfa(
        model = model_syntax,
        data = current_data,
        ordered = ordered_arg_lavaan,
        estimator = chosen_estimator,
		missing = 'listwise'
        # Add other lavaan options if needed, e.g., missing = 'listwise' (though data should be pre-cleaned)
        # std.lv = TRUE (to fix latent variances to 1 for identification, common practice)
      )
      cfa_analysis_results_rv$lavaan_object <- cfa_fit_object

      # --- Extract and Display Fit Measures ---
      if (!is.null(cfa_fit_object)) {
        fit_indices_to_get <- c(
          "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr",
          "chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "cfi.robust", "tli.robust",
          "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
          "srmr_bentler" # For robust estimators
        )
        fm_raw <- tryCatch(lavaan::fitMeasures(cfa_fit_object, fit.measures = fit_indices_to_get), error = function(e) NULL)

        if(!is.null(fm_raw)){
            # 1. Temel İndeksler (Ki-Kare, df, p) - Her zaman Scaled öncelikli
           chi_val <- fm_raw["chisq.scaled"] %||% fm_raw["chisq"] %||% NA
           df_val <- fm_raw["df.scaled"] %||% fm_raw["df"] %||% NA
           pval_val <- fm_raw["pvalue.scaled"] %||% fm_raw["pvalue"] %||% NA

           # 2. SRMR Seçimi (Kategorik veride 'srmr_bentler' daha doğrudur)
           srmr_val <- if(correlation_type == "poly") {
              fm_raw["srmr_bentler"] %||% fm_raw["srmr"] %||% NA
           } else {
              fm_raw["srmr"] %||% fm_raw["srmr_bentler"] %||% NA
           }

           # 3. Uyum İndeksleri (CFI, TLI, RMSEA)
           if (correlation_type == "poly") {
              # --- KATEGORİK (Mplus ULSMV/WLSMV Uyumu) ---
              # Mplus uyumu için 'scaled' değerleri esastır.
              cfi_val   <- fm_raw["cfi.scaled"] %||% fm_raw["cfi.robust"] %||% fm_raw["cfi"] %||% NA
              tli_val   <- fm_raw["tli.scaled"] %||% fm_raw["tli.robust"] %||% fm_raw["tli"] %||% NA
              rmsea_val <- fm_raw["rmsea.scaled"] %||% fm_raw["rmsea.robust"] %||% fm_raw["rmsea"] %||% NA
              
              rmsea_low <- fm_raw["rmsea.ci.lower.scaled"] %||% fm_raw["rmsea.ci.lower.robust"] %||% fm_raw["rmsea.ci.lower"] %||% NA
              rmsea_upp <- fm_raw["rmsea.ci.upper.scaled"] %||% fm_raw["rmsea.ci.upper.robust"] %||% fm_raw["rmsea.ci.upper"] %||% NA
              
           } else {
              # --- SÜREKLİ (MLR Uyumu) ---
              # Standart Robust analizlerde 'robust' değerleri esastır.
              cfi_val   <- fm_raw["cfi.robust"] %||% fm_raw["cfi.scaled"] %||% fm_raw["cfi"] %||% NA
              tli_val   <- fm_raw["tli.robust"] %||% fm_raw["tli.scaled"] %||% fm_raw["tli"] %||% NA
              rmsea_val <- fm_raw["rmsea.robust"] %||% fm_raw["rmsea.scaled"] %||% fm_raw["rmsea"] %||% NA
              
              rmsea_low <- fm_raw["rmsea.ci.lower.robust"] %||% fm_raw["rmsea.ci.lower.scaled"] %||% fm_raw["rmsea.ci.lower"] %||% NA
              rmsea_upp <- fm_raw["rmsea.ci.upper.robust"] %||% fm_raw["rmsea.ci.upper.scaled"] %||% fm_raw["rmsea.ci.upper"] %||% NA
           }
					
            chi_df_ratio <- if (!is.na(df_val) && df_val > 0) (chi_val / df_val) else NA

            fit_measures_for_table <- data.frame(
              Measure = c("Chi-Square", "Degrees of Freedom (df)", "Chi-Sq/df Ratio", "p-value",
                          "CFI", "TLI (NNFI)", "RMSEA", "RMSEA 90% CI Lower", "RMSEA 90% CI Upper", "SRMR"),
              Value = c(chi_val, df_val, chi_df_ratio, pval_val,
                        cfi_val, tli_val, rmsea_val, rmsea_low, rmsea_upp, srmr_val),
              stringsAsFactors = FALSE
            )
            # Round numeric values for display
            fit_measures_for_table$Value <- ifelse(sapply(fit_measures_for_table$Value, is.numeric) & !is.na(fit_measures_for_table$Value),
                                                   round(as.numeric(fit_measures_for_table$Value), 3),
                                                   fit_measures_for_table$Value)
            fit_measures_for_table$Value[fit_measures_for_table$Measure == "p-value"] <- format.pval(pval_val, digits=3, eps=0.001)


            cfa_analysis_results_rv$fit_measures_df <- fit_measures_for_table
        } else {
            cfa_analysis_results_rv$fit_measures_df <- data.frame(Message = "Could not retrieve fit measures.")
        }

        output$cfa_fit_measures_table <- renderTable({
          req(cfa_analysis_results_rv$fit_measures_df)
          cfa_analysis_results_rv$fit_measures_df
        }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)
      }

      # --- Extract and Display Standardized Factor Loadings ---
      if (!is.null(cfa_fit_object)) {
        std_solution <- tryCatch(lavaan::standardizedSolution(cfa_fit_object, type = "std.all", ci = TRUE, level = 0.95), error = function(e) NULL)
        if(!is.null(std_solution)){
            loadings_data <- std_solution[std_solution$op == "=~",
                                          c("lhs", "rhs", "est.std", "se", "z", "pvalue", "ci.lower", "ci.upper")]
            colnames(loadings_data) <- c("Latent_Factor", "Indicator_Variable", "Std_Loading",
                                         "Std_Error", "Z_value", "P_value", "CI_Lower_95", "CI_Upper_95")

            # Round numeric columns
            numeric_cols_ld <-sapply(loadings_data, is.numeric)
            loadings_data[numeric_cols_ld] <- lapply(loadings_data[numeric_cols_ld], round, 3)
            loadings_data$"P_value" <- format.pval(as.numeric(loadings_data$"P_value"), digits=3, eps=0.001)

            cfa_analysis_results_rv$factor_loadings_df <- loadings_data
        } else {
            cfa_analysis_results_rv$factor_loadings_df <- data.frame(Message = "Could not retrieve standardized factor loadings.")
        }

        output$cfa_factor_loadings_table <- renderTable({
          req(cfa_analysis_results_rv$factor_loadings_df)
          cfa_analysis_results_rv$factor_loadings_df
        }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)
      }

      # --- Extract and Display Modification Indices (Top 20) ---
      if (!is.null(cfa_fit_object)) {
        mod_indices <- tryCatch(lavaan::modificationIndices(cfa_fit_object, sort. = TRUE, minimum.value = 3.84, maximum.number = 20, free.remove = FALSE, op = c("=~", "~~")), error=function(e)NULL) # Common ops
        if(!is.null(mod_indices) && nrow(mod_indices)>0){
            # Select and rename columns for better readability
            mod_indices_display <- mod_indices[, c("lhs", "op", "rhs", "mi", "epc"), drop = FALSE] # Added epc.std if available
            if("sepc.all" %in% colnames(mod_indices)) mod_indices_display$sepc.all <- mod_indices$sepc.all
            if("power" %in% colnames(mod_indices)) mod_indices_display$power <- mod_indices$power

            colnames(mod_indices_display)[colnames(mod_indices_display) == "mi"] <- "Modification_Index (MI)"
            colnames(mod_indices_display)[colnames(mod_indices_display) == "epc"] <- "Expected_Parameter_Change (EPC)"
            if("sepc.all" %in% colnames(mod_indices_display)) colnames(mod_indices_display)[colnames(mod_indices_display) == "sepc.all"] <- "Std_EPC_All"
            if("power" %in% colnames(mod_indices_display)) colnames(mod_indices_display)[colnames(mod_indices_display) == "power"] <- "Power_to_Detect_Misspec"


            numeric_cols_mi <-sapply(mod_indices_display, is.numeric)
            mod_indices_display[numeric_cols_mi] <- lapply(mod_indices_display[numeric_cols_mi], round, 3)

            cfa_analysis_results_rv$modification_indices_df <- mod_indices_display
        } else if (!is.null(mod_indices) && nrow(mod_indices) == 0) {
            cfa_analysis_results_rv$modification_indices_df <- data.frame(Message = "No modification indices above the threshold (3.84) or the model is saturated.")
        } else {
            cfa_analysis_results_rv$modification_indices_df <- data.frame(Message = "Could not retrieve modification indices.")
        }
        output$cfa_modification_indices_table <- renderTable({
          req(cfa_analysis_results_rv$modification_indices_df)
          cfa_analysis_results_rv$modification_indices_df
        }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)
      }

      # --- Generate and Display Path Diagram ---
      if (!is.null(cfa_fit_object)) {
        # semPlot can be slow with complex models or large datasets
        # Using a reactiveVal for the plot object itself might be better if it's computationally intensive
        # For now, direct rendering.
        output$cfa_path_diagram_output <- renderPlot({
          req(cfa_analysis_results_rv$lavaan_object) # Ensure the lavaan object is available
          semPlot::semPaths(
            object = cfa_analysis_results_rv$lavaan_object,
            what = "std",             # Show standardized parameters
            whatLabels = "std",       # Label edges with standardized estimates
            intercepts = FALSE,       # Typically do not show intercepts in path diagrams
            layout = "tree",          # Common layout for CFA, 'spring' is also good
            edge.label.cex = 0.8,     # Adjust edge label size
            sizeMan = 6,              # Size of manifest variables
            sizeLat = 9,              # Size of latent variables
            edge.color = "black",
            rotation = 2,             # For 'tree' layout, 1=top-bottom, 2=left-right, etc.
            style = "lisrel",         # LISREL style boxes
            nCharNodes = 0,           # Don't abbreviate node names
            curve = 1.5,              # Curvature for covariances
            residuals = TRUE,         # Show residual variances
            residScale = 8,           # Scale for residual variance arrows/circles
            theme = "gray"
          )
          title(main = paste("CFA Path Diagram (Estimator: ", chosen_estimator, ")", sep=""), cex.main = 1.2)
        })
      }
      showNotification("CFA analysis completed successfully!", type = "message", duration = 4)

    }, error = function(e) {
      showNotification(
        paste("Error during CFA analysis:", e$message,
              "Please check your model syntax, data, and selected options (estimator, correlation type)."),
        type = "error",
        duration = 10 # Keep error message longer
      )
      # Clear previous results on error
      cfa_analysis_results_rv$lavaan_object <- NULL
      cfa_analysis_results_rv$fit_measures_df <- data.frame(Error = e$message)
      cfa_analysis_results_rv$factor_loadings_df <- data.frame(Error = e$message)
      cfa_analysis_results_rv$modification_indices_df <- data.frame(Error = e$message)
      output$cfa_fit_measures_table <- renderTable(cfa_analysis_results_rv$fit_measures_df)
      output$cfa_factor_loadings_table <- renderTable(cfa_analysis_results_rv$factor_loadings_df)
      output$cfa_modification_indices_table <- renderTable(cfa_analysis_results_rv$modification_indices_df)
      output$cfa_path_diagram_output <- renderPlot({ plot(NULL,xlim=c(0,1),ylim=c(0,1),main="Path Diagram Error"); text(0.5,0.5,e$message)})
    }) # End tryCatch
  }) # End observeEvent for run_cfa_button

  # --- Download Handlers ---
  output$download_fit_measures_button <- downloadHandler(
    filename = function() {
      paste0("cfa_fit_measures_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(cfa_analysis_results_rv$fit_measures_df)
      write.csv(cfa_analysis_results_rv$fit_measures_df, file, row.names = FALSE)
    }
  )

  output$download_factor_loadings_button <- downloadHandler(
    filename = function() {
      paste0("cfa_factor_loadings_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(cfa_analysis_results_rv$factor_loadings_df)
      write.csv(cfa_analysis_results_rv$factor_loadings_df, file, row.names = FALSE)
    }
  )

  # No download for modification indices in this version, but could be added similarly

  output$download_path_diagram_button <- downloadHandler(
    filename = function() {
      paste0("cfa_path_diagram_", Sys.Date(), ".svg") # SVG for scalable vector graphics
    },
    content = function(file) {
      req(cfa_analysis_results_rv$lavaan_object)
      # Plotting to an SVG device
      svg(file, width = 10, height = 7.5) # Adjust dimensions as needed
      semPlot::semPaths(
            object = cfa_analysis_results_rv$lavaan_object,
            what = "std", whatLabels = "std", intercepts = FALSE, layout = "tree",
            edge.label.cex = 0.8, sizeMan = 6, sizeLat = 9, edge.color = "black",
            rotation = 2, style = "lisrel", nCharNodes = 0, curve = 1.5,
            residuals = TRUE, residScale = 8, theme = "gray"
      )
      title(main = paste("CFA Path Diagram (Estimator: ", input$cfa_estimator_select, ")", sep=""), cex.main = 1.2) # Use input for estimator here
      dev.off() # Close the SVG device
    }
  )
  # No explicit return needed for server modules unless chaining data
}
