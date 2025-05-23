# Measurement Invariance Server Module
 
# Helper function for safe access (coalesce for NULL/NA)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# Helper function to add significance stars
add_significance_stars <- function(p_values) {
  sapply(p_values, function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01)  return("**")
    if (p < 0.05)  return("*")
    if (p < 0.1)   return(".")
    return("") 
  })
}

inv_server <- function(input, output, session, data) {
  analysis_results_rv <- reactiveValues(
    fit_measures_df = NULL,
    model_comparison_df = NULL,
    lavaan_models_list = NULL 
  )

  observe({
    req(data()) 
    current_data <- data()
    col_names <- names(current_data)
    is_potentially_categorical <- sapply(current_data, function(col_item) {
      is.factor(col_item) || is.character(col_item) || (is.numeric(col_item) && length(unique(na.omit(col_item))) <= 10)
    })
    grouping_variable_choices <- col_names[is_potentially_categorical]
    updateSelectInput(session, "grouping_variable_select",
                      choices = c("Select a grouping variable" = "", grouping_variable_choices),
                      selected = "") 
  })

  observeEvent(input$correlation_matrix_type, {
    req(input$correlation_matrix_type) 
    estimator_choices_list <- if (input$correlation_matrix_type == "pea") { 
      c("Default (ML if complete, MLR if missing)" = "default", "ML", "MLR", "MLM", "MLMV", "GLS")
    } else { 
      c("Default (WLSMV if categorical)" = "default", "WLSMV", "ULSMV", "DWLS")
    }
    selected_estimator_val <- if (input$correlation_matrix_type == "pea") "default" else "default"
    updateSelectInput(session, "estimator_method_select",
                      choices = estimator_choices_list,
                      selected = selected_estimator_val)
  })

  observeEvent(input$run_invariance_button, {
    validate(
      need(data(), "Please upload your dataset."),
      need(input$inv_model_syntax, "Please define the factor structure model using lavaan syntax."),
      need(input$grouping_variable_select != "", "Please select a grouping variable."),
      need(length(input$invariance_levels_checkbox) > 0, "Please select at least one level of invariance to test.")
    )

    progress_id <- showNotification("Starting Measurement Invariance analysis...", duration = NULL, type = "message") 
    on.exit(removeNotification(progress_id), add = TRUE) 

    if (!exists("clean_missing_data")) {
        showNotification("Error: 'clean_missing_data' function not found. Please ensure 'utils.R' is sourced.", type = "error", duration = 10) 
        return()
    }
    cleaned_data_result <- clean_missing_data(data()) 
    current_cleaned_data <- cleaned_data_result$cleaned_data
    rows_removed_count <- cleaned_data_result$removed_rows

    output$data_cleaning_summary_text <- renderText({
      if (rows_removed_count > 0) {
        paste(rows_removed_count, "rows were removed due to missing or invalid data. Remaining rows for analysis:", nrow(current_cleaned_data))
      } else {
        "No rows with missing or invalid data were found in the dataset. Proceeding with analysis."
      }
    })

    validate(
      need(nrow(current_cleaned_data) > 0, "All rows were removed after data cleaning. Please check your dataset."),
      need(input$grouping_variable_select %in% names(current_cleaned_data), "The selected grouping variable was not found in the cleaned dataset."),
      need(length(unique(na.omit(current_cleaned_data[[input$grouping_variable_select]]))) >= 2, "The grouping variable must have at least two distinct categories.")
    )
    
    current_cleaned_data[[input$grouping_variable_select]] <- as.factor(current_cleaned_data[[input$grouping_variable_select]])
    manifest_vars <- unique(unlist(lavaan::lavaanify(input$inv_model_syntax)$rhs[lavaan::lavaanify(input$inv_model_syntax)$op == "=~"]))
    ordered_arg <- if (input$correlation_matrix_type == "poly") manifest_vars else FALSE
    chosen_estimator <- input$estimator_method_select
    if (chosen_estimator == "default") {
        chosen_estimator <- if (input$correlation_matrix_type == "poly") "WLSMV" else "ML"
    }

    tryCatch({
      fitted_lavaan_models <- list()
      selected_levels <- input$invariance_levels_checkbox

      run_cfa_model <- function(level_name, group_equal_arg = NULL) {
        showNotification(paste("Running", level_name, "Model..."), type = "message", duration = NULL, id = paste0(level_name, "_msg"))
        model_fit <- tryCatch(lavaan::cfa(
          model = input$inv_model_syntax, data = current_cleaned_data, group = input$grouping_variable_select,
          group.equal = group_equal_arg, ordered = ordered_arg, estimator = chosen_estimator, 
          group.label = levels(current_cleaned_data[[input$grouping_variable_select]])
        ), error = function(e){ 
          # message(paste(level_name, "model failed:", conditionMessage(e))) # Debugging message removed
          NULL
        })
        removeNotification(paste0(level_name, "_msg"))
        return(model_fit)
      }

      if ("configural" %in% selected_levels) fitted_lavaan_models$configural <- run_cfa_model("Configural")
      if ("metric" %in% selected_levels) fitted_lavaan_models$metric <- run_cfa_model("Metric", "loadings")
      if ("scalar" %in% selected_levels) fitted_lavaan_models$scalar <- run_cfa_model("Scalar", c("loadings", "intercepts"))
      if ("strict" %in% selected_levels) fitted_lavaan_models$strict <- run_cfa_model("Strict", c("loadings", "intercepts", "residuals"))
      
      analysis_results_rv$lavaan_models_list <- fitted_lavaan_models

      if (length(fitted_lavaan_models) > 0) {
        fit_indices_to_extract <- c(
          "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic",
          "chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "cfi.robust", "tli.robust",
          "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
          "srmr_bentler" 
        )
        all_fit_measures_list <- lapply(names(fitted_lavaan_models), function(model_name) {
          model <- fitted_lavaan_models[[model_name]]
          if (!is.null(model) && inherits(model, "lavaan")) { 
            fm <- tryCatch(lavaan::fitMeasures(model, fit.measures = fit_indices_to_extract), error = function(e) {
                # message(paste("fitMeasures failed for model", model_name, ":", conditionMessage(e))) # Debugging message removed
                NULL
            })
            if (is.null(fm)) return(data.frame(Invariance_Level = model_name, Chi_Square=NA, df=NA, p_value=NA, CFI=NA, TLI=NA, RMSEA=NA, RMSEA_CI_Lower=NA, RMSEA_CI_Upper=NA, SRMR=NA, AIC=NA, BIC=NA, Chi_Sq_df_Ratio=NA, stringsAsFactors = FALSE))
            
            get_fit_val <- function(base_name, robust_suffix = ".robust", scaled_suffix = ".scaled") {
                val <- fm[paste0(base_name, scaled_suffix)] %||% fm[paste0(base_name, robust_suffix)] %||% fm[base_name] %||% NA
                return(val)
            }
            
            chi_val <- get_fit_val("chisq"); df_val <- get_fit_val("df"); pval_val <- get_fit_val("pvalue")
            cfi_val <- get_fit_val("cfi"); tli_val <- get_fit_val("tli"); rmsea_val <- get_fit_val("rmsea")
            rmsea_low <- get_fit_val("rmsea.ci.lower"); rmsea_upp <- get_fit_val("rmsea.ci.upper")
            srmr_val <- fm["srmr_bentler"] %||% fm["srmr"] %||% NA 
            aic_val <- fm["aic"] %||% NA; bic_val <- fm["bic"] %||% NA
            chi_df_ratio <- if (!is.na(df_val) && df_val > 0) (chi_val / df_val) else NA
            
            data.frame(Invariance_Level = model_name, Chi_Square = chi_val, df = df_val, p_value = pval_val, CFI = cfi_val, TLI = tli_val, RMSEA = rmsea_val, RMSEA_CI_Lower = rmsea_low, RMSEA_CI_Upper = rmsea_upp, SRMR = srmr_val, AIC = aic_val, BIC = bic_val, Chi_Sq_df_Ratio = chi_df_ratio, stringsAsFactors = FALSE)
          } else { 
            data.frame(Invariance_Level = model_name, Chi_Square=NA, df=NA, p_value=NA, CFI=NA, TLI=NA, RMSEA=NA, RMSEA_CI_Lower=NA, RMSEA_CI_Upper=NA, SRMR=NA, AIC=NA, BIC=NA, Chi_Sq_df_Ratio=NA, stringsAsFactors = FALSE)
          }
        })
        fit_measures_summary_df <- dplyr::bind_rows(all_fit_measures_list)
        analysis_results_rv$fit_measures_df <- fit_measures_summary_df
        
        output$invariance_fit_measures_table <- renderTable({
          req(analysis_results_rv$fit_measures_df); df_to_display <- analysis_results_rv$fit_measures_df
          display_cols_fit <- c("Invariance_Level", "Chi_Square", "df", "p_value", "CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC")
          df_to_display <- df_to_display[, intersect(display_cols_fit, colnames(df_to_display)), drop = FALSE]
          round_digits_cols <- c("Chi_Square", "CFI", "TLI", "RMSEA", "SRMR") 
          for(col_name in intersect(round_digits_cols, colnames(df_to_display))){
              if(is.numeric(df_to_display[[col_name]])) df_to_display[[col_name]] <- round(df_to_display[[col_name]], 3)
          }
          if("AIC" %in% colnames(df_to_display) && is.numeric(df_to_display$AIC)) df_to_display$AIC <- round(df_to_display$AIC, 1)
          if("BIC" %in% colnames(df_to_display) && is.numeric(df_to_display$BIC)) df_to_display$BIC <- round(df_to_display$BIC, 1)
          if("p_value" %in% colnames(df_to_display) && is.numeric(df_to_display$p_value)) df_to_display$p_value <- format.pval(df_to_display$p_value, digits=3, eps=0.001)
          df_to_display
        }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

        # --- Model Comparison (Using lavTestLRT) ---
        valid_lavaan_models_for_comparison <- Filter(function(m) !is.null(m) && inherits(m, "lavaan"), fitted_lavaan_models)
        
        # message("DEBUG inv_server: Number of valid lavaan models for comparison: ", length(valid_lavaan_models_for_comparison)) # Debugging message removed
        if (length(valid_lavaan_models_for_comparison) > 0) {
            # message("DEBUG inv_server: Names of valid lavaan models: ", paste(names(valid_lavaan_models_for_comparison), collapse=", ")) # Debugging message removed
        }

        if (length(valid_lavaan_models_for_comparison) > 1) {
          model_order <- c("configural", "metric", "scalar", "strict")
          existing_ordered_names <- intersect(model_order, names(valid_lavaan_models_for_comparison))
          
          if (length(existing_ordered_names) > 1) {
            ordered_models_to_compare <- valid_lavaan_models_for_comparison[existing_ordered_names]
            
            # message("DEBUG inv_server: Number of models for comparison after ordering & filtering: ", length(ordered_models_to_compare)) # Debugging message removed
            # message("DEBUG inv_server: Names of models for comparison: ", paste(names(ordered_models_to_compare), collapse=", ")) # Debugging message removed
            # message("DEBUG inv_server: Classes of models for comparison: ") # Debugging message removed
            # print(sapply(ordered_models_to_compare, class)) # Debugging message removed

            model_comparison_lrt_output <- NULL
            try_lrt_result <- try({
                 model_comparison_lrt_output <- suppressWarnings(do.call(lavaan::lavTestLRT, unname(ordered_models_to_compare)))
              }, silent = TRUE)

              if (inherits(try_lrt_result, "try-error")) {
                error_msg_lrt <- as.character(try_lrt_result) 
                # message("DEBUG inv_server: lavaan::lavTestLRT failed. Error: ", error_msg_lrt) # Debugging message removed
                analysis_results_rv$model_comparison_df <- data.frame(Message = paste("Model comparison (lavTestLRT) failed:", error_msg_lrt))
              } else if (is.null(model_comparison_lrt_output)) {
                # message("DEBUG inv_server: lavaan::lavTestLRT returned NULL or failed silently.") # Debugging message removed
                analysis_results_rv$model_comparison_df <- data.frame(Message = "Model comparison (lavTestLRT) returned NULL or failed silently.")
              } else { 
                # message("DEBUG inv_server: lavaan::lavTestLRT successful.") # Debugging message removed
                lrt_table_from_lavaan <- as.data.frame(model_comparison_lrt_output)
                # message("DEBUG inv_server: Raw lavTestLRT output table (lrt_table_from_lavaan):") # Debugging message removed
                # print(lrt_table_from_lavaan) # Debugging message removed
                
                comparison_df_final <- data.frame(
                  Model_Name_Row = paste0("fit_", existing_ordered_names), 
                  Df = NA_integer_, AIC = NA_real_, BIC = NA_real_, Chisq = NA_real_,
                  `Chisq diff` = NA_real_, RMSEA = NA_real_, `Df diff` = NA_integer_, 
                  `Pr(>Chisq)` = NA_real_, Sig = NA_character_,
                  check.names = FALSE, stringsAsFactors = FALSE
                )
                rownames(comparison_df_final) <- comparison_df_final$Model_Name_Row

                for(i in seq_along(existing_ordered_names)){
                    model_name_loop <- existing_ordered_names[i]
                    fit_m_row <- analysis_results_rv$fit_measures_df[analysis_results_rv$fit_measures_df$Invariance_Level == model_name_loop, ]
                    if(nrow(fit_m_row) == 1){
                        row_idx_target <- paste0("fit_", model_name_loop)
                        comparison_df_final[row_idx_target, "Df"]    <- fit_m_row$df
                        comparison_df_final[row_idx_target, "AIC"]   <- fit_m_row$AIC
                        comparison_df_final[row_idx_target, "BIC"]   <- fit_m_row$BIC
                        comparison_df_final[row_idx_target, "Chisq"] <- fit_m_row$Chi_Square
                        comparison_df_final[row_idx_target, "RMSEA"] <- fit_m_row$RMSEA 
                    }
                }
                
                if(nrow(lrt_table_from_lavaan) == length(existing_ordered_names)){
                    if("Chisq diff" %in% colnames(lrt_table_from_lavaan)) {
                        comparison_df_final[, "Chisq diff"] <- as.numeric(as.character(lrt_table_from_lavaan$"Chisq diff"))
                    }
                    if("Df diff" %in% colnames(lrt_table_from_lavaan)) {
                        comparison_df_final[, "Df diff"]    <- as.integer(as.character(lrt_table_from_lavaan$"Df diff"))
                    }
                    if("Pr(>Chisq)" %in% colnames(lrt_table_from_lavaan)) {
                        comparison_df_final[, "Pr(>Chisq)"] <- as.numeric(as.character(lrt_table_from_lavaan$"Pr(>Chisq)"))
                    }
                } else {
                    # message("DEBUG inv_server: lavTestLRT output row count mismatch. Diff stats might be misaligned.") # Debugging message removed
                }
                
                comparison_df_final$Sig <- add_significance_stars(comparison_df_final$"Pr(>Chisq)")
                comparison_df_final$Model_Name_Row <- NULL 
                analysis_results_rv$model_comparison_df <- comparison_df_final
              }
            } else { 
             analysis_results_rv$model_comparison_df <- data.frame(Message = "Fewer than two models available in the standard invariance sequence for comparison.")
            }
        } else { 
          analysis_results_rv$model_comparison_df <- data.frame(Message = "At least two models must be successfully fitted to perform a comparison.")
        }
        
        output$model_comparison_table <- renderTable({
            req(analysis_results_rv$model_comparison_df)
            df_to_show <- analysis_results_rv$model_comparison_df
            if(any(c("Message", "Error") %in% colnames(df_to_show))) {
                return(df_to_show)
            }
            
            if("Df" %in% colnames(df_to_show)) df_to_show$"Df" <- ifelse(is.na(df_to_show$"Df"), "", as.integer(df_to_show$"Df"))
            if("AIC" %in% colnames(df_to_show)) df_to_show$"AIC" <- ifelse(is.na(df_to_show$"AIC"), "", sprintf("%.1f", df_to_show$"AIC"))
            if("BIC" %in% colnames(df_to_show)) df_to_show$"BIC" <- ifelse(is.na(df_to_show$"BIC"), "", sprintf("%.1f", df_to_show$"BIC"))
            if("Chisq" %in% colnames(df_to_show)) df_to_show$"Chisq" <- ifelse(is.na(df_to_show$"Chisq"), "", sprintf("%.2f", df_to_show$"Chisq"))
            if("Chisq diff" %in% colnames(df_to_show)) df_to_show$"Chisq diff" <- ifelse(is.na(df_to_show$"Chisq diff"), "", sprintf("%.2f", df_to_show$"Chisq diff"))
            if("RMSEA" %in% colnames(df_to_show)) df_to_show$"RMSEA" <- ifelse(is.na(df_to_show$"RMSEA"), "", sprintf("%.5f", df_to_show$"RMSEA"))
            if("Df diff" %in% colnames(df_to_show)) df_to_show$"Df diff" <- ifelse(is.na(df_to_show$"Df diff"), "", as.integer(df_to_show$"Df diff"))
            
            if("Pr(>Chisq)" %in% colnames(df_to_show)) {
                 p_values_to_format <- df_to_show$"Pr(>Chisq)"
                 if(is.numeric(p_values_to_format)){ 
                     df_to_show$"Pr(>Chisq)" <- sapply(p_values_to_format, function(p_val) {
                        if(is.na(p_val)) return("")
                        ifelse(p_val < 0.00000000000000022, "<2.2e-16", 
                               ifelse(p_val < 0.001, sprintf("%.3e", p_val), 
                                      sprintf("%.4f", p_val))) 
                     })
                 } else { 
                     df_to_show$"Pr(>Chisq)" <- as.character(p_values_to_format)
                 }
            }
            
            if("Sig" %in% colnames(df_to_show)){
                sig_col_data <- df_to_show$Sig
                df_to_show$Sig <- NULL 
                df_to_show$Sig <- sig_col_data 
            } else { df_to_show$Sig <- "" }
            
            desired_col_order <- c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "RMSEA", "Df diff", "Pr(>Chisq)", "Sig")
            existing_cols_ordered <- intersect(desired_col_order, colnames(df_to_show))
            df_to_show <- df_to_show[, existing_cols_ordered, drop = FALSE]

            df_to_show
        }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE, na = "") 
      } else { 
        analysis_results_rv$fit_measures_df <- data.frame(Message = "No models were successfully fitted or selected.")
        output$invariance_fit_measures_table <- renderTable(analysis_results_rv$fit_measures_df)
        analysis_results_rv$model_comparison_df <- data.frame(Message = "No models available for comparison.")
        output$model_comparison_table <- renderTable(analysis_results_rv$model_comparison_df)
      }
      showNotification("Measurement Invariance analysis complete!", type = "message", duration = 5)

    }, error = function(e) {
      # Keep detailed error logging for development if needed, but simplify for user notification
      # cat("--- ERROR CAUGHT IN inv_server.R (MAIN TRY-CATCH) ---\n"); print(e); cat("--- END ---\n")
      user_error_message <- if (!is.null(conditionMessage(e))) conditionMessage(e) else "An unspecified error occurred during MI analysis."
      showNotification(paste("Error (MI):", user_error_message), type = "error", duration = 10)
      analysis_results_rv$fit_measures_df <- data.frame(Error = user_error_message)
      output$invariance_fit_measures_table <- renderTable(analysis_results_rv$fit_measures_df)
      analysis_results_rv$model_comparison_df <- data.frame(Error = user_error_message)
      output$model_comparison_table <- renderTable(analysis_results_rv$model_comparison_df)
    }) 
  }) 

  # --- Download Handlers ---
  output$download_fit_measures_button <- downloadHandler(
    filename = function() paste0("measurement_invariance_fit_measures_", Sys.Date(), ".csv"),
    content = function(file) {
      req(analysis_results_rv$fit_measures_df)
      validate(need(!("Error" %in% colnames(analysis_results_rv$fit_measures_df)) && !("Message" %in% colnames(analysis_results_rv$fit_measures_df)), 
                    "Fit measures not available for download or contain an error/message."))
      write.csv(analysis_results_rv$fit_measures_df, file, row.names = FALSE, na = "") 
    }
  )

  output$download_model_comparison_button <- downloadHandler(
    filename = function() paste0("measurement_invariance_model_comparison_", Sys.Date(), ".csv"),
    content = function(file) {
      req(analysis_results_rv$model_comparison_df)
      validate(need(!("Error" %in% colnames(analysis_results_rv$model_comparison_df)) && !("Message" %in% colnames(analysis_results_rv$model_comparison_df)), 
                    "Model comparison results not available for download or contain an error/message."))
      
      df_to_download <- analysis_results_rv$model_comparison_df
      model_column_name_for_csv <- "Model" 
      if (!model_column_name_for_csv %in% colnames(df_to_download)) {
          if(!is.null(rownames(df_to_download))) {
            df_to_download <- cbind(Model = rownames(df_to_download), df_to_download)
          }
      }
      
      # Apply specific formatting for CSV
      if("Df" %in% colnames(df_to_download)) df_to_download$"Df" <- ifelse(is.na(df_to_download$"Df"), "", as.integer(df_to_download$"Df"))
      if("AIC" %in% colnames(df_to_download)) df_to_download$"AIC" <- ifelse(is.na(df_to_download$"AIC"), "", sprintf("%.1f", as.numeric(df_to_download$"AIC")))
      if("BIC" %in% colnames(df_to_download)) df_to_download$"BIC" <- ifelse(is.na(df_to_download$"BIC"), "", sprintf("%.1f", as.numeric(df_to_download$"BIC")))
      if("Chisq" %in% colnames(df_to_download)) df_to_download$"Chisq" <- ifelse(is.na(df_to_download$"Chisq"), "", sprintf("%.2f", as.numeric(df_to_download$"Chisq")))
      if("Chisq diff" %in% colnames(df_to_download)) df_to_download$"Chisq diff" <- ifelse(is.na(df_to_download$"Chisq diff"), "", sprintf("%.2f", as.numeric(df_to_download$"Chisq diff")))
      if("RMSEA" %in% colnames(df_to_download)) df_to_download$"RMSEA" <- ifelse(is.na(df_to_download$"RMSEA"), "", sprintf("%.5f", as.numeric(df_to_download$"RMSEA")))
      if("Df diff" %in% colnames(df_to_download)) df_to_download$"Df diff" <- ifelse(is.na(df_to_download$"Df diff"), "", as.integer(df_to_download$"Df diff"))
      
      if("Pr(>Chisq)" %in% colnames(df_to_download)) {
           p_values_csv <- as.numeric(df_to_download$"Pr(>Chisq)") 
           df_to_download$"Pr(>Chisq)" <- sapply(p_values_csv, function(p_val) {
              if(is.na(p_val)) return("") 
              ifelse(p_val < 0.00000000000000022, "<2.2e-16", 
                     ifelse(p_val < 0.001, sprintf("%.3e", p_val), 
                            sprintf("%.4f", p_val))) 
           })
      }
      if("Sig" %in% colnames(df_to_download)){
          sig_col_data_csv <- df_to_download$Sig
          df_to_download$Sig <- NULL 
          df_to_download$Sig <- sig_col_data_csv 
      }
      
      desired_csv_cols <- c("Model", "Df", "AIC", "BIC", "Chisq", "Chisq diff", "RMSEA", "Df diff", "Pr(>Chisq)", "Sig")
      existing_csv_cols <- intersect(desired_csv_cols, colnames(df_to_download))
      df_to_download_final <- df_to_download[, existing_csv_cols, drop = FALSE]

      write.csv(df_to_download_final, file, row.names = FALSE, na = "") 
    }
  )
}
