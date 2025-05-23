# Assumptions Server Module 
assumptions_server <- function(input, output, session, data) {
  assumption_calculation_results_rv <- reactiveValues(
    descriptives = NULL, 
    multicollinearity = NULL,
    # Store the 1-row data.frames from user's assumptions() output
    Mardia_Skewness = NULL, 
    Mardia_Kurtosis = NULL,
    # Mah_significant and n_outlier are used by wrangling_server_outliers
    # but assumptions() returns them, so we can store if needed, though not directly used in this UI part
    Mah_significant_outliers = NULL, 
    n_outliers_found = NULL
  )

  # --- Descriptive Statistics ---
  observeEvent(input$run_descriptives_button, {
    validate(need(data(), "Please upload your dataset to calculate descriptive statistics."))
    current_data <- data()
    if (is.null(current_data)) {
      showNotification("Data is not available for descriptive statistics.", type = "warning") 
      output$descriptives_table_output <- renderTable(data.frame(Message = "Data not loaded."))
      return()
    }
    if (!exists("assumptions")) {
      showNotification("Error: The 'assumptions' function is not found. Please check 'utils.R'.", type = "error", duration = 10) 
      output$descriptives_table_output <- renderTable(data.frame(Error = "Core function missing."))
      return()
    }
    
    progress_id <- showNotification("Calculating descriptive statistics...", duration = NULL, type = "message") 
    on.exit(removeNotification(progress_id), add = TRUE)

    tryCatch({
      message("DEBUG (Descriptives): Calling user's assumptions() function from utils.R.")
      results_from_utils <- assumptions(current_data, mah_p_threshold = 0.001) # Pass threshold
      
      assumption_calculation_results_rv$descriptives <- results_from_utils$descriptives
      assumption_calculation_results_rv$multicollinearity <- results_from_utils$multicollinearity
      assumption_calculation_results_rv$Mah_significant_outliers <- results_from_utils$Mah_significant # Match user's return name
      assumption_calculation_results_rv$n_outliers_found <- results_from_utils$n_outlier       # Match user's return name
      assumption_calculation_results_rv$Mardia_Skewness <- results_from_utils$Mardia_Skewness # User's return name
      assumption_calculation_results_rv$Mardia_Kurtosis <- results_from_utils$Mardia_Kurtosis # User's return name

      message("DEBUG (Descriptives): Stored Mardia_Skewness from assumptions(). Content:")
      print(assumption_calculation_results_rv$Mardia_Skewness)
      message("DEBUG (Descriptives): Stored Mardia_Kurtosis from assumptions(). Content:")
      print(assumption_calculation_results_rv$Mardia_Kurtosis)

      output$descriptives_table_output <- renderTable({
        req(assumption_calculation_results_rv$descriptives)
        df_to_display <- as.data.frame(assumption_calculation_results_rv$descriptives)
        # Rounding is now done in utils.R's assumptions() for descriptives
        df_to_display 
      }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "auto")

      showNotification("Descriptive statistics calculated.", type = "message") 

    }, error = function(e) {
      cat("--- ERROR CAUGHT IN assumptions_server.R (Descriptives) ---\n"); print(e); cat("--- END ---\n")
      showNotification(paste("Error (Descriptives):", conditionMessage(e)), type = "error", duration = 10) 
      output$descriptives_table_output <- renderTable(data.frame(Error = conditionMessage(e)))
    })
  })

  output$download_descriptives_button <- downloadHandler(
    filename = function() paste0("descriptive_statistics_", Sys.Date(), ".csv"),
    content = function(file) {
      req(assumption_calculation_results_rv$descriptives)
      write.csv(as.data.frame(assumption_calculation_results_rv$descriptives), file, row.names = TRUE)
    }
  )

  # --- Collinearity Statistics ---
  observeEvent(input$run_collinearity_button, {
    validate(need(data(), "Please upload your dataset to calculate collinearity statistics."))
    current_data <- data()
    if (is.null(current_data)) { 
        showNotification("Data is not available for collinearity.", type = "warning"); return()
    }
    if (!exists("assumptions")) { 
        showNotification("Error: 'assumptions' function missing.", type = "error"); return()
    }
    
    progress_id <- showNotification("Calculating collinearity statistics...", duration = NULL, type = "message")
    on.exit(removeNotification(progress_id), add = TRUE)

    tryCatch({
      if (is.null(assumption_calculation_results_rv$multicollinearity)) {
        message("DEBUG (Collinearity): Calling user's assumptions() function from utils.R.")
        results_from_utils <- assumptions(current_data, mah_p_threshold = 0.001) 
        assumption_calculation_results_rv$descriptives <- results_from_utils$descriptives
        assumption_calculation_results_rv$multicollinearity <- results_from_utils$multicollinearity
        assumption_calculation_results_rv$Mah_significant_outliers <- results_from_utils$Mah_significant
        assumption_calculation_results_rv$n_outliers_found <- results_from_utils$n_outlier
        assumption_calculation_results_rv$Mardia_Skewness <- results_from_utils$Mardia_Skewness
        assumption_calculation_results_rv$Mardia_Kurtosis <- results_from_utils$Mardia_Kurtosis
      } else {
        message("Collinearity: Using previously calculated results from assumptions().")
      }

      output$collinearity_table_output <- renderTable({
        req(assumption_calculation_results_rv$multicollinearity)
        as.data.frame(assumption_calculation_results_rv$multicollinearity) # Rounding in utils.R
      }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)

      showNotification("Collinearity statistics calculated.", type = "message")
    }, error = function(e) {
      cat("--- ERROR CAUGHT IN assumptions_server.R (Collinearity) ---\n"); print(e); cat("--- END ---\n") 
      showNotification(paste("Error (Collinearity):", conditionMessage(e)), type = "error", duration = 10)
      output$collinearity_table_output <- renderTable(data.frame(Error = conditionMessage(e)))
    })
  })

  # --- Multivariate Normality Tests ---
  observeEvent(input$run_normality_tests_button, {
    validate(need(data(), "Please upload your dataset to calculate multivariate normality tests."))
    current_data <- data()
    
    if (is.null(current_data) || !all(sapply(current_data, is.numeric)) || any(is.na(current_data)) || ncol(current_data) < 2) {
        showNotification("Data is unsuitable for normality tests (must be numeric, complete, >=2 vars).", type = "error", duration = 8)
        output$multivariate_normality_table_output <- renderTable(data.frame(Message = "Data unsuitable for tests."))
        return()
    }
    
    message("DEBUG (Normality - Input Data): Preparing to display normality. current_data dims: ", paste(dim(current_data), collapse="x"))
    progress_id <- showNotification("Calculating multivariate normality tests...", duration = NULL, type = "message")
    on.exit(removeNotification(progress_id), add = TRUE)

    output$multivariate_normality_table_output <- renderTable({
      tryCatch({
        mardia_s_res_from_util <- NULL
        mardia_k_res_from_util <- NULL

        if (!is.null(assumption_calculation_results_rv$Mardia_Skewness) && !is.null(assumption_calculation_results_rv$Mardia_Kurtosis)) {
          message("DEBUG (Normality - Mardia): Using pre-calculated Mardia results from reactiveValues.")
          mardia_s_res_from_util <- assumption_calculation_results_rv$Mardia_Skewness # User's name
          mardia_k_res_from_util <- assumption_calculation_results_rv$Mardia_Kurtosis # User's name
        } else if (exists("assumptions")) {
          message("DEBUG (Normality - Mardia): Mardia results not in reactiveValues, calling user's assumptions() from utils.R.")
          all_assumption_results <- assumptions(current_data, mah_p_threshold = 0.001) 
          
          assumption_calculation_results_rv$descriptives <- all_assumption_results$descriptives
          assumption_calculation_results_rv$multicollinearity <- all_assumption_results$multicollinearity
          assumption_calculation_results_rv$Mah_significant_outliers <- all_assumption_results$Mah_significant
          assumption_calculation_results_rv$n_outliers_found <- all_assumption_results$n_outlier
          assumption_calculation_results_rv$Mardia_Skewness <- all_assumption_results$Mardia_Skewness # Store with user's name
          assumption_calculation_results_rv$Mardia_Kurtosis <- all_assumption_results$Mardia_Kurtosis # Store with user's name
          
          mardia_s_res_from_util <- assumption_calculation_results_rv$Mardia_Skewness
          mardia_k_res_from_util <- assumption_calculation_results_rv$Mardia_Kurtosis
        } else {
          message("DEBUG (Normality - Mardia): 'assumptions' function not found in utils.R.")
        }
        
        message("DEBUG (Normality - Mardia): Content of Mardia Skewness result from utils.R (mardia_s_res_from_util):")
        print(mardia_s_res_from_util)
        message("DEBUG (Normality - Mardia): Content of Mardia Kurtosis result from utils.R (mardia_k_res_from_util):")
        print(mardia_k_res_from_util)
        
        energy_results <- NULL
        if(nrow(current_data) >= 2 && ncol(current_data) >=1 && nrow(current_data) >= ncol(current_data)){
             energy_results <- tryCatch(energy::mvnorm.test(as.matrix(current_data), R = 199), error = function(e){
                 message(paste("DEBUG: Energy test failed:", e$message)); NULL
             })
        } else { message("DEBUG: Skipping Energy test due to data conditions.") }

        normality_summary_df <- data.frame(
          Test_Name = c("Mardia's Skewness", "Mardia's Kurtosis", "Energy Test (E-test)"),
          Statistic = NA_real_, p_value = NA_character_, Result_Interpretation = NA_character_,
          stringsAsFactors = FALSE
        )
        
        # Populate from mardia_s_res_from_util (user's structure from utils.R)
        # Expected columns from user's Mardia output: "Test", "Statistic", "p-value", "Result"
        if(!is.null(mardia_s_res_from_util) && is.data.frame(mardia_s_res_from_util) && nrow(mardia_s_res_from_util) == 1 && 
           all(c("Statistic", "p-value", "Result") %in% colnames(mardia_s_res_from_util))){
            
            statistic_val_skew <- as.numeric(mardia_s_res_from_util[1, "Statistic"]) 
            p_val_skew <- as.numeric(mardia_s_res_from_util[1, "p-value"])       
            result_interpretation_skew <- ifelse(as.character(mardia_s_res_from_util[1, "Result"]) != "NO", "Normality Supported (p >= 0.05)", "Normality Violated (p < 0.05)")

            normality_summary_df[1, "Statistic"] <- round(statistic_val_skew, 3)
            normality_summary_df[1, "p_value"] <- format.pval(p_val_skew, digits = 3, eps = 0.001)
            normality_summary_df[1, "Result_Interpretation"] <- result_interpretation_skew
        } else {
            normality_summary_df[1, "Result_Interpretation"] <- "Mardia Skewness result not available/error"
        }

        if(!is.null(mardia_k_res_from_util) && is.data.frame(mardia_k_res_from_util) && nrow(mardia_k_res_from_util) == 1 && 
           all(c("Statistic", "p-value", "Result") %in% colnames(mardia_k_res_from_util))){

            statistic_val_kurt <- as.numeric(mardia_k_res_from_util[1, "Statistic"]) 
            p_val_kurt <- as.numeric(mardia_k_res_from_util[1, "p-value"])       
            result_interpretation_kurt <- ifelse(as.character(mardia_k_res_from_util[1, "Result"]) != "NO", "Normality Supported (p >= 0.05)", "Normality Violated (p < 0.05)")

            normality_summary_df[2, "Statistic"] <- round(statistic_val_kurt, 3)
            normality_summary_df[2, "p_value"] <- format.pval(p_val_kurt, digits = 3, eps = 0.001)
            normality_summary_df[2, "Result_Interpretation"] <- result_interpretation_kurt
        } else {
            normality_summary_df[2, "Result_Interpretation"] <- "Mardia Kurtosis result not available/error"
        }

        if(!is.null(energy_results)){
            normality_summary_df[3, "Statistic"] <- round(energy_results$statistic, 3)
            normality_summary_df[3, "p_value"] <- format.pval(energy_results$p.value, digits = 3, eps = 0.001)
            normality_summary_df[3, "Result_Interpretation"] <- ifelse(energy_results$p.value >= 0.05, "Normality Supported (p >= 0.05)", "Normality Violated (p < 0.05)")
        } else {
            normality_summary_df[3, "Result_Interpretation"] <- "Energy test failed or not run"
        }
        
        showNotification("Multivariate normality tests calculated.", type = "message")
        normality_summary_df
        
      }, error = function(e) {
        cat("--- ERROR CAUGHT IN assumptions_server.R (Normality) ---\n"); print(e); cat("--- END ---\n")
        showNotification(paste("Error (Normality):", conditionMessage(e)), type = "error", duration = 10) 
        data.frame(Error = conditionMessage(e))
      })
    }, rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
