# EFA Server Modules
 
# ---------------------------------------------------------------------------
# Server Module for Factor Retention Methods
# ---------------------------------------------------------------------------

efa_server_fac_ret <- function(input, output, session, data) {
  observeEvent(input$run_factor_ret, {
    # Ensure data is available before proceeding
    validate(need(data(), "Please upload your data set first."))

    # Render the scree plot
    output$scree_plot <- renderPlot({
      # Ensure the data is suitable for SCREE plot (e.g., numeric matrix or data frame)
      # Add error handling if data() is not in the correct format
      current_data <- data()
      if (!is.data.frame(current_data) && !is.matrix(current_data) || !all(sapply(current_data, is.numeric))) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "Invalid data for Scree Plot", xlab = "", ylab = "")
        text(0.5, 0.5, "Data must be a numeric data frame or matrix.")
        return()
      }
      tryCatch({
        scree_analysis_result <- EFAtools::SCREE(current_data)
        plot(scree_analysis_result)
      }, error = function(e) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "Error in Scree Plot generation", xlab = "", ylab = "")
        text(0.5, 0.5, paste("Scree Plot Error:", e$message))
      })
    })

    # Render the table for factor retention method results
    output$dim_ret_results <- renderTable({
      # Ensure the factor_ret function is available (should be in utils.R)
      if (!exists("factor_ret")) {
        return(data.frame(Error = "factor_ret function not found. Check utils.R"))
      }
      # Add error handling for the factor_ret function call
      tryCatch({
        # The factor_ret function is expected to be defined in utils.R
        # and handle different methods based on input$dimension_methods
        factor_ret(x = data(), method = input$dimension_methods)
      }, error = function(e) {
        data.frame(Error = paste("Factor retention analysis failed:", e$message))
      })
    }, rownames = TRUE) # Display row names if factor_ret provides them
  })
}


# ---------------------------------------------------------------------------
# Server Module for Running Exploratory Factor Analysis (EFA)
# ---------------------------------------------------------------------------
# This module performs the EFA calculation and returns the results reactively.
efa_server_analysis <- function(input, output, session, data) {
  # reactiveVal to store the EFA results object
  efa_object_rv <- reactiveVal(NULL)

  observeEvent(input$run_efa, {
    # Ensure data is available
    validate(need(data(), "Please upload your data set to run EFA."))
    validate(need(input$number_factor, "Please specify the number of factors."))
    validate(need(input$rotating_method, "Please select a rotation method."))
    validate(need(input$fact_method, "Please select a factoring method."))
    validate(need(input$cor_kind, "Please select a correlation type."))

    # Show a notification that EFA is running
    id <- showNotification("Running EFA, please wait...", duration = NULL, type = "message")
    on.exit(removeNotification(id), add = TRUE) # Ensure notification is removed

    current_data <- data() # Get the reactive data

    # Perform EFA calculation
    # Using tryCatch to handle potential errors during the psych::fa call
    calculated_efa <- tryCatch({
      psych::fa(
        r = current_data, # psych::fa can take raw data or a correlation matrix
        nfactors = as.integer(input$number_factor),
        rotate = input$rotating_method,
        fm = input$fact_method,
        cor = ifelse(input$cor_kind == "pea", "cor", "poly") # "cor" for Pearson, "poly" for polychoric
        # You might want to add other parameters like n.obs if r is a correlation matrix,
        # or error handling for non-numeric data if current_data is not pre-processed.
      )
    }, error = function(e) {
      # If an error occurs, show a notification and set results to NULL
      showNotification(paste("EFA Calculation Error:", e$message), type = "error", duration = 5)
      return(NULL)
    })

    # Update the reactiveVal with the EFA results
    efa_object_rv(calculated_efa)

    # If EFA was successful, show a success message
    if (!is.null(calculated_efa)) {
      showNotification("EFA completed successfully!", type = "message", duration = 3)
    }
  })

  # Return the reactive expression containing the EFA results
  return(reactive(efa_object_rv()))
}


# ---------------------------------------------------------------------------
# Server Module for Reporting EFA Results
# ---------------------------------------------------------------------------
# This module takes the EFA results and settings as inputs and renders the report.
efa_server_report <- function(input, output, session, data, efa_output_reactive, efa_settings_reactive) {

  # Observe changes in the EFA results or settings to update the report
  observe({
    # Ensure all necessary reactive inputs are available
    # data() is the original dataset for KMO, Bartlett, heatmap
    # efa_output_reactive() contains the EFA object from efa_server_analysis
    # efa_settings_reactive() contains settings like number_factor, cor_kind from efa_analysis UI
    req(data(), efa_output_reactive(), efa_settings_reactive())

    current_efa_results <- efa_output_reactive() # This is the object from psych::fa
    current_settings <- efa_settings_reactive()   # This is a list of settings
    current_raw_data <- data()                    # Raw data for KMO, Bartlett etc.

    # If EFA calculation failed in the analysis module, current_efa_results will be NULL.
    # Handle this gracefully by showing informative messages in the outputs.
    if (is.null(current_efa_results)) {
      output$kmo_result <- renderText({"KMO analysis cannot be performed. EFA results are not available."})
      output$bartlett <- renderTable(data.frame(Message = "Bartlett's test cannot be performed. EFA results are not available."))
      output$robust_msa <- renderTable(data.frame(Message = "MSA values cannot be calculated. EFA results are not available."))
      output$heat_map <- renderPlot({ plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Heatmap not available.", xlab="", ylab=""); text(0.5,0.5,"EFA results are missing.") })
      output$efa_result_str <- renderTable(data.frame(Message = "EFA structure cannot be displayed. EFA results are not available."))
      output$efa_result_interf_cor <- renderTable(data.frame(Message = "Interfactor correlations cannot be displayed. EFA results are not available."))
      output$efa_result_expl_var <- renderTable(data.frame(Message = "Explained variance cannot be displayed. EFA results are not available."))
      # Also disable or clear download button content
      output$download_efa_loadings <- downloadHandler(
        filename = function() "efa_error.txt",
        content = function(file) writeLines("EFA results are not available for download.", file)
      )
      return() # Stop further processing for this observation
    }

    # --- KMO, Bartlett Test, and MSA ---
    output$kmo_result <- renderText({
      validate(need(try(ncol(current_raw_data) > 1 && nrow(current_raw_data) > 0), "Data must have multiple columns and rows for KMO."))
      tryCatch({
        kmo_analysis <- psych::KMO(current_raw_data)
        kmo_interpret <- dplyr::case_when(
          kmo_analysis$MSA < 0.50 ~ "Unacceptable", # Adjusted threshold based on common guidelines
          kmo_analysis$MSA < 0.60 ~ "Miserable",
          kmo_analysis$MSA < 0.70 ~ "Mediocre",
          kmo_analysis$MSA < 0.80 ~ "Middling",
          kmo_analysis$MSA < 0.90 ~ "Meritorious",
          kmo_analysis$MSA <= 1.00 ~ "Marvelous",
          TRUE ~ "Value out of range"
        )
        paste0("<b>Overall KMO MSA = ", round(kmo_analysis$MSA, 3),
               " (Interpretation: ", kmo_interpret, ")</b>")
      }, error = function(e) {
        paste0("<b>KMO Analysis Error: ", e$message, "</b>")
      })
    })

    output$bartlett <- renderTable({
      validate(need(try(ncol(current_raw_data) > 1 && nrow(current_raw_data) > 0), "Data must have multiple columns and rows for Bartlett's test."))
      tryCatch({
        # Determine correlation matrix based on user's choice for EFA
        cor_matrix_for_bartlett <- if (current_settings$cor_kind == "pea") {
          cor(current_raw_data, use = "pairwise.complete.obs") # Using pairwise for robustness if NAs slipped through
        } else {
          # Polychoric can fail if data is not suitable (e.g., continuous, too few categories)
          # Or if 'psych' package version has issues with certain data.
          poly_cor_result <- psych::polychoric(current_raw_data, correct = 0) # Added correct=0 as sometimes it helps
          poly_cor_result$rho
        }
        validate(need(try(nrow(cor_matrix_for_bartlett) == ncol(cor_matrix_for_bartlett)), "Correlation matrix is not square."))

        bartlett_test <- psych::cortest.bartlett(cor_matrix_for_bartlett, n = nrow(current_raw_data))
        bartlett_result_df <- data.frame(
          Value = c(round(bartlett_test$chisq, 2), bartlett_test$df, format.pval(bartlett_test$p.value, digits = 3, eps = 0.001))
        )
        rownames(bartlett_result_df) <- c("Chi-square", "Degrees of Freedom", "p-value")
        bartlett_result_df
      }, error = function(e) {
        data.frame(Error = paste("Bartlett's Test Error:", e$message))
      })
    }, rownames = TRUE)

    output$robust_msa <- renderTable({
      validate(need(try(ncol(current_raw_data) > 1 && nrow(current_raw_data) > 0), "Data must have multiple columns and rows for MSA values."))
      tryCatch({
        kmo_analysis <- psych::KMO(current_raw_data)
        msa_results_df <- data.frame(MSA_Value = round(kmo_analysis$MSAi, 3))
        # Interpretation based on a common threshold (0.50)
        msa_results_df$Interpretation <- ifelse(kmo_analysis$MSAi < 0.50, "Potentially problematic (Consider Removal)", "Acceptable")
        # rownames(msa_results_df) <- paste("Item", seq_along(kmo_analysis$MSAi), sep = "_") # Or use actual item names if available
        rownames(msa_results_df) <- colnames(current_raw_data) # Use actual column names
        msa_results_df
      }, error = function(e) {
        data.frame(Error = paste("MSA Values Calculation Error:", e$message))
      })
    }, rownames = TRUE)

    # --- Inter-Item Correlation Heatmap ---
    output$heat_map <- renderPlot({
      validate(need(try(ncol(current_raw_data) > 1 && nrow(current_raw_data) > 0), "Data must have multiple columns and rows for heatmap."))
      tryCatch({
        cor_matrix_for_heatmap <- if (current_settings$cor_kind == "pea") {
          cor(current_raw_data, use = "pairwise.complete.obs")
        } else {
          psych::polychoric(current_raw_data, correct = 0)$rho
        }
        
        # Determine plot type based on number of items
        plot_method <- if (ncol(current_raw_data) < 21) "square" else "circle" # Using "color" instead of "tile" with "lab = TRUE" for ggcorrplot
        label_visibility <- ncol(current_raw_data) < 21

        ggcorrplot::ggcorrplot(
          corr = cor_matrix_for_heatmap,
          hc.order = TRUE,      # Hierarchical clustering
          type = "lower",       # Show lower triangle
          method = plot_method, # "circle" or "color" (like "tile")
          lab = label_visibility, # Show correlation coefficients if few items
          lab_size = 6,
          colors = c("#6D9EC1", "white", "#E46726"), # Blue, white, red
          title = "Inter-Item Correlation Matrix Heatmap"
        )
      }, error = function(e) {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Heatmap Generation Error", xlab="", ylab="");
        text(0.5,0.5, e$message)
      })
    })

    # --- EFA Structure (Factor Loadings) ---
    output$efa_result_str <- renderTable({
      # 'loadings' is the standard element for factor loadings in psych::fa output
      # 'Structure' might exist for PCA or specific rotations, but 'loadings' is more general for FA.
      validate(need(current_efa_results$loadings, "Factor loadings are not available in EFA results."))
      
      num_factors_to_display <- as.integer(current_settings$number_factor)
      actual_num_factors_extracted <- ncol(current_efa_results$loadings)
      
      # Ensure we don't try to display more factors than extracted
      factors_to_show <- min(num_factors_to_display, actual_num_factors_extracted)
      
      if (factors_to_show < 1) {
        return(data.frame(Message = "No factors to display or specified number of factors is zero."))
      }

      # Extract the loadings matrix, ensuring it's treated as a matrix
      loadings_matrix <- as.matrix(current_efa_results$loadings[, 1:factors_to_show, drop = FALSE])
      
      # Apply a threshold for displaying loadings (e.g., abs(loading) > 0.3) - Optional
      # loadings_matrix[abs(loadings_matrix) < 0.3] <- NA # or "" or "."
      
      loadings_df <- as.data.frame(loadings_matrix)
      colnames(loadings_df) <- colnames(current_efa_results$loadings)[1:factors_to_show] # Preserve factor names
      
      # Add communalities (h2) if desired
      if(!is.null(current_efa_results$communality)){
          loadings_df$h2_communality <- round(current_efa_results$communality, 3)
      }

      round(loadings_df, 3) # Round for display
    }, rownames = TRUE)

    # --- Download Handler for EFA Loadings ---
    output$download_efa_loadings <- downloadHandler(
      filename = function() {
        paste0("efa_loadings_nfactors_", current_settings$number_factor, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Re-create the table to be downloaded, similar to renderTable logic
        if (is.null(current_efa_results) || is.null(current_efa_results$loadings)) {
          write.csv(data.frame(Error = "Factor loadings not available for download."), file, row.names = FALSE)
          return()
        }
        num_factors_to_display <- as.integer(current_settings$number_factor)
        actual_num_factors_extracted <- ncol(current_efa_results$loadings)
        factors_to_show <- min(num_factors_to_display, actual_num_factors_extracted)

        if (factors_to_show < 1) {
           write.csv(data.frame(Message = "No factors to display or specified number of factors is zero."), file, row.names = FALSE)
           return()
        }
        
        loadings_matrix_dl <- as.matrix(current_efa_results$loadings[, 1:factors_to_show, drop = FALSE])
        loadings_df_dl <- as.data.frame(loadings_matrix_dl)
        colnames(loadings_df_dl) <- colnames(current_efa_results$loadings)[1:factors_to_show]
        
        if(!is.null(current_efa_results$communality)){
            loadings_df_dl$h2_communality <- current_efa_results$communality
        }
        
        write.csv(round(loadings_df_dl, 3), file, row.names = TRUE)
      }
    )

    # --- Interfactor Correlation ---
    output$efa_result_interf_cor <- renderTable({
      num_factors <- as.integer(current_settings$number_factor)
      if (num_factors <= 1) {
        return(data.frame(Message = "Interfactor correlations are not applicable for a single factor solution."))
      }
      # For oblique rotations, inter-factor correlations are in $Phi
      # For orthogonal rotations, this matrix is an identity matrix (or not present/NULL).
      # psych::fa stores factor correlations in $Phi if an oblique rotation was performed.
      if (is.null(current_efa_results$Phi)) {
        # This could mean an orthogonal rotation was used, or something else.
        # Check if rotation was orthogonal. Common orthogonal rotations: varimax, quartimax, equamax, ...
        orthogonal_rotations <- c("none", "varimax", "quartimax", "bentlerT", "equamax", "geominT", "bifactor") 
        # Note: "none" is no rotation, effectively orthogonal.
        # "geomin" (oblique) vs "geominQ" (oblique) and "geominT" (orthogonal) in psych.
        # Your UI lists "geominQ" as oblique.
        
        if (tolower(current_settings$rotating_method) %in% tolower(orthogonal_rotations)) {
          return(data.frame(Message = "Orthogonal rotation was used. Factors are uncorrelated by definition."))
        } else {
          # If it's an oblique rotation but Phi is NULL, it's unexpected.
          return(data.frame(Message = "Interfactor correlation matrix (Phi) is not available. This might indicate an issue or an orthogonal rotation was effectively performed."))
        }
      }
      
      interfactor_cor_matrix <- as.matrix(current_efa_results$Phi)
      interfactor_cor_df <- as.data.frame(round(interfactor_cor_matrix, 3))
      
      # Naming rows and columns based on the number of factors
      factor_names <- paste0("F", 1:nrow(interfactor_cor_df)) # Assuming factors are F1, F2...
      rownames(interfactor_cor_df) <- factor_names
      colnames(interfactor_cor_df) <- factor_names
      
      interfactor_cor_df
    }, rownames = TRUE)

    # --- Explained Variance Ratios ---
    output$efa_result_expl_var <- renderTable({
      validate(need(current_efa_results$Vaccounted, "Explained variance data (Vaccounted) is not available."))
      
      # Vaccounted typically has rows like "SS loadings", "Proportion Var", "Cumulative Var", etc.
      # And columns for each factor, plus sometimes a total.
      explained_var_df <- as.data.frame(current_efa_results$Vaccounted)
      
      # Ensure column names are descriptive if they are generic (like PA1, PA2)
      # The psych::fa object usually names them based on the factoring method (e.g., MR1, MR2 or FA1, FA2)
      # Or based on the number of factors (e.g., if nfactors=3, columns might be Factor1, Factor2, Factor3)
      # We can try to make them more generic if needed, or use what psych::fa provides.
      # For now, use the column names from Vaccounted directly.
      
      # The number of columns in Vaccounted might be nfactors or nfactors + summary columns.
      # Let's display up to the number of factors requested.
      num_factors_to_show_in_var <- min(as.integer(current_settings$number_factor), ncol(explained_var_df))
      
      if (num_factors_to_show_in_var < 1) {
          return(data.frame(Message = "No variance data to display for the specified number of factors."))
      }

      # Select relevant columns (usually the factors themselves, not summary columns if any are beyond nfactors)
      # psych::fa's Vaccounted usually has columns for each factor.
      # If it has more columns than factors (e.g. for PCA components before selection), subsetting might be needed.
      # However, for FA, Vaccounted columns usually correspond to the extracted factors.
      
      # Let's assume Vaccounted columns are already correctly representing the factors.
      # No, Vaccounted columns are for each factor.
      # The rows are: SS loadings, Proportion Var, Cumulative Var, Proportion Explained, Cumulative Proportion.
      # The code in the original user provided `efa_server_report` was trying to reshape it.
      # Let's use the direct output from psych::fa which is usually well-formatted.
      
      # Example rows from psych::fa$Vaccounted:
      #                       MR1   MR2   MR3
      # SS loadings           2.50  1.80  1.20
      # Proportion Var        0.25  0.18  0.12
      # Cumulative Var        0.25  0.43  0.55
      # Proportion Explained  0.45  0.33  0.22  (Proportion of explained variance if >1 factor)
      # Cumulative Proportion 0.45  0.78  1.00  (Cumulative proportion of explained variance)

      # The original code was trying to construct this from a vector.
      # `current_efa_results$Vaccounted` should already be a matrix/data.frame.
      
      # Check if Vaccounted is a vector and needs reshaping (unlikely for modern psych versions)
      if(is.vector(current_efa_results$Vaccounted) && !is.list(current_efa_results$Vaccounted)) {
         # This was the old logic, might not be needed.
         num_factors_val <- as.integer(current_settings$number_factor)
         if (length(current_efa_results$Vaccounted) %% num_factors_val == 0 && num_factors_val > 0) {
            num_rows_for_vaccounted <- length(current_efa_results$Vaccounted) / num_factors_val
            explained_var_df_reshaped <- data.frame(
              matrix(
                as.numeric(current_efa_results$Vaccounted),
                ncol = num_factors_val,
                nrow = num_rows_for_vaccounted, # Should be 5 if standard
                byrow = FALSE # Data is usually column-major from psych
              )
            )
            # Assign standard row names if reshaping
            if(num_rows_for_vaccounted == 5) {
                 rownames(explained_var_df_reshaped) <- c(
                    "SS loadings", "Proportion Var", "Cumulative Var",
                    "Proportion Explained", "Cumulative Proportion"
                )
            } else if (num_rows_for_vaccounted == 3 && !"Proportion Explained" %in% rownames(current_efa_results$Vaccounted)) {
                # Older PCA style output from fa.parallel might have 3 rows
                 rownames(explained_var_df_reshaped) <- c(
                    "SS loadings", "Proportion Var", "Cumulative Var"
                )
            }
            # Assign column names
            colnames(explained_var_df_reshaped) <- paste0("Factor", 1:num_factors_val)
            explained_var_df <- explained_var_df_reshaped
         } else {
            return(data.frame(Error = "Vaccounted data has an unexpected structure (vector form)."))
         }
      } else {
         # If Vaccounted is already a data.frame/matrix, use it directly
         explained_var_df <- as.data.frame(current_efa_results$Vaccounted)
      }

      round(explained_var_df, 3) # Round for display
    }, rownames = TRUE)

  }) # End of observe block
}
