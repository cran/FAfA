# Reliability Analysis Server Module
# Assumes 'reliability_func' from utils.R handles dependencies like 'psych', 'MBESS', 'lavaan', 'sirt'.
 
reliability_server <- function(input, output, session, data) {
  # reactiveVal to store the calculated reliability result string
  reliability_output_rv <- reactiveVal(NULL)

  observeEvent(input$run_reliability_button, {
    # --- Input Validations ---
    validate(need(data(), "Please upload your dataset to calculate reliability."))
    validate(need(input$reliability_coefficient_select, "Please select a reliability coefficient."))

    current_data <- data() 
    selected_method <- input$reliability_coefficient_select
    
    # Ensure data is not NULL before proceeding with further checks
    if (is.null(current_data)) {
      showNotification("Data is not available for reliability analysis.", type = "warning", duration = 5)
      reliability_output_rv("Data not loaded or is NULL.")
      return()
    }
    
    # Specific validations for methods requiring additional input
    if (selected_method == "s_alpha") {
      validate(need(input$strata_definition_input, "Please define strata membership for Stratified Alpha (e.g., 1,1,2,2)."))
      user_strata_text <- input$strata_definition_input
      strata_values <- tryCatch(as.numeric(unlist(strsplit(user_strata_text, ","))), 
                                warning = function(w) {
                                  showNotification("Strata definition contains non-numeric or improperly formatted values.", type="error", duration=7)
                                  return(NA) # Return NA to fail the need() check
                                })
      
      validate(
          need(!any(is.na(strata_values)), "Strata definition must contain only comma-separated integers."),
          need(
            length(strata_values) == ncol(current_data),
            paste0(
              "The number of strata definitions provided (", length(strata_values),
              ") does not match the number of variables/items in your dataset (", ncol(current_data), "). ",
              "Please provide one stratum integer for each item."
            )
          )
      )
    }

    if (selected_method == "structure") {
      validate(need(input$cfa_model_for_reliability_input != "", "Please define the CFA model structure for Structural Reliability using lavaan syntax."))
    }
    
    # Data content validation
    if(!all(sapply(current_data, is.numeric))) {
        showNotification("Warning: Reliability analysis typically requires all data to be numeric. Non-numeric columns might cause errors or be ignored by the calculation function. Attempting to proceed with numeric columns only if possible.", type = "warning", duration = 8)
        # Depending on reliability_func, this might still fail if it doesn't handle mixed types or subsetting.
    }
    # NAs should be handled by clean_missing_data in app.R, but a check here is good.
    # Most reliability functions in R handle NAs via listwise/pairwise or require complete data.
    if(any(is.na(current_data))){ 
        showNotification("Warning: Missing values (NA) detected in the data passed to reliability. Ensure data is cleaned appropriately, as NAs can affect results (most functions use listwise/pairwise deletion or require complete data).", type = "warning", duration = 8)
    }


    # Ensure the 'reliability_func' exists (expected from utils.R)
    if (!exists("reliability_func")) {
      showNotification("Critical Error: The 'reliability_func' is not found. Please ensure 'utils.R' is correctly sourced and the function is defined.", type = "error", duration = 10)
      reliability_output_rv("Error: Calculation function not available.")
      return()
    }

    progress_id <- showNotification(paste("Calculating", selected_method, "reliability..."), duration = NULL, type = "message")
    on.exit(removeNotification(progress_id), add = TRUE)

    # --- Calculate Reliability using reliability_func ---
    tryCatch({
      # Determine correlation type for reliability_func
      # Only pass cor_kind if 'theta' is selected, otherwise reliability_func might not expect it or use its default
      correlation_argument <- if (selected_method == "theta") {
        input$correlation_type_radio # Use the ID from the conditionalPanel
      } else {
        "cor" # Default or irrelevant for other methods in your reliability_func
      }

      calculated_reliability_value <- reliability_func(
        x = current_data,
        method = selected_method,
        cor_kind = correlation_argument, 
        defined_structure = input$cfa_model_for_reliability_input, 
        strata_define = input$strata_definition_input        
      )
      
      if(is.numeric(calculated_reliability_value)){
          calculated_reliability_value <- round(as.numeric(calculated_reliability_value), 3)
      } # If it's already a string (e.g., "Calculation failed"), it will remain so.

      coeff_name_map <- c(
        alpha = "Cronbach's Alpha",
        omega = "McDonald's Omega",
        theta = "Armor's Theta",
        structure = "Structural Reliability",
        s_alpha = "Stratified Alpha"
      )
      display_coeff_name <- coeff_name_map[selected_method] %||% selected_method # Fallback to method ID if name not in map

      # Check if calculation failed within reliability_func (it might return a specific string)
      if (grepl("failed", calculated_reliability_value, ignore.case = TRUE) || grepl("N/A", calculated_reliability_value, ignore.case = TRUE)) {
          reliability_output_rv(paste0("For ", display_coeff_name, ", the result is: ", calculated_reliability_value))
          showNotification(paste0(display_coeff_name, " calculation issue: ", calculated_reliability_value), type = "warning", duration = 7)
      } else {
          reliability_output_rv(paste0("The calculated ", display_coeff_name, " is: ", calculated_reliability_value))
          showNotification("Reliability calculation completed!", type = "message", duration = 4)
      }

    }, error = function(e) {
      error_message_detail <- paste("Error during reliability calculation for", selected_method, ":", conditionMessage(e))
      showNotification(error_message_detail, type = "error", duration = 10)
      reliability_output_rv(paste("Calculation failed. Details:", conditionMessage(e)))
      # Print full error to console for detailed debugging by developer
      cat("--- ERROR CAUGHT IN reliability_server.R ---\n")
      print(e)
      cat("--- END OF ERROR DETAILS ---\n\n")
    }) # End tryCatch

  }) # End observeEvent

  # Render the reliability result
  output$reliability_result_output <- renderText({
    reliability_output_rv() %||% "Click 'Calculate Reliability' to see the result."
  })
}
