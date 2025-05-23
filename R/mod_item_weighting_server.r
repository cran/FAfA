# mod_item_weighting_server.R
 
#' Item Weighting Server Module Logic
#'
#' Handles the server-side logic for applying an item weighting algorithm
#' (defined in utils.R) to the dataset, displaying a preview of the
#' weighted scores, and providing a download option.
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param data A reactive expression returning the current dataset.
#'
#' @return This module primarily updates output elements and does not return a
#'   single reactive value for further chaining in the main app logic, unless
#'   the weighted data itself is intended to be passed on.
#'
#' @import shiny
#' @importFrom utils head write.csv
#' @noRd
item_weighting_server <- function(input, output, session, data) {
  # reactiveVal to store the weighted data
  weighted_data_rv <- reactiveVal()

  observeEvent(input$calculate_weighted_scores_button, {
    # Validate that data is available
    validate(need(data(), "Please upload your dataset first to apply item weighting."))
    
    current_data <- data()

    if (!exists("item_weighting")) {
      showNotification("Error: The 'item_weighting' function is not found. Please ensure it's defined in 'utils.R' and that 'utils.R' is sourced.", type = "error", duration = 10)
      weighted_data_rv(NULL) # Clear any previous result
      return()
    }
    

    if(!all(sapply(current_data, is.numeric))) {
        showNotification("Warning: Item weighting typically requires all data to be numeric. Non-numeric columns might cause errors or be ignored by the weighting function.", type = "warning", duration = 8)
    }
    if(any(is.na(current_data))){
        showNotification("Warning: Missing values (NA) detected in the data. The item weighting function might fail or produce unexpected results. Ensure data is cleaned.", type = "warning", duration = 8)
    }


    progress_id <- showNotification("Calculating weighted scores, please wait...", duration = NULL, type = "message")
    on.exit(removeNotification(progress_id), add = TRUE)

    tryCatch({
      calculated_weighted_data <- item_weighting(current_data)
      weighted_data_rv(calculated_weighted_data)

      output$weighted_scores_table_output <- renderTable({
        req(weighted_data_rv()) # Ensure weighted data is available
        # Display the first 10 rows (or fewer if the dataset is small)
        head(weighted_data_rv(), n = 10)
      }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE) # Added some styling to the table

      showNotification("Item weighting completed successfully!", type = "message", duration = 4)

    }, error = function(e) {
      showNotification(
        paste("Error during item weighting:", e$message,
              "Please check your data and the 'item_weighting' function in 'utils.R'."),
        type = "error",
        duration = 10 # Keep error message longer
      )
      weighted_data_rv(NULL) # Clear previous result on error
      output$weighted_scores_table_output <- renderTable({ data.frame(Error = paste("Weighting failed:", e$message)) })
    }) # End tryCatch
  }) # End observeEvent

  # Download handler for the weighted scores
  output$download_weighted_data_button <- downloadHandler(
    filename = function() {
      paste0("weighted_scores_dataset_", Sys.Date(), ".csv") # Changed to .csv for wider compatibility
    },
    content = function(file) {
      req(weighted_data_rv()) # Ensure weighted data is available before download
      # Using write.csv for better default handling of various data types
      write.csv(weighted_data_rv(), file, row.names = FALSE)
    }
  )
}
