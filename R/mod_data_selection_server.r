# mod_data_selection_server.R
 
#' Data Selection Server Logic
#'
#' Handles the server-side logic for the data selection module, including
#' triggering analysis, displaying a preview of the data, and showing
#' summary statistics in infoBoxes.
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param data A reactive expression returning the initially loaded and cleaned dataset
#'   from the main app server (app_server.R).
#'
#' @return This module primarily updates output elements and does not return a
#'   single reactive value for further chaining in the main app logic, as the
#'   'data' it receives is already the one to be used by other modules
#'   after the 'Analyze Data' button is clicked (via the 'analyzed_data' reactive).
#'
#' @import shiny
#' @importFrom utils head 
#' @importFrom shinydashboard renderInfoBox infoBox 
#' @importFrom stats na.omit
#' @noRd
data_selection_server <- function(input, output, session, data) {
  analyzed_data <- eventReactive(input$analyze_data, {
        req(data()) 
    data()      
  })
  # Output for displaying the first 10 rows of the uploaded data.
  # This uses the 'data' reactive directly, so it updates as soon as a file is loaded.
  output$mydatatable <- renderTable({
    # Validate that data is available before trying to display it.
    validate(need(data(), "Please upload your dataset to see a preview."))
    # Display the first 10 rows.
    utils::head(data(), 10)
  })

  # --- Summary Statistics InfoBoxes ---
  output$n_var <- renderInfoBox({
    validate(need(analyzed_data(), "Click 'Analyze Data' to view summary statistics."))
    infoBox(
      title = "Number of Variables", # English title
      value = ncol(analyzed_data()),
      subtitle = "Total columns in the dataset", # English subtitle
      color = "green", # Theme color for the box
      fill = FALSE,    # Whether the box background is filled with color
      icon = icon("columns") # Changed icon for variables
    )
  })

  # InfoBox for the sample size (number of rows).
  output$n <- renderInfoBox({
    validate(need(analyzed_data(), "Click 'Analyze Data' to view summary statistics."))
    infoBox(
      title = "Sample Size", # English title
      value = nrow(analyzed_data()),
      subtitle = "Total rows (observations)", # English subtitle
      color = "green",
      fill = FALSE,
      icon = icon("users") # Changed icon for sample size
    )
  })


  output$min_value <- renderInfoBox({
    validate(need(analyzed_data(), "Click 'Analyze Data' to view summary statistics."))
    # Add a tryCatch for robustness in case data is not purely numeric
    min_val <- tryCatch({
      # Calculate min only for numeric columns, then take overall min
      numeric_data_only <- analyzed_data()[,sapply(analyzed_data(), is.numeric), drop = FALSE]
      if(ncol(numeric_data_only) == 0) return(NA) # No numeric columns
      min(sapply(numeric_data_only, min, na.rm = TRUE), na.rm = TRUE)
    }, error = function(e) { NA }) # Return NA on error

     display_min_val <- "N/A" # Default display value
     if (!is.na(min_val) && !is.infinite(min_val)) {
       display_min_val <- round(min_val, 2)
     } else if (is.infinite(min_val) && min_val < 0) {
       display_min_val <- "-Infinity (check data)"
     } else if (is.na(min_val) || (is.infinite(min_val) && min_val > 0) ){ # Handles NA or +Inf from empty numeric data
        display_min_val <- "N/A (no numeric data or all NA)"
     }


    infoBox(
      title = "Dataset Minimum Value", 
      value = display_min_val,
      subtitle = "Smallest value in numeric columns", 
      color = "blue",
      fill = FALSE,
      icon = icon("arrow-down")
    )
  })

  # InfoBox for the maximum value across the entire dataset.
  output$max_value <- renderInfoBox({
    validate(need(analyzed_data(), "Click 'Analyze Data' to view summary statistics."))
    max_val <- tryCatch({
      numeric_data_only <- analyzed_data()[,sapply(analyzed_data(), is.numeric), drop = FALSE]
      if(ncol(numeric_data_only) == 0) return(NA)
      max(sapply(numeric_data_only, max, na.rm = TRUE), na.rm = TRUE)
    }, error = function(e) { NA })

    display_max_val <- "N/A"
    if (!is.na(max_val) && !is.infinite(max_val)) {
      display_max_val <- round(max_val, 2)
    } else if (is.infinite(max_val) && max_val > 0) {
      display_max_val <- "+Infinity (check data)"
    } else if (is.na(max_val) || (is.infinite(max_val) && max_val < 0) ){
       display_max_val <- "N/A (no numeric data or all NA)"
    }

    infoBox(
      title = "Dataset Maximum Value", # English title
      value = display_max_val,
      subtitle = "Largest value in numeric columns", # English subtitle
      color = "blue",
      fill = FALSE,
      icon = icon("arrow-up")
    )
  })

  output$num_cat <- renderInfoBox({
    validate(need(analyzed_data(), "Click 'Analyze Data' to view summary statistics."))

    num_cat_display <- "N/A" # Default display
    
    # Use previously calculated min_val and max_val if possible, or recalculate robustly
    min_val_for_cat <- tryCatch({
      numeric_data_only <- analyzed_data()[,sapply(analyzed_data(), is.numeric), drop = FALSE]
      if(ncol(numeric_data_only) == 0) return(NA)
      min(sapply(numeric_data_only, min, na.rm = TRUE), na.rm = TRUE)
    }, error = function(e) { NA })
    
    max_val_for_cat <- tryCatch({
      numeric_data_only <- analyzed_data()[,sapply(analyzed_data(), is.numeric), drop = FALSE]
      if(ncol(numeric_data_only) == 0) return(NA)
      max(sapply(numeric_data_only, max, na.rm = TRUE), na.rm = TRUE)
    }, error = function(e) { NA })

    if (!is.na(min_val_for_cat) && !is.na(max_val_for_cat) && 
        !is.infinite(min_val_for_cat) && !is.infinite(max_val_for_cat)) {
      
      # Check if data in numeric columns appears to be integer-based for this calculation to make sense
      numeric_data_for_check <- analyzed_data()[,sapply(analyzed_data(), is.numeric), drop = FALSE]
      if (ncol(numeric_data_for_check) > 0) {
          is_likely_integer_based <- all(sapply(numeric_data_for_check, function(col) {
                                            all(na.omit(col) %% 1 == 0)
                                        }))
          if(is_likely_integer_based){
            num_cat_display <- max_val_for_cat - min_val_for_cat + 1
          } else {
            num_cat_display <- "Mixed/Non-integer data"
          }
      } else {
          num_cat_display <- "No numeric data"
      }
    } else if (is.na(min_val_for_cat) || is.na(max_val_for_cat) || is.infinite(min_val_for_cat) || is.infinite(max_val_for_cat)) {
        num_cat_display <- "N/A (no valid numeric range)"
    }

    infoBox(
      title = "Implied Range of Categories", 
      value = num_cat_display,
      subtitle = "(Max - Min + 1) for integer numeric data", 
      color = "yellow",
      fill = FALSE,
      icon = icon("sort-numeric-down") 
    )
  })
}
