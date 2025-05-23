#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
# Server Logic
app_server <- function(input, output, session) {

  # --- Initial Data Loading and Cleaning (Supporting Multiple Formats with Dynamic Header) ---
  shared_data_reactive <- reactive({
    inFile <- input[["data_selection-file1"]] # From data_selection_ui module

    user_has_header <- input[["data_selection-has_header_checkbox"]]
    if(is.null(user_has_header)) {
      user_has_header <- TRUE # Default if checkbox not rendered yet
    }

    if (is.null(inFile)) {
      return(NULL)
    }

    tryCatch({
      ext <- tools::file_ext(tolower(inFile$name))

      df <- switch(ext,
                   "csv" = utils::read.csv(inFile$datapath, header = user_has_header, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", " ", ".", "-", "?", "missing")),
                   "dat" = utils::read.table(inFile$datapath, header = user_has_header, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", " ", ".", "-", "?", "missing")),
                   "txt" = utils::read.delim(inFile$datapath, header = user_has_header, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", " ", ".", "-", "?", "missing")),
                   "sav" = {
                     if (!requireNamespace("haven", quietly = TRUE)) {
                       showNotification("Package 'haven' is required to load .sav files. Please install it.", type = "error", duration = 7)
                       return(NULL)
                     }
                     haven::read_sav(inFile$datapath)
                   },
                   "xlsx" = {
                     if (!requireNamespace("readxl", quietly = TRUE)) {
                       showNotification("Package 'readxl' is required to load .xlsx files. Please install it.", type = "error", duration = 7)
                       return(NULL)
                     }
                     readxl::read_excel(inFile$datapath, col_names = user_has_header, na = c("", "NA", "N/A", " ", ".", "-", "?", "missing"))
                   },
                   "xls" = {
                     if (!requireNamespace("readxl", quietly = TRUE)) {
                       showNotification("Package 'readxl' is required to load .xls files. Please install it.", type = "error", duration = 7)
                       return(NULL)
                     }
                     readxl::read_excel(inFile$datapath, col_names = user_has_header, na = c("", "NA", "N/A", " ", ".", "-", "?", "missing"))
                   },
                   {
                     showNotification(
                       paste("Unsupported file type: '", ext, "'. Please upload a supported file (CSV, DAT, TXT, SAV, XLS, XLSX).", sep=""),
                       type = "error",
                       duration = 7
                     )
                     return(NULL)
                   }
      )

      if (is.null(df)) {
        return(NULL)
      }

      # Call clean_missing_data directly as it's part of the package.
      cleaned_result <- clean_missing_data(df)

      if(!is.null(cleaned_result$cleaned_data)){
        return(cleaned_result$cleaned_data)
      } else {
        # This case might occur if clean_missing_data itself returns NULL for cleaned_data
        # or if the structure of df prevents clean_missing_data from working correctly.
        showNotification("Data cleaning resulted in no data or an issue within the cleaning function. Please check data and function.", type = "warning", duration = 8)
        return(NULL) # Or return(df) if returning raw data is more appropriate in this scenario
      }
    }, error = function(e) {
      showNotification(paste("Error loading or processing file:", e$message), type = "error", duration = 7)
      return(NULL)
    })
  })

  # --- Data Selection Module ---
  # The data_selection_server module receives the shared_data_reactive.
  # It's expected to update UI elements like infoBoxes based on this data,
  # rather than directly passing a processed dataset to the next module in the main chain.
  callModule(data_selection_server, "data_selection", data = shared_data_reactive)


  # --- Wrangling Data Modules Chain ---
  # data_after_exclusion_reactive holds the data after variables are excluded.
  data_after_exclusion_reactive <- callModule(
    module = wrangling_server_ex_var,
    id = "wrangling_ex_var",
    data = shared_data_reactive # Starts with the raw, cleaned data
  )

  # data_for_outliers_module_input determines which data goes to the outliers module.
  # Priority is given to data from which variables have been excluded;
  # if not available, the original cleaned data is used.
  data_for_outliers_module_input <- reactive({
    excluded_data_val <- data_after_exclusion_reactive()
    if (!is.null(excluded_data_val)) {
      return(excluded_data_val)
    }
    # If variable exclusion was not performed or resulted in NULL,
    # use the original cleaned data (shared_data_reactive).
    return(shared_data_reactive())
  })

  # data_without_outliers_reactive holds the data after outliers are removed.
  data_without_outliers_reactive <- callModule(
    module = wrangling_server_outliers,
    id = "wrangling_outliers",
    data = data_for_outliers_module_input # Starts with data from which variables were excluded (or original)
  )

  # final_wrangled_data_reactive represents the final dataset after all wrangling steps.
  # This will be passed as input to all subsequent analysis modules.
  final_wrangled_data_reactive <- reactive({
    # Priority is given to data from which outliers have been removed.
    data_after_outliers_val <- data_without_outliers_reactive()
    if (!is.null(data_after_outliers_val)) {
      return(data_after_outliers_val)
    }
    # If outlier removal was not performed or resulted in NULL,
    # use the data from which variables were excluded.
    excluded_data_val <- data_after_exclusion_reactive()
    if (!is.null(excluded_data_val)) {
      return(excluded_data_val)
    }
    # If that's also not available (no wrangling was done),
    # use the original cleaned data (shared_data_reactive).
    return(shared_data_reactive())
  })

  # The wrangling_server_split module takes final_wrangled_data_reactive
  # but creates two separate datasets internally and makes them downloadable.
  # It does not directly affect the main data flow (i.e., doesn't return a single dataset to the next module).
  callModule(
    module = wrangling_server_split,
    id = "wrangling_split",
    data = final_wrangled_data_reactive
  )

  # --- Analysis Modules ---
  # All analysis modules use final_wrangled_data_reactive as input.
  callModule(assumptions_server, "assumptions", data = final_wrangled_data_reactive)
  callModule(efa_server_fac_ret, "efa_fac_ret", data = final_wrangled_data_reactive)

  # A reactive block to get EFA settings from the efa_analysis UI.
  # This will be passed to the efa_server_analysis and efa_server_report modules.
  efa_settings_reactive <- callModule(
    module = function(input, output, session) {
      reactive({
        list(
          number_factor = input$number_factor,
          rotating_method = input$rotating_method,
          fact_method = input$fact_method,
          cor_kind = input$cor_kind
          # These input IDs should match the IDs of UI elements in the efa_ui_analysis module.
          # e.g., ns("number_factor") in UI becomes "efa_analysis-number_factor" here.
        )
      })
    },
    id = "efa_analysis" # The ID of the efa_ui_analysis module
  )

  # Module that runs the EFA analysis and returns the EFA object.
  returned_efa_object_reactive <- callModule(
    module = efa_server_analysis,
    id = "efa_analysis", # Should be the same as the efa_ui_analysis module's ID (it gets inputs from there)
    data = final_wrangled_data_reactive
  )

  # Module that reports the EFA results.
  callModule(
    module = efa_server_report,
    id = "efa_report",
    data = final_wrangled_data_reactive, # Can also take raw data for KMO, Bartlett.
    efa_output_reactive = returned_efa_object_reactive, # The EFA object
    efa_settings_reactive = efa_settings_reactive       # EFA settings
  )

  callModule(ega_server, "ega", data = final_wrangled_data_reactive)
  callModule(cfa_server, "cfa", data = final_wrangled_data_reactive)
  callModule(inv_server, "inv", data = final_wrangled_data_reactive)
  callModule(reliability_server, "reliability", data = final_wrangled_data_reactive)
  callModule(item_weighting_server, "item_weighting", data = final_wrangled_data_reactive)

  # --- About Module ---
  callModule(about_server, "about") # The 'about' module does not take data.
}
