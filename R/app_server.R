#' The application server-side
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # --- Initial Data Loading ---
  shared_data_reactive <- reactive({
    inFile <- input[["data_selection-file1"]] 
    user_has_header <- input[["data_selection-has_header_checkbox"]] %||% TRUE

    if (is.null(inFile)) return(NULL)

    tryCatch({
      ext <- tools::file_ext(tolower(inFile$name))
      df <- switch(ext,
                   "csv" = utils::read.csv(inFile$datapath, header = user_has_header, na.strings = c("NA", "", " ", ".", "na", "NaN")),
                   "xlsx" = readxl::read_excel(inFile$datapath, col_names = user_has_header, na = c("NA", "", " ")),
                   "sav" = haven::read_sav(inFile$datapath),
                   utils::read.table(inFile$datapath, header = user_has_header, na.strings = c("NA", "", " "))
      )
      
      # ÖNEMLİ DEĞİŞİKLİK: remove_na = FALSE
      # Kayıp verileri silme, olduğu gibi bırak. "Missing Values" modülü halledecek.
      cleaned_result <- clean_missing_data(df, remove_na = FALSE)
      
      if(!is.null(cleaned_result$cleaned_data)) return(cleaned_result$cleaned_data)
      return(NULL)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })

  # --- Data Selection ---
  data_selection_server("data_selection", data = shared_data_reactive)

  # --- Missing Value ---
  processed_missing_data <- mod_missing_server("missing_val", data = shared_data_reactive)

  # --- Wrangling Chain ---
  data_after_exclusion_reactive <- wrangling_server_ex_var("wrangling_ex_var", data = processed_missing_data)

  data_for_outliers_module_input <- reactive({
    excluded_data_val <- data_after_exclusion_reactive()
    if (!is.null(excluded_data_val)) return(excluded_data_val)
    val_missing <- processed_missing_data()
    if(!is.null(val_missing)) return(val_missing)
    return(shared_data_reactive())
  })

  data_without_outliers_reactive <- wrangling_server_outliers("wrangling_outliers", data = data_for_outliers_module_input)

  final_wrangled_data_reactive <- reactive({
    data_after_outliers_val <- data_without_outliers_reactive()
    if (!is.null(data_after_outliers_val)) return(data_after_outliers_val)
    excluded_data_val <- data_after_exclusion_reactive()
    if (!is.null(excluded_data_val)) return(excluded_data_val)
    missing_val_data <- processed_missing_data()
    if(!is.null(missing_val_data)) return(missing_val_data)
    return(shared_data_reactive())
  })

  wrangling_server_split("wrangling_split", data = final_wrangled_data_reactive)

  # --- Analysis Modules ---
  assumptions_server("assumptions", data = final_wrangled_data_reactive)
  mod_itemrest_server("item_rest", data = final_wrangled_data_reactive)
  efa_server_fac_ret("efa_fac_ret", data = final_wrangled_data_reactive)

  efa_settings_reactive <- moduleServer("efa_analysis", function(input, output, session) {
      reactive({
        list(number_factor = input$number_factor, rotating_method = input$rotating_method, fact_method = input$fact_method, cor_kind = input$cor_kind)
      })
  })

  returned_efa_object_reactive <- efa_server_analysis("efa_analysis", data = final_wrangled_data_reactive)
  efa_server_report("efa_report", data = final_wrangled_data_reactive, efa_output_reactive = returned_efa_object_reactive, efa_settings_reactive = efa_settings_reactive)

  ega_server("ega", data = final_wrangled_data_reactive)
  cfa_server("cfa", data = final_wrangled_data_reactive)
  inv_server("inv", data = final_wrangled_data_reactive)
  reliability_server("reliability", data = final_wrangled_data_reactive)
  item_weighting_server("item_weighting", data = final_wrangled_data_reactive)
  about_server("about")
}