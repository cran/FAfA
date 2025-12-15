#' Data Selection Server Logic (Modern)
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @export
data_selection_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    analyzed_data <- eventReactive(input$analyze_data, {
      req(data())
      data()
    })

    # Preview Table
    output$mydatatable <- renderTable({
      validate(need(data(), "Please upload your dataset."))
      utils::head(data(), 10)
    })

    # --- Value Boxes (bslib) ---

    output$n_var_box <- renderUI({
      req(analyzed_data())
      value_box(
        title = "Variables", value = ncol(analyzed_data()),
        showcase = bsicons::bs_icon("columns"), theme = "primary"
      )
    })

    output$n_obs_box <- renderUI({
      req(analyzed_data())
      value_box(
        title = "Sample Size", value = nrow(analyzed_data()),
        showcase = bsicons::bs_icon("people"), theme = "success"
      )
    })

    get_numeric_data <- reactive({
      req(analyzed_data())
      df <- analyzed_data()
      df[, sapply(df, is.numeric), drop = FALSE]
    })

    output$min_val_box <- renderUI({
      req(get_numeric_data())
      val <- tryCatch(min(get_numeric_data(), na.rm = TRUE), error = function(e) NA)
      val <- if(is.infinite(val)) "NA" else round(val, 2)
      value_box(
        title = "Min Value", value = val,
        showcase = bsicons::bs_icon("arrow-down"), theme = "info"
      )
    })

    output$max_val_box <- renderUI({
      req(get_numeric_data())
      val <- tryCatch(max(get_numeric_data(), na.rm = TRUE), error = function(e) NA)
      val <- if(is.infinite(val)) "NA" else round(val, 2)
      value_box(
        title = "Max Value", value = val,
        showcase = bsicons::bs_icon("arrow-up"), theme = "warning"
      )
    })

    output$cat_range_box <- renderUI({
      req(get_numeric_data())
      num_data <- get_numeric_data()
      res <- "N/A"
      if(ncol(num_data) > 0) {
        min_v <- min(num_data, na.rm=TRUE); max_v <- max(num_data, na.rm=TRUE)
        if(!is.infinite(min_v)) res <- paste0(min_v, " - ", max_v)
      }
      value_box(
        title = "Range", value = res,
        showcase = bsicons::bs_icon("rulers"), theme = "secondary"
      )
    })
  })
}
