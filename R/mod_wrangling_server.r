#' Wrangling Server Modules (Modern)
#' @import shiny
#' @export
#' @param id Module namespace ID.
#' @param data Input data (reactive)

# 1. Exclude Variables Server
wrangling_server_ex_var <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Tracks names of currently excluded variables
    excluded_vars_rv <- reactiveVal(character(0))

    # Active dataset: NULL when nothing excluded (so fallback chain in app_server works),
    # otherwise the original data minus excluded columns.
    data_after_exclusion_rv <- reactive({
      excl <- excluded_vars_rv()
      if (length(excl) == 0) return(NULL)
      req(data())
      remaining <- setdiff(names(data()), excl)
      if (length(remaining) == 0) return(NULL)
      data()[, remaining, drop = FALSE]
    })

    # When upstream data changes, drop any excluded names that no longer exist.
    # This handles new file uploads without wiping valid exclusions on re-imputation.
    observeEvent(data(), {
      req(data())
      excl <- excluded_vars_rv()
      if (length(excl) > 0) {
        valid_excl <- intersect(excl, names(data()))
        excluded_vars_rv(valid_excl)
      }
    }, ignoreNULL = TRUE)

    # Keep both checkbox lists in sync with current exclusion state
    observe({
      req(data())
      excl  <- excluded_vars_rv()
      avail <- setdiff(names(data()), excl)
      updateCheckboxGroupInput(session, "available_vars_checkbox",
                               choices = avail, selected = NULL)
      updateCheckboxGroupInput(session, "excluded_vars_checkbox",
                               choices = excl, selected = NULL)
    })

    # Summary counts
    output$variable_summary <- renderUI({
      req(data())
      excl    <- excluded_vars_rv()
      total   <- length(names(data()))
      n_excl  <- length(excl)
      n_act   <- total - n_excl
      div(
        class = "d-flex justify-content-around text-center py-1",
        div(h5(total, class = "mb-0 text-primary"), tags$small("Total")),
        div(h5(n_excl, class = "mb-0 text-danger"),  tags$small("Excluded")),
        div(h5(n_act,  class = "mb-0 text-success"), tags$small("Active"))
      )
    })

    # --- Exclude ---
    observeEvent(input$exclude_button, {
      req(data())
      selected <- input$available_vars_checkbox
      if (is.null(selected) || length(selected) == 0) {
        showNotification("Please select at least one variable to exclude.",
                         type = "warning")
        return()
      }
      new_excl   <- union(excluded_vars_rv(), selected)
      n_remaining <- length(names(data())) - length(new_excl)
      if (n_remaining == 0) {
        showNotification("Cannot exclude all variables.", type = "error")
        return()
      }
      excluded_vars_rv(new_excl)
      showNotification(
        paste0(length(selected), " variable(s) excluded. ",
               "Active dataset: ", n_remaining, " variable(s)."),
        type     = "message",
        duration = 4
      )
    })

    # --- Recover selected ---
    observeEvent(input$recover_button, {
      selected <- input$excluded_vars_checkbox
      if (is.null(selected) || length(selected) == 0) {
        showNotification("Please select at least one variable to recover.",
                         type = "warning")
        return()
      }
      new_excl <- setdiff(excluded_vars_rv(), selected)
      excluded_vars_rv(new_excl)
      req(data())
      n_act <- length(names(data())) - length(new_excl)
      showNotification(
        paste0(length(selected), " variable(s) recovered. ",
               "Active dataset: ", n_act, " variable(s)."),
        type     = "message",
        duration = 4
      )
    })

    # --- Reset all ---
    observeEvent(input$reset_button, {
      excluded_vars_rv(character(0))
      showNotification("All variables restored to active dataset.",
                       type = "message")
    })

    # --- Download active data ---
    output$download_excluded_data_button <- downloadHandler(
      filename = function() "active_dataset.csv",
      content  = function(file) {
        d <- data_after_exclusion_rv()
        if (is.null(d)) d <- data()
        write.csv(d, file, row.names = FALSE)
      }
    )

    return(data_after_exclusion_rv)
  })
}

# 2. Split Data
wrangling_server_split <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    split_datasets_rv <- reactiveValues(first_half = NULL, second_half = NULL)

    observeEvent(input$split_data_button, {
      req(data())
      tryCatch({
        df <- data()
        n1 <- floor((input$split_percentage_slider / 100) * nrow(df))
        idx <- sample.int(nrow(df), n1)
        split_datasets_rv$first_half  <- df[ idx, , drop = FALSE]
        split_datasets_rv$second_half <- df[-idx, , drop = FALSE]
        showNotification("Data split successfully!", type = "message")
      }, error = function(e) showNotification(e$message, type = "error"))
    })

    output$download_first_subset_button <- downloadHandler(
      filename = "subset1.csv",
      content  = function(file) write.csv(split_datasets_rv$first_half,  file, row.names = FALSE)
    )
    output$download_second_subset_button <- downloadHandler(
      filename = "subset2.csv",
      content  = function(file) write.csv(split_datasets_rv$second_half, file, row.names = FALSE)
    )
  })
}

# 3. Manage Outliers
wrangling_server_outliers <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    outlier_info_rv <- reactiveValues(table = NULL, count = NULL,
                                      data_clean = NULL, indices = NULL)

    observeEvent(input$check_outliers_button, {
      req(data())
      tryCatch({
        res <- assumptions(data(), mah_p_threshold = input$mah_p_value_threshold_input)
        outlier_info_rv$table   <- res$Mah_significant
        outlier_info_rv$count   <- res$n_outlier
        outlier_info_rv$indices <- res$Mah_significant$Row_Number_In_Complete_Data
        showNotification("Outlier check complete.", type = "message")
      }, error = function(e) showNotification(e$message, type = "error"))
    })

    observeEvent(input$remove_outliers_button, {
      req(data(), outlier_info_rv$indices)
      clean <- stats::na.omit(data())
      if (length(outlier_info_rv$indices) > 0) {
        outlier_info_rv$data_clean <- clean[-outlier_info_rv$indices, , drop = FALSE]
        showNotification("Outliers removed!", type = "message")
      }
    })

    output$outliers_table     <- renderTable({ outlier_info_rv$table })
    output$outlier_count_text <- renderText({ paste("Outliers found:", outlier_info_rv$count) })

    output$download_data_no_outliers_button <- downloadHandler(
      filename = "data_no_outliers.csv",
      content  = function(file) write.csv(outlier_info_rv$data_clean, file, row.names = FALSE)
    )

    return(reactive(outlier_info_rv$data_clean))
  })
}
