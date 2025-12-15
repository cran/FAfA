#' Wrangling Server Modules (Modern)
#' @import shiny
#' @export
#' @param id Module namespace ID.
#' @param data Input data (reactive)
# 1. Exclude Variables
wrangling_server_ex_var <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    data_after_exclusion_rv <- reactiveVal()

    observeEvent(input$exclude_button, {
      req(data(), input$excluded_variables_input)
      current_data <- data()
      tryCatch({
        # (Mevcut ayrıştırma mantığı aynen korunur)
        parts <- trimws(unlist(strsplit(input$excluded_variables_input, ",")))
        user_excluded_indices <- integer(0)
        for (part in parts) {
          if (grepl(":", part)) {
            range_parts <- as.integer(trimws(unlist(strsplit(part, ":"))))
            user_excluded_indices <- c(user_excluded_indices, seq(range_parts[1], range_parts[2]))
          } else {
            user_excluded_indices <- c(user_excluded_indices, as.integer(part))
          }
        }
        if(length(user_excluded_indices) > 0) {
           data_after_exclusion_rv(current_data[, -user_excluded_indices, drop = FALSE])
           showNotification("Variables excluded!", type = "message")
        }
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    })

    output$download_excluded_data_button <- downloadHandler(
      filename = function() "excluded_vars_data.csv",
      content = function(file) write.csv(data_after_exclusion_rv(), file, row.names = FALSE)
    )

    return(reactive(data_after_exclusion_rv()))
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
        split_datasets_rv$first_half <- df[idx, , drop=FALSE]
        split_datasets_rv$second_half <- df[-idx, , drop=FALSE]
        showNotification("Data split successfully!", type = "message")
      }, error = function(e) showNotification(e$message, type="error"))
    })

    output$download_first_subset_button <- downloadHandler(
      filename = "subset1.csv", content = function(file) write.csv(split_datasets_rv$first_half, file, row.names=FALSE)
    )
    output$download_second_subset_button <- downloadHandler(
      filename = "subset2.csv", content = function(file) write.csv(split_datasets_rv$second_half, file, row.names=FALSE)
    )
  })
}

# 3. Manage Outliers
wrangling_server_outliers <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    outlier_info_rv <- reactiveValues(table = NULL, count = NULL, data_clean = NULL, indices = NULL)

    observeEvent(input$check_outliers_button, {
      req(data())
      tryCatch({
        res <- assumptions(data(), mah_p_threshold = input$mah_p_value_threshold_input)
        outlier_info_rv$table <- res$Mah_significant
        outlier_info_rv$count <- res$n_outlier
        outlier_info_rv$indices <- res$Mah_significant$Row_Number_In_Complete_Data
        showNotification("Outlier check complete.", type="message")
      }, error = function(e) showNotification(e$message, type="error"))
    })

    observeEvent(input$remove_outliers_button, {
      req(data(), outlier_info_rv$indices)
      # Basit çıkarma mantığı (na.omit varsayımıyla)
      clean <- stats::na.omit(data()) # Önce temizle
      if(length(outlier_info_rv$indices) > 0){
         outlier_info_rv$data_clean <- clean[-outlier_info_rv$indices, , drop=FALSE]
         showNotification("Outliers removed!", type="message")
      }
    })

    output$outliers_table <- renderTable({ outlier_info_rv$table })
    output$outlier_count_text <- renderText({ paste("Outliers found:", outlier_info_rv$count) })

    output$download_data_no_outliers_button <- downloadHandler(
      filename = "data_no_outliers.csv", content = function(file) write.csv(outlier_info_rv$data_clean, file, row.names=FALSE)
    )

    return(reactive(outlier_info_rv$data_clean))
  })
}
