#' Reliability Analysis Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive)
#' @export
reliability_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Helper for NULL
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

    reliability_output_rv <- reactiveVal(NULL)

    # --- YENİ KISIM 1: Listeyi Doldur ---
    observe({
      req(data())
      # Veri seti değiştiğinde seçim listesini güncelle
      updateSelectizeInput(session, "reliability_item_select", choices = names(data()))
    })
    # ------------------------------------

    observeEvent(input$run_reliability_button, {
      req(data(), input$reliability_coefficient_select)

      # --- YENİ KISIM 2: Veri Seçimi Mantığı ---
      raw_data <- data()

      # Eğer kullanıcı madde seçmişse, sadece o sütunları al.
      # Seçmemişse (NULL ise) tüm veriyi kullan.
      if (!is.null(input$reliability_item_select) && length(input$reliability_item_select) > 0) {
        current_data <- raw_data[, input$reliability_item_select, drop = FALSE]
      } else {
        current_data <- raw_data
      }
      # -----------------------------------------

      if(!all(sapply(current_data, is.numeric))) {
        showNotification("Warning: Non-numeric columns present. They will be excluded automatically if possible.", type="warning")
      }

      progress_id <- showNotification("Calculating...", duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        # utils.R'deki reliability_func fonksiyonunu cagriyoruz
        res <- reliability_func(
          x = current_data,
          method = input$reliability_coefficient_select,
          cor_kind = input$correlation_type_radio %||% "cor",
          defined_structure = input$cfa_model_for_reliability_input, # (Not: UI'da bu input gizli/yok ama fonksiyonda var, sorun değil NULL gider)
          strata_define = input$strata_definition_input
        )
        reliability_output_rv(res)

      }, error = function(e) {
        reliability_output_rv(paste("Error:", e$message))
      })
    })

    output$reliability_result_output <- renderText({
      reliability_output_rv() %||% "Result will appear here."
    })
  })
}
