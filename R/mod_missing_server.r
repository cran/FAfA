#' Missing Value Handling Server Module
#'
#' @param id Module namespace ID.
#' @param data Reactive containing the input dataset.
#' @return Reactive containing the processed (imputed) dataset.
#' @import shiny
#' @importFrom naniar vis_miss mcar_test
#' @importFrom ggplot2 theme element_text
#' @importFrom stats na.omit median
#' @importFrom utils write.csv
#' @importFrom graphics image
#' @export
mod_missing_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    processed_data_rv <- reactiveVal(NULL)

    # Initialize with original data
    observeEvent(data(), {
      req(data())
      processed_data_rv(data())
    })

    # 1. Visualization
    output$missing_plot <- renderPlot({
      req(processed_data_rv())
      current_df <- processed_data_rv()

      if (requireNamespace("naniar", quietly = TRUE)) {
        naniar::vis_miss(current_df) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
      } else {
        image(is.na(current_df), main = "Black areas represent missing values", axes = FALSE)
      }
    })

    # 2. MCAR Test (Button Triggered)
    mcar_result_trigger <- eventReactive(input$run_mcar_button, {
      req(data())
      raw_df <- data()

      if (sum(is.na(raw_df)) == 0) {
        return("No missing values found. MCAR test not applicable.")
      }

      showNotification("Running Little's MCAR Test...", type = "message")

      if (requireNamespace("naniar", quietly = TRUE)) {
        tryCatch({
          res <- naniar::mcar_test(raw_df)
          p_val <- res$p.value
          interp <- if(!is.na(p_val) && p_val < 0.05) {
            "RESULT: p < 0.05\nInterpretation: Data is likely NOT MCAR (Systematic missingness)."
          } else {
            "RESULT: p > 0.05\nInterpretation: Data is likely Missing Completely at Random (MCAR)."
          }
          paste0("Statistic: ", round(res$statistic, 2), "\ndf: ", res$df, "\np-value: ", format.pval(p_val, eps=0.001), "\n\n", interp)
        }, error = function(e) paste("Error running MCAR test. Ensure all columns are numeric.\nDetails:", e$message))
      } else {
        "Package 'naniar' is required."
      }
    })

    output$mcar_output <- renderPrint({
      req(mcar_result_trigger())
      cat(mcar_result_trigger())
    })

    # 3. Summary Table
    output$missing_summary_table <- renderTable({
      req(processed_data_rv())
      df <- processed_data_rv()
      miss_counts <- colSums(is.na(df))
      summary_df <- data.frame(Variable = names(df), Missing_Count = miss_counts, Missing_Percentage = round((miss_counts / nrow(df)) * 100, 2))
      summary_df[summary_df$Missing_Count > 0, ]
    }, rownames = FALSE, striped = TRUE)

    # 4. Imputation Logic (UPDATED FOR 2 RF METHODS)
    observeEvent(input$apply_imputation, {
      req(data())
      raw_df <- data()
      method <- input$imputation_method

      # Uzun süren işlemler için bildirim
      if(grepl("missForest", method) || method %in% c("amelia", "mice")) {
        showNotification("Running advanced imputation... Please wait.", type = "message", duration = 5)
      }

      clean_df <- tryCatch({

        if (method == "listwise") {
          stats::na.omit(raw_df)

        } else if (method == "mean") {
          as.data.frame(lapply(raw_df, function(x) {
            if(is.numeric(x)) x[is.na(x)] <- mean(x, na.rm = TRUE)
            return(x)
          }))

        } else if (method == "median") {
          as.data.frame(lapply(raw_df, function(x) {
            if(is.numeric(x)) x[is.na(x)] <- stats::median(x, na.rm = TRUE)
            return(x)
          }))

        } else if (method == "amelia") {
          if (!requireNamespace("Amelia", quietly = TRUE)) stop("Package 'Amelia' required.")
          Amelia::amelia(raw_df, m = 1, p2s = 0, idvars = NULL)$imputations[[1]]

        } else if (method == "mice") {
          if (!requireNamespace("mice", quietly = TRUE)) stop("Package 'mice' required.")
          mice::complete(mice::mice(raw_df, m = 1, printFlag = FALSE), 1)

        } else if (method == "missForest_cont") {
          # SÜREKLİ ATAMA (Regresyon)
          if (!requireNamespace("missForest", quietly = TRUE)) stop("Package 'missForest' required.")
          # Doğrudan nümerik olarak veriyoruz
          missForest::missForest(raw_df, verbose = FALSE)$ximp

        } else if (method == "missForest_cat") {
          # KATEGORİK ATAMA (Sınıflandırma)
          if (!requireNamespace("missForest", quietly = TRUE)) stop("Package 'missForest' required.")

          # Hepsini faktöre çevir ki RF sınıflandırma yapsın
          df_factors <- raw_df
          df_factors[] <- lapply(df_factors, as.factor)

          # missForest çalıştır
          rf_out <- missForest::missForest(df_factors, verbose = FALSE)$ximp

          # Tekrar nümeriğe çevir
          rf_out[] <- lapply(rf_out, function(x) as.numeric(as.character(x)))
          rf_out

        } else {
          raw_df
        }

      }, error = function(e) {
        showNotification(paste("Imputation Error:", e$message), type = "error", duration = 8)
        return(raw_df)
      })

      processed_data_rv(clean_df)

      if(method != "none") {
        showNotification(paste("Applied:", method, "| Data updated."), type = "message")
      }
    })

    # 5. Download Handler
    output$download_data <- downloadHandler(
      filename = function() paste0("processed_data_", input$imputation_method, "_", Sys.Date(), ".csv"),
      content = function(file) {
        req(processed_data_rv())
        utils::write.csv(processed_data_rv(), file, row.names = FALSE)
      }
    )

    return(processed_data_rv)
  })
}
