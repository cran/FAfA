#' EFA Factor Retention Server Module
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @export
efa_server_fac_ret <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run_factor_ret, {
      req(data())

      # Scree Plot
      output$scree_plot <- renderPlot({
        tryCatch({
          if (requireNamespace("EFAtools", quietly = TRUE)) {
            plot(EFAtools::SCREE(data()))
          } else {
            psych::scree(data())
          }
        }, error = function(e) {
          plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Error in Scree Plot")
          text(0.5, 0.5, e$message)
        })
      })

      # Table Results
      output$dim_ret_results <- renderTable({
        if(exists("factor_ret")) {
          factor_ret(data(), method = input$dimension_methods)
        } else {
          data.frame(Error = "Function factor_ret not found.")
        }
      }, rownames=TRUE)
    })
  })
}

#' EFA Analysis Server Module
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @export
efa_server_analysis <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    efa_res <- reactiveVal(NULL)

    observeEvent(input$run_efa, {
      req(data())
      showNotification("Running EFA...", type="message")

      tryCatch({
        cor_type_arg <- if(input$cor_kind == "pea") "cor" else "poly"

        res <- psych::fa(
          r = data(),
          nfactors = as.numeric(input$number_factor),
          rotate = input$rotating_method,
          fm = input$fact_method,
          cor = cor_type_arg
        )

        efa_res(res)
        showNotification("EFA Completed!", type="message")

      }, error = function(e) {
        showNotification(paste("EFA Failed:", e$message), type="error", duration=10)
        efa_res(NULL)
      })
    })
    return(efa_res)
  })
}

#' EFA Reporting Server Module
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @param efa_output_reactive Reactive containing the EFA results.
#' @param efa_settings_reactive Reactive containing the EFA settings.
#' @export
efa_server_report <- function(id, data, efa_output_reactive, efa_settings_reactive) {
  moduleServer(id, function(input, output, session) {

    # KMO & Bartlett logic
    output$kmo_result <- renderUI({
      req(data())
      tryCatch({
        kmo_res <- psych::KMO(data())
        val <- round(kmo_res$MSA, 3)
        color <- if(val >= 0.8) "green" else if(val >= 0.6) "orange" else "red"
        HTML(paste0("<b>KMO Measure of Sampling Adequacy:</b> <span style='color:", color, "'>", val, "</span>"))
      }, error = function(e) paste("Error:", e$message))
    })

    output$bartlett <- renderTable({
      req(data())
      tryCatch({
        cor_mat <- if(efa_settings_reactive()$cor_kind == "pea") stats::cor(data()) else psych::polychoric(data())$rho
        test <- psych::cortest.bartlett(cor_mat, n = nrow(data()))
        data.frame(
          Statistic = round(test$chisq, 2),
          df = test$df,
          p_value = if(test$p.value < 0.001) "< .001" else round(test$p.value, 3)
        )
      }, error = function(e) data.frame(Error = e$message))
    })

    # Loadings Table
    output$efa_result_str <- renderTable({
      req(efa_output_reactive())
      res <- efa_output_reactive()
      loadings_df <- as.data.frame(unclass(res$loadings))
      if(!is.null(res$communality)) {
        loadings_df$h2 <- res$communality
      }
      return(loadings_df)
    }, rownames = TRUE, digits = 3)

    # Variance Table
    output$efa_result_expl_var <- renderTable({
      req(efa_output_reactive())
      res <- efa_output_reactive()
      if(is.null(res$Vaccounted)) {
        return(data.frame(Message = "Variance table not available."))
      }
      as.data.frame(unclass(res$Vaccounted))
    }, rownames = TRUE, digits = 3)

    # Phi Matrix
    output$efa_result_interf_cor <- renderTable({
      req(efa_output_reactive())
      res <- efa_output_reactive()
      if(is.null(res$Phi)) {
        return(data.frame(
          Info = "Correlations not available. Reasons: 1) Orthogonal rotation used, or 2) Only 1 factor extracted."
        ))
      }
      as.data.frame(unclass(res$Phi))
    }, rownames = TRUE, digits = 3)

    # Heatmap
    output$heat_map <- renderPlot({
      req(data())
      cor_mat <- if(efa_settings_reactive()$cor_kind == "pea") stats::cor(data()) else psych::polychoric(data())$rho
      ggcorrplot::ggcorrplot(cor_mat, type="lower", lab = (ncol(cor_mat) < 15))
    })

    # Download Loadings
    output$download_efa_loadings <- downloadHandler(
      filename = "efa_loadings.csv",
      content = function(file) {
        req(efa_output_reactive())
        res <- efa_output_reactive()
        write.csv(unclass(res$loadings), file)
      }
    )
  })
}
