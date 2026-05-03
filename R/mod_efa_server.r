#' Shared Helper: Compute correlation matrix
#' Reused across modules; cached via reactive in callers.
#' @noRd
compute_cor_matrix <- function(data, kind = c("pea", "poly")) {
  kind <- match.arg(kind)
  d <- stats::na.omit(data)
  if (kind == "pea") stats::cor(d)
  else suppressWarnings(psych::polychoric(d)$rho)
}

#' EFA Factor Retention Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @noRd
efa_server_fac_ret <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Show placeholder until Run is pressed
    results_rv <- reactiveValues(table = NULL, scree = NULL, ready = FALSE)

    numeric_complete_data <- function(d) {
      if (is.null(d)) return(NULL)
      d <- as.data.frame(d)
      d <- d[, sapply(d, is.numeric), drop = FALSE]
      if (ncol(d) < 2) return(NULL)
      d <- stats::na.omit(d)
      if (nrow(d) < 3) return(NULL)
      d
    }

    # Invalidate results when method or data change so stale results
    # are not shown after a new selection
    observeEvent(list(input$dimension_methods, input$fac_ret_cor_kind, data()), {
      results_rv$ready <- FALSE
      results_rv$table <- NULL
      results_rv$scree <- NULL
    }, ignoreInit = TRUE)

    # Run only when button is pressed - no reactive dependency on inputs inside
    observeEvent(input$run_factor_ret, {
      d <- data()
      req(d)

      # Snapshot inputs at the moment the button is pressed
      method <- isolate(input$dimension_methods)

      progress_id <- showNotification("Running factor retention analysis...",
                                      type = "message", duration = NULL,
                                      id = "fac_ret_progress")

      tryCatch({
        d_clean <- numeric_complete_data(d)
        if (is.null(d_clean)) {
          stop("At least 2 numeric variables and 3 complete rows are required.")
        }

        if (identical(method, "scree_plot")) {
          ev <- eigen(stats::cor(d_clean), symmetric = TRUE, only.values = TRUE)$values
          results_rv$scree <- data.frame(
            Factor = seq_along(ev),
            Eigenvalue = ev
          )
          tbl <- data.frame(
            Result = "Scree plot generated. Inspect the elbow in the plot below."
          )
        } else {
          tbl <- if (exists("factor_ret")) {
            factor_ret(d_clean, method = method)
          } else {
            data.frame(Error = "Function factor_ret not found.")
          }
          results_rv$scree <- NULL
        }

        results_rv$table <- tbl
        results_rv$ready <- TRUE
        removeNotification("fac_ret_progress")
        showNotification("Factor retention analysis complete.",
                         type = "message", duration = 4)
      }, error = function(e) {
        removeNotification("fac_ret_progress")
        showNotification(paste("Error:", e$message),
                         type = "error", duration = 8)
      })
    })

    # Table: reads from reactiveValues, no dependency on input$dimension_methods
    output$dim_ret_results <- renderTable({
      if (!results_rv$ready) {
        validate(need(FALSE, "Select a method and press Run Analysis."))
      }
      results_rv$table
    }, rownames = TRUE)

    output$scree_plot <- renderPlot({
      validate(need(results_rv$ready, "Press Run Analysis to create the scree plot."))
      scr <- results_rv$scree
      validate(need(
        !is.null(scr) && nrow(scr) > 0,
        "Select 'Scree Plot' and press Run Analysis to generate the scree plot."
      ))

      graphics::plot(
        scr$Factor, scr$Eigenvalue,
        type = "b", pch = 19,
        xlab = "Factor", ylab = "Eigenvalue",
        main = "Scree Plot"
      )
      graphics::abline(h = 1, lty = 2, col = "gray50")
    })
  })
}

#' EFA Analysis Server Module
#' Returns a list with `efa_object` and `settings` reactives.
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @noRd
efa_server_analysis <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    efa_res <- reactiveVal(NULL)

    # Centralised settings reactive (replaces the broken cross-namespace pattern)
    settings <- reactive({
      list(
        number_factor   = input$number_factor,
        rotating_method = input$rotating_method,
        fact_method     = input$fact_method,
        cor_kind        = input$cor_kind
      )
    })

    observeEvent(list(data(), settings()), {
      efa_res(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$run_efa, {
      d <- data()
      req(d)
      err <- validate_data(d, min_n = 30, min_p = 3, require_numeric = TRUE)
      if (!is.null(err)) {
        showNotification(err, type = "error", duration = 8)
        return()
      }
      showNotification("Running EFA...", type = "message")

      tryCatch({
        cor_type_arg <- if (input$cor_kind == "pea") "cor" else "poly"

        res <- psych::fa(
          r        = d,
          nfactors = as.numeric(input$number_factor),
          rotate   = input$rotating_method,
          fm       = input$fact_method,
          cor      = cor_type_arg
        )

        efa_res(res)
        showNotification("EFA Completed!", type = "message")
      }, error = function(e) {
        showNotification(paste("EFA Failed:", e$message),
                         type = "error", duration = 8)
        efa_res(NULL)
      })
    })

    list(efa_object = efa_res, settings = settings)
  })
}

#' EFA Reporting Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @param efa_output_reactive Reactive containing the EFA results.
#' @param efa_settings_reactive Reactive containing the EFA settings.
#' @noRd
efa_server_report <- function(id, data, efa_output_reactive, efa_settings_reactive) {
  moduleServer(id, function(input, output, session) {

    # ---- A2 fix: cache correlation matrix; recomputed only when data or
    #              cor_kind change. Used by KMO, Bartlett, heatmap, and range.
    cor_matrix_rv <- reactive({
      req(data())
      tryCatch(
        compute_cor_matrix(data(), efa_settings_reactive()$cor_kind),
        error = function(e) NULL
      )
    })

    # ---- KMO ----
    output$kmo_result <- renderUI({
      d <- data(); req(d)
      tryCatch({
        cm <- cor_matrix_rv(); req(cm)
        kmo_res <- psych::KMO(cm)
        val <- round(kmo_res$MSA, 3)
        color <- if (val >= 0.8) "green" else if (val >= 0.6) "orange" else "red"
        HTML(paste0("<b style='font-size:1.4rem;color:", color, ";'>", val, "</b>"))
      }, error = function(e) tags$span(class = "text-danger",
                                        paste("Error:", e$message)))
    })

    # ---- Bartlett ----
    output$bartlett <- renderTable({
      d <- data(); req(d)
      tryCatch({
        cm <- cor_matrix_rv(); req(cm)
        test <- psych::cortest.bartlett(cm, n = nrow(d))
        data.frame(
          Statistic = round(test$chisq, 2),
          df        = test$df,
          p_value   = if (test$p.value < 0.001) "< .001"
                      else format(round(test$p.value, 3), nsmall = 3)
        )
      }, error = function(e) data.frame(Error = e$message))
    })

    # ---- Loadings ----
    output$efa_result_str <- renderTable({
      req(efa_output_reactive())
      res <- efa_output_reactive()
      loadings_df <- as.data.frame(unclass(res$loadings))
      if (!is.null(res$communality)) loadings_df$h2 <- res$communality
      loadings_df
    }, rownames = TRUE, digits = 3)

    # ---- Variance ----
    output$efa_result_expl_var <- renderTable({
      req(efa_output_reactive())
      res <- efa_output_reactive()
      if (is.null(res$Vaccounted)) {
        return(data.frame(Message = "Variance table not available."))
      }
      as.data.frame(unclass(res$Vaccounted))
    }, rownames = TRUE, digits = 3)

    # ---- Phi ----
    output$efa_result_interf_cor <- renderTable({
      req(efa_output_reactive())
      res <- efa_output_reactive()
      if (is.null(res$Phi)) {
        return(data.frame(
          Info = "Correlations not available. Reasons: 1) Orthogonal rotation used, or 2) Only 1 factor extracted."
        ))
      }
      as.data.frame(unclass(res$Phi))
    }, rownames = TRUE, digits = 3)

    # ---- Heatmap with palette options ----
    palettes <- list(
      "blue_red"   = c(low = "#0072B2", mid = "white",   high = "#D55E00"),
      "viridis"    = c(low = "#440154", mid = "#21908C", high = "#FDE725"),
      "warm"       = c(low = "#FFFFCC", mid = "#FD8D3C", high = "#800026"),
      "cool"       = c(low = "#F7FBFF", mid = "#6BAED6", high = "#08306B"),
      "grayscale"  = c(low = "#FFFFFF", mid = "#999999", high = "#000000")
    )

    output$heat_map <- renderPlot({
      req(data())
      cm <- cor_matrix_rv(); req(cm)
      pal <- palettes[[input$heatmap_palette %||% "blue_red"]]
      lab_show <- isTRUE(input$heatmap_show_values) && ncol(cm) < 25
      suppressWarnings(
        ggcorrplot::ggcorrplot(
          cm,
          type     = "lower",
          lab      = lab_show,
          colors   = c(pal[["low"]], pal[["mid"]], pal[["high"]]),
          tl.cex   = 10,
          lab_size = 3
        )
      )
    })

    # ---- Off-diagonal correlation summary ----
    output$cor_range_text <- renderText({
      cm <- cor_matrix_rv(); req(cm)
      tri <- cm[upper.tri(cm)]
      paste0(
        "Off-diagonal correlations - ",
        "Min: ",    round(min(tri),  3), "  |  ",
        "Max: ",    round(max(tri),  3), "  |  ",
        "Mean: ",   round(mean(tri), 3), "  |  ",
        "Median: ", round(stats::median(tri), 3)
      )
    })

    # ---- Download loadings ----
    output$download_efa_loadings <- downloadHandler(
      filename = "efa_loadings.csv",
      content  = function(file) {
        req(efa_output_reactive())
        write.csv(unclass(efa_output_reactive()$loadings), file)
      }
    )
  })
}
