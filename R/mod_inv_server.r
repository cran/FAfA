#' Measurement Invariance Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive)
#' @noRd
inv_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    analysis_results_rv <- reactiveValues(
      fit_measures_df = NULL,
      model_comparison_df = NULL
    )


    observe({
      req(data())
      current_data <- data()
      col_names <- names(current_data)


      updateSelectizeInput(session, "builder_items", choices = col_names)
      updateSelectizeInput(session, "builder_cov_items", choices = col_names)

      is_cat <- sapply(current_data, function(x) is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(stats::na.omit(x))) <= 10))
      updateSelectInput(session, "grouping_variable_select", choices = c("", col_names[is_cat]))
    })

    append_syntax <- function(new_text) {
      current <- input$inv_model_syntax
      if (is.null(current) || current == "") {
        updateTextAreaInput(session, "inv_model_syntax", value = new_text)
      } else {
        updateTextAreaInput(session, "inv_model_syntax", value = paste(current, new_text, sep = "\n"))
      }
    }

    observeEvent(input$btn_add_to_model, {
      req(input$builder_factor_name, input$builder_items)
      f_name <- trimws(input$builder_factor_name)
      items_str <- paste(input$builder_items, collapse = " + ")
      new_line <- paste0(f_name, " =~ ", items_str)
      append_syntax(new_line)

      updateTextInput(session, "builder_factor_name", value = "")
      updateSelectizeInput(session, "builder_items", selected = character(0))
    })

    observeEvent(input$btn_add_cov, {
      req(input$builder_cov_items)
      if(length(input$builder_cov_items) != 2) {
        showNotification("Select exactly 2 variables for covariance.", type = "warning")
        return()
      }
      new_line <- paste0(input$builder_cov_items[1], " ~~ ", input$builder_cov_items[2])
      append_syntax(new_line)

      updateSelectizeInput(session, "builder_cov_items", selected = character(0))
    })

    observeEvent(input$correlation_matrix_type, {
      req(input$correlation_matrix_type)
      estimator_choices <- if (input$correlation_matrix_type == "pea") {
        c("Default (MLR)" = "default", "MLR", "ML", "GLS")
      } else {
        c("Default (WLSMV)" = "default", "WLSMV", "ULSMV", "DWLS")
      }
      updateSelectInput(
        session,
        "invariance_estimator_select",
        choices = estimator_choices,
        selected = "default"
      )
    }, ignoreInit = FALSE)

    observeEvent(input$run_invariance_button, {
      validate(
        need(data(), "Upload data."),
        need(input$inv_model_syntax, "Define model."),
        need(input$grouping_variable_select, "Select grouping var."),
        need(length(input$invariance_levels_checkbox) > 0, "Select levels.")
      )

      clean_res <- clean_missing_data(data())
      dat <- clean_res$cleaned_data

      grp <- input$grouping_variable_select
      dat[[grp]] <- as.factor(dat[[grp]])

      is_poly <- (input$correlation_matrix_type %||% "pea") == "poly"
      est_sel <- input$invariance_estimator_select %||% "default"
      est <- if (est_sel == "default") {
        if(isTRUE(is_poly)) "WLSMV" else "MLR"
      } else {
        est_sel
      }
      manifest_vars <- unique(unlist(lavaan::lavaanify(input$inv_model_syntax)$rhs[lavaan::lavaanify(input$inv_model_syntax)$op == "=~"]))
      ordered_arg <- if(isTRUE(is_poly)) manifest_vars else FALSE

      extract_fit_indices <- function(model, model_name) {
        fm <- tryCatch(lavaan::fitMeasures(model), error = function(e) NULL)
        if(is.null(fm)) return(NULL)

        chi <- fm["chisq.scaled"] %||% fm["chisq"]
        df  <- fm["df.scaled"] %||% fm["df"]
        p   <- fm["pvalue.scaled"] %||% fm["pvalue"]
        cfi <- fm["cfi.scaled"] %||% fm["cfi.robust"] %||% fm["cfi"]
        tli <- fm["tli.scaled"] %||% fm["tli.robust"] %||% fm["tli"]
        rmsea     <- fm["rmsea.scaled"] %||% fm["rmsea.robust"] %||% fm["rmsea"]
        rmsea_low <- fm["rmsea.ci.lower.scaled"] %||% fm["rmsea.ci.lower.robust"] %||% fm["rmsea.ci.lower"]
        rmsea_upp <- fm["rmsea.ci.upper.scaled"] %||% fm["rmsea.ci.upper.robust"] %||% fm["rmsea.ci.upper"]
        srmr <- fm["srmr"] %||% fm["srmr_bentler"]

        data.frame(
          Model = model_name,
          ChiSq = round(chi, 2),
          df = df,
          p = ifelse(is.na(p), NA, format.pval(p, digits = 3, eps = 0.001)),
          CFI = round(cfi, 3),
          TLI = round(tli, 3),
          RMSEA = round(rmsea, 3),
          RMSEA_90_CI_Lower = round(rmsea_low, 3),
          RMSEA_90_CI_Upper = round(rmsea_upp, 3),
          SRMR = round(srmr, 3),
          check.names = FALSE
        )
      }

      showNotification("Running Analysis...", type="message")

      tryCatch({
        analysis_results_rv$fit_measures_df <- NULL
        analysis_results_rv$model_comparison_df <- NULL

        mods <- list()
        levels <- input$invariance_levels_checkbox

        run_mod <- function(lvl, eq=NULL) {
          tryCatch(
            lavaan::cfa(
              input$inv_model_syntax,
              data = dat,
              group = grp,
              group.equal = eq,
              estimator = est,
              ordered = ordered_arg,
              missing = "listwise",
              mimic = "Mplus"
            ),
            error=function(e) NULL
          )
        }

        metric_constraints <- "loadings"
        scalar_constraints <- if (isTRUE(is_poly)) {
          c("loadings", "thresholds")
        } else {
          c("loadings", "intercepts")
        }
        strict_constraints <- if (isTRUE(is_poly)) {
          c("loadings", "thresholds", "residuals")
        } else {
          c("loadings", "intercepts", "residuals")
        }

        if("configural" %in% levels) mods$configural <- run_mod("Configural")
        if("metric" %in% levels)     mods$metric     <- run_mod("Metric", metric_constraints)
        if("scalar" %in% levels)     mods$scalar     <- run_mod("Scalar", scalar_constraints)
        if("strict" %in% levels)     mods$strict     <- run_mod("Strict", strict_constraints)

        # Fit Measures Table
        res_list <- lapply(names(mods), function(n) {
          m <- mods[[n]]
          if(is.null(m)) return(NULL)
          extract_fit_indices(m, n)
        })
        analysis_results_rv$fit_measures_df <- do.call(rbind, res_list)

        # Comparison
        valid_mods <- Filter(Negate(is.null), mods)
        if(length(valid_mods) > 1) {
          lrt <- tryCatch(as.data.frame(do.call(lavaan::lavTestLRT, unname(valid_mods))), error = function(e) NULL)
          fit_df <- analysis_results_rv$fit_measures_df
          comp_df <- fit_df
          comp_df$Compared_To <- c(NA, head(comp_df$Model, -1))
          comp_df$Delta_CFI <- round(c(NA, diff(comp_df$CFI)), 3)
          comp_df$Delta_TLI <- round(c(NA, diff(comp_df$TLI)), 3)
          comp_df$Delta_RMSEA <- round(c(NA, diff(comp_df$RMSEA)), 3)
          comp_df$Delta_SRMR <- round(c(NA, diff(comp_df$SRMR)), 3)

          if(!is.null(lrt) && nrow(lrt) == nrow(comp_df)) {
            lrt_cols <- intersect(c("Chisq diff", "Df diff", "Pr(>Chisq)"), names(lrt))
            if(length(lrt_cols) > 0) {
              comp_df <- cbind(comp_df, lrt[, lrt_cols, drop = FALSE])
            }
          }

          analysis_results_rv$model_comparison_df <- comp_df[, c(
            "Model", "Compared_To", "CFI", "TLI", "RMSEA", "SRMR",
            "Delta_CFI", "Delta_TLI", "Delta_RMSEA", "Delta_SRMR",
            setdiff(names(comp_df), c("Model", "Compared_To", "CFI", "TLI", "RMSEA", "SRMR",
                                      "Delta_CFI", "Delta_TLI", "Delta_RMSEA", "Delta_SRMR"))
          )]
        }

      }, error = function(e) showNotification(e$message, type="error"))
    })

    output$invariance_fit_measures_table <- renderTable({
      analysis_results_rv$fit_measures_df
    }, rownames=FALSE, striped=TRUE, digits=3)

    output$model_comparison_table <- renderTable({
      tab <- analysis_results_rv$model_comparison_df
      delta_cols <- intersect(
        c("Delta_CFI", "Delta_TLI", "Delta_RMSEA", "Delta_SRMR"),
        names(tab)
      )
      for (col in delta_cols) {
        tab[[col]] <- ifelse(
          is.na(tab[[col]]),
          NA,
          formatC(tab[[col]], digits = 3, format = "f")
        )
      }
      tab
    }, rownames=FALSE, striped=TRUE, digits=3)

    output$download_fit_measures_button <- downloadHandler(
      filename="inv_fit.csv", content=function(f) {
        req(analysis_results_rv$fit_measures_df)
        write.csv(analysis_results_rv$fit_measures_df, f, row.names=FALSE)
      }
    )
    output$download_model_comparison_button <- downloadHandler(
      filename="inv_comp.csv", content=function(f) {
        req(analysis_results_rv$model_comparison_df)
        write.csv(analysis_results_rv$model_comparison_df, f, row.names=FALSE)
      }
    )
  })
}
