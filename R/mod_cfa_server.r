#' Confirmatory Factor Analysis (CFA) Server Module
#'
#' @param id Module namespace ID.
#' @param data Reactive containing the input dataset.
#' @importFrom utils head
#' @noRd
cfa_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    cfa_analysis_results_rv <- reactiveValues(
      lavaan_object          = NULL,
      fit_measures_df        = NULL,
      factor_loadings_df     = NULL,
      modification_indices_df = NULL
    )

    build_bifactor_layout <- function(fit) {
      load_rows <- tryCatch(lavaan::parameterTable(fit), error = function(e) NULL)
      if (is.null(load_rows)) return(NULL)

      load_rows <- load_rows[load_rows$op == "=~", c("lhs", "rhs"), drop = FALSE]
      if (nrow(load_rows) == 0) return(NULL)

      latent_names <- unique(load_rows$lhs)
      item_names <- unique(load_rows$rhs)
      if (length(latent_names) < 2 || length(item_names) < 3) return(NULL)

      loading_counts <- stats::setNames(
        vapply(latent_names, function(f) sum(load_rows$lhs == f), integer(1)),
        latent_names
      )
      general_name <- names(which.max(loading_counts))
      if (loading_counts[[general_name]] < max(3, ceiling(0.75 * length(item_names)))) {
        return(NULL)
      }

      group_names <- setdiff(latent_names, general_name)
      if (length(group_names) == 0) return(NULL)

      group_items <- lapply(group_names, function(g) load_rows$rhs[load_rows$lhs == g])
      names(group_items) <- group_names

      ordered_items <- unique(unlist(group_items, use.names = FALSE))
      ordered_items <- c(ordered_items, setdiff(item_names, ordered_items))
      item_x <- stats::setNames(seq(-1.35, 0.35, length.out = length(ordered_items)), ordered_items)

      sem_model <- tryCatch(semPlot::semPlotModel(fit), error = function(e) NULL)
      if (is.null(sem_model)) return(NULL)
      node_names <- sem_model@Vars$name

      layout <- matrix(0, nrow = length(node_names), ncol = 2)
      rownames(layout) <- node_names

      for (item in intersect(ordered_items, node_names)) {
        layout[item, ] <- c(item_x[[item]], 1)
      }

      group_y <- seq(-0.85, 0.45, length.out = length(group_names))
      for (i in seq_along(group_names)) {
        g <- group_names[[i]]
        if (!(g %in% node_names)) next
        x_vals <- item_x[intersect(group_items[[g]], names(item_x))]
        layout[g, ] <- c(mean(x_vals, na.rm = TRUE), group_y[[i]])
      }

      if (general_name %in% node_names) {
        layout[general_name, ] <- c(1.25, 0)
      }

      layout
    }

        # 1. POPULATE SELECTORS
        observe({
      req(data())
      vars <- names(data())
      updateSelectizeInput(session, "builder_items",    choices = vars)
      updateSelectizeInput(session, "builder_cov_items",choices = vars)
      updateSelectizeInput(session, "bf_group_items",   choices = vars)
    })

        # Bifactor state: tracks general name + group factors
        bf_state <- reactiveValues(
      general  = "g",
      groups   = list()   # list of named character vectors: list(S1=c("i1","i2"), ...)
    )

        # Helper: append line to syntax textarea
        append_syntax <- function(new_text) {
      cur <- input$cfa_model_syntax_input
      val <- if (is.null(cur) || nchar(trimws(cur)) == 0) new_text
             else paste(cur, new_text, sep = "\n")
      updateTextAreaInput(session, "cfa_model_syntax_input", value = val)
    }

        # TAB 1: First-order factor  (=~)
        observeEvent(input$btn_add_factor, {
      f_name <- trimws(input$builder_factor_name %||% "")
      if (nchar(f_name) == 0) {
        showNotification("Factor name is required.", type = "warning"); return()
      }
      if (is.null(input$builder_items) || length(input$builder_items) == 0) {
        showNotification("Select at least one indicator.", type = "warning"); return()
      }
      append_syntax(paste0(f_name, " =~ ", paste(input$builder_items, collapse = " + ")))
      updateTextInput(session, "builder_factor_name", value = "")
      updateSelectizeInput(session, "builder_items", selected = character(0))
    })

    # Residual covariance (~~)
    observeEvent(input$btn_add_cov, {
      if (length(input$builder_cov_items) != 2) {
        showNotification("Select exactly 2 variables.", type = "warning"); return()
      }
      append_syntax(paste0(input$builder_cov_items[1], " ~~ ", input$builder_cov_items[2]))
      updateSelectizeInput(session, "builder_cov_items", selected = character(0))
    })

        # TAB 2: Higher-order factor
        observeEvent(input$btn_add_ho, {
      g_name <- trimws(input$ho_factor_name %||% "")
      fo     <- input$ho_first_order
      if (nchar(g_name) == 0) {
        showNotification("General factor name is required.", type = "warning"); return()
      }
      if (is.null(fo) || length(fo) < 2) {
        showNotification("Select at least 2 first-order factors.", type = "warning"); return()
      }
      append_syntax(paste0("\n# Higher-order factor\n", g_name, " =~ ",
                           paste(fo, collapse = " + ")))
      updateTextInput(session, "ho_factor_name", value = "")
      updateSelectizeInput(session, "ho_first_order", selected = character(0))
      showNotification(paste0("Higher-order factor ", g_name, " added."), type = "message")
    })

    # Populate ho_first_order choices from syntax (extract LHS of =~)
    observe({
      syn <- input$cfa_model_syntax_input %||% ""
      lines <- strsplit(syn, "\n")[[1]]
      fo_factors <- unique(trimws(sub("\\s*=~.*", "", grep("=~", lines, value = TRUE))))
      fo_factors <- fo_factors[nchar(fo_factors) > 0]
      updateSelectizeInput(session, "ho_first_order",
                           choices  = fo_factors,
                           selected = character(0))
    })

        # TAB 3: Bifactor
    
    # Add group factor to bifactor state
    observeEvent(input$btn_add_bf_group, {
      g_name  <- trimws(input$bf_general_name %||% "g")
      s_name  <- trimws(input$bf_group_name   %||% "")
      s_items <- input$bf_group_items
      if (nchar(s_name) == 0) {
        showNotification("Group factor name is required.", type = "warning"); return()
      }
      if (is.null(s_items) || length(s_items) == 0) {
        showNotification("Select at least one item for this group.", type = "warning"); return()
      }
      bf_state$general <- g_name
      bf_state$groups[[s_name]] <- s_items
      updateTextInput(session, "bf_group_name", value = "")
      updateSelectizeInput(session, "bf_group_items", selected = character(0))
      showNotification(
        paste0("Group factor ", s_name, " added (", length(s_items), " items). ",
               length(bf_state$groups), " group(s) defined so far."),
        type = "message"
      )
    })

    # Finalize bifactor syntax
    observeEvent(input$btn_finalize_bf, {
      if (length(bf_state$groups) < 1) {
        showNotification("Add at least one group factor first.", type = "warning"); return()
      }
      g   <- bf_state$general
      grps <- bf_state$groups
      all_items <- unique(unlist(grps))

      lines <- character(0)
      lines <- c(lines, "# --- Bifactor Model ---")

      # General factor
      lines <- c(lines, paste0(g, " =~ ", paste(all_items, collapse = " + ")))

      # Group factors
      for (s_name in names(grps)) {
        lines <- c(lines, paste0(s_name, " =~ ", paste(grps[[s_name]], collapse = " + ")))
      }

      # Orthogonality: g ~~ 0*Si and Si ~~ 0*Sj
      all_factors <- c(g, names(grps))
      orth_pairs <- utils::combn(all_factors, 2, simplify = FALSE)
      lines <- c(lines, "# Orthogonality constraints")
      for (pair in orth_pairs) {
        lines <- c(lines, paste0(pair[1], " ~~ 0*", pair[2]))
      }

      append_syntax(paste(lines, collapse = "\n"))
      # Reset state
      bf_state$groups  <- list()
      bf_state$general <- "g"
      showNotification("Bifactor syntax added. Check estimator: ML or WLSMV.", type = "message")
    })

        # TAB 4: Templates
        observeEvent(input$tpl_correlated, {
      tpl <- paste(
        "# Correlated factors model",
        "F1 =~ item1 + item2 + item3",
        "F2 =~ item4 + item5 + item6",
        "# F1 and F2 are allowed to correlate (default in lavaan CFA)",
        sep = "\n"
      )
      updateTextAreaInput(session, "cfa_model_syntax_input", value = tpl)
    })

    observeEvent(input$tpl_ho, {
      tpl <- paste(
        "# Higher-order (second-order) factor model",
        "# Step 1: first-order factors",
        "F1 =~ item1 + item2 + item3",
        "F2 =~ item4 + item5 + item6",
        "F3 =~ item7 + item8 + item9",
        "# Step 2: general factor loads on first-order factors",
        "g =~ F1 + F2 + F3",
        sep = "\n"
      )
      updateTextAreaInput(session, "cfa_model_syntax_input", value = tpl)
    })

    observeEvent(input$tpl_bifactor, {
      tpl <- paste(
        "# Bifactor model",
        "# General factor loads on ALL items",
        "g  =~ item1 + item2 + item3 + item4 + item5 + item6",
        "# Group (specific) factors",
        "S1 =~ item1 + item2 + item3",
        "S2 =~ item4 + item5 + item6",
        "# Orthogonality constraints (all factors uncorrelated)",
        "g  ~~ 0*S1",
        "g  ~~ 0*S2",
        "S1 ~~ 0*S2",
        sep = "\n"
      )
      updateTextAreaInput(session, "cfa_model_syntax_input", value = tpl)
    })

    observeEvent(input$btn_clear_syntax, {
      updateTextAreaInput(session, "cfa_model_syntax_input", value = "")
      showNotification("Syntax cleared.", type = "message")
    })

    # Dynamic estimator selection
    observeEvent(input$cfa_correlation_type_radio, {
      req(input$cfa_correlation_type_radio)
      estimator_choices <- if (input$cfa_correlation_type_radio == "pea") {
        c("Default (MLR)" = "default", "MLR", "ML", "GLS")
      } else {
        c("Default (WLSMV)" = "default", "WLSMV", "ULSMV", "DWLS")
      }
      updateSelectInput(session, "cfa_estimator_select", choices = estimator_choices, selected = "default")
    })

    # Run analysis
    observeEvent(input$run_cfa_button, {
      validate(
        need(data(), "Upload data."),
        need(input$cfa_model_syntax_input, "Define model.")
      )

      current_data <- data()
      model_syntax <- input$cfa_model_syntax_input
      corr_type <- input$cfa_correlation_type_radio
      est_sel <- input$cfa_estimator_select

      final_estimator <- est_sel
      if (est_sel == "default") {
        final_estimator <- if (corr_type == "poly") "WLSMV" else "MLR"
      }

      manifest_vars <- unique(unlist(lavaan::lavaanify(model_syntax)$rhs[lavaan::lavaanify(model_syntax)$op == "=~"]))
      ordered_arg <- if (corr_type == "poly") manifest_vars else FALSE

      progress_id <- showNotification("Running CFA...", duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        fit <- lavaan::cfa(
          model = model_syntax,
          data = current_data,
          ordered = ordered_arg,
          estimator = final_estimator,
          missing = "listwise",
          mimic = "Mplus" # Critical for Mplus match
        )
        cfa_analysis_results_rv$lavaan_object <- fit

        # Fit Measures
        fm_raw <- tryCatch(lavaan::fitMeasures(fit), error = function(e) NULL)
        if(!is.null(fm_raw)) {
          chi <- fm_raw["chisq.scaled"] %||% fm_raw["chisq"]
          df  <- fm_raw["df.scaled"] %||% fm_raw["df"]
          p   <- fm_raw["pvalue.scaled"] %||% fm_raw["pvalue"]
          cfi <- fm_raw["cfi.scaled"] %||% fm_raw["cfi.robust"] %||% fm_raw["cfi"]
          tli <- fm_raw["tli.scaled"] %||% fm_raw["tli.robust"] %||% fm_raw["tli"]
          rmsea     <- fm_raw["rmsea.scaled"] %||% fm_raw["rmsea.robust"] %||% fm_raw["rmsea"]
          rmsea_low <- fm_raw["rmsea.ci.lower.scaled"] %||% fm_raw["rmsea.ci.lower.robust"] %||% fm_raw["rmsea.ci.lower"]
          rmsea_upp <- fm_raw["rmsea.ci.upper.scaled"] %||% fm_raw["rmsea.ci.upper.robust"] %||% fm_raw["rmsea.ci.upper"]
          srmr <- fm_raw["srmr"] %||% fm_raw["srmr_bentler"]

          cfa_analysis_results_rv$fit_measures_df <- data.frame(
            Measure = c("Chi-Square", "Degrees of Freedom (df)", "p-value",
                        "CFI", "TLI (NNFI)", "RMSEA", "RMSEA 90% CI Lower", "RMSEA 90% CI Upper", "SRMR"),
            Value = round(c(chi, df, p, cfi, tli, rmsea, rmsea_low, rmsea_upp, srmr), 3)
          )
          if(!is.na(p)) cfa_analysis_results_rv$fit_measures_df$Value[3] <- format.pval(p, digits=3, eps=0.001)
        }

        # Loadings
        std_sol <- lavaan::standardizedSolution(fit)
        loadings <- std_sol[std_sol$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
        colnames(loadings) <- c("Factor", "Item", "Std. Estimate", "SE", "p-value")
        loadings[,3:4] <- round(loadings[,3:4], 3)
        loadings[,5]   <- format.pval(loadings[,5], digits=3, eps=0.001)
        cfa_analysis_results_rv$factor_loadings_df <- loadings

        # Mod Indices
        mod_ind <- lavaan::modificationIndices(fit, sort. = TRUE, minimum.value = 3.84)
        cfa_analysis_results_rv$modification_indices_df <- head(mod_ind[, c("lhs","op","rhs","mi","epc")], 20)

        showNotification("CFA Analysis Complete!", type = "message")

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 8)
        cfa_analysis_results_rv$lavaan_object <- NULL
      })
    })

    # Render outputs

    output$cfa_path_diagram_output <- renderPlot({
      req(cfa_analysis_results_rv$lavaan_object)

      # UI Controls
      selected_layout <- input$plot_layout %||% "tree"
      rotation_val    <- input$plot_rotation %||% 2
      box_width       <- input$plot_man_size %||% 10
      label_cex       <- input$plot_edge_label_cex %||% 0.8

      if(selected_layout == "tree2") {
        selected_layout <- "tree"
        rotation_val <- 2
      }

      bifactor_layout <- build_bifactor_layout(cfa_analysis_results_rv$lavaan_object)
      if (!is.null(bifactor_layout) && selected_layout %in% c("tree", "tree2")) {
        selected_layout <- bifactor_layout
      }

      what_labels <- if(!is.null(input$plot_show_labels) && input$plot_show_labels) "std" else "hide"

      is_poly <- (input$cfa_correlation_type_radio == "poly")
      custom_edge_width <- if(is_poly) 0.5 else 1.2
      # For polychoric/ordinal: residuals are fixed to 1.0 (WLSMV identification
      # constraint) and overlap badly. Hide them - loadings carry the key info.
      show_residuals <- !is_poly

      semPlot::semPaths(
        object = cfa_analysis_results_rv$lavaan_object,
        what = "std",
        whatLabels = what_labels,
        layout = selected_layout,
        rotation = rotation_val,

        shapeMan      = "rectangle",
        sizeMan       = box_width,
        sizeMan2      = box_width / 2,
        sizeLat       = box_width,
        sizeLat2      = box_width / 2,

        label.cex     = 1.2,
        edge.label.cex = label_cex,
        edge.width    = custom_edge_width,
        edge.color    = "black",
        style         = "lisrel",

        intercepts    = FALSE,
        thresholds    = FALSE,
        residuals     = show_residuals,
        residScale    = 15,

        reorder          = FALSE,
        optimizeLatRes   = TRUE,
        curve            = 2.5,
        mar              = c(5, 5, 5, 5),
        nCharNodes       = 0,
        theme            = "gray"
      )
    })

    output$cfa_fit_measures_table <- renderTable({ cfa_analysis_results_rv$fit_measures_df }, striped = TRUE, bordered = TRUE)
    output$cfa_factor_loadings_table <- renderTable({ cfa_analysis_results_rv$factor_loadings_df }, striped = TRUE)
    output$cfa_modification_indices_table <- renderTable({ cfa_analysis_results_rv$modification_indices_df }, striped = TRUE)

    # Downloads
    output$download_fit_measures_button <- downloadHandler(
      filename = "cfa_fit_measures.csv", content = function(file) write.csv(cfa_analysis_results_rv$fit_measures_df, file)
    )
    output$download_factor_loadings_button <- downloadHandler(
      filename = "cfa_factor_loadings.csv", content = function(file) write.csv(cfa_analysis_results_rv$factor_loadings_df, file)
    )
    output$download_path_diagram_button <- downloadHandler(
      filename = "cfa_path_diagram.svg",
      content = function(file) {
        svg(file, width = 12, height = 8)

        selected_layout <- input$plot_layout %||% "tree"
        rotation_val <- input$plot_rotation %||% 2
        box_width <- input$plot_man_size %||% 10
        if(selected_layout == "tree2") { selected_layout <- "tree"; rotation_val <- 2 }
        bifactor_layout <- build_bifactor_layout(cfa_analysis_results_rv$lavaan_object)
        if (!is.null(bifactor_layout) && selected_layout %in% c("tree", "tree2")) {
          selected_layout <- bifactor_layout
        }
        is_poly <- (input$cfa_correlation_type_radio == "poly")
        custom_edge_width <- if(is_poly) 0.5 else 1.2

        is_poly_dl <- (input$cfa_correlation_type_radio == "poly")
        semPlot::semPaths(
          object = cfa_analysis_results_rv$lavaan_object,
          what = "std", whatLabels = "std",
          layout = selected_layout, rotation = rotation_val,
          shapeMan = "rectangle", sizeMan = box_width, sizeMan2 = box_width/2,
          edge.width    = custom_edge_width,
          thresholds    = FALSE,
          reorder       = FALSE,
          residScale    = 15,
          residuals     = !is_poly_dl,
          edge.color    = "black",
          style         = "lisrel",
          intercepts    = FALSE,
          nCharNodes    = 0
        )
        dev.off()
      }
    )
  })
}
