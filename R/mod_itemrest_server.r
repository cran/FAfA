#' ItemRest Analysis Server Module
#'
#' Reads the structured `itemrest_result` object returned by the internal
#' itemrest() engine. The relevant fields are:
#'   * descriptive_stats : list(n_items, n_obs, min_value, max_value)
#'   * initial_efa       : list(efa, alpha, explained_var, loadings)
#'   * problem_items     : list(cross_3, cross_2, low_loading) - character vectors
#'   * removal_summary   : data frame of all tested removal combinations,
#'                         columns: Removed_Items, Total_Explained_Var,
#'                         Factor_Loading_Range, Cronbachs_Alpha, Cross_Loading
#'   * recommended       : recommended strategy list, or NULL
#'   * settings          : list(n_factors, cor_method, extract, rotate, auto_n_factors)
#'
#' @param id Module namespace ID.
#' @param data Reactive containing the input dataset.
#' @import shiny
#' @importFrom stats na.omit
#' @noRd
mod_itemrest_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    analysis_results <- reactiveVal(list(res = NULL))

    observeEvent(data(), {
      req(data())
      analysis_results(list(res = NULL))
    }, ignoreNULL = TRUE)

    observeEvent(input$run_itemrest, {
      req(data())

      df <- data()
      if (!all(sapply(df, is.numeric))) {
        showNotification("Item drop-out requires all variables to be numeric.",
                         type = "error", duration = 8)
        return()
      }
      df_clean <- stats::na.omit(df)
      num_factors_arg <- if (is.na(input$n_factors)) NULL else input$n_factors
      max_removed_total_arg <- if (is.na(input$max_removed_total)) {
        NULL
      } else {
        as.integer(input$max_removed_total)
      }
      loading_diff_arg <- if (identical(input$loading_diff_mode, "howard")) {
        "howard"
      } else {
        input$loading_diff
      }

      progress_id <- showNotification(
        "Running item drop-out search. With many problem items this may take a while...",
        type = "message", duration = NULL, id = "ir_progress"
      )

      tryCatch({
        ir_result <- suppressMessages(suppressWarnings(
          itemrest(
            data                  = df_clean,
            cor_method            = input$cor_method,
            n_factors             = num_factors_arg,
            extract               = input$extraction_method,
            rotate                = input$rotation_method,
            min_loading           = input$min_loading,
            loading_diff          = loading_diff_arg,
            search                = input$search_method,
            max_iter              = as.integer(input$max_iter),
            max_removed_total     = max_removed_total_arg,
            max_removed_per_iter  = as.integer(input$max_removed_per_iter),
            min_items_per_factor  = as.integer(input$min_items_per_factor)
          )
        ))
        analysis_results(list(res = ir_result))
        removeNotification("ir_progress")
        showNotification("Analysis Complete!", type = "message", duration = 4)
      }, error = function(e) {
        removeNotification("ir_progress")
        showNotification(paste("Analysis Failed:", e$message),
                         type = "error", duration = 8)
      })
    })

    metric_box <- function(label, val, theme = "secondary") {
      if (is.null(val) || (is.character(val) && nchar(trimws(val)) == 0))
        return(NULL)
      div(
        class = paste0("p-2 rounded text-center border-start border-3 border-",
                       theme, " bg-light"),
        tags$div(class = "small text-muted text-uppercase",
                 style = "letter-spacing:0.05em", label),
        tags$div(class = "fw-bold", style = "font-size:1.05rem;", val)
      )
    }

    fmt_pct <- function(x) {
      if (is.null(x) || is.na(x)) return(NA_character_)
      paste0(formatC(100 * x, digits = 2, format = "f"), "%")
    }
    fmt_num <- function(x, d = 3) {
      if (is.null(x) || is.na(x)) return(NA_character_)
      formatC(x, digits = d, format = "f")
    }

    output$optimal_strategy_ui <- renderUI({
      out_data <- analysis_results()
      res <- out_data$res

      if (is.null(res)) {
        return(div(
          class = "text-muted text-center p-4",
          bsicons::bs_icon("hourglass-split", size = "2em"),
          tags$p(class = "mt-2 mb-0",
                 "Run the analysis to see the result.")
        ))
      }

      ds        <- res$descriptive_stats
      eff       <- res$initial_efa
      settings  <- res$settings
      problems  <- res$problem_items
      all_probs <- res$all_problem_items
      recommended <- res$recommended
      removal   <- res$removal_summary    # full df or NULL

      n_items    <- ds$n_items   %||% NA
      n_obs      <- ds$n_obs     %||% NA
      n_factors  <- settings$n_factors %||% NA
      auto_flag  <- isTRUE(settings$auto_n_factors)
      init_alpha <- if (!is.null(eff$alpha)) eff$alpha else NA
      init_var   <- if (!is.null(eff$explained_var)) eff$explained_var else NA

      n_lowload <- length(problems$low_loading)
      n_cross2  <- length(problems$cross_2)
      n_cross3  <- length(problems$cross_3)
      n_total_problems <- length(all_probs)

      rec_row <- NULL
      rec_status <- "none"   # "none", "optimal", "fallback"

      if (n_total_problems == 0) {
        rec_status <- "none"
      } else if (!is.null(recommended) && !is.null(recommended$row)) {
        rec_row <- recommended$row[1, , drop = FALSE]
        rec_status <- "optimal"
      } else if (is.data.frame(removal) && nrow(removal) >= 1) {
        rec_row <- removal[1, , drop = FALSE]
        rec_status <- "fallback"
      }

      # Extract recommendation fields safely
      rec_removed   <- if (!is.null(rec_row)) as.character(rec_row$Removed_Items)        else NULL
      rec_var       <- if (!is.null(rec_row)) rec_row$Total_Explained_Var                 else NULL
      rec_loadrange <- if (!is.null(rec_row)) as.character(rec_row$Factor_Loading_Range) else NULL
      rec_alpha     <- if (!is.null(rec_row)) rec_row$Cronbachs_Alpha                    else NULL
      rec_cross     <- if (!is.null(rec_row)) as.character(rec_row$Has_Cross_Loading)    else NULL
      rec_low       <- if (!is.null(rec_row)) as.character(rec_row$Has_Low_Loading)      else NULL

      # -------- Top alert --------
      top_alert <- switch(
        rec_status,
        "none" = div(
          class = "alert alert-success d-flex align-items-center mb-3 p-3",
          bsicons::bs_icon("check-circle-fill", size = "1.4em"),
          div(class = "ms-3",
              tags$div(class = "small text-uppercase fw-bold",
                       style = "letter-spacing:0.05em;", "Result"),
              tags$div(style = "font-size:1.05rem;",
                       "No problematic items detected. The current item set is clean."))
        ),
        "optimal" = div(
          class = "alert alert-success d-flex align-items-center mb-3 p-3",
          bsicons::bs_icon("trophy-fill", size = "1.4em"),
          div(class = "ms-3",
              tags$div(class = "small text-uppercase fw-bold",
                       style = "letter-spacing:0.05em;", "Optimal Strategy"),
              tags$div(style = "font-size:1.1rem;",
                       tags$b("Remove: "), rec_removed),
              tags$div(class = "small text-muted mt-1",
                       "Use the Exclude Variables tab to remove these items from the active dataset."))
        ),
        "fallback" = div(
          class = "alert alert-warning d-flex align-items-center mb-3 p-3",
          bsicons::bs_icon("exclamation-triangle-fill", size = "1.4em"),
          div(class = "ms-3",
              tags$div(class = "small text-uppercase fw-bold",
                       style = "letter-spacing:0.05em;", "Best Available Strategy"),
              tags$div(style = "font-size:1.1rem;",
                       tags$b("Remove: "), rec_removed),
              tags$div(class = "small mt-1",
                       "No fully clean removal combination was found. ",
                       "The top row of the comparison table is shown. Use Exclude Variables ",
                       "if you decide to remove these items."))
        )
      )

      # -------- Settings note (auto vs manual factor count) --------
      settings_note <- div(
        class = "small text-muted mb-3",
        bsicons::bs_icon("info-circle"), " ",
        if (auto_flag) {
          paste0("Number of factors determined automatically by parallel analysis: ",
                 n_factors, ".")
        } else {
          paste0("Number of factors set manually: ", n_factors, ".")
        },
        " Extraction: ", toupper(settings$extract %||% "-"),
        " | Rotation: ", tools::toTitleCase(settings$rotate %||% "-"),
        " | Correlation: ", tools::toTitleCase(settings$cor_method %||% "-"),
        " | Search: ", gsub("_", " ", tools::toTitleCase(settings$search %||% "-")), "."
      )

      # -------- Dataset overview row --------
      overview_row <- div(
        class = "row g-2 mb-3",
        div(class = "col-md-3", metric_box("Items",         n_items,            "primary")),
        div(class = "col-md-3", metric_box("Sample (n)",    n_obs,              "primary")),
        div(class = "col-md-3", metric_box("Factors",       n_factors,          "primary")),
        div(class = "col-md-3", metric_box("Initial Alpha", fmt_num(init_alpha), "info"))
      )

      # -------- Comparison row (only when a removal is recommended) --------
      comparison_row <- if (rec_status %in% c("optimal", "fallback")) {
        div(
          class = "row g-2 mb-3",
          div(class = "col-md-3",
              metric_box("Variance (original)", fmt_pct(init_var), "secondary")),
          div(class = "col-md-3",
              metric_box("Variance (after removal)", fmt_pct(rec_var), "success")),
          div(class = "col-md-3",
              metric_box("Alpha (after removal)", fmt_num(rec_alpha), "success")),
          div(class = "col-md-3",
              metric_box("Loading range",
                         rec_loadrange,
                         if (!is.null(rec_cross) &&
                             tolower(rec_cross) == "no" &&
                             !is.null(rec_low) &&
                             tolower(rec_low) == "no") "success" else "warning"))
        )
      } else {
        div(
          class = "row g-2 mb-3",
          div(class = "col-md-6",
              metric_box("Variance Explained", fmt_pct(init_var), "info")),
          div(class = "col-md-6",
              metric_box("Initial Alpha", fmt_num(init_alpha), "info"))
        )
      }

      # -------- Problem-item breakdown (always shown when problems exist) --------
      problems_row <- if (n_total_problems > 0) {
        div(
          class = "row g-2 mb-3",
          div(class = "col-md-4", metric_box("Cross-loading (>=3 factors)",
                                             n_cross3, "danger")),
          div(class = "col-md-4", metric_box("Cross-loading (>=2 factors)",
                                             n_cross2, "warning")),
          div(class = "col-md-4", metric_box("Low-loading items",
                                             n_lowload, "warning"))
        )
      } else NULL

      tagList(top_alert, settings_note, overview_row, comparison_row, problems_row)
    })

    output$removal_summary_table <- renderTable({
      req(analysis_results())
      res <- analysis_results()$res
      if (is.null(res)) return(NULL)
      tab <- res$removal_summary
      if (is.null(tab)) {
        return(data.frame(
          Status = "No removal combinations were tested (no problematic items detected)."
        ))
      }

      display_cols <- c(
        "Iteration",
        "Removed_This_Step",
        "Removed_Items",
        "N_Removed",
        "N_Remaining",
        "Total_Explained_Var",
        "Factor_Loading_Range",
        "Cronbachs_Alpha",
        "Has_Cross_Loading",
        "Cross_Loading_Items",
        "Has_Low_Loading",
        "Low_Loading_Items"
      )
      tab <- tab[, intersect(display_cols, names(tab)), drop = FALSE]
      if ("Total_Explained_Var" %in% names(tab)) {
        tab$Total_Explained_Var <- round(tab$Total_Explained_Var, 3)
      }
      if ("Cronbachs_Alpha" %in% names(tab)) {
        tab$Cronbachs_Alpha <- round(tab$Cronbachs_Alpha, 3)
      }
      tab
    }, striped = TRUE, hover = TRUE, digits = 3)
  })
}
