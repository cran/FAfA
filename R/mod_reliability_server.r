#' Reliability Analysis Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive)
#' @noRd
reliability_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    reliability_output_rv <- reactiveVal(NULL)

    # Cache results keyed by (method, items, cor_kind, model_str, strata) tuple
    # Avoids recomputing when user re-selects a previously calculated coefficient
    cache_rv <- reactiveValues(store = list())
    strata_choice_cache <- reactiveValues(values = list())

    # Update item selector when data changes; clear cache too
    observeEvent(data(), {
      req(data())
      updateSelectizeInput(session, "reliability_item_select", choices = names(data()))
      cache_rv$store <- list()
    }, ignoreNULL = TRUE)

    active_reliability_items <- reactive({
      req(data())
      selected <- input$reliability_item_select
      if (!is.null(selected) && length(selected) > 0) {
        selected
      } else {
        names(data())
      }
    })

    output$strata_item_selectors <- renderUI({
      items <- active_reliability_items()
      n_factors <- input$strata_factor_count %||% 2
      n_factors <- max(1, as.integer(n_factors))

      tagList(lapply(seq_len(n_factors), function(i) {
        selectizeInput(
          session$ns(paste0("strata_items_", i)),
          paste0("Factor ", i, " Items:"),
          choices = items,
          selected = character(0),
          multiple = TRUE,
          options = list(
            placeholder = paste0("Select items for factor ", i, "..."),
            plugins = list("remove_button")
          )
        )
      }))
    })

    strata_selections <- reactive({
      n_factors <- input$strata_factor_count %||% 2
      n_factors <- max(1, as.integer(n_factors))
      lapply(seq_len(n_factors), function(i) {
        input[[paste0("strata_items_", i)]] %||% character(0)
      })
    })

    observeEvent(
      list(active_reliability_items(), input$strata_factor_count, strata_selections()),
      {
        items <- active_reliability_items()
        n_factors <- input$strata_factor_count %||% 2
        n_factors <- max(1, as.integer(n_factors))
        selections <- strata_selections()

        for (i in seq_len(n_factors)) {
          current_selected <- intersect(selections[[i]] %||% character(0), items)
          selected_elsewhere <- unique(unlist(selections[-i], use.names = FALSE))
          selected_elsewhere <- intersect(selected_elsewhere, items)

          available_choices <- c(
            current_selected,
            setdiff(items, unique(c(current_selected, selected_elsewhere)))
          )

          input_id <- paste0("strata_items_", i)
          cached_choices <- strata_choice_cache$values[[input_id]] %||% character(0)

          if (!identical(cached_choices, available_choices)) {
            freezeReactiveValue(input, input_id)
            updateSelectizeInput(
              session,
              input_id,
              choices = available_choices,
              selected = current_selected
            )
            strata_choice_cache$values[[input_id]] <- available_choices
          }
        }
      },
      ignoreInit = FALSE
    )

    build_strata_definition <- function(items, n_factors) {
      assignments <- rep(NA_integer_, length(items))
      names(assignments) <- items
      duplicate_items <- character(0)

      for (i in seq_len(n_factors)) {
        selected <- input[[paste0("strata_items_", i)]] %||% character(0)
        selected <- intersect(selected, items)
        if (length(selected) == 0) next

        duplicate_items <- union(duplicate_items, selected[!is.na(assignments[selected])])
        assignments[selected] <- i
      }

      unassigned_items <- names(assignments)[is.na(assignments)]
      if (length(duplicate_items) > 0) {
        stop(
          "Each item can belong to only one factor. Duplicate item(s): ",
          paste(duplicate_items, collapse = ", ")
        )
      }
      if (length(unassigned_items) > 0) {
        stop(
          "Assign every item to a factor before calculating Stratified Alpha. Missing item(s): ",
          paste(unassigned_items, collapse = ", ")
        )
      }

      paste(assignments, collapse = ",")
    }

    observeEvent(input$run_reliability_button, {
      req(data(), input$reliability_coefficient_select)

      raw_data <- data()

      # If user picked specific items, subset; otherwise use all variables
      if (!is.null(input$reliability_item_select) &&
          length(input$reliability_item_select) > 0) {
        current_data <- raw_data[, input$reliability_item_select, drop = FALSE]
      } else {
        current_data <- raw_data
      }

      if (!all(sapply(current_data, is.numeric))) {
        showNotification(
          "Warning: Non-numeric columns present. They will be excluded automatically if possible.",
          type = "warning", duration = 6
        )
      }

      strata_define_arg <- NULL
      if (input$reliability_coefficient_select == "s_alpha") {
        n_factors <- max(1, as.integer(input$strata_factor_count %||% 2))
        strata_error <- NULL
        tryCatch({
          strata_define_arg <- build_strata_definition(names(current_data), n_factors)
        }, error = function(e) {
          strata_error <<- e$message
        })

        if (!is.null(strata_error)) {
          showNotification(strata_error, type = "error", duration = 8)
          reliability_output_rv(paste("Error:", strata_error))
          return()
        }
      }

      # Pick the right correlation argument for each method
      cor_arg <- if (input$reliability_coefficient_select == "cr") {
        input$cr_correlation_type_radio %||% "cor"
      } else {
        input$correlation_type_radio %||% "cor"
      }

      # Build cache key ? uniquely identifies this computation
      cache_key <- paste(
        input$reliability_coefficient_select,
        paste(sort(names(current_data)), collapse = ","),
        cor_arg,
        digest_str(input$cfa_model_for_reliability_input %||% ""),
        strata_define_arg %||% "",
        sep = "|"
      )

      # Cache hit: return immediately
      if (!is.null(cache_rv$store[[cache_key]])) {
        reliability_output_rv(cache_rv$store[[cache_key]])
        showNotification("Result retrieved from cache.", type = "message", duration = 4)
        return()
      }

      progress_id <- showNotification("Calculating...", duration = NULL, type = "message")
      on.exit(removeNotification(progress_id), add = TRUE)

      tryCatch({
        res <- reliability_func(
          x                 = current_data,
          method            = input$reliability_coefficient_select,
          cor_kind          = cor_arg,
          defined_structure = input$cfa_model_for_reliability_input,
          strata_define     = strata_define_arg
        )
        cache_rv$store[[cache_key]] <- res
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

# Lightweight string hash for cache keys (avoids dependency on digest package)
digest_str <- function(s) {
  if (is.null(s) || nchar(s) == 0) return("EMPTY")
  paste0("h", abs(sum(utf8ToInt(s) * seq_along(utf8ToInt(s)))))
}
