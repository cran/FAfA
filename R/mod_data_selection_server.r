#' Data Selection Server Logic (Modern)
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @importFrom rlang .data
#' @noRd
data_selection_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    analyzed_data <- eventReactive(input$analyze_data, {
      req(data())
      data()
    })

    # ---- Preview Table ----
    output$mydatatable <- renderTable({
      validate(need(data(), "Please upload your dataset."))
      utils::head(data(), 10)
    })

    # ---- Helper: numeric subset ----
    get_numeric_data <- reactive({
      req(analyzed_data())
      df <- analyzed_data()
      df[, sapply(df, is.numeric), drop = FALSE]
    })

    # ---- Value boxes (top row) ----
    output$n_var_box <- renderUI({
      req(analyzed_data())
      value_box(title = "Variables", value = ncol(analyzed_data()),
                showcase = bsicons::bs_icon("columns"), theme = "primary")
    })

    output$n_obs_box <- renderUI({
      req(analyzed_data())
      value_box(title = "Sample Size", value = nrow(analyzed_data()),
                showcase = bsicons::bs_icon("people"), theme = "success")
    })

    output$n_missing_box <- renderUI({
      req(analyzed_data())
      n_miss <- sum(is.na(analyzed_data()))
      pct    <- round(100 * n_miss / (nrow(analyzed_data()) * ncol(analyzed_data())), 1)
      value_box(title = "Missing Cells",
                value = paste0(n_miss, " (", pct, "%)"),
                showcase = bsicons::bs_icon("question-octagon"),
                theme    = if (pct > 5) "warning" else "info")
    })

    output$n_complete_box <- renderUI({
      req(analyzed_data())
      n_complete <- sum(stats::complete.cases(analyzed_data()))
      pct <- round(100 * n_complete / nrow(analyzed_data()), 1)
      value_box(title = "Complete Cases",
                value = paste0(n_complete, " (", pct, "%)"),
                showcase = bsicons::bs_icon("check-circle"),
                theme    = "secondary")
    })

    # ---- Variable type breakdown ----
    output$var_types_table <- renderTable({
      req(analyzed_data())
      df <- analyzed_data()
      types <- sapply(df, function(x) {
        if (is.numeric(x))  "Numeric"
        else if (is.factor(x))  "Factor"
        else if (is.character(x)) "Character"
        else if (is.logical(x))  "Logical"
        else class(x)[1]
      })
      tab <- as.data.frame(table(Type = types), responseName = "Count")
      tab$Percent <- paste0(round(100 * tab$Count / sum(tab$Count), 1), "%")
      tab
    })

    # ---- Per-variable summary (single pass per column) ----
    output$variable_summary_table <- renderTable({
      req(analyzed_data())
      df <- analyzed_data()
      # Compute all stats in ONE iteration over columns
      summarise_col <- function(x) {
        is_num <- is.numeric(x)
        no_na  <- if (any(is.na(x))) x[!is.na(x)] else x
        list(
          Type     = class(x)[1],
          N_Unique = length(unique(no_na)),
          Missing  = length(x) - length(no_na),
          Min      = if (is_num && length(no_na)) min(no_na)             else NA,
          Max      = if (is_num && length(no_na)) max(no_na)             else NA,
          Mean     = if (is_num && length(no_na)) round(mean(no_na), 2)  else NA,
          SD       = if (is_num && length(no_na) > 1)
                       round(stats::sd(no_na), 2) else NA
        )
      }
      stats_list <- lapply(df, summarise_col)
      out <- data.frame(
        Variable = names(df),
        do.call(rbind.data.frame, stats_list),
        check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
      )
      out
    }, striped = TRUE, bordered = TRUE, na = "-", digits = 2)

    # ---- Categories distribution: counts per unique value level ----
    output$category_dist_plot <- renderPlot({
      req(analyzed_data())
      df <- analyzed_data()
      num_df <- df[, sapply(df, is.numeric), drop = FALSE]
      if (ncol(num_df) == 0) {
        graphics::plot.new(); graphics::title("No numeric variables.")
        return()
      }

      # Detect "categorical-like" variables: integer-like with <= 10 unique levels
      is_likert <- sapply(num_df, function(x) {
        u <- unique(stats::na.omit(x))
        length(u) <= 10 && all(u == round(u))
      })

      if (any(is_likert)) {
        cat_df <- num_df[, is_likert, drop = FALSE]
        # build long data frame: variable + value
        long <- do.call(rbind, lapply(names(cat_df), function(v) {
          data.frame(Variable = v, Value = cat_df[[v]], stringsAsFactors = FALSE)
        }))
        long <- long[!is.na(long$Value), ]
        ggplot2::ggplot(long, ggplot2::aes(x = factor(.data[["Value"]]))) +
          ggplot2::geom_bar(fill = "#2563EB", alpha = 0.85) +
          ggplot2::facet_wrap(~ Variable, scales = "free_y") +
          ggplot2::labs(x = "Category", y = "Count",
                        title = "Response Distribution per Variable") +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
      } else {
        # Continuous case: show histograms
        long <- do.call(rbind, lapply(names(num_df), function(v) {
          data.frame(Variable = v, Value = num_df[[v]], stringsAsFactors = FALSE)
        }))
        long <- long[!is.na(long$Value), ]
        ggplot2::ggplot(long, ggplot2::aes(x = .data[["Value"]])) +
          ggplot2::geom_histogram(fill = "#16A34A", alpha = 0.85, bins = 25) +
          ggplot2::facet_wrap(~ Variable, scales = "free") +
          ggplot2::labs(x = NULL, y = "Count",
                        title = "Distribution per Variable") +
          ggplot2::theme_minimal(base_size = 11)
      }
    })
  })
}
