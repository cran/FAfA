#' Data Selection UI Module
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinycssloaders withSpinner
#' @noRd
data_selection_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # ---- Top: Upload + Quick Summary ----
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Data Upload", class = "bg-primary text-white", bs_icon("upload")),
        card_body(
          markdown("
Please upload your data file. Supported formats: **.csv, .txt, .dat, .sav, .xlsx, .xls**.

*Ensure missing values are represented as NA.*
"),
          fileInput(ns("file1"), label = "Choose Data File:",
                    accept = c(".csv", ".txt", ".dat", ".sav", ".xlsx", ".xls"),
                    placeholder = "No file selected"),
          checkboxInput(ns("has_header_checkbox"),
                        label = "My data has a header row", value = TRUE),
          actionButton(ns("analyze_data"), "Analyze Data",
                       icon = icon("play"), class = "btn-success w-100")
        )
      ),
      card(
        card_header("Dataset Overview", bs_icon("speedometer2")),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            uiOutput(ns("n_var_box")),
            uiOutput(ns("n_obs_box")),
            uiOutput(ns("n_missing_box")),
            uiOutput(ns("n_complete_box"))
          )
        )
      )
    ),

    # ---- Tabs: Preview / Variable Summary / Distributions ----
    navset_card_tab(
      nav_panel(
        title = "Data Preview", icon = bs_icon("table"),
        tableOutput(ns("mydatatable")) %>% withSpinner(type = 8, color = "#2C3E50")
      ),
      nav_panel(
        title = "Variable Types", icon = bs_icon("diagram-2"),
        layout_columns(
          col_widths = c(4, 8),
          div(
            h6("Type Breakdown", class = "text-muted"),
            tableOutput(ns("var_types_table"))
          ),
          div(
            h6("Per-Variable Detail", class = "text-muted"),
            div(style = "max-height:480px; overflow-y:auto;",
                tableOutput(ns("variable_summary_table"))
            )
          )
        )
      ),
      nav_panel(
        title = "Distributions", icon = bs_icon("bar-chart-line"),
        p(class = "text-muted small",
          "Likert-like variables (integer-valued, <= 10 unique levels) shown as bar charts. ",
          "Continuous variables shown as histograms."),
        plotOutput(ns("category_dist_plot"), height = "550px") %>%
          withSpinner(type = 8, color = "#2C3E50")
      )
    )
  )
}
