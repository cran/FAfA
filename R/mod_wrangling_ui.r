#' Wrangling UI Modules
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd

# 1. Exclude Variables
wrangling_ui_ex_var <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),

      # --- LEFT: Available Variables ---
      card(
        card_header(
          class = "bg-success text-white",
          bs_icon("check2-square"), " Available Variables"
        ),
        card_body(
          p(class = "text-muted small mb-2",
            "Tick the variables you want to remove from the active dataset, then click Exclude."),
          div(
            style = paste(
              "max-height: 380px; overflow-y: auto;",
              "border: 1px solid #dee2e6; border-radius: 6px; padding: 8px 12px;"
            ),
            checkboxGroupInput(
              ns("available_vars_checkbox"),
              label    = NULL,
              choices  = NULL,
              selected = NULL
            )
          ),
          hr(class = "my-2"),
          actionButton(
            ns("exclude_button"),
            label = "Exclude Selected",
            icon  = icon("minus-circle"),
            class = "btn-danger w-100"
          )
        )
      ),

      # --- RIGHT: Excluded Variables + Summary + Actions ---
      card(
        card_header(
          class = "bg-danger text-white",
          bs_icon("x-circle"), " Excluded Variables"
        ),
        card_body(
          # Variable count summary
          uiOutput(ns("variable_summary")),
          hr(class = "my-2"),

          # List of excluded vars (tick to recover)
          p(class = "text-muted small mb-2",
            "Tick variables to bring back, then click Recover."),
          div(
            style = paste(
              "max-height: 240px; overflow-y: auto;",
              "border: 1px solid #dee2e6; border-radius: 6px; padding: 8px 12px;"
            ),
            checkboxGroupInput(
              ns("excluded_vars_checkbox"),
              label    = NULL,
              choices  = NULL,
              selected = NULL
            )
          ),
          hr(class = "my-2"),

          # Recover / Reset
          div(
            class = "d-flex gap-2 mb-2",
            actionButton(
              ns("recover_button"),
              label = "Recover Selected",
              icon  = icon("undo"),
              class = "btn-success flex-fill"
            ),
            actionButton(
              ns("reset_button"),
              label = "Reset All",
              icon  = icon("refresh"),
              class = "btn-outline-secondary flex-fill"
            )
          ),

          # Download active dataset
          downloadButton(
            ns("download_excluded_data_button"),
            label = "Download Active Data",
            class = "w-100"
          )
        )
      )
    )
  )
}

# 2. Split Data
wrangling_ui_split <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = 8,
      style = "margin: 0 auto;",
      card(
        card_header("Split Dataset", class = "bg-warning text-dark", bs_icon("scissors")),
        card_body(
          p("Randomly split data into two subsets (e.g., for EFA/CFA)."),
          sliderInput(ns("split_percentage_slider"), "First Subset %:",
                      min = 10, max = 90, value = 50, step = 5, post = "%"),
          actionButton(ns("split_data_button"), "Split Data",
                       icon = icon("random"), class = "btn-warning w-100"),
          hr(),
          div(class = "d-flex gap-2",
              downloadButton(ns("download_first_subset_button"),  "Save Part 1", class = "flex-fill"),
              downloadButton(ns("download_second_subset_button"), "Save Part 2", class = "flex-fill")
          )
        )
      )
    )
  )
}

# 3. Manage Outliers
wrangling_ui_outliers <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Outlier Detection", class = "bg-dark text-white", bs_icon("search")),
        card_body(
          p("Detect multivariate outliers using Mahalanobis Distance."),
          numericInput(ns("mah_p_value_threshold_input"), "P-value Threshold:",
                       value = 0.001, min = 0.00001, max = 0.05, step = 0.0001),
          actionButton(ns("check_outliers_button"), "Find Outliers",
                       icon = icon("search"), class = "btn-primary w-100 mb-2"),
          actionButton(ns("remove_outliers_button"), "Remove & Update",
                       icon = icon("user-minus"), class = "btn-danger w-100"),
          hr(),
          downloadButton(ns("download_data_no_outliers_button"), "Download Clean Data", class = "w-100")
        )
      ),
      card(
        card_header("Outlier Results"),
        card_body(
          textOutput(ns("outlier_count_text")),
          tableOutput(ns("outliers_table")) %>% withSpinner(type = 8, color = "#2C3E50")
        )
      )
    )
  )
}
