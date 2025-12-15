#' Wrangling UI Modules
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd

# 1. Exclude Variables
wrangling_ui_ex_var <- function(id) {
  ns <- NS(id)
  tagList(
    # Tek kart olduğu için col_widths = 12 veya tek değer veriyoruz
    layout_columns(
      col_widths = 8, # Kartın çok geniş olmaması için 8 birim (12 üzerinden)
      style = "margin: 0 auto;", # Ortalamak için
      card(
        card_header("Exclude Variables", class = "bg-danger text-white", bs_icon("x-circle")),
        card_body(
          p("Enter column numbers to exclude (comma separated)."),
          textInput(ns("excluded_variables_input"), "Column indices (e.g., 1,3,5):"),
          actionButton(ns("exclude_button"), "Exclude", icon = icon("trash"), class = "btn-danger w-100"),
          hr(),
          downloadButton(ns("download_excluded_data_button"), "Download New Data", class = "w-100")
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
      col_widths = 8, # Tek kart, 8 birim genişlik
      style = "margin: 0 auto;",
      card(
        card_header("Split Dataset", class = "bg-warning text-dark", bs_icon("scissors")),
        card_body(
          p("Randomly split data into two subsets (e.g., for EFA/CFA)."),
          sliderInput(ns("split_percentage_slider"), "First Subset %:", min = 10, max = 90, value = 50, step = 5, post = "%"),
          actionButton(ns("split_data_button"), "Split Data", icon = icon("random"), class = "btn-warning w-100"),
          hr(),
          div(class="d-flex gap-2",
              downloadButton(ns("download_first_subset_button"), "Save Part 1", class = "flex-fill"),
              downloadButton(ns("download_second_subset_button"), "Save Part 2", class = "flex-fill")
          )
        )
      )
    )
  )
}

# 3. Manage Outliers
# Burada 2 kart var, o yüzden c(4, 8) kullanımı doğrudur.
wrangling_ui_outliers <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Outlier Detection", class = "bg-dark text-white", bs_icon("search")),
        card_body(
          p("Detect multivariate outliers using Mahalanobis Distance."),
          numericInput(ns("mah_p_value_threshold_input"), "P-value Threshold:", value = 0.001, min = 0.00001, max = 0.05, step = 0.0001),
          actionButton(ns("check_outliers_button"), "Find Outliers", icon = icon("search"), class = "btn-primary w-100 mb-2"),
          actionButton(ns("remove_outliers_button"), "Remove & Update", icon = icon("user-minus"), class = "btn-danger w-100"),
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
