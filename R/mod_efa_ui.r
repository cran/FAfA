#' EFA UI Modules
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd

# 1. Factor Retention
efa_ui_fac_ret <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Retention Methods", class = "bg-primary text-white", bs_icon("gear")),
        card_body(
          selectInput(ns("dimension_methods"), "Select Method:",
            choices = c(
              "Optimal Parallel Analysis (MRFA)" = "pa_mrfa",
              "Traditional Parallel Analysis" = "pa_traditional",
              "Hull Method" = "hull_method",
              "MAP (Original)" = "map_method_tra",
              "MAP (Revised)" = "map_method_rev",
              "EGA (TMFG)" = "EGA_tmfg",
              "EGA (Glasso)" = "EGA_glasso",
              "Empirical Kaiser (EKC)" = "EK_C",
              "Comparison Data (CD)" = "comp_data_method"
            ), selected = "hull_method"),
          actionButton(ns("run_factor_ret"), "Run Analysis", icon = icon("play"), class = "btn-success w-100")
        )
      ),
      card(
        card_header("Results"),
        card_body(
          tableOutput(ns("dim_ret_results")) %>% withSpinner(type = 8, color = "#2C3E50"),
          plotOutput(ns("scree_plot"), height = "300px")
        )
      )
    )
  )
}

# 2. EFA Setup
efa_ui_analysis <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("EFA Configuration", class = "bg-primary text-white", bs_icon("sliders")),
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          radioButtons(ns("cor_kind"), "Correlation:", choices = c("Pearson" = "pea", "Polychoric" = "poly"), selected = "poly"),
          numericInput(ns("number_factor"), "No. of Factors:", value = 1, min = 1),
          selectInput(ns("fact_method"), "Extraction:", choices = c("MinRes"="minres", "ML"="ml", "PA"="pa", "ULS"="uls"), selected = "minres"),
          selectInput(ns("rotating_method"), "Rotation:", choices = c("Oblimin"="oblimin", "Promax"="promax", "Varimax"="varimax", "GeominQ"="geominQ"), selected = "oblimin")
        ),
        hr(),
        actionButton(ns("run_efa"), "Run EFA", icon = icon("play"), class = "btn-success w-100 btn-lg")
      )
    )
  )
}

# 3. EFA Report
efa_ui_report <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("KMO & Bartlett", bs_icon("check-circle")),
        card_body(
          htmlOutput(ns("kmo_result")),
          tableOutput(ns("bartlett"))
        )
      ),
      card(
        card_header("Loadings & Variance", bs_icon("table")),
        navset_card_tab(
          nav_panel("Loadings", tableOutput(ns("efa_result_str")), downloadButton(ns("download_efa_loadings"), "Download")),
          nav_panel("Variance", tableOutput(ns("efa_result_expl_var"))),
          nav_panel("Phi Matrix", tableOutput(ns("efa_result_interf_cor")))
        )
      )
    ),
    card(
      card_header("Heatmap", bs_icon("grid-3x3")),
      plotOutput(ns("heat_map"), height = "500px") %>% withSpinner(type = 8, color = "#2C3E50")
    )
  )
}