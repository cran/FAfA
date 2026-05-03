#' EFA Replicability UI Module
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd
efa_ui_replicability <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Replicability Configuration", class = "bg-primary text-white", bs_icon("shuffle")),
        card_body(
          div(
            class = "d-grid gap-3",
            div(
              class = "p-3 rounded",
              style = "background:#f8fafc; border:1px solid #e2e8f0;",
              radioButtons(
                ns("cor_kind"), "Correlation:",
                choices = c("Pearson" = "pea", "Polychoric" = "poly"),
                selected = "poly",
                inline = TRUE
              ),
              tags$p(
                class = "text-muted small mb-0",
                "Use the same correlation choice as the main EFA module."
              )
            ),
            numericInput(ns("number_factor"), "No. of Factors:", value = 1, min = 1),
            selectizeInput(
              ns("fact_method"), "Extraction:",
              choices = c(
                "Minimum Residuals"         = "minres",
                "Maximum Likelihood"        = "ml",
                "Principal Axis"            = "pa",
                "Unweighted Least Squares"  = "uls",
                "Weighted Least Squares"    = "wls",
                "Minimum Rank"              = "minrank",
                "Minimum Chi-Square"        = "minchi",
                "Generalized Least Squares" = "gls"
              ),
              selected = "minres",
              options = list(dropdownParent = "body")
            ),
            selectizeInput(
              ns("rotating_method"), "Rotation:",
              choices = list(
                "None" = list("None" = "none"),
                "Oblique" = list(
                  "Oblimin"    = "oblimin",
                  "Promax"     = "promax",
                  "Quartimin"  = "quartimin",
                  "BiquartMin" = "biquartimin",
                  "GeominQ"    = "geominQ",
                  "BentlerQ"   = "bentlerQ",
                  "Simplimax"  = "simplimax",
                  "Cluster"    = "cluster"
                ),
                "Orthogonal" = list(
                  "Varimax"    = "varimax",
                  "Quartimax"  = "quartimax",
                  "Equamax"    = "equamax",
                  "BentlerT"   = "bentlerT",
                  "GeominT"    = "geominT",
                  "Bifactor"   = "bifactor"
                )
              ),
              selected = "oblimin",
              options = list(dropdownParent = "body")
            ),
            sliderInput(
              ns("split_prop"), "Split 1 proportion:",
              min = 0.30, max = 0.70, value = 0.50, step = 0.05
            ),
            numericInput(ns("seed"), "Random seed:", value = 1234, min = 1, step = 1)
          ),
          hr(),
          actionButton(
            ns("run_replicability"), "Run Replicability",
            icon = icon("play"), class = "btn-success w-100 btn-lg"
          )
        )
      ),
      card(
        card_header("Replicability Results", bs_icon("table")),
        card_body(
          navset_card_tab(
            nav_panel(
              title = "Loadings Comparison", icon = bs_icon("columns-gap"),
              tableOutput(ns("loadings_comparison")) %>%
                shinycssloaders::withSpinner(type = 8, color = "#2C3E50"),
              downloadButton(ns("download_replicability"), "Download CSV", class = "btn-sm mt-2")
            ),
            nav_panel(
              title = "Summary", icon = bs_icon("clipboard-data"),
              tableOutput(ns("summary_table")),
              tableOutput(ns("factor_congruence"))
            )
          )
        )
      )
    )
  )
}
