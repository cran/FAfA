#' ItemRest Analysis UI Module
#'
#' @param id Module namespace ID.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd
mod_itemrest_ui <- function(id) {
  ns <- NS(id)
  setting_note <- function(...) {
    helpText(class = "small text-muted mb-3", ...)
  }

  tagList(
    layout_columns(
      col_widths = c(4, 8),
      
      # Analysis Settings Card
      card(
        card_header(
          class = "bg-info text-white",
          "Analysis Settings", 
          bs_icon("sliders")
        ),
        card_body(
          p("Automated item removal strategies for EFA."),
          
          # Inputs based on itemrest function arguments [cite: 78-84]
          numericInput(
            ns("n_factors"),
            "Number of Factors (Optional):",
            value = NA, 
            min = 1,
            step = 1
          ),
          setting_note(
            "Leave empty to estimate the number of factors with parallel analysis. ",
            "Set a value when you already know the target factor count."
          ),
          
          selectInput(
            ns("cor_method"),
            "Correlation Method:",
            choices = c(
              "Pearson" = "pearson",
              "Spearman" = "spearman",
              "Polychoric" = "polychoric"
            ),
            selected = "pearson"
          ),
          setting_note(
            "Pearson is fast for continuous data. Spearman is rank-based. ",
            "Polychoric is usually better for ordinal/Likert items, but can be slower."
          ),

          selectInput(
            ns("search_method"),
            "Search Method:",
            choices = c(
              "All Subsets" = "all_subsets",
              "Stepwise" = "stepwise",
              "Backward" = "backward"
            ),
            selected = "all_subsets"
          ),
          setting_note(
            tags$b("All Subsets:"), " tests combinations of initially problematic items. ",
            tags$b("Stepwise:"), " re-detects problematic items after each removal round. ",
            tags$b("Backward:"), " removes one best item per round and is usually fastest."
          ),

          numericInput(
            ns("max_removed_per_iter"),
            "Max Items per Tested Removal:",
            value = 3,
            min = 1,
            step = 1
          ),
          setting_note(
            "For All Subsets and Stepwise, this limits how many items are tested ",
            "together in one removal combination. For example, 3 means the app can ",
            "try removing 1, 2, or 3 items at once. Backward ignores this setting ",
            "because it tests one item at a time."
          ),

          numericInput(
            ns("max_iter"),
            "Max Iterations (Stepwise / Backward):",
            value = 20,
            min = 1,
            step = 1
          ),
          setting_note(
            "Used by Stepwise and Backward. It limits how many rounds the algorithm can run. ",
            "A higher value allows more re-check/removal cycles but increases runtime."
          ),

          numericInput(
            ns("max_removed_total"),
            "Max Removed Total (Optional):",
            value = NA,
            min = 1,
            step = 1
          ),
          setting_note(
            "Optional global safety limit for total removed items. ",
            "Leave empty if you do not want to impose a total removal cap."
          ),

          numericInput(
            ns("min_loading"),
            "Minimum Loading:",
            value = 0.30,
            min = 0,
            max = 1,
            step = 0.05
          ),
          setting_note(
            "Items with no absolute loading at or above this threshold are flagged as low-loading."
          ),

          selectInput(
            ns("loading_diff_mode"),
            "Cross-loading Criterion:",
            choices = c(
              "Loading difference" = "numeric",
              "Howard (2016)" = "howard"
            ),
            selected = "numeric"
          ),
          setting_note(
            "Loading difference flags items whose strongest and second-strongest loadings are too close. ",
            "Howard (2016) applies a stricter rule: primary >= .40, secondary < .30, and difference >= .20."
          ),

          numericInput(
            ns("loading_diff"),
            "Minimum Loading Difference:",
            value = 0.10,
            min = 0,
            max = 1,
            step = 0.05
          ),
          setting_note(
            "Used when Cross-loading Criterion is Loading difference. ",
            "Smaller values are more permissive; larger values flag more potential cross-loadings."
          ),

          numericInput(
            ns("min_items_per_factor"),
            "Minimum Items per Factor:",
            value = 3,
            min = 1,
            step = 1
          ),
          setting_note(
            "Removal strategies that would leave fewer than this many items per factor are skipped."
          ),
          
          selectInput(
            ns("extraction_method"),
            "Extraction Method:",
            choices = c(
              "Unweighted Least Squares" = "uls",
              "Minimum Residual" = "minres",
              "Maximum Likelihood" = "ml",
              "Principal Axis" = "pa"
            ),
            selected = "uls"
          ),
          setting_note(
            "EFA extraction method passed to psych::fa. ULS is the default and is usually stable for this workflow."
          ),
          
          selectInput(
            ns("rotation_method"),
            "Rotation Method:",
            choices = c(
              "Oblimin" = "oblimin",
              "Varimax" = "varimax",
              "Promax" = "promax",
              "GeominQ" = "geominQ"
            ),
            selected = "oblimin"
          ),
          setting_note(
            "Rotation method passed to psych::fa. Oblique rotations such as Oblimin/Promax allow factors to correlate."
          ),
          
          actionButton(
            ns("run_itemrest"),
            "Run ItemRest Analysis",
            class = "btn-info w-100",
            icon = icon("play")
          ),
          tags$p(
            class = "text-muted small mt-3 mb-0",
            "Recommended items are shown on the right. To remove them from analyses, use the Exclude Variables tab."
          )
        )
      ),
      
      # Optimal Strategy Results (styled card UI)
      card(
        card_header("Optimal Strategy", bs_icon("trophy")),
        card_body(
          uiOutput(ns("optimal_strategy_ui")),
          tags$p(class = "text-muted small mt-2 mb-0",
                 "The strategy above represents the cleanest factor structure found.")
        )
      )
    ),
    
    # Detailed Summary
    card(
      card_header("Comparative Removal Strategies"),
      card_body(
        tableOutput(ns("removal_summary_table")),
        p(em("This table compares model fit and structure across different item removal thresholds."))
      )
    )
  )
}
