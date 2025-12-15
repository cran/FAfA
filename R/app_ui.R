#' The application User-Interface
#'
#' @param request Internal parameter for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd
app_ui <- function(request) {
  tagList(
    # External Resources
    golem_add_external_resources(),

    # Modern bslib Layout with Sidebar Navigation
    page_fillable(
      title = "FAfA: Factor Analysis for All",
      theme = bs_theme(
        version = 5,
        preset = "spacelab",
        primary = "#446E9B",
        "enable-rounded" = TRUE
      ),

      # Sidebar Navigation List
      navset_pill_list(
        id = "tabs",
        widths = c(3, 9), # Sidebar width 3/12, Main width 9/12

        # --- Data & Wrangling Group ---
        nav_item(
          div(
            class = "mt-3 mb-2 text-primary fw-bold text-uppercase",
            style = "letter-spacing: 1px; font-size: 0.85rem;",
            bs_icon("database-gear"), " Data & Wrangling"
          )
        ),
        nav_panel(
          title = "Select Data",
          value = "data",
          icon = bs_icon("file-earmark-arrow-up"),
          data_selection_ui("data_selection")
        ),
        nav_panel(
          title = "Exclude Variables",
          value = "ex_var",
          icon = bs_icon("x-circle"),
          wrangling_ui_ex_var("wrangling_ex_var")
        ),
        nav_panel(
          title = "Missing Values",
          value = "missing_val",
          icon = bs_icon("patch-question"),
          mod_missing_ui("missing_val")
        ),
        nav_panel(
          title = "Split Dataset",
          value = "split_data",
          icon = bs_icon("scissors"),
          wrangling_ui_split("wrangling_split")
        ),
        nav_panel(
          title = "Manage Outliers",
          value = "outliers",
          icon = bs_icon("exclamation-triangle"),
          wrangling_ui_outliers("wrangling_outliers")
        ),

        # --- Assumption Checks Group ---
        nav_item(
          div(
            class = "mt-4 mb-2 text-primary fw-bold text-uppercase",
            style = "letter-spacing: 1px; font-size: 0.85rem;",
            bs_icon("clipboard-check"), " Assumption Checks"
          )
        ),
        nav_panel(
          title = "Assumptions",
          value = "assumptions",
          icon = bs_icon("check2-square"),
          assumptions_ui("assumptions")
        ),

        # --- Exploratory Factor Analysis Group ---
        nav_item(
          div(
            class = "mt-4 mb-2 text-primary fw-bold text-uppercase",
            style = "letter-spacing: 1px; font-size: 0.85rem;",
            bs_icon("search"), " Exploratory Factor Analysis"
          )
        ),
        nav_panel(
          title = "Factor Retention",
          value = "fac_ret",
          icon = bs_icon("graph-up"),
          efa_ui_fac_ret("efa_fac_ret")
        ),
        nav_panel(
          title = "EFA Setup & Analysis",
          value = "efa",
          icon = bs_icon("sliders"),
          efa_ui_analysis("efa_analysis")
        ),

        nav_panel(
          title = "Item Drop Out",
          value = "item_rest",
          icon = bs_icon("list-check"),
          mod_itemrest_ui("item_rest")
        ),
        nav_panel(
          title = "EFA Reporting",
          value = "report_efa",
          icon = bs_icon("file-text"),
          efa_ui_report("efa_report")
        ),

        # --- Advanced Analysis Group ---
        nav_item(
          div(
            class = "mt-4 mb-2 text-primary fw-bold text-uppercase",
            style = "letter-spacing: 1px; font-size: 0.85rem;",
            bs_icon("cpu"), " Advanced Analysis"
          )
        ),
        nav_panel(
          title = "Exploratory Graph Analysis",
          value = "ega",
          icon = bs_icon("diagram-3"),
          ega_ui("ega")
        ),
        nav_panel(
          title = "Confirmatory Factor Analysis",
          value = "cfa",
          icon = bs_icon("diagram-2"),
          cfa_ui("cfa")
        ),
        nav_panel(
          title = "Measurement Invariance",
          value = "inv",
          icon = bs_icon("arrow-left-right"),
          inv_ui("inv")
        ),
        nav_panel(
          title = "Reliability Analysis",
          value = "reliability",
          icon = bs_icon("award"),
          reliability_ui("reliability")
        ),
        nav_panel(
          title = "Item Weighting",
          value = "item_weight",
          icon = bs_icon("calculator"),
          item_weighting_ui("item_weighting")
        ),

        # --- Info Group ---
        nav_item(
          div(
            class = "mt-4 mb-2 text-primary fw-bold text-uppercase",
            style = "letter-spacing: 1px; font-size: 0.85rem;",
            bs_icon("info-circle"), " Info"
          )
        ),
        nav_panel(
          title = "About",
          value = "about",
          icon = bs_icon("question-circle"),
          about_ui("about")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "FAfA"
    )
  )
}
