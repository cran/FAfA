#' Confirmatory Factor Analysis (CFA) UI Module
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinycssloaders withSpinner
#' @noRd
cfa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),

      # -- LEFT: Model Builder ----------------------------------
      card(
        card_header(class = "bg-info text-white",
                    bs_icon("magic"), " Model Builder"),
        card_body(
          navset_pill(
            id = ns("builder_tabs"),

            # -- Tab 1: First-order factors --
            nav_panel("1st-Order", value = "tab_fo",
              p(class = "text-muted small mt-2 mb-1",
                "Define each latent factor and its indicators."),
              textInput(ns("builder_factor_name"), "Factor Name:",
                        placeholder = "e.g. F1, Anxiety ..."),
              selectizeInput(
                ns("builder_items"), "Indicators:",
                choices = NULL, multiple = TRUE,
                options = list(placeholder = "Select observed variables...")
              ),
              actionButton(ns("btn_add_factor"), "Add to Syntax",
                           icon = icon("code"),
                           class = "btn-secondary btn-sm w-100 mb-3"),
              hr(class = "my-2"),
              h6("Add Residual Covariance (~~)", class = "text-muted"),
              selectizeInput(
                ns("builder_cov_items"), NULL,
                choices = NULL, multiple = TRUE,
                options = list(maxItems = 2,
                               placeholder = "Select 2 variables...")
              ),
              actionButton(ns("btn_add_cov"), "Add Covariance",
                           icon = icon("link"),
                           class = "btn-outline-secondary btn-sm w-100")
            ),

            # -- Tab 2: Higher-order --
            nav_panel("Higher-Order", value = "tab_ho",
              div(
                class = "alert alert-info p-2 mt-2",
                style = "font-size:0.78rem;",
                bs_icon("info-circle"), " ",
                "Higher-order (second-order) model: a general factor ",
                tags$b("loads on first-order latent factors"),
                " (not directly on items). ",
                "First-order factors must already be defined in the syntax."
              ),
              textInput(ns("ho_factor_name"), "General Factor Name:",
                        placeholder = "e.g. g, General, HO ..."),
              selectizeInput(
                ns("ho_first_order"), "First-Order Factors:",
                choices = NULL, multiple = TRUE,
                options = list(placeholder = "Select latent factors ...")
              ),
              p(class = "text-muted small",
                "Tip: Type factor names manually if not yet in syntax."),
              actionButton(ns("btn_add_ho"), "Add Higher-Order Factor",
                           icon = icon("layer-group"),
                           class = "btn-primary btn-sm w-100")
            ),

            # -- Tab 3: Bifactor --
            nav_panel("Bifactor", value = "tab_bf",
              div(
                class = "alert alert-warning p-2 mt-2",
                style = "font-size:0.78rem;",
                bs_icon("info-circle"), " ",
                "Bifactor model: a ",
                tags$b("general factor (g)"),
                " loads on ALL items; specific group factors load on ",
                "subsets of items. All factors are ",
                tags$b("orthogonal"),
                " to each other."
              ),
              textInput(ns("bf_general_name"), "General Factor Name:",
                        value = "g", placeholder = "e.g. g"),
              hr(class = "my-2"),
              h6("Group Factor", class = "text-primary"),
              textInput(ns("bf_group_name"), "Group Factor Name:",
                        placeholder = "e.g. S1, Specific1 ..."),
              selectizeInput(
                ns("bf_group_items"), "Items for this Group:",
                choices = NULL, multiple = TRUE,
                options = list(placeholder = "Select observed variables...")
              ),
              actionButton(ns("btn_add_bf_group"), "Add Group Factor",
                           icon = icon("plus"),
                           class = "btn-secondary btn-sm w-100 mb-2"),
              hr(class = "my-2"),
              actionButton(ns("btn_finalize_bf"), "Finalize Bifactor Syntax",
                           icon = icon("check-circle"),
                           class = "btn-success btn-sm w-100"),
              p(class = "text-muted small mt-1",
                "Finalize: adds g =~ all items, all orthogonality constraints (~~0*) to syntax.")
            ),

            # -- Tab 4: Templates --
            nav_panel("Templates", value = "tab_tpl",
              p(class = "text-muted small mt-2",
                "Load a template into the syntax editor, then modify."),
              actionButton(ns("tpl_correlated"), "Correlated Factors",
                           class = "btn-outline-primary btn-sm w-100 mb-2"),
              actionButton(ns("tpl_ho"), "Higher-Order (2nd-order)",
                           class = "btn-outline-primary btn-sm w-100 mb-2"),
              actionButton(ns("tpl_bifactor"), "Bifactor",
                           class = "btn-outline-primary btn-sm w-100 mb-2"),
              actionButton(ns("btn_clear_syntax"), "Clear Syntax",
                           icon = icon("trash"),
                           class = "btn-outline-danger btn-sm w-100 mt-3")
            )
          )
        )
      ),

      # -- RIGHT: Syntax + Settings + Results ------------------
      tagList(
        card(
          card_header(class = "bg-primary text-white",
                      bs_icon("sliders"), " CFA Analysis Setup"),
          card_body(
            textAreaInput(
              ns("cfa_model_syntax_input"),
              label = "Model Syntax (lavaan):",
              placeholder = paste(
                "# First-order:",
                "F1 =~ item1 + item2 + item3",
                "F2 =~ item4 + item5 + item6",
                "",
                "# Higher-order:",
                "g =~ F1 + F2",
                "",
                "# Bifactor orthogonality:",
                "g ~~ 0*F1",
                sep = "\n"
              ),
              rows = 8, resize = "vertical"
            ),
            layout_columns(
              col_widths = c(6, 6),
              radioButtons(
                ns("cfa_correlation_type_radio"), "Data Type:",
                choices  = c("Continuous (Pearson)" = "pea",
                             "Ordinal (Polychoric)" = "poly"),
                inline   = TRUE, selected = "pea"
              ),
              selectInput(
                ns("cfa_estimator_select"), "Estimator:",
                choices  = c("ML", "MLR", "MLM", "GLS", "WLS",
                             "WLSMV", "DWLS"),
                selected = "ML"
              )
            ),
            div(
              class = "alert alert-secondary p-2",
              style = "font-size:0.76rem;",
              bs_icon("lightbulb"), " ",
              tags$b("Ordinal/Polychoric:"), " WLSMV recommended.  ",
              tags$b("Bifactor:"), " add ",
              tags$code("orthogonal = TRUE"),
              " constraint or use the Bifactor builder tab.  ",
              tags$b("Higher-order:"), " g =~ F1 + F2 in lavaan syntax."
            ),
            actionButton(ns("run_cfa_button"), "Run CFA",
                         icon = icon("play"),
                         class = "btn-success w-100 btn-lg")
          )
        ),

        # Results tabs
        navset_card_tab(
          full_screen = TRUE,
          nav_panel("Path Diagram", icon = bs_icon("diagram-3"),
            layout_sidebar(
              sidebar = sidebar(
                title = "Plot Settings", width = 250,
                selectInput(ns("plot_layout"), "Layout:",
                  choices = c("Tree (top-down)"   = "tree",
                              "Tree (left-right)"  = "tree2",
                              "Spring"             = "spring",
                              "Circle"             = "circle")),
                sliderInput(ns("plot_rotation"),       "Rotation:",    1, 4, 2, 1),
                sliderInput(ns("plot_man_size"),        "Box Width:",   4, 20, 10),
                sliderInput(ns("plot_edge_label_cex"), "Label Size:",  0.5, 1.5, 0.8, 0.1),
                checkboxInput(ns("plot_show_labels"), "Show Estimates", TRUE)
              ),
              plotOutput(ns("cfa_path_diagram_output"), height = "600px") %>%
                withSpinner(type = 8, color = "#2C3E50"),
              downloadButton(ns("download_path_diagram_button"),
                             "Download (.svg)", class = "btn-sm")
            )
          ),
          nav_panel("Fit Measures", icon = bs_icon("table"),
            tableOutput(ns("cfa_fit_measures_table")),
            downloadButton(ns("download_fit_measures_button"), "Download CSV", class = "btn-sm")
          ),
          nav_panel("Factor Loadings", icon = bs_icon("list-ol"),
            tableOutput(ns("cfa_factor_loadings_table")),
            downloadButton(ns("download_factor_loadings_button"), "Download CSV", class = "btn-sm")
          ),
          nav_panel("Modification Indices", icon = bs_icon("tools"),
            tableOutput(ns("cfa_modification_indices_table"))
          )
        )
      )
    )
  )
}
