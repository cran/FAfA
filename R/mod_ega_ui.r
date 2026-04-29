#' EGA UI Module
#' @noRd
ega_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),

      # Sol Taraf: Ayarlar
      card(
        card_header("EGA Setup", class = "bg-primary text-white", bs_icon("diagram-2")),
        card_body(
          selectInput(ns("ega_estimation_method_select"), "Network Model:",
                      choices = c("TMFG" = "TMFG", "Glasso" = "glasso"), selected = "TMFG"),
          selectInput(ns("ega_algorithm_select"), "Community Detection:",
                      choices = c(
                        "Walktrap (default)"  = "walktrap",
                        "Louvain"             = "louvain",
                        "Leiden"              = "leiden",
                        "Fast Greedy"         = "fast_greedy",
                        "Edge Betweenness"    = "edge_betweenness",
                        "Label Propagation"   = "label_prop"
                      ), selected = "walktrap"),
          radioButtons(ns("ega_correlation_type_radio"), "Correlation:",
                       choices = c("Auto" = "cor_auto", "Pearson" = "pearson"), inline = TRUE),
          actionButton(ns("run_ega_button"), "Run EGA",
                       icon = icon("project-diagram"), class = "btn-success w-100")
        )
      ),

      # Sağ Taraf: Plot
      card(
        card_header("Network Plot"),
        plotOutput(ns("ega_network_plot_output"), height = "500px") %>% withSpinner(type = 8, color = "#2C3E50"),
        downloadButton(ns("download_ega_plot_button"), "Download Plot")
      )
    ),

    card(
      card_header("Dimensions & Structure"),
      navset_card_tab(
        nav_panel(
          title = "Item Allocation", icon = bs_icon("diagram-3"),
          verbatimTextOutput(ns("ega_dimensionality_summary_output")),
          tableOutput(ns("ega_item_community_table_output")) %>%
            withSpinner(type = 8, color = "#2C3E50")
        ),
        nav_panel(
          title = "Adjacency Matrix", icon = bs_icon("table"),
          div(
            style = "overflow-x: auto;",
            tableOutput(ns("ega_network_table_output"))
          ),
          downloadButton(ns("download_ega_network_button"), "Download CSV", class = "btn-sm mt-2")
        )
      )
    )
  )
}
