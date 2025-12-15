#' EGA UI Module
#' @noRd
ega_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("EGA Setup", class = "bg-primary text-white", bs_icon("diagram-2")),
        card_body(
          selectInput(ns("ega_estimation_method_select"), "Network Method:", choices = c("TMFG", "glasso"), selected = "TMFG"),
          radioButtons(ns("ega_correlation_type_radio"), "Correlation:", choices = c("Auto"="cor_auto", "Pearson"="pearson"), inline = TRUE),
          actionButton(ns("run_ega_button"), "Run EGA", icon = icon("project-diagram"), class = "btn-success w-100")
        )
      ),
      card(
        card_header("Network Plot"),
        plotOutput(ns("ega_network_plot_output"), height = "500px") %>% withSpinner(type = 8, color = "#2C3E50"),
        downloadButton(ns("download_ega_plot_button"), "Download Plot")
      )
    ),
    card(
      card_header("Network Structure & Dimensions"),
      layout_columns(
        col_widths = c(6, 6),
        div(h5("Adjacency Matrix"), tableOutput(ns("ega_network_table_output"))),
        div(h5("Item Allocation"), verbatimTextOutput(ns("ega_dimensionality_summary_output")), tableOutput(ns("ega_item_community_table_output")))
      )
    )
  )
}