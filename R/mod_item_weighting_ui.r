#' Item Weighting UI
#' @noRd
item_weighting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Item Weighting (Kilic & Dogan)", class = "bg-primary text-white"),
      card_body(
        p("Applies psychometric weighting based on item difficulty and discrimination."),
        actionButton(ns("calculate_weighted_scores_button"), "Calculate Scores", icon = icon("balance-scale"), class = "btn-primary"),
        hr(),
        h5("Preview"),
        tableOutput(ns("weighted_scores_table_output")),
        downloadButton(ns("download_weighted_data_button"), "Download Weighted Data")
      )
    )
  )
}