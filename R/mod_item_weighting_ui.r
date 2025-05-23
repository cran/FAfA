# Item Weighting UI Module
 
item_weighting_ui <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(
        width = 11, # Or 12 for full width
        box(
          title = "Introduction to Item Weighting",
          collapsible = TRUE,
          status = "info", # Using "info" for introductory boxes
          solidHeader = TRUE,
          width = NULL,
          strong(
          "
		  Item weighting is a technique that can be used to adjust item scores, potentially based on psychometric properties like item difficulty or discrimination (reliability).
The goal of applying such weights might be to create a total score that better reflects the underlying construct or to examine if such weighting improves aspects like construct validity when the weighted scores are used in further analyses.
This module applies a specific item weighting algorithm to your dataset: **the weighting method proposed by Kilic & Dogan (2019) [doi: 10.21031/epod.516057]**. The exact nature of this weighting is determined by the implementation of the 'item_weighting' function within your 'utils.R' file.
		  "
          ),
          br(),
          em(
            "This module applies a specific item weighting algorithm (defined in 'utils.R') to your dataset.",br(),
            "The exact nature of the weighting depends on the 'item_weighting' function implemented in your 'utils.R' file."
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "Calculate and View Weighted Scores",
          collapsible = TRUE,
          status = "primary", # "primary" for action-oriented boxes
          solidHeader = TRUE,
          width = NULL,
          p("Click the button below to apply the item weighting procedure to your current dataset."),
          actionButton(ns("calculate_weighted_scores_button"), "Calculate Weighted Scores", icon = icon("balance-scale")), # Changed ID
          br(),br(),
          h4("Preview of Weighted Scores (First 10 Rows):"),
          tableOutput(ns("weighted_scores_table_output")) %>% withSpinner(type = 8, color = "#728FCE"), # Changed ID
          br(),
          downloadButton(ns("download_weighted_data_button"), "Download Weighted Scores (.csv)", icon = icon("download")) # Changed ID
        )
      )
    )
  )
}
