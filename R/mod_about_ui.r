# About UI Module
 
about_ui <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(
        width = 11, # Or 12 for full width
        box(
          title = "About This Application (FAfA: Factor Analysis for All)",
          collapsible = TRUE,
          status = "info", # Using "info" for informational boxes
          solidHeader = TRUE,
          width = NULL,
          # Using htmlOutput as the server will generate HTML
          htmlOutput(ns("application_description_html"))
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "About the Developer",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          htmlOutput(ns("developer_info_html"))
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "Contributors & Version Information",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          htmlOutput(ns("contributors_version_html"))
        )
      )
    ),
  fluidRow(
      column(
        width = 11,
        box(
          title = "How to Cite This Package?",
          collapsible = TRUE,
          collapsed = FALSE, 
          status = "primary", 
          solidHeader = TRUE,
          width = NULL,
          htmlOutput(ns("citation_info_html")) 
        )
      )
    )
  )
}
