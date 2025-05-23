# Assumptions UI Module 

assumptions_ui <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(
        width = 11, # Or 12 for full width
        box(
          title = "Understanding Assumptions for Factor Analysis",
          collapsible = TRUE,
          status = "info", # Using "info" for introductory boxes
          solidHeader = TRUE,
          width = NULL,
          strong(
            "Multivariate analysis techniques, including factor analysis, rely on several key assumptions about the data.", br(),
            "Common assumptions relevant to factor analysis include the absence of severe multicollinearity, multivariate normality (though some methods are robust to violations), adequate sample size, and management of missing values and multivariate outliers."
          ),
          br(),
          em(
            "This section allows you to examine statistics related to these assumptions:",br(),
            "1. Descriptive Statistics: To understand the basic characteristics of your variables.",br(),
            "2. Collinearity Statistics: To check for high intercorrelations among variables.",br(),
            "3. Multivariate Normality Tests: To assess if the data meet the assumption of multivariate normality.",br(),
            "Understanding these can help you choose appropriate factor extraction methods and interpret your results more accurately. For instance, if multivariate normality is violated, robust estimation methods might be preferred."
          )
        )
      )
    ),
    fluidRow(
      # Descriptive Statistics Box
      column(
        width = 6, # Adjust width as needed
        box(
          title = "Descriptive Statistics",
          collapsible = TRUE,
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          p("Calculate and view basic descriptive statistics for each variable in your dataset."),
          actionButton(ns("run_descriptives_button"), "Calculate Descriptive Statistics", icon = icon("table-list")),
          br(),br(),
          tableOutput(ns("descriptives_table_output")) %>% withSpinner(type = 8, color = "#728FCE"),
          br(),
          strong("Key to Table Columns:"),
          tags$ul(
            tags$li(strong("N:"), "Number of non-missing observations for the variable."),
            tags$li(strong("N (missing):"), "Number of missing values for the variable."),
            tags$li(strong("Min:"), "Minimum observed value."),
            tags$li(strong("Max:"), "Maximum observed value."),
            tags$li(strong("Median:"), "Median value."),
            tags$li(strong("Mean:"), "Mean (average) value."),
            tags$li(strong("Skewness:"), "Measure of asymmetry (0 indicates symmetry)."),
            tags$li(strong("Kurtosis:"), "Measure of 'tailedness' (Kurtosis; 0 for normal distribution).")
          ),
          br(),
          downloadButton(ns("download_descriptives_button"), "Download Descriptive Statistics (.csv)", icon = icon("download"))
        )
      ),
      # Collinearity and Normality Box
      column(
        width = 5, # Adjust width
        box(
          title = "Collinearity Statistics",
          collapsible = TRUE,
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          p("Assess multicollinearity, which occurs when variables are highly correlated. High multicollinearity can destabilize factor analysis results."),
          actionButton(ns("run_collinearity_button"), "Calculate Collinearity Statistics", icon = icon("link-slash")),
          br(),br(),
          tableOutput(ns("collinearity_table_output")) %>% withSpinner(type = 8, color = "#728FCE"),
          br(),
          strong("Key to Table Values (Summary):"),
          tags$ul(
            tags$li(strong("VIF_min/max:"), "Minimum/Maximum Variance Inflation Factor. VIF > 10 may indicate problematic multicollinearity."),
            tags$li(strong("TOL_min/max:"), "Minimum/Maximum Tolerance (1/VIF). Tolerance < 0.10 (or < 0.20) may be a concern."),
            tags$li(strong("CI_min/max:"), "Minimum/Maximum Condition Index. CI > 30 may indicate serious multicollinearity.")
          ),
          br(),
          em(
            "General Guidelines (e.g., Kline, 2016; Tabachnick & Fidell, 2012):",br(),
            "- Tolerance (TOL) values should ideally be > 0.10.", br(),
            "- Variance Inflation Factor (VIF) values should ideally be < 10.", br(),
            "- Condition Index (CI) values should ideally be < 30."
          )
          # No download for collinearity summary in original, can be added if needed.
        ),
        box(
          title = "Multivariate Normality Tests",
          collapsible = TRUE,
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          p("Test whether your data follow a multivariate normal distribution. This assumption is important for some estimation methods like Maximum Likelihood (ML)."),
          actionButton(ns("run_normality_tests_button"), "Calculate Normality Tests", icon = icon("chart-area")),
          br(),br(),
          tableOutput(ns("multivariate_normality_table_output")) %>% withSpinner(type = 8, color = "#728FCE"),
          br(),
          strong("Key to Table Columns:"),
          tags$ul(
            tags$li(strong("Test:"), "Name of the multivariate normality test (Mardia's Skewness, Mardia's Kurtosis, Energy Test)."),
            tags$li(strong("Statistic:"), "The calculated test statistic value."),
            tags$li(strong("p-value:"), "The p-value associated with the test statistic."),
            tags$li(strong("Result:"), "Interpretation of the p-value (typically, p < 0.05 suggests a violation of multivariate normality). 'YES' indicates normality is supported, 'NO' indicates violation.")
          )
          # No download for normality tests in original.
        )
      )
    )
  )
}
