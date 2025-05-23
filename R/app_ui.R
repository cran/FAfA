#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard menuSubItem
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard tabItems
#' @importFrom shinydashboard tabItem
#' @noRd
app_ui <- function(request) {
    tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # UI Definition
        dashboardPage(
          skin = "black",
          dashboardHeader(title = "Factor Analysis for All (FAfA)"),
          dashboardSidebar(
            sidebarMenu(
              id = "tabs",
              menuItem("Select Data", tabName = "data", icon = icon("file-import", lib = "font-awesome")),
              menuItem("Wrangling Data", startExpanded = FALSE, icon = icon("wrench", lib = "font-awesome"),
                       menuSubItem("Exclude Variables", tabName = "ex_var", icon = icon("columns", lib = "font-awesome")),
                       menuSubItem("Split Dataset", tabName = "split_data", icon = icon("object-ungroup", lib = "font-awesome")),
                       menuSubItem("Manage Outliers", tabName = "outliers", icon = icon("user-ninja", lib = "font-awesome"))
              ),
              menuItem("Check Assumptions", tabName = "assumptions", icon = icon("tasks", lib = "font-awesome")),
              menuItem("Exploratory Factor Analysis", startExpanded = FALSE, icon = icon("magnifying-glass-chart", lib = "font-awesome"),
                       menuSubItem("Factor Retention", tabName = "fac_ret", icon = icon("stairs", lib = "font-awesome")),
                       menuSubItem("EFA Setup & Analysis", tabName = "efa", icon = icon("cogs", lib = "font-awesome")),
                       menuSubItem("EFA Reporting", tabName = "report_efa", icon = icon("chart-pie", lib = "font-awesome"))
              ),
              menuItem("Exploratory Graph Analysis", tabName = "ega", icon = icon("project-diagram", lib = "font-awesome")),
              menuItem("Confirmatory Factor Analysis", tabName = "cfa", icon = icon("check-double", lib = "font-awesome")),
              menuItem("Measurement Invariance", tabName = "inv", icon = icon("equals", lib = "font-awesome"), badgeLabel = "New"),
              menuItem("Reliability Analysis", tabName = "reliability", icon = icon("award", lib = "font-awesome")),
              menuItem("Item Weighting", tabName = "item_weight", icon = icon("balance-scale", lib = "font-awesome")),
              menuItem("About", tabName = "about", icon = icon("info-circle", lib = "font-awesome"))
            )
          ),
          dashboardBody(
            tabItems(
              # Ensure data_selection_ui("data_selection") uses the UI with the header checkbox
              tabItem(tabName = "data", data_selection_ui("data_selection")),
              tabItem(tabName = "ex_var", wrangling_ui_ex_var("wrangling_ex_var")),
              tabItem(tabName = "split_data", wrangling_ui_split("wrangling_split")),
              tabItem(tabName = "outliers", wrangling_ui_outliers("wrangling_outliers")),
              tabItem(tabName = "assumptions", assumptions_ui("assumptions")),
              tabItem(tabName = "fac_ret", efa_ui_fac_ret("efa_fac_ret")),
              tabItem(tabName = "efa", efa_ui_analysis("efa_analysis")),
              tabItem(tabName = "report_efa", efa_ui_report("efa_report")),
              tabItem(tabName = "ega", ega_ui("ega")),
              tabItem(tabName = "cfa", cfa_ui("cfa")),
              tabItem(tabName = "inv", inv_ui("inv")),
              tabItem(tabName = "reliability", reliability_ui("reliability")),
              tabItem(tabName = "item_weight", item_weighting_ui("item_weighting")),
              tabItem(tabName = "about", about_ui("about"))
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
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
