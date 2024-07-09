#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      skin = "black",
      dashboardHeader(title = "Factor Analysis for All"),
      ## Sidebar content
      dashboardSidebar(
        sidebarMenu(
          menuItem("Select Data", tabName = "data", icon = icon("paperclip", lib = "font-awesome")),
          menuItem("Wrangling Data", startExpanded = FALSE, tabName = "w_data", icon = icon("trowel", lib = "font-awesome"),
                   menuSubItem("Exclude The Variables", tabName = "ex_var", icon = icon("star-half-stroke", lib = "font-awesome")),
                   menuSubItem("Splitting the Data", tabName = "split_data", icon = icon("scissors", lib = "font-awesome")),
                   menuSubItem("Examined Outliers", tabName = "outliers", icon = icon("user-check", lib = "font-awesome"))
          ),
          menuItem("Assumptions", tabName = "assumptions", icon = icon("bolt", lib = "font-awesome")),
          menuItem("Exploratory Factor Analysis", startExpanded = FALSE, tabName = "E_efa", icon = icon("binoculars", lib = "font-awesome"),
                   menuSubItem("Factor Retention", tabName = "fac_ret", icon = icon("stairs", lib = "font-awesome")),
                   menuSubItem("Exploratory Factor Analysis", tabName = "efa", icon = icon("mug-hot", lib = "font-awesome")),
                   menuSubItem("Reporting EFA", tabName = "report_efa", icon = icon("clipboard", lib = "font-awesome"))
          ),
          menuItem("Exploratory Graph Analysis", tabName = "ega", icon = icon("megaport", lib = "font-awesome")),
          menuItem("Confirmatory Factor Analysis", tabName = "cfa", icon = icon("feather", lib = "font-awesome")),
          menuItem("Reliability Analysis", tabName = "reliability", icon = icon("rocket", lib = "font-awesome")),
          menuItem("Item Weighting", tabName = "item_weight", icon = icon("dumbbell", lib = "font-awesome"), badgeLabel = "New"),
          menuItem("About", tabName = "about", icon = icon("address-card", lib = "font-awesome"))
        )
      ),
      ## Body content
      dashboardBody(
        tabItems(
          # Data Selection TAB
          tabItem(tabName = "data",
                  fluidRow(
                    box(
                      title = "Please upload your data file.",
                      collapsible = TRUE,
                      status = "success",
                      solidHeader = TRUE,
                      width = 5,
                      strong("Please upload your data as .dat.
                              In addition, any row and column name should not be added.
                              You will see the first ten rows in your data set when you upload your file.
                              Then some summary statistics will emerge."), br(),
                      em("Please be sure about your missing values.
                          There should be no missing values in your data set.
                          If you have missing values, please indicate these values in the data set as NA.
                          In this case, the analyzes in this software deal with the missing values with listwise deletion.")
                    )),
                  fluidRow(
                    box(title = "Select Your '.dat' File.",
                        collapsible = TRUE,
                        status = "success",
                        solidHeader = TRUE,
                        width = 5,
                        fileInput(inputId = "file1",
                                  label = "Choose a dat File",
                                  accept = ".dat", placeholder = "Upload Your .dat file"))),
                  fluidRow(
                    box(title = "Here is the first 10 rows your data",
                        collapsible = TRUE,
                        status = "success",
                        solidHeader = TRUE,
                        width = 5,
                        tableOutput("mydatatable"))),
                  fluidRow(infoBoxOutput("n_var", width = 3),
                           infoBoxOutput("n", width = 3),
                           infoBoxOutput("min_value", width = 3),
                           infoBoxOutput("max_value", width = 3),
                           infoBoxOutput("num_cat", width = 3))
          ),
          # Data Wrangling TAB
          tabItem(tabName = "ex_var",
                  fluidRow(
                    box(
                      title = "Attention Please",
                      collapsible = TRUE,
                      status = "success",
                      solidHeader = TRUE,
                      width = 5,
                      strong("You have to reload the data after each process.", br(),
                             "For example, if you removed some variables, you should download and then upload this data set to the application again.", br(),
                             "For another example, if you remove outliers from the dataset, you need to reload the dataset without outliers.")
                    )),
                  fluidRow(
                    box(title = "Exclude the Some Variables the Data Set",
                        strong("If you want to remove some variables from the data set,", br(),
                               "please write the column numbers of the variables in the field below.", br(),
                               "For example, if you want to exclude the 3rd, 5th and 6th variables,
                                write 3,5,6 in the box below."), br(),
                        textAreaInput(inputId = "excluded_variables", label = "Exclude This Variables"),
                        actionButton(inputId = "exclude_the_variables", "Exclude the Variables"),
                        downloadButton(outputId = "excluded_data", label = "Download Excluded Data")
                    ))),
          tabItem(tabName = "split_data",
                  fluidRow(
                    box(
                      title = "Attention Please",
                      collapsible = TRUE,
                      status = "success",
                      solidHeader = TRUE,
                      width = 5,
                      strong("You have to reload the data after each process.", br(),
                             "For example, if you removed some variables, you should download and then upload this data set to the application again.", br(),
                             "For another example, if you remove outliers from the dataset, you need to reload the dataset without outliers.")
                    )),
                  fluidRow(
                    column(width = 11,
                           box(
                             title = "Do you want to split data set to half for EFA and CFA?",
                             collapsible = TRUE,
                             strong("There are some drawbacks to performing both exploratory and
                            confirmatory factor analysis on the same data set.", br(),
                                    "Therefore, if you do not have the opportunity to collect data again,
                            you can randomly divide the data set into two and perform EFA in one part and CFA in the other part.", br(),
                                    "Do you want to split your dataset into two accordingly?"), br(),
                             actionButton("split_the_data", "Split My Data"), br(),
                             status = "info",
                             solidHeader = TRUE)),
                    column(width = 11,
                           box(title = "Do you want to download splitted data set for EFA?",
                               collapsible = TRUE,
                               strong("If you split the dataset into two, you can use the first dataset for EFA.", br(),
                                      "For this reason, you can download and analyze the data set you will use here."), br(),
                               downloadButton(outputId = "split_download_efa", label = "Download EFA Data")
                           )),
                    column(width = 11,
                           box(title = "Do you want to download splitted data set for CFA?",
                               collapsible = TRUE,
                               strong("If you split the dataset into two, you can use the second dataset for CFA.", br(),
                                      "For this reason, you can download and analyze the data set you will use here."), br(),
                               downloadButton(outputId = "split_download_cfa", label = "Download CFA data")))
                  )),
          # Outliers TAB
          tabItem(tabName = "outliers",
                  fluidRow(
                    box(
                      title = "Attention Please",
                      collapsible = TRUE,
                      status = "success",
                      solidHeader = TRUE,
                      width = 5,
                      strong("You have to reload the data after each process.", br(),
                             "For example, if you removed some variables, you should download and then upload this data set to the application again.", br(),
                             "For another example, if you remove outliers from the dataset, you need to reload the dataset without outliers.")
                    )),
                  fluidRow(
                    # Mahalonobis Distance
                    column(width = 11,
                           box(
                             title = "Mahalonobis Distance",
                             collapsible = TRUE,
                             tableOutput("mah") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                             status = "info",
                             solidHeader = TRUE,
                             strong("Row_number: The row number of the individual who is an outlier", br(),
                                    "MD: Mahalanobis Distance", br(),
                                    "MD_p: Mahalanobis Distance p value")
                           )),
                    # Removing Outliers
                    column(width = 11,
                           box(
                             title = "Do you want to remove the outliers from data?",
                             collapsible = TRUE,
                             strong("In your data set have",
                                    textOutput("n_mah") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                    "Outliers. Do you want to remove from the data?"), br(),
                             actionButton("remove_outliers", "Remove Outliers from My Data"), br(),
                             status = "info",
                             solidHeader = TRUE,
                             br(), strong("If you want to download your data set without outliers, please click."), br(),
                             downloadButton(outputId = "dl", label = "Download the data set without outliers")
                           ))
                  )),
          # Assumptions TAB
          tabItem(tabName = "assumptions",
                  fluidRow(
                    # Information
                    column(width = 11,
                           box(
                             title = "Checking the factor analysis assumptions",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             strong("Multivariate analysis techniques have some common assumptions.", br(),
                                    "Factor analysis has assumptions such as collinearity, multivariate normality, missing values, and multivariate outliers.", br(),
                                    "You can examine the related statistics and then take some precautions.", br(),
                                    "For example, when the assumption of the multivariate normal distribution does not hold,", br(),
                                    "you can use factor extraction methods that are strong against violation of this assumption.")
                           )),
                    # Descriptive Statistics
                    column(width  = 11,
                           box(
                             title = "Descriptive Statistics",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             tableOutput("desc_table") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                             strong("N: The number of observations", br(),
                                    "N(missing): The number of missing values", br(),
                                    "Min: Minimum value of the variable", br(),
                                    "Max: Maximum value of the variable"), br(),
                             downloadButton(outputId = "download_desc", label = "Download Descriptive Statistics")
                           )),
                    # Collinearity Statistics
                    column(width = 11,
                           box(
                             title = "Collinearity Statistics",
                             collapsible = TRUE,
                             tableOutput("collinearity") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                             status = "info",
                             solidHeader = TRUE,
                             strong("VIF_min: Minimum of the Variance Inflation Factor value", br(),
                                    "VIF_max: Maximum of the Variance Inflation Factor value", br(),
                                    "TOL_min: Minimum of the Tolerance Value", br(),
                                    "TOL_max: Maximum of the Tolerance Value", br(),
                                    "CI_min: Minimum of the Conditional Index", br(),
                                    "CI_max: Maximum of the Conditional Index"), br(),
                             br(),
                             em("TV values should be bigger than 0.10", br(),
                                "VIF values should be smaller than 10", br(),
                                "CI values should be smaller than 30  (Kline, 2016; Tabachnik ve Fidell, 2012).")
                           )),
                    # Multivariate Normality
                    column(width = 11,
                           box(
                             title = "Multivariate Normality Tests",
                             collapsible = TRUE,
                             tableOutput("m_normality") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                             status = "info",
                             solidHeader = TRUE,
                             strong("Statistic: Test statistic value", br(),
                                    "p_value: Hypothesis test's p value", br(),
                                    "Result: 'Yes' means your data follow multivariate normal distribution and", br(),
                                    "'No' means your data do not.")
                           )),
                  )),
          # EFA TAB
          tabItem(
            tabName = "fac_ret",
            fluidRow(
              # Information
              column(width = 11,
                     box(
                       title = "Factor Retention",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       strong("In exploratory factor analysis, it is important to decide on the number of factors.", br(),
                              "For this, the findings of more than one method need to be looked at.", br(),
                              "In this way, techniques used to reduce factors have now been added to this application.", br(),
                              "You can choose the methods via the buttons below.")
                     )),
              # Method Selection
              column(width = 11,
                     box(
                       title = "Choose the Methods",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       strong("Please select the method by which you wish to analyse the results."),
                       selectInput(
                         inputId = "dimension_methods",
                         label = "Please select methods",
                         choices = c("Optimal Paralel Analysis" = "pa_mrfa",
                                     "Paralel Analysis" = "pa_traditional",
                                     "HULL" = "hull_method",
                                     "Minimum Average Partial" = "map_method_tra",
                                     "Minimum Average Partial Revised" = "map_method_rev",
                                     "Exploratory Graph Analysis (TMFG)" = "EGA_tmfg",
                                     "Exploratory Graph Analysis (Glasso)" = "EGA_glasso",
                                     "Empirical Kaiser Criterion" = "EK_C",
                                     "Comparison Data" = "comp_data_method"),
                         selected = "hull_method")
                     )),
              # Results
              column(width = 11,
                     box(
                       title = "The Results of The Methods",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       plotOutput(outputId = "scree_plot") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                       strong("Only scree plot is not optimal option. Please examine the other methods results.")
                     )),
              column(width = 11,
                     box(
                       title = "The Results of The Methods",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       tableOutput(outputId = "dim_ret_results") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE")
                     ))
            )),
          # Exploratory Factor Analysis TAB
          tabItem(
            tabName = "efa",
            fluidRow(
              # Information
              column(width = 11,
                     box(
                       title = "Exploratory Factor Analysis",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       strong("Exploratory factor analysis will conduct.", br(),
                              "First of all, you have to make some decisions such as deciding the number of factors.", br(),
                              "You can use up-to-date methods to make decision.", br(),
                              "To conduct analysis, you should choice the correlation matrix.", br(),
                              "If your data has 4 or less categories, you should use polychoric correlation matrix.")
                     )),
              column(width = 11,
                     box(
                       title = "Select the Correlation Matrix",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       radioButtons(inputId = "cor_kind",
                                    label = "Select the Correlation Type",
                                    choices = c("Pearson" = "pea",
                                                "Polychoric" = "poly"),
                                    inline = TRUE,
                                    selected = "poly"))
              ),
              column(width = 11,
                     box(
                       title = "Select the Factoring Method",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       radioButtons(inputId = "fact_method",
                                    label = "Select a Factoring Method",
                                    choices = c("Principle Axis" = "pa",
                                                "Minimum Residual" = "minres",
                                                "Unweighted Least Squares" = "uls",
                                                "Alpha Factoring" = "alpha",
                                                "Principle Component Analysis" = "pca"),
                                    selected = "pa"))
              ),
              column(width = 11,
                     box(
                       title = "Select the Rotating Method",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       selectInput(
                         inputId = "rotating_method",
                         label = "Select a Rotating Method:",
                         choices = c("none",
                                     "varimax",
                                     "quartimax",
                                     "bentlerT",
                                     "equamax",
                                     "promax",
                                     "oblimin",
                                     "simplimax",
                                     "bentlerQ",
                                     "geominQ"),
                         selected = "pa",
                         width = "200px"),
                       strong("Orthogonal Methods: "), "varimax, quartimax, bentlerT, equamax", br(),
                       strong("Oblique Methods: "), "promax, oblimin, simplimax, bentlerQ, geominQ", br(),
                     )
              ),
              column(width = 11,
                     box(
                       title = "Select the Number of Factors",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       numericInput(inputId = "number_factor",
                                    value = 1,
                                    label = "The number of factos",
                                    min = 1,
                                    step = 1,
                                    width = "200px"),
                       strong("Please use Dimension Reduction sub section to decide the number of factors.")
                     )
              )
            )),
          # Reporting EFA TAB
          tabItem(
            tabName = "report_efa",
            fluidRow(
              # KMO, Bartlett and Correlation Tables
              column(width = 11,
                     box(
                       title = "KMO, Bartlett Test and Inter-Item Correlation",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       em("The KMO value less than 0.50 indicates that the sample size is inadequate.", br(),
                          "The heterogeneity of the data set is responsible for the inadequacy of the sample.", br(),
                          "Even if the sample size increases, the KMO value may remain low if the heterogeneity does not change (if the variance does not increase).", br(),
                          "In such a circumstance, it is advised to recollect the data or reconsider which variables will be included in the analysis (Field, 2009)."),
                       br(),
                       htmlOutput(outputId = "kmo_result") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                       br(),
                       em("The Bartlett test determines whether the inter-item correlation matrix differs from the unit matrix statistically significant.", br(),
                          "If the result of this test is significant (p<0.05), the inter-item correlation matrix is different from the unit matrix."),
                       tableOutput(outputId = "bartlett") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                       br(),
                       em("Below MSA values,0.50 indicates that the item does not
                            measure the same domain as the others and should be removed from the pool (Lorenzo-Seva & Ferrando (2021)."), br(),
                       tableOutput(outputId = "robust_msa") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                       plotOutput(outputId = "heat_map", width = "700px", height = "700px") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                       strong("Note: The plot was created in accordance with the correlation matrix that you selected in the 'Exploratory Factor Analysis' section.")
                     )),
              # EFA structure
              column(width = 11,
                     box(
                       title = "Results of the Exploratory Factor Analysis - Examine the Structure",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       tableOutput(outputId = "efa_result_str") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                       downloadButton(outputId = "download_efa_loadings", label = "Download EFA Loadings")
                     )), br(),
              # Interfactor correlation
              column(width = 11,
                     box(
                       title = "Examine Interfactor Correlation",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       tableOutput(outputId = "efa_result_interf_cor") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE")
                     )), br(),
              column(width = 11,
                     box(
                       title = "Examine Explained Variance Ratios",
                       collapsible = TRUE,
                       status = "info",
                       solidHeader = TRUE,
                       tableOutput(outputId = "efa_result_expl_var") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE")
                     ))
            )),
          # EGA TAB
          tabItem(tabName = "ega",
                  fluidRow(
                    column(width = 11,
                           box(
                             title = "Exploratory Graph Analysis",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             strong("You can examine the exploratory graph analysis results in this section.", br())
                           )),
                    column(width = 11,
                           box(title = "Specification of EGA",
                               collapsible = TRUE,
                               status = "info",
                               solidHeader = TRUE,
                               strong("Please select an estimation method of the EGA.", br(),
                                      "There are two type of the estimation method as Glasso and TMFG.", br(),
                                      "TMFG is better than glasso for unidimensional structures."), br(),
                               selectInput(inputId = "est_method_ega",
                                           label = "Select an estimation method",
                                           choices = c("TMFG" = "TMFG",
                                                       "Glasso" = "glasso"),
                                           selected = "TMFG")
                           )),
                    column(width = 11,
                           box(title = "Results of the EGA",
                               collapsible = TRUE,
                               status = "info",
                               solidHeader = TRUE,
                               strong("Here is the network results", br()),
                               tableOutput(outputId = "ega_network") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"), br(),
                               downloadButton(outputId = "ega_network_results", label = "Download EGA Network Results")
                           )),
                    column(width = 11,
                           box(title = "The EGA Graph",
                               collapsible = TRUE,
                               status = "info",
                               solidHeader = TRUE,
                               plotOutput(outputId = "ega_network_plot") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                               downloadButton(outputId = "ega_network_download", label = "Download EGA Graph")
                           )),
                  )),
          # CFA TAB
          tabItem(tabName = "cfa",
                  fluidRow(
                    # Results of CFA
                    column(width = 11,
                           box(
                             title = "Specification of the CFA",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             strong("Select a correlation method. If your data have 4 or less categories, you should choose 'Polychoric' correlation.", br(),
                                    "But your data have 5 or more categories, you can select 'Pearson' correlastion.", br(),
                                    "In some situations, polychoric correlation matrix can not be calculated.", br(),
                                    "In such a case you can use the pearson correlation matrix."), br(),
                             radioButtons(inputId = "cfa_cor_mat",
                                          label = "Select a correlation matrix",
                                          choices = c("Polychoric" = "ordered",
                                                      "Pearson" = "pea"),
                                          selected = "ordered",
                                          width = "400px"), br(),
                             selectInput(inputId = "cfa_est_method",
                                         label = "Select an estimation method",
                                         choices = c("ULSMV" = "ULSMV",
                                                     "DWLS" = "DWLS",
                                                     "ULS" = "ULS",
                                                     "WLSMV" = "WLSMV",
                                                     "MLR" = "MLR",
                                                     "ML" = "ML"),
                                         selected = "ULSMV",
                                         width = "400px")
                           )),
                    # Define the structure
                    column(width = 11,
                           box(
                             title = "Please define the structure of the variables",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             strong("Please use V (uppercase) for defining. Because your uploaded data was named as V1, V2...", br(),
                                    "if you have a multidimensional structure then you must define each dimension on a separate line.", br(),
                                    "e.g. f1 =~ V1 + V2", br(),
                                    "f2 =~ V1 + V2", br()),
                             strong(br(), "If you define correlated errors you can use ~~ operator. e.g. V1 ~~ V2", br()),
                             textAreaInput(inputId = "cfa_define",
                                           label = "Please define your structure.",
                                           value = "f1 =~ V1 + V2 f2 =~ V3 + V4"),
                             actionButton(inputId = "define_structure", "Define!"), br(),
                           )),
                    column(width = 11,
                           box(
                             title = "Results of the CFA (Fit Indices)",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             tableOutput(outputId = "cfa_results") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                             downloadButton(outputId = "download_fits", label = "Download Fit Results")
                           )),
                    column(width = 11,
                           box(
                             title = "Results of the CFA (Factor Loadings)",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             tableOutput(outputId = "cfa_factor_loadings") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                             downloadButton(outputId = "download_cfa_loadings", label = "Download Factor Loadings")
                           )),
                    column(width = 11,
                           box(
                             title = "Modification Suggestions (Sorted & First 20 rows)",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             tableOutput(outputId = "cfa_modification") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE")
                           )),
                    column(width = 11,
                           box(
                             title = "Path Diagram of the Model",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             plotOutput(outputId = "path_diagram") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                             downloadButton(outputId = "save_diagram", label = "Save the Path Diagram"), br(),
                           ))
                  )),
          # Reliability Tab
          tabItem(tabName = "reliability",
                  fluidRow(
                    # Information
                    column(width = 11,
                           box(
                             title = "Reliability Analysis",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             strong("This section gives the reliability coefficients in terms of internal consistency.
                                    Some of these coefficients require defining the structure.
                                    Therefore, a definition should be made for the construct reliability and stratified alpha coefficients.
                                    As with CFA, the measured construct should be defined in building reliability.
                                    For the stratified alpha coefficient, we should specify the strata.
                                    For example, a definition as 1,1,1,2,2,2 means that items 1, 2, and 3 belong to the first layer, and items 4, 5 and 6 to the second layer.
                                    While reporting the reliability coefficients, Cronbach alpha and other reliability coefficients should be reported.")
                           )),
                    # Reliability Coeff
                    column(width = 11,
                           box(
                             title = "Select Reliability Coefficient",
                             collapsible = TRUE,
                             selectInput(inputId = "reliability_coeff",
                                         label = "Select a reliability coefficient",
                                         choices = c("Cronbach Alpha" = "alpha",
                                                     "McDonald Omega" = "omega",
                                                     "Armor Theta" = "theta",
                                                     "Structural Reliability" = "structure",
                                                     "Stratified Alpha" = "s_alpha"),
                                         selected = "alpha"), br(),
                             strong("If you choose 'Structural Reliability' please define the structure below."), br(),
                             textAreaInput(
                               inputId = "defined_structure",
                               label = "Please define the structure",
                               value = "f1 =~ V1 + V2 + V3 f2 =~ V3 + V4 + V5",
                               width = "1000px"), br(),
                             strong("If you choose 'Stratified Alpha' lease define which item belongs to which factor."), br(),
                             textAreaInput(inputId = "strata_define",
                                           label = "Please define which item belongs to which factor",
                                           value = "1,1,1,1,1,1,2,2,2,2"),
                             verbatimTextOutput(outputId = "reliability_result") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                             status = "info",
                             solidHeader = TRUE
                           ))
                  )),
          # Item Weighting Tab
          tabItem(tabName = "item_weight",
                  fluidRow(
                    # Information about item weighting
                    column(width = 11,
                           box(
                             title = "Item Weighting Analysis",
                             collapsible = TRUE,
                             status = "info",
                             solidHeader = TRUE,
                             strong("Item weighting can increase the validity and reliability of the data set.
                                    Therefore, in this section, weighting is made with the method developed by Kilic & Dogan (2019) -  doi: 10.21031/epod.516057.
                                    You can do EFA or CFA with the weighted item scores matrix.
                                    Reviewing the results, you can use the weighted scores in your future analysis.
                                    For example, you can weigh your items with this method, and then save the weighted results.
                                    You can use these weighted results for regression, t-test or other analysis.")
                           )),
                    # Weight
                    column(width = 11,
                           box(title = "If you want to use item weighting, please use this button.",
                               collapsible = TRUE,
                               strong("Item weighting"), br(),
                               actionButton(inputId = "weight_item", "Weight My Items"), br(),
                               tableOutput(outputId = "weighted_scores") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                               strong("If you want to download weighted scores please use following button."), br(),
                               downloadButton(outputId = "download_weighted_scores", label = "Download Weighted Scores"), br(),
                               status = "info",
                               solidHeader = TRUE
                           ))
                  )),
          # About Tab
          tabItem(tabName = "about",
                  fluidRow(
                    column(width = 11,
                           box(
                             title = "About the Application and Aim",
                             collapsible = TRUE,
                             htmlOutput(outputId = "about"),
                             status = "info",
                             solidHeader = TRUE
                           )),
                    column(width = 11,
                           box(
                             title = "About the Developer",
                             collapsible = TRUE,
                             htmlOutput(outputId = "developer"),
                             status = "info",
                             solidHeader = TRUE
                           )),
                    column(width = 11,
                           box(
                             title = "Thanks to Contributors",
                             collapsible = TRUE,
                             htmlOutput(outputId = "contributer"),
                             status = "info",
                             solidHeader = TRUE
                           ))
                  ))
        )
      )
    )
  )
}
###############################################################################################################

app_ui_tr <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(skin = "black",
                  dashboardHeader(title = "Faktor Analizi icin Her sey"),

                  ## Sidebar content
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("Veri Secimi", tabName = "data", icon = icon("paperclip", lib = "font-awesome")),
                      menuItem("Veri Manipulasyonu", startExpanded = FALSE, tabName = "w_data", icon = icon("trowel", lib = "font-awesome"),
                               menuSubItem("Degisken Silme", tabName = "ex_var", icon = icon("star-half-stroke", lib = "font-awesome")),
                               menuSubItem("Veri Setini Bolme", tabName = "split_data", icon = icon("scissors", lib = "font-awesome")),
                               menuSubItem("Uc Degerleri Inceleme", tabName = "outliers", icon = icon("user-check", lib = "font-awesome"))
                      ),
                      menuItem("Varsayimlar", tabName = "assumptions", icon = icon("bolt", lib = "font-awesome")),
                      menuItem("Acimlayici Faktor Analizi", startExpanded = FALSE, tabName = "E_efa", icon = icon("binoculars", lib = "font-awesome"),
                               menuSubItem("Faktor Sayisi Belirleme", tabName = "fac_ret", icon = icon("stairs", lib = "font-awesome")),
                               menuSubItem("Acimlayici Faktor Analizi", tabName = "efa", icon = icon("mug-hot", lib = "font-awesome")),
                               menuSubItem("AFA Raporlama", tabName = "report_efa", icon = icon("clipboard", lib = "font-awesome"))
                      ),
                      menuItem("Acimlayici Grafik Analizi", tabName = "ega", icon = icon("megaport", lib = "font-awesome")),
                      menuItem("Dogrulayici Faktor Analizi", tabName = "cfa", icon = icon("feather", lib = "font-awesome")),
                      menuItem("Guvenirlik Analizi", tabName = "reliability", icon = icon("rocket", lib = "font-awesome")),
                      menuItem("Madde Agirliklandirma", tabName = "item_weight", icon = icon("dumbbell", lib = "font-awesome"), badgeLabel = "New"),
                      menuItem("Hakkinda", tabName = "about", icon = icon("address-card", lib = "font-awesome"))
                    )
                  ),

                  ## Body content
                  dashboardBody(
                    tabItems(
                      #----------------------------------------------Veri Secimi TAB--------------------------------------------------------
                      tabItem(tabName = "data",
                              fluidRow(
                                box(
                                  title = "Veri setinizi yukleyiniz.",
                                  collapsible = TRUE,
                                  status = "success",
                                  solidHeader = TRUE,
                                  width = 5,
                                  strong("Veri setini lutfen .dat olarak yukleyiniz. Bazi durumlarda .txt, .prn uzantilarini da gorebilmektedir.
                                  Ayrica veri setinde satir ve sutun isimleri de bulunmalidir.
                                  Asagida veri setinizdeki ilk 10 satiri gorebilirsiniz.
                                  Daha sonra ozet istatistikler bulunacaktir."), br(),
                                  em("Veri setindeki kayip verileri inceleyiniz. Yuklediginiz veri setinde kayip veri olmadigindan emin olunuz.
                                  Veri setinde kayip veri varsa NA ile gosterilmis olmalidir. Bos hucreleri uygulama kabul etmemektedir.
                                  Kayip veriler NA ile gosterildiginde uygulama satir silme yontemi ile kayip verilerle basa cikmaktadir.")
                                )
                              ),
                              fluidRow(
                                box(title = "'.dat' Uzantili Dosyanizi Seciniz.",
                                    collapsible = TRUE,
                                    status = "success",
                                    solidHeader = TRUE,
                                    width = 5,
                                    fileInput(inputId = "file1",
                                              label = "Bir dat dosyasi seciniz.",
                                              accept = ".dat", placeholder = "Bir dat dosyasi seciniz."))
                              ),
                              fluidRow(
                                box(title = "Veri setinizdeki ilk 10 satir",
                                    collapsible = TRUE,
                                    status = "success",
                                    solidHeader = TRUE,
                                    width = 5,
                                    tableOutput("mydatatable"))
                              ),
                              fluidRow(infoBoxOutput("n_var", width = 3),
                                       infoBoxOutput("n", width = 3),
                                       infoBoxOutput("min_value", width = 3),
                                       infoBoxOutput("max_value", width = 3),
                                       infoBoxOutput("num_cat", width = 3))
                      ),
                      #----------------------------------------------Veri Manipulasyonu TAB--------------------------------------------------------
                      tabItem(tabName = "ex_var",
                              fluidRow(
                                box(
                                  title = "Lutfen Dikkat",
                                  collapsible = TRUE,
                                  status = "success",
                                  solidHeader = TRUE,
                                  width = 5,
                                  strong("Her islemden sonra yeni veri setini yuklemeniz gerekmektedir.", br(),
                                         "ornegin bazi degiskenleri sildikten sonra veri setini bilgisayariniza kaydedip veri okuma asamasinda indirdiginiz
veri setini yeniden okutmalisiniz.", br(),
                                         "Baska bir ornek olarak, eger veri setinden uc degerleri cikardiysaniz, uc deger bulunmayan veri setini bilgisayariniza kaydedip ilk asamaya donerek
uc deger olmayan veri setini yeniden okutmalisiniz.")
                                )
                              ),
                              fluidRow(
                                box(title = "Veri Setinden Degisken cikart",
                                    strong("Eger veri setinden bazi degiskenleri silmek istiyorsaniz,", br(),
                                           "asagidaki kutuya degiskenlerin kacinci sutunda oldugunu yaziniz.", br(),
                                           "ornegin, 3., 5. ve 6. degiskenleri veri setinden cikarmak icin kutuya 3,5,6 yazilmalidir."), br(),
                                    textAreaInput(inputId = "excluded_variables", label = "Bu Degiskenleri Veri Setinden cikart"),
                                    actionButton(inputId = "exclude_the_variables", "Degiskenleri cikart"),
                                    downloadButton(outputId = "excluded_data", label = "Yeni Veri Setini Kaydet")
                                )
                              )
                      ),
                      tabItem(tabName = "split_data",
                              fluidRow(
                                box(
                                  title = "Lutfen Dikkat",
                                  collapsible = TRUE,
                                  status = "success",
                                  solidHeader = TRUE,
                                  width = 5,
                                  strong("Her islemden sonra yeni veri setini yuklemeniz gerekmektedir.", br(),
                                         "ornegin bazi degiskenleri sildikten sonra veri setini bilgisayariniza kaydedip veri okuma asamasinda indirdiginiz
veri setini yeniden okutmalisiniz.", br(),
                                         "Baska bir ornek olarak, eger veri setinden uc degerleri cikardiysaniz, uc deger bulunmayan veri setini bilgisayariniza kaydedip ilk asamaya donerek
uc deger olmayan veri setini yeniden okutmalisiniz.")
                                )
                              ),
                              fluidRow(
                                column(width = 11,
                                       box(
                                         title = "AFA ve DFA icin veri setini ikiye bolmek ister misiniz?",
                                         collapsible = TRUE,
                                         strong("Ayni veri setinde acimlayici ve dogrulayici faktor analizini gerceklestirmenin bazi sakincalari bulunmaktadir.", br(),
                                                "Bu nedenle, yeniden veri toplama sansiniz yoksa,
veri setini rastgele olarak ikiye bolup birinde AFA digerinde DFA yapabilirsiniz.", br(),
                                                "Buna gore veri setini rastgele olarak ikiye bolmek ister misiniz?"), br(),
                                         actionButton("split_the_data", "Veri Setimi Bol"), br(),
                                         status = "info",
                                         solidHeader = TRUE)
                                ),
                                column(width = 11,
                                       box(title = "AFA icin bolunmus veri setini kaydetmek ister misiniz?",
                                           collapsible = TRUE,
                                           strong("Eger veri setini ikiye bolduyseniz, birinci veri setini AFA icin kullanabilirsiniz.", br(),
                                                  "Bu nedenle, veri setini kaydedip yeniden yukleyerek AFA analizlerini gerceklestirebilirsiniz."), br(),
                                           downloadButton(outputId = "split_download_efa", label = "AFA Veri Setini Kaydet"))
                                ),
                                column(width = 11,
                                       box(title = "DFA icin bolunmus veri setini kaydetmek ister misiniz?",
                                           collapsible = TRUE,
                                           strong("Eger veri setini ikiye bolduyseniz, birinci veri setini DFA icin kullanabilirsiniz.", br(),
                                                  "Bu nedenle, veri setini kaydedip yeniden yukleyerek DFA analizlerini gerceklestirebilirsiniz."), br(),
                                           downloadButton(outputId = "split_download_cfa", label = "DFA Veri Setini Kaydet"))
                                )
                              )
                      ),
                      tabItem(tabName = "outliers",
                              fluidRow(
                                box(
                                  title = "Lutfen Dikkat",
                                  collapsible = TRUE,
                                  status = "success",
                                  solidHeader = TRUE,
                                  width = 5,
                                  strong("Her islemden sonra yeni veri setini yuklemeniz gerekmektedir.", br(),
                                         "ornegin bazi degiskenleri sildikten sonra veri setini bilgisayariniza kaydedip veri okuma asamasinda indirdiginiz
veri setini yeniden okutmalisiniz.", br(),
                                         "Baska bir ornek olarak, eger veri setinden uc degerleri cikardiysaniz, uc deger bulunmayan veri setini bilgisayariniza kaydedip ilk asamaya donerek
uc deger olmayan veri setini yeniden okutmalisiniz.")
                                )
                              ),
                              fluidRow(
                                # Mahalonobis Distance
                                column(width = 11,
                                       box(
                                         title = "Mahalonobis Uzakligi",
                                         collapsible = TRUE,
                                         tableOutput("mah") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Row_number: Uc deger olan bireyin sira numarasi", br(),
                                                "MD: Mahalanobis Uzakligi", br(),
                                                "MD_p: Mahalanobis Uzakligi p-degeri")
                                       )
                                ),
                                # Removing Outliers
                                column(width = 11,
                                       box(
                                         title = "Uc Degerleri Veri Setinden cikartmak Ister misiniz?",
                                         collapsible = TRUE,
                                         strong("Veri setinizde",
                                                textOutput("n_mah") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                                "uc deger bulunmaktadir. Bunlari veri setinden cikartmak ister misiniz?"), br(),
                                         actionButton("remove_outliers", "Uc Degerleri Veri Setimden cikart"), br(),
                                         status = "info",
                                         solidHeader = TRUE,
                                         br(), strong("Uc degerlerin olmadigi veri setini kaydetmek icin lutfen tiklayiniz."), br(),
                                         downloadButton(outputId = "dl", label = "Uc Deger Bulunmayan Veri Setini Kaydet")
                                       )
                                )
                              )
                      ),
                      #----------------------------------------------Varsayimlar TAB--------------------------------------------------------
                      tabItem(tabName = "assumptions",
                              fluidRow(
                                # Information
                                column(width = 11,
                                       box(
                                         title = "Faktor Analizi Varsayimlarinin Kontrolu",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("cok degiskenli istatistik tekniklerinin bazi varsayimlari bulunmaktadir.", br(),
                                                "Faktor analizi de coklu dogrusal baglanti, cok degiskenli normallik, kayip veriler ve cok degiskenli uc degerler acisindan veri setinin incelenmesini gerektirmektedir.", br(),
                                                "Bu uygulamada anilan varsayimlari inceleyebilir ve bunlara yonelik olarak onlemler alabilirsiniz.", br(),
                                                "ornegin, cok degiskenli normal dagilim varsayimi saglanmadiginda,", br(),
                                                "bu varsayimin ihlaline karsi guclu olan faktor cikarma ya da kestirim yontemleri tercih edilebilir.")
                                       )
                                ),
                                # Descriptive Statistics
                                column(width = 11,
                                       box(
                                         title = "Tanimlayici Istatistikler",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         tableOutput("desc_table") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         strong("N: orneklem Buyuklugu", br(),
                                                "N(missing): Kayip veri miktari", br(),
                                                "Min: Degiskenin en kucuk degeri", br(),
                                                "Max: Degiskenin en buyuk degeri"), br(),
                                         downloadButton(outputId = "download_desc", label = "Tanimlayici Istatistikleri Indir")
                                       )
                                ),
                                # Collinearity Statistics
                                column(width = 11,
                                       box(
                                         title = "Coklu Dogrusal Baglanti Istatistikleri",
                                         collapsible = TRUE,
                                         tableOutput("collinearity") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("VIF_min: Variance Inflation Factor istatistiginin en kucuk degeri", br(),
                                                "VIF_max: Variance Inflation Factor istatistiginin en buyuk degeri", br(),
                                                "TOL_min: Tolerance Value istatistiginin en kucuk degeri", br(),
                                                "TOL_max: Tolerance Value istatistiginin en buyuk degeri", br(),
                                                "CI_min: Conditional Index istatistiginin en kucuk degeri", br(),
                                                "CI_max: Conditional Index istatistiginin en buyuk degeri"), br(),
                                         br(),
                                         em("TV degeri 0.10'dan buyuk", br(),
                                            "VIF degeri 10'dan kucuk", br(),
                                            "CI degeri 30'dan kucuk olmalidir (Kline, 2011; Tabachnik ve Fidell, 2012).")
                                       )
                                ),
                                # Multivariate Normality
                                column(width = 11,
                                       box(
                                         title = "cok Degiskenli Normallik Testleri",
                                         collapsible = TRUE,
                                         tableOutput("m_normality") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Statistic: Testin istatistik degeri", br(),
                                                "p_value: Hipotez testinin p-degeri", br(),
                                                "Result: 'Yes' degiskenlerin cok degiskenli normal dagilima uygun oldugu", br(),
                                                "'No' degiskenlerin cok degiskenli normal dagilima uygun olmadigi anlamina gelmektedir.")
                                       )
                                )
                              )
                      ),
                      #----------------------------------------------AFA TAB--------------------------------------------------------
                      tabItem(tabName = "fac_ret",
                              fluidRow(
                                # Information
                                column(width = 11,
                                       box(
                                         title = "Faktor Sayisi Belirleme",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Acimlayici faktor analizinde onemli kararlardan biri de faktor sayisina karar vermektir.", br(),
                                                "Bu amacla birden fazla yontemin sonucu incelenebilir.", br(),
                                                "Bu nedenle, faktor sayisi belirleme yontemleri de bu uygulamaya eklenmistir.", br(),
                                                "Asagida menuden yontemleri secebilirsiniz.")
                                       )
                                ),
                                # Method Selection
                                column(width = 11,
                                       box(
                                         title = "Bir Yontem Seciniz",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Sonuclarini incelemek istediginiz yontemi seciniz."),
                                         selectInput(inputId = "dimension_methods",
                                                     label = "Lutfen bir yontem seciniz",
                                                     choices = c("Optimal Paralel Analysis" = "pa_mrfa",
                                                                 "Paralel Analysis" = "pa_traditional",
                                                                 "HULL" = "hull_method",
                                                                 "Minimum Average Partial" = "map_method_tra",
                                                                 "Minimum Average Partial Revised" = "map_method_rev",
                                                                 "Exploratory Graph Analysis (TMFG)" = "EGA_tmfg",
                                                                 "Exploratory Graph Analysis (Glasso)" = "EGA_glasso",
                                                                 "Empirical Kaiser Criterion" = "EK_C",
                                                                 "Comparison Data" = "comp_data_method"),
                                                     selected = "hull_method")
                                       )
                                ),
                                # Results
                                column(width = 11,
                                       box(
                                         title = "Yontemlerin Sonuclari",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         plotOutput(outputId = "scree_plot") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         strong("Sadece yamac grafigine gore karar vermek dogru olmayacaktir. Lutfen objektif yontem sonuclarini da inceleyiniz.")
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "Yontemlerin Sonuclari",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         tableOutput(outputId = "dim_ret_results") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE")
                                       )
                                )
                              )
                      ),
                      # Exploratory Factor Analysis TAB
                      tabItem(tabName = "efa",
                              fluidRow(
                                # Information
                                column(width = 11,
                                       box(
                                         title = "Acimlayici Faktor Analizi",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Acimlayici faktor analizi gerceklestirmek icin bazi kararlar verilmelidir.", br(),
                                                "Ilk olarak faktor sayisina karar verirken objektif ve guncel yontemler dikkate alinabilir.", br(),
                                                "Uygulamaya guncel literaturde kendine yer bulan faktor sayisi belirleme yontemleri eklenmistir ancak her durumda dogru sonuc veren bir yontem bulunmamaktadir.", br(),
                                                "AFA gerceklestirmek icin bir diger karar kullanilacak korelasyon matrisidir.", br(),
                                                "Eger degiskenlerin 4 ya da daha az kategoriye sahipse bu durumda polikorik korelasyon matrisini kullanmalisiniz.")
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "Korelasyon Matrisini Seciniz",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         radioButtons(inputId = "cor_kind",
                                                      label = "Korelasyon Matrisini Seciniz",
                                                      choices = c("Pearson" = "pea",
                                                                  "Polychoric" = "poly"),
                                                      inline = TRUE,
                                                      selected = "poly")
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "Faktor cikarma Yontemini Seciniz",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         radioButtons(inputId = "fact_method",
                                                      label = "Bir faktor cikarma yontemi seciniz",
                                                      choices = c("Principle Axis" = "pa",
                                                                  "Minimum Residual" = "minres",
                                                                  "Unweighted Least Squares" = "uls",
                                                                  "Alpha Factoring" = "alpha",
                                                                  "Principle Component Analysis" = "pca"),
                                                      selected = "pa")
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "Bir Dondurme Yontemi Seciniz",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         selectInput(inputId = "rotating_method",
                                                     label = "Bir Dondurme Yontemi Seciniz:",
                                                     choices = c("none",
                                                                 "varimax",
                                                                 "quartimax",
                                                                 "bentlerT",
                                                                 "equamax",
                                                                 "promax",
                                                                 "oblimin",
                                                                 "simplimax",
                                                                 "bentlerQ",
                                                                 "geominQ"),
                                                     selected = "none",
                                                     width = "200px"),
                                         strong("Dik Dondurme Yontemleri: "), "varimax, quartimax, bentlerT, equamax", br(),
                                         strong("Egik Dondurme Yontemleri: "), "promax, oblimin, simplimax, bentlerQ, geominQ", br()
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "Faktor Sayisini Seciniz",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         numericInput(inputId = "number_factor",
                                                      value = 1,
                                                      label = "Faktor Sayisi",
                                                      min = 1,
                                                      step = 1,
                                                      width = "200px"),
                                         strong("Lutfen faktor sayisina karar vermek icin Faktor Sayisi Belirleme alt bolumunu kullaniniz.")
                                       )
                                )
                              )
                      ),
                      # EFA Report TAB
                      tabItem(tabName = "report_efa",
                              fluidRow(
                                # KMO, Bartlett and Correlation Tables
                                column(width = 11,
                                       box(
                                         title = "KMO, Bartlett Testi ve Maddeler Arasi Korelasyonlar",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         em("KMO degerinin 0.50'den kucuk olmasi maddeler arasi korelasyonlarin yetersiz oldugu seklinde yorumlanabilir.", br(),
                                            "Veri setindeki heterojenligin yeterli olmamasi orneklemle de iliskili olabilir.", br(),
                                            "Ancak orneklem buyuklugu artasa bile KMO degeri hala ayni kalabilir (eger varyans yukselmezse KMO da artmayacaktir).", br(),
                                            "Boyle durumlarda yeniden veri toplanmasi ya da bazi degiskenlerin analizden cikarilmasi onerilmektedir (Field, 2009)."),
                                         br(),
                                         htmlOutput(outputId = "kmo_result") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         br(),
                                         em("Bartlett testi maddeler arasi korelasyon matrisinin birim matristen istatistiksel olarak anlamli duzeyde farklilasip farklilasmadigini test etmektedir.", br(),
                                            "Bu testin sonucu anlamli cikarsa (p<0.05), maddeler arasi korelasyon matrisinin
birim matristen istatistiksel olarak anlamli duzeyde farklilastigi yorumu yapilabilir."),
                                         tableOutput(outputId = "bartlett") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         br(),
                                         em("MSA'nin 0.50'nin altindaki degerleri maddenin diger maddelerle ayni ozelligi olcmedigi seklinde yorumlanabilir ve bu maddelerin olcekten
cikartilmasi onerilmektedir (Lorenzo-Seva & Ferrando, 2021)."), br(),
                                         tableOutput(outputId = "robust_msa") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         plotOutput(outputId = "heat_map", width = "700px", height = "700px") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         strong("Not: Grafik Acimlayici Faktor Analizi bolumunde sectiginiz korelasyon matrisine gore olusturulmustur.")
                                       )
                                ),
                                # EFA structure
                                column(width = 11,
                                       box(
                                         title = "AFA Sonuclari - Yapinin Incelenmesi",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         tableOutput(outputId = "efa_result_str") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         downloadButton(outputId = "download_efa_loadings", label = "EFA Yuklemelerini Indir")
                                       )
                                ),
                                br(),
                                # Interfactor correlation
                                column(width = 11,
                                       box(
                                         title = "Faktorler Arasi Korelasyonlarin Incelenmesi",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         tableOutput(outputId = "efa_result_interf_cor") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE")
                                       )
                                ),
                                br(),
                                column(width = 11,
                                       box(
                                         title = "Aciklanan Varyans Oranlarinin Incelenmesi",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         tableOutput(outputId = "efa_result_expl_var") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE")
                                       )
                                )
                              )
                      ),
                      #----------------------------------------------EGA TAB--------------------------------------------------------
                      tabItem(tabName = "ega",
                              fluidRow(
                                column(width = 11,
                                       box(
                                         title = "Acimlayici Grafik Analizi",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Acimlayici grafik analizi sonuclarini bu bolumde inceleyebilirsiniz.", br())
                                       )
                                ),
                                column(width = 11,
                                       box(title = "EGA'nin ozelliklerini Belirleyiniz.",
                                           collapsible = TRUE,
                                           status = "info",
                                           solidHeader = TRUE,
                                           strong("Lutfen bir kestirim yontemi seciniz.", br(),
                                                  "Iki tur kestirim yontemi bulunmaktadir: Glasso ve TMFG.", br(),
                                                  "Tek boyutlu yapilarda TMFG, Glasso'dan daha iyi calismaktadir."), br(),
                                           selectInput(inputId = "est_method_ega",
                                                       label = "Bir kestirim yontemi seciniz.",
                                                       choices = c("TMFG" = "TMFG",
                                                                   "Glasso" = "glasso"),
                                                       selected = "TMFG")
                                       )
                                ),
                                column(width = 11,
                                       box(title = "EGA Sonuclari",
                                           collapsible = TRUE,
                                           status = "info",
                                           solidHeader = TRUE,
                                           strong("Ag grafigi sonuclari", br()),
                                           tableOutput(outputId = "ega_network") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"), br(),
                                           downloadButton(outputId = "ega_network_results", label = "EGA sonuclarini kaydet.")
                                       )
                                ),
                                column(width = 11,
                                       box(title = "EGA Grafigi",
                                           collapsible = TRUE,
                                           status = "info",
                                           solidHeader = TRUE,
                                           plotOutput(outputId = "ega_network_plot") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                           downloadButton(outputId = "ega_network_download", label = "EGA grafigini kaydet.")
                                       )
                                )
                              )
                      ),
                      #----------------------------------------------CFA TAB--------------------------------------------------------
                      tabItem(tabName = "cfa",
                              fluidRow(
                                # Results of CFA
                                column(width = 11,
                                       box(
                                         title = "Dogrulayici Faktor Analizinin ozelliklerinin Tanimlanmasi",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Bir korelasyon matrisi seciniz. Eger degiskenleriniz 4 ya da daha az kategoriye sahipse polikorik korelasyonu secmelisiniz", br(),
                                                "Fakat degiskenleriniz 5 ya da daha fazla kategoriye sahipse Pearson korelasyonu secilebilir. Ancak oncelikle polikorik korelasyon sonuclarini
incelemek yarariniza olacaktir.", br(),
                                                "Bazi durumlarda polikorik korelasyon matrisi hesaplanamayabilir.", br(),
                                                "Boyle durumlarda Pearson'i secebilirsiniz."), br(),
                                         radioButtons(inputId = "cfa_cor_mat",
                                                      label = "Bir korelasyon matrisi seciniz.",
                                                      choices = c("Polychoric" = "ordered",
                                                                  "Pearson" = "pea"),
                                                      selected = "ordered",
                                                      width = "400px"), br(),
                                         selectInput(inputId = "cfa_est_method",
                                                     label = "Bir kestirim yontemi seciniz",
                                                     choices = c("ULSMV" = "ULSMV",
                                                                 "DWLS" = "DWLS",
                                                                 "ULS" = "ULS",
                                                                 "WLSMV" = "WLSMV",
                                                                 "MLR" = "MLR",
                                                                 "ML"= "ML"),
                                                     selected = "ULSMV",
                                                     width = "400px")
                                       )
                                ),
                                # Define the structure
                                column(width = 11,
                                       box(
                                         title = "Yapiyi tanimlayiniz.",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Lutfen V'leri buyuk harf olarak yaziniz. cunku yuklediginiz veri setindeki degisken isimleri otomatik olarak V ile gosterilmektedir. ornegin; V1, V2...", br(),
                                                "cok boyutlu bir yapiniz varsa her bir boyut yeni satirda tanimlanmalidir.", br(),
                                                "ornegin: f1 =~ V1 + V2", br(),
                                                "f2 =~ V1 + V2", br()),
                                         strong(br(), "Eger hatalar arasinda iliski kuracaksaniz (modifikasyon yapacaksaniz) ~~ operatorunu kullaniniz. ornegin V1 ~~ V2 birinci ve ikinci maddelerin hata terimleri arasinda
iliski tanimlamaktadir."), br(),
                                         textAreaInput(inputId = "cfa_define",
                                                       label = "Lutfen yapiyi tanimlayiniz.",
                                                       value = "f1 =~ V1 + V2 f2 =~ V3 + V4"),
                                         actionButton(inputId = "define_structure", "Tanimla!"), br()
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "DFA Sonuclari (Uyum Indeksleri)",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         tableOutput(outputId = "cfa_results") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         downloadButton(outputId = "download_fits", label = "Uyum indekslerini kaydet")
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "DFA Sonuclari (Faktor Yukleri)",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         tableOutput(outputId = "cfa_factor_loadings") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         downloadButton(outputId = "download_cfa_loadings", label = "Faktor Yuklerini Kaydet")
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "Modifikasyon onerileri (onem duzeyine gore siralanmis ilk 20 oneri)",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         tableOutput(outputId = "cfa_modification") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE")
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "Modelin Yol Grafigi",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         plotOutput(outputId = "path_diagram") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         downloadButton(outputId = "save_diagram", label = "Grafigi Kaydet"), br()
                                       )
                                )
                              )
                      ),
                      #----------------------------------------------Guvenirlik Analizi TAB--------------------------------------------------------
                      tabItem(tabName = "reliability",
                              fluidRow(
                                # Information
                                column(width = 11,
                                       box(
                                         title = "Guvenirlik Analizi",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Bu bolumde ic tutarlilik anlamindaki guvenirlik katsayilari sunulmustur.
                                        Bu guvenirlik katsayilarindan tabakali alfa ve yapi guvenirligi olculen faktor yapisini tanimlamayi gerektirmektedir.
                                        Yapi guvenirligi icin tanimlama yaparken DFA bolumunde oldugu gibi yapi tanimlanmaktadir.
                                        Tabakali alfa katsayisi icin ise hangi maddenin hangi tabakada (boyutta) yer aldigi belirtilmelidir.
                                        ornegin, 1,1,1,2,2,2 seklindeki bir tanimlama; 1, 2 ve 3 numarali maddelerin birinci tabakaya,
                                        4, 5 ve 6 numarali maddelerin ise ikinci tabakaya ait oldugunu anlatmaktadir.
                                        Guvenirlik katsayilari raporlanirken Cronbach alpha'nin yaninda diger guvenirlik katsayilari da raporlanmalidir.")
                                       )
                                ),
                                # Reliability Coeff
                                column(width = 11,
                                       box(
                                         title = "Bir Guvenirlik Katsayisi Seciniz",
                                         collapsible = TRUE,
                                         selectInput(inputId = "reliability_coeff",
                                                     label = "Bir guvenirlik katsayisi seciniz",
                                                     choices = c("Cronbach Alpha" = "alpha",
                                                                 "McDonald Omega" = "omega",
                                                                 "Armor Theta" = "theta",
                                                                 "Structural Reliability" = "structure",
                                                                 "Stratified Alpha" = "s_alpha"),
                                                     selected = "alpha"), br(),
                                         strong("Eger yapi guvenirligini sectiyseniz lutfen yapiyi tanimlayiniz."), br(),
                                         textAreaInput(inputId = "defined_structure",
                                                       label = "Lutfen yapiyi tanimlayiniz.",
                                                       value = "f1 =~ V1 + V2 + V3 f2 =~ V3 + V4 + V5",
                                                       width = "1000px"), br(),
                                         strong("Eger tabakali alfayi sectiyseniz lutfen hangi maddenin hangi tabakaya (faktore) ait oldugunu tanimlayiniz."), br(),
                                         textAreaInput(inputId = "strata_define",
                                                       label = "Lutfen maddelerin ait olduklari tabakalari belirtiniz.",
                                                       value = "1,1,1,1,1,1,2,2,2,2"),
                                         verbatimTextOutput(outputId = "reliability_result") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         status = "info",
                                         solidHeader = TRUE
                                       )
                                )
                              )
                      ),
                      #----------------------------------------------Madde Agirliklandirma TAB--------------------------------------------------------
                      tabItem(tabName = "item_weight",
                              fluidRow(
                                # Information about item weighting
                                column(width = 11,
                                       box(
                                         title = "Madde Agirliklandirma Analizi",
                                         collapsible = TRUE,
                                         status = "info",
                                         solidHeader = TRUE,
                                         strong("Madde agirliklandirma veri setinin gecerlik ve guvenirligini artirabilir.
Bu nedenle bu bolumde Kilic & Dogan (2019) - [doi: 10.21031/epod.516057] tarafindan onerilen madde agirliklandirma yontemi
uygulamaya eklenmistir. Agirliklandirilmis madde puanlari ile de AFA ya da DFA yapilabilir.
Agirliklandirilmamis sonuclar ile karsilastirilarak ileriki analizlerde hangi veri setinin (agirliklandirilmis ya da agirliklandirilmamis) karar verilebilir.
ornegin bu yontemle maddeleri agirliklandirdiktan sonra yeni veri setini kaydedebilirsiniz.
Kaydettiginiz veri seti ile AFA ya da DFA yaparak sonuclari karsilastirabilirsiniz.
Agirliklandirilmis veri setini kullanarak t-testi, ANOVA ya da yapisal esitlik modellemesi gibi ileriki analizlerde bu puanlari kullanabilirsiniz.")
                                       )
                                ),
                                # Weight
                                column(width = 11,
                                       box(
                                         title = "Eger madde agirliklandirmasini kullanmak istiyorsaniz lutfen bu butonu kullaniniz.",
                                         collapsible = TRUE,
                                         strong("Item weighting"), br(),
                                         actionButton(inputId = "weight_item", "Weight My Items"), br(),
                                         tableOutput(outputId = "weighted_scores") %>% shinycssloaders::withSpinner(type = 8, color = "#728FCE"),
                                         strong("Agirliklandirilmis veri setini bilgisayariniza kaydetmek icin su butonu kullanabilirsiniz:"), br(),
                                         downloadButton(outputId = "download_weighted_scores", label = "Agirliklandirilmis Puanlari Kaydet"), br(),
                                         status = "info",
                                         solidHeader = TRUE
                                       )
                                )
                              )
                      ),
                      #----------------------------------------------Hakkinda TAB--------------------------------------------------------
                      tabItem(tabName = "about",
                              fluidRow(
                                # Information about the application
                                column(width = 11,
                                       box(
                                         title = "Uygulamanin Amaci ve Hakkinda Bolumu",
                                         collapsible = TRUE,
                                         htmlOutput(outputId = "about"),
                                         status = "info",
                                         solidHeader = TRUE
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "Gelistirici Hakkinda",
                                         collapsible = TRUE,
                                         htmlOutput(outputId = "developer"),
                                         status = "info",
                                         solidHeader = TRUE
                                       )
                                ),
                                column(width = 11,
                                       box(
                                         title = "Katki Sunanlara Tesekkurler",
                                         collapsible = TRUE,
                                         htmlOutput(outputId = "contributer"),
                                         status = "info",
                                         solidHeader = TRUE
                                       )
                                )
                              )
                      )
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
