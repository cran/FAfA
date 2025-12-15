#' About Server Module
#'
#' @param id Module namespace ID.
#' @export
about_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Output for the application description
    output$application_description_html <- renderUI({
      tagList(
        h4(strong("FAfA: Factor Analysis for All")),

        p(strong("Aim:")),
        p("The FAfA (Factor Analysis for All) Shiny application is a powerful and user-friendly tool designed to simplify Exploratory Factor Analysis (EFA), Confirmatory Factor Analysis (CFA), and Measurement Invariance workflows for researchers. Developed with R and Shiny, FAfA aims to unify these psychometric procedures within a single, intuitive interface, reducing the need for multiple software tools or complex manual preprocessing steps. It enables users to diagnose and handle missing data, validate assumptions, perform random dataset splits, conduct comprehensive reliability analyses (including Stratified Alpha), apply automated item drop-out strategies, and utilize item weighting techniques to enhance construct validity."),
        br(),

        h5(strong("Overview")),
        p("FAfA provides a comprehensive suite of tools for psychometric analysis. It leverages established R packages such as 'psych', 'lavaan', 'missForest', and 'EGAnet', ensuring that statistical analyses are accurate and reliable. Users can easily upload their datasets (e.g., CSV, Excel, SAV, DAT), perform advanced diagnostics for missing values and outliers, configure key parameters for their analyses (like estimator types, rotation methods), and interpret results through a responsive user interface."),
        br(),

        h5(strong("Key Features and Technical Details")),
        tags$ul(
          tags$li(strong("Unified EFA, CFA & Invariance Workflow:"), " Conduct EFA, CFA, and Measurement Invariance testing within the same environment using a seamless workflow."),
          tags$li(strong("Advanced Missing Data Handling:"), " Analyze missingness patterns, test for MCAR, and apply robust imputation methods such as MICE and missForest (Random Forest)."),
          tags$li(strong("Item Drop Out Analysis:"), " Utilize automated strategies to identify and remove problematic items, optimizing scale length and factor structure."),
          tags$li(strong("Reliability Analysis:"), " Evaluate internal consistency using Cronbach's Alpha, McDonald's Omega, Armor's Theta, and Stratified Alpha for multidimensional scales."),
          tags$li(strong("Interactive Model Builder:"), " Easily define factor structures and covariances for CFA and Measurement Invariance without manually writing complex syntax."),
          tags$li(strong("Assumption Checking & Wrangling:"), " Built-in diagnostics for multivariate normality, outliers (Mahalanobis Distance), and multicollinearity."),
          tags$li(strong("Random Dataset Splitting:"), " Supports rigorous validation by allowing users to randomly split datasets (e.g., for EFA on one half, CFA on the other)."),
          tags$li(strong("Interactive and Reproducible Results:"), " Provides real-time updates with outputs like path diagrams, scree plots, and fit statistics (CFI, TLI, RMSEA). Supports exporting results for publication.")
        )
      )
    })

    # Output for developer information
    output$developer_info_html <- renderUI({
      tagList(
        p(
          strong("Lead Application Developer:"), br(),
          "Abdullah Faruk KILIC, PhD, Assoc. Prof.", br(),
          "Trakya University, Faculty of Education", br(),
          "Department of Educational Science", br(),
          "Division of Measurement and Evaluation in Education"
        ),
        p(strong("Email 1:"), tags$a(href = "mailto:abdullahfarukkilic@gmail.com", "abdullahfarukkilic@gmail.com")),
        p(strong("Email 2:"), tags$a(href = "mailto:afarukkilic@trakya.edu.tr", "afarukkilic@trakya.edu.tr")),
        p(strong("ResearchGate:"), tags$a(href = "https://www.researchgate.net/profile/Abdullah-Kilic-2", target = "_blank", "View ResearchGate Profile")),
        p(strong("Google Scholar:"), tags$a(href = "https://scholar.google.com/citations?user=AP7LlpoAAAAJ&hl=en", target = "_blank", "View Google Scholar Profile")),

        hr(),

        p(
          strong("Author (Item Drop Out & Missing Data):"), br(),
          "Ahmet CALISKAN", br(),
          "Trakya University, Faculty of Education", br(),
          "Department of Educational Science", br(),
          "Division of Measurement and Evaluation in Education"
        ),
        p(strong("Email:"), tags$a(href = "mailto:ahmetcaliskan@trakya.edu.tr", "ahmetcaliskan@trakya.edu.tr")),
        p(em("Contributed to the development of the Item Drop Out module and provided insights on missing data handling.")),

        br(),
        p(em("Please feel free to contact us regarding any errors or feedback on the application."))
      )
    })

    # Output for contributors and version information
    output$contributors_version_html <- renderUI({
      tagList(
        p(strong("Acknowledgements:")),
        p("A big thank you to the following researchers for their valuable feedback on the initial versions of FAfA:"),
        tags$ul(
          tags$li(strong("Seda Nur SAKAR:"), tags$a(href = "https://avesis.hacettepe.edu.tr/sedasakar", target = "_blank", "Researcher's Profile")),
          tags$li(strong("Tugay KACAK:"), tags$a(href = "https://personel.trakya.edu.tr/tugaykacak/", target = "_blank", "Researcher's Profile")),
          tags$li(strong("Basak ERDEM KARA:"), tags$a(href = "https://avesis.anadolu.edu.tr/basakerdem", target = "_blank", "Researcher's Profile")),
          tags$li(strong("Meltem ACAR GUVENDIR:"), tags$a(href = "https://personel.trakya.edu.tr/meltemacar/", target = "_blank", "Researcher's Profile")),
          tags$li(strong("Alperen YANDI:"), tags$a(href = "https://www.linkedin.com/in/alperen-yandi-36404891", target = "_blank", "Researcher's Profile")),
          tags$li(strong("Murat Dogan SAHIN:"), tags$a(href = "https://avesis.anadolu.edu.tr/mdsahin", target = "_blank", "Researcher's Profile"))
        ),
        br(),
        p(strong("What's New in Version 0.5:")),
        tags$ul(
          tags$li("Added 'Item Drop Out' module for automated item reduction strategies."),
          tags$li("Introduced 'Missing Data' module with advanced imputation methods (MICE, missForest)."),
          tags$li("Added 'Stratified Alpha' to Reliability Analysis."),
          tags$li("Enhanced CFA and Invariance modules with an interactive 'Model Builder'."),
          tags$li("Improved UI consistency and performance.")
        ),
        br(),
        p(em("Current Version: 0.5"))
      )
    })

    # Output for citation information
    output$citation_info_html <- renderUI({
      tagList(
        tags$p("If you use this package in your work, please cite it as follows:"),
        tags$br(),
        tags$p(
          style = "font-family: monospace; margin-left: 20px;",
          "Kilic, A. F. (2024). FAfA: Factor analysis for all: An R package to conduct factor analysis with RShiny application. ",
          tags$em("Journal of Measurement and Evaluation in Education and Psychology, 15"),
          HTML("(4), 446&ndash;451. "),
          tags$a(href = "https://doi.org/10.21031/epod.1555805", target = "_blank", "https://doi.org/10.21031/epod.1555805")
        ),
        tags$br(),
        tags$p("BibTeX citation:"),
        tags$pre(
          style = "font-family: monospace; background-color: #f0f0f0; padding: 10px; border-radius: 5px; white-space: pre-wrap; word-wrap: break-word;",
          paste(
            "@article{Kilic2024FAfA,",
            "  author    = {Kilic, Abdullah Faruk},",
            "  title     = {FAfA: Factor analysis for all An R package to conduct factor analysis with RShiny application},",
            "  journal   = {Journal of Measurement and Evaluation in Education and Psychology},",
            "  volume    = {15},",
            "  number    = {4},",
            " pages = {446\u2013451},",
            "  year      = {2024},",
            "  doi       = {10.21031/epod.1555805},",
            "  URL       = {https://doi.org/10.21031/epod.1555805}",
            "}",
            sep = "\n"
          )
        )
      )
    })

  })
}
