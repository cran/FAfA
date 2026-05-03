#' About Server Module
#'
#' @param id Module namespace ID.
#' @noRd
about_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Output for the application description
    output$application_description_html <- renderUI({
      tagList(
        h4(strong("FAfA: Factor Analysis for All")),

        p(strong("Aim:")),
        p("The FAfA (Factor Analysis for All) Shiny application is a powerful and user-friendly tool designed to simplify Exploratory Factor Analysis (EFA), Confirmatory Factor Analysis (CFA), and Measurement Invariance workflows for researchers. Developed with R and Shiny, FAfA aims to unify these psychometric procedures within a single, intuitive interface, reducing the need for multiple software tools or complex manual preprocessing steps. It enables users to diagnose and handle missing data, validate assumptions, perform random dataset splits, conduct EFA replicability checks across two random half-samples, conduct comprehensive reliability analyses (including Stratified Alpha), apply automated item drop-out strategies, and utilize item weighting techniques to enhance construct validity."),
        br(),

        h5(strong("Overview")),
        p("FAfA provides a comprehensive suite of tools for psychometric analysis. It leverages established R packages such as 'psych', 'lavaan', 'missForest', and 'EGAnet', ensuring that statistical analyses are accurate and reliable. Users can easily upload their datasets (e.g., CSV, Excel, SAV, DAT), perform advanced diagnostics for missing values and outliers, configure key parameters for their analyses (like estimator types, rotation methods), and interpret results through a responsive user interface."),
        br(),

        h5(strong("Key Features and Technical Details")),
        tags$ul(
          tags$li(strong("Unified EFA, CFA & Invariance Workflow:"), " Conduct EFA, CFA, and Measurement Invariance testing within the same environment using a seamless workflow."),
          tags$li(strong("Advanced Missing Data Handling:"), " Analyze missingness patterns, test for MCAR, and apply robust imputation methods such as MICE and missForest (Random Forest)."),
          tags$li(strong("Item Drop Out Analysis:"), " Utilize automated strategies to identify and remove problematic items, optimizing scale length and factor structure."),
          tags$li(strong("EFA Replicability:"), " Randomly split the dataset into two half-samples, run the same EFA configuration on both samples, and compare item-level factor loadings side by side."),
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
        div(
          class = "d-flex flex-wrap gap-2 mt-2 mb-3",
          tags$a(
            href = "https://www.instagram.com/afarukkilic/",
            target = "_blank", rel = "noopener",
            class = "btn btn-sm",
            style = "background: linear-gradient(45deg,#f09433,#e6683c,#dc2743,#cc2366,#bc1888); color:#fff; border:none;",
            tags$i(class = "bi bi-instagram"), " Instagram"
          ),
          tags$a(
            href = "https://www.researchgate.net/profile/Abdullah-Kilic-2",
            target = "_blank", rel = "noopener",
            class = "btn btn-sm",
            style = "background:#00CCBB; color:#fff; border:none;",
            tags$i(class = "bi bi-file-earmark-text"), " ResearchGate"
          ),
          tags$a(
            href = "https://scholar.google.com/citations?user=AP7LlpoAAAAJ&hl=en",
            target = "_blank", rel = "noopener",
            class = "btn btn-sm btn-primary",
            tags$i(class = "bi bi-mortarboard"), " Google Scholar"
          )
        ),

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
        p(strong("What's New in Version 1.1:")),
        tags$ul(
          tags$li(strong("Performance improvements:"),
            " Polychoric correlations cached across KMO, Bartlett, heatmap and report views (3x faster on large datasets). Assumptions results cached across descriptives, collinearity and normality. Variable summary table reduced from seven column scans to a single pass."),
          tags$li(strong("Reliability caching:"),
            " Reliability coefficients are cached by method, item set, and correlation type. Re-running the same calculation returns instantly."),
          tags$li(strong("CFA Higher-Order and Bifactor builders:"),
            " New tabs in the Model Builder for second-order and bifactor models. Bifactor builder generates orthogonality constraints automatically. Templates added for correlated, higher-order and bifactor specifications."),
          tags$li(strong("EFA enhancements:"),
            " Eight extraction methods (added WLS, Minimum Rank, Minimum Chi-Square, GLS); fourteen rotation methods grouped into Oblique and Orthogonal. KMO and Bartlett moved to a compact horizontal card. Heatmap supports five colour palettes including grayscale, with off-diagonal correlation summary."),
          tags$li(strong("EFA Replicability module:"),
            " Added random split-half EFA replication with the same correlation, extraction, factor number, and rotation options used in EFA. Item-level factor loadings are reported in a comparative table with factor congruence and primary factor agreement summaries."),
          tags$li(strong("Factor Retention correlation choice:"),
            " Pearson and Polychoric options added."),
          tags$li(strong("EGA community detection:"),
            " Six algorithms available (Walktrap, Louvain, Leiden, Fast Greedy, Edge Betweenness, Label Propagation). Results redesigned with a tabbed layout."),
          tags$li(strong("Reliability expanded:"),
            " Added McDonald's Omega Hierarchical and Composite Reliability with AVE (CFA-based)."),
          tags$li(strong("Data Selection redesigned:"),
            " New tabs for Data Preview, Variable Types, and Distributions. Per-variable summary table with Type, N_Unique, Missing, Min, Max, Mean and SD. Likert-style and continuous variables visualised separately."),
          tags$li(strong("Exclude Variables module redesigned:"),
            " Dual-panel checkbox layout with real-time variable counters. Selective recovery and full reset supported."),
          tags$li(strong("Item Drop Out / ItemRest module:"),
            " The ItemRest package was used as a foundation and modified for FAfA. A search algorithm was added, and the optimal strategy is presented as a structured card with metric badges and fit indices instead of raw text."),
          tags$li(strong("Imputation iteration controls:"),
            " MICE and missForest iteration counts configurable via sliders."),
          tags$li(strong("CFA path diagram fix for ordinal data:"),
            " Overlapping residual arrows hidden for polychoric/WLSMV analyses (residuals fixed to 1.0 by identification constraints)."),
          tags$li(strong("Modern UI redesign:"),
            " Dark sidebar navigation, Inter typography, gradient card headers, refined buttons and tables, color-coded notifications. Multicollinearity interpretation thresholds (VIF, TOL, CI) shown below the collinearity table."),
          tags$li(strong("Outlier index drift fix:"),
            " Outlier removal snapshots the dataset and refuses if upstream data has changed since detection."),
          tags$li(strong("About page:"),
            " Instagram, ResearchGate, and Google Scholar profile links added as interactive buttons.")
        ),
        br(),
        p(em("Current Version: 1.1"))
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
