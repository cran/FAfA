 
about_server <- function(input, output, session) {

  # Output for the application description
  output$application_description_html <- renderUI({ # Changed to renderUI for direct HTML
    tagList( # Use tagList for structured HTML elements
      h4(strong("FAfA: Factor Analysis for All")), # Heading for the app name
      p(strong("Aim:")),
      p("The FAfA (Factor Analysis for All) Shiny application is a powerful and user-friendly tool designed to simplify both Exploratory Factor Analysis (EFA) and Confirmatory Factor Analysis (CFA) for researchers. Developed with R and Shiny, FAfA aims to unify EFA and CFA workflows within a single, intuitive interface, reducing the need for multiple software tools or complex manual preprocessing steps. It enables users to validate assumptions, perform random dataset splits for cross-validation, conduct reliability analyses, and apply item weighting techniques to potentially enhance construct validity."),
      br(),
      h5(strong("Overview")), # Using h5 for subheadings
      p("FAfA provides a comprehensive suite of tools for factor analysis. It leverages established R packages such as 'psych' for EFA and 'lavaan' for CFA, ensuring that the statistical analyses are accurate and reliable. Users can easily upload their datasets (e.g., CSV, Excel, SAV, DAT), configure key parameters for their analyses (like the number of factors, rotation methods, and estimation methods), and interpret results through a responsive user interface."),
      br(),
      h5(strong("Key Features and Technical Details")),
      tags$ul( # Using an unordered list for features
        tags$li(strong("Unified EFA and CFA Workflow:"), " Conduct EFA and CFA within the same environment. Key R packages include 'psych' and 'lavaan'."),
        tags$li(strong("Assumption Checking:"), " Built-in diagnostics for EFA (e.g., KMO, Bartlett's test) and guidance for CFA (e.g., data screening, sample size considerations). Results are presented clearly."),
        tags$li(strong("Random Dataset Splitting:"), " Supports rigorous validation by allowing users to randomly split datasets (e.g., for EFA on one half, CFA on the other). Implemented using R's sampling functions."),
        tags$li(strong("Reliability Analysis:"), " Includes reliability assessment tools (e.g., Cronbach's alpha) to evaluate the internal consistency of measurement scales."),
        tags$li(strong("Item Weighting (Conceptual):"), " While direct item weighting application might be complex and context-specific, FAfA's structure allows for analysis of datasets that might have undergone such procedures externally, focusing on the factor analytic aspects."),
        tags$li(strong("Interactive and Reproducible Results:"), " Provides an interactive experience with real-time updates. Outputs like factor loadings, scree plots, path diagrams, and fit statistics are rendered using 'ggplot2', 'semPlot', 'DT', etc. Supports exporting results to promote reproducibility.")
      )
    )
  })

  # Output for developer information
  output$developer_info_html <- renderUI({
    tagList(
      p(
        strong("Application Developer:"), br(),
        "Abdullah Faruk KILIC, PhD, Assoc. Prof.", br(),
        "Trakya University, Faculty of Education", br(),
        "Department of Educational Science", br(),
        "Division of Measurement and Evaluation in Education"
      ),
      br(),
      p(strong("Email 1:"), tags$a(href = "mailto:abdullahfarukkilic@gmail.com", "abdullahfarukkilic@gmail.com")),
      p(strong("Email 2:"), tags$a(href = "mailto:afarukkilic@trakya.edu.tr", "afarukkilic@trakya.edu.tr")),
      p(strong("ResearchGate:"), tags$a(href = "https://www.researchgate.net/profile/Abdullah-Kilic-2", target = "_blank", "View ResearchGate Profile")),
      p(strong("Google Scholar:"), tags$a(href = "https://scholar.google.com/citations?user=AP7LlpoAAAAJ&hl=en", target = "_blank", "View Google Scholar Profile")), # Changed hl=tr to hl=en
      br(),
      p(em("Please feel free to contact me regarding any errors or feedback on the application."))
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
        tags$li(strong("Alperen YANDI:"), tags$a(href = "https://www.linkedin.com/in/alperen-yandi-36404891", target = "_blank", "Researcher's Profile")), # Removed ?originalSubdomain=tr
        tags$li(strong("Murat Dogan SAHIN:"), tags$a(href = "https://avesis.anadolu.edu.tr/mdsahin", target = "_blank", "Researcher's Profile"))
      ),
      br(),
      p(strong("What's New in This Version (Conceptual based on previous updates):")),
      tags$ul(
        tags$li("Improved application stability and addressed various bugs."),
        tags$li("Added Measurement Invariance Analysis capabilities."),
        tags$li("Refined CFA fit index reporting and options."),
        tags$li("Enhanced mechanisms for handling missing data consistently across modules."),
        tags$li("Standardized UI language to English and improved user guidance.")
      ),
      br(),
      p(em("Current Version: 0.3"))
    )
  })

#to cite
output$citation_info_html <- renderUI({
  tagList(
    tags$p(
      "If you use this package in your work, please cite it as follows:"
    ),
    tags$br(),
    tags$p(
      style = "font-family: monospace; margin-left: 20px;", 
      "Kilic, A. F. (2024). FAfA: Factor analysis for all: An R package to conduct factor analysis with RShiny application. ",
      tags$em("Journal of Measurement and Evaluation in Education and Psychology, 15"), 
	  HTML("(4), 446&ndash;451. "),
      tags$a(href = "https://doi.org/10.21031/epod.1555805", target = "_blank", "https://doi.org/10.21031/epod.1555805")
    ),
    tags$br(),
    tags$p(
      "BibTeX citation:"
    ),
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


}
