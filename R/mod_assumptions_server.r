#' Assumptions Server Logic (Modern)
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @export
assumptions_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    results_rv <- reactiveValues(desc = NULL, multi = NULL, norm = NULL)

    observeEvent(input$run_descriptives_button, {
      req(data())
      res <- assumptions(data()) # utils.R'deki fonksiyon
      results_rv$desc <- res$descriptives
    })

    observeEvent(input$run_collinearity_button, {
      req(data())
      res <- assumptions(data())
      results_rv$multi <- res$multicollinearity
    })

    observeEvent(input$run_normality_tests_button, {
      req(data())
      res <- assumptions(data())
      # Tabloyu oluştur
      norm_df <- rbind(res$Mardia_Skewness, res$Mardia_Kurtosis)
      # Energy test ekleme mantığı (varsa)
      try({
        en <- energy::mvnorm.test(as.matrix(data()), R=100)
        norm_df <- rbind(norm_df, data.frame(Test="Energy", Statistic=en$statistic, p.value=en$p.value, Result=ifelse(en$p.value>0.05,"Yes","No"), check.names=F))
      }, silent=TRUE)
      results_rv$norm <- norm_df
    })

    output$descriptives_table_output <- renderTable({ results_rv$desc }, rownames=TRUE)
    output$collinearity_table_output <- renderTable({ results_rv$multi })
    output$multivariate_normality_table_output <- renderTable({ results_rv$norm })

    output$download_descriptives_button <- downloadHandler(
      filename="descriptives.csv", content=function(f) write.csv(results_rv$desc, f)
    )
  })
}
