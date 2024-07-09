utils::globalVariables(c("MD_p", "excluded_data_set", "splitted_efa", "splitted_cfa", "ass", "data_without_outlier", "kmo_analysis", "cfa_result_first", "weighted_scores"))

#' The application server-side
#'
#' @param input, output, session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @import golem
#' @import utils
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom stats na.omit
#' @importFrom stats pchisq
#' @importFrom stats mahalanobis
#' @importFrom stats cov
#' @importFrom stats cor
#' @importFrom stats predict
#' @importFrom stats residuals
#' @importFrom stats coef
#' @importFrom stats update
#' @importFrom stats fitted
#' @importFrom stats anova
#' @importFrom dplyr transmute
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom psych fa.parallel
#' @importFrom psych alpha
#' @importFrom psych principal
#' @importFrom psych KMO
#' @importFrom psych cortest.bartlett
#' @importFrom psych fa
#' @importFrom lavaan cfa
#' @importFrom lavaan fitmeasures
#' @importFrom lavaan standardizedsolution
#' @importFrom lavaan modificationindices
#' @importFrom grDevices dev.off
#' @importFrom grDevices jpeg
#' @importFrom grDevices svg
#' @noRd
app_server <- function(input, output, session) {

  load_required_packages <- function() {
    packages <- c(
      "EFA.MRFA", "EFA.dimensions", "EFAtools", "EGAnet", "MBESS",
      "dplyr", "energy", "ggcorrplot", "golem", "lavaan", "mctest",
      "moments", "mvnormalTest", "pastecs", "psych", "psychometric",
      "semPlot", "semTools", "sirt", "stats", "utils", "grDevices"
    )

    lapply(packages, function(pkg) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(paste("Package '", pkg, "' is required but not installed.", sep = ""))
      }
    })
  }

  load_required_packages()


  # Reactive Values for data
  rv <- reactiveValues(
    excluded_data_set = NULL,
    splitted_efa = NULL,
    splitted_cfa = NULL,
    data_without_outlier = NULL,
    weighted_scores = NULL,
    MD_p = NULL,
    ass = NULL,
    kmo_analysis = NULL,
    cfa_result_first= NULL

  )

  # Reading the user uploaded data
  my_data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- utils::read.table(inFile$datapath)
    data
  })

  # Assumption Function
  assumptions <- function(x) {
    # create a data frame for descriptives.
    descr <- as.data.frame(matrix(NA, nrow = ncol(x), ncol = 8))
    colnames(descr) <- c("N", "N (missing)", "Min", "Max", "Median", "Mean", "Skewness", "Kurtosis")

    # create a function to obtain mean values with missing data
    missing_mean <- function(x) {
      mean(x, na.rm = TRUE)
    }

    descriptives <- pastecs::stat.desc(x)
    descr[, c(1, 3, 4)] <- t(descriptives[c(1, 4, 5), ])
    descr[, 2] <- colSums(apply(x, 2, is.na))
    descr[, 5] <- t(descriptives[8, ])
    descr[, 6] <- apply(x, 2, missing_mean)
    descr[, 7] <- moments::skewness(x, na.rm = TRUE)
    descr[, 8] <- moments::kurtosis(x, na.rm = TRUE) - 3

    # Define the model to obtain VIF & TV values
    x <- stats::na.omit(x)
    x_new <- x
    x_new$rn <- 1:nrow(x)
    model_for_collinearity <- stats::lm(as.formula(paste(colnames(x_new)[ncol(x_new)], "~", paste(colnames(x_new)[1:(ncol(x_new) - 1)], collapse = "+"), sep = "")), data = x_new)

    # Calculate VIF & TV values
    mc_VIF_TOL <- as.data.frame(mctest::mctest(model_for_collinearity, type = "i")$idiags[, 1:2])
    mc_CI <- mctest::eigprop(mod = model_for_collinearity)$ci # CI values

    # Obtain multicollinearity statistics
    mc_control <- data.frame(
      VIF_min = min(mc_VIF_TOL$VIF),
      VIF_max = max(mc_VIF_TOL$VIF),
      TOL_min = min(mc_VIF_TOL$TOL),
      TOL_max = max(mc_VIF_TOL$TOL),
      CI_min = min(mc_CI),
      CI_max = max(mc_CI)
    )

    # Mahalanobis Distance
    distance <- as.matrix(stats::mahalanobis(x, colMeans(x), cov = stats::cov(x)))
    Mah_significant <- x %>%
      dplyr::transmute(Row_Number = 1:nrow(x), MD = distance, MD_p = stats::pchisq(distance, df = (ncol(x) - 1), lower.tail = FALSE)) %>%
      dplyr::filter(MD_p <= 0.001)

    # Mardia's kurtosis and skewness
    mardia_kurt <- mvnormalTest::mardia(x)$mv.test[2, ]
    mardia_skew <- mvnormalTest::mardia(x)$mv.test[1, ]

    # Summary for all statistics
    return(list(
      descriptives = round(descr, 2),
      multicollinearity = round(mc_control, 2),
      Mah_significant = Mah_significant,
      n_outlier = nrow(Mah_significant),
      Mardia_Kurtosis = mardia_kurt,
      Mardia_Skewness = mardia_skew
    ))
  }

  # Exclude the variables
  observeEvent(input$exclude_the_variables, {
    variables_defined <- input$excluded_variables
    user_excluded_variables <- as.numeric(unlist(strsplit(variables_defined, ",")))
    rv$excluded_data_set <- my_data()[, -user_excluded_variables]
    showNotification(paste("The variables excluded the data set. Your data set has now ", ncol(rv$excluded_data_set), " variables and ", nrow(rv$excluded_data_set), " individuals.", sep = ""))
  })

  output$excluded_data <- downloadHandler(
    filename = function() {
      paste("excluded_data_set.dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$excluded_data_set, file, row.names = FALSE, col.names = FALSE)
    }
  )

  # Splitting data
  observeEvent(input$split_the_data, {
    selected <- sample.int(n = nrow(my_data()), size = floor(.50 * nrow(my_data())), replace = FALSE)
    rv$splitted_efa <- my_data()[selected, ]
    rv$splitted_cfa <- my_data()[-selected, ]
    showNotification(paste("The data was split. The first data set has ", nrow(rv$splitted_efa), " and the second one has", nrow(rv$splitted_cfa), " individuals.", sep = ""))
  })

  output$split_download_efa <- downloadHandler(
    filename = function() {
      paste("first_half_efa.dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$splitted_efa, file, row.names = FALSE, col.names = FALSE)
    }
  )

  output$split_download_cfa <- downloadHandler(
    filename = function() {
      paste("second_half_cfa.dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$splitted_cfa, file, row.names = FALSE, col.names = FALSE)
    }
  )

  # Outliers
  output$mah <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    ass <- assumptions(my_data())
    ass$Mah_significant
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  output$n_mah <- renderText({
    validate(need(input$file1, 'Please upload your dataset.'))
    ass <- assumptions(my_data())
    ass$n_outlier
  })

  observeEvent(input$remove_outliers, {
    ass <- assumptions(my_data())
    if (nrow(ass$Mah_significant) == 0) {
      rv$data_without_outlier <- my_data()
      showNotification("Your data has zero outliers. We use the full data now.")
    } else {
      rv$data_without_outlier <- my_data()[-ass$Mah_significant$Row_Number, ]
      showNotification(paste("The outliers were removed from the data. Your data has ", nrow(rv$data_without_outlier), " individuals now.", sep = ""))
    }
  })

  output$dl <- downloadHandler(
    filename = function() {
      paste("my_data_without_outliers_", Sys.Date(), ".dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$data_without_outlier, file, row.names = FALSE, col.names = FALSE)
    }
  )

  # Select Data TAB
  output$mydatatable <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    head(my_data(), 10)
  })

  # InfoBoxes
  output$n_var <- renderInfoBox({
    validate(need(input$file1, 'Please upload your dataset.'))
    infoBox(title = "NVar", value = ncol(my_data()), subtitle = "The number of variables", color = "green", fill = FALSE, icon = icon("bolt-lightning"))
  })

  output$n <- renderInfoBox({
    validate(need(input$file1, 'Please upload your dataset.'))
    infoBox(title = "n", value = nrow(my_data()), subtitle = "Sample Size", color = "green", fill = FALSE, icon = icon("people-roof"))
  })

  output$min_value <- renderInfoBox({
    validate(need(input$file1, 'Please upload your dataset.'))
    infoBox(title = "min_value", value = min(my_data()), subtitle = "Minimum value of the variables", icon = icon("arrow-down"), color = "blue", fill = FALSE)
  })

  output$max_value <- renderInfoBox({
    validate(need(input$file1, 'Please upload your dataset.'))
    infoBox(title = "max_value", value = max(my_data()), subtitle = "Maximum value of the variables", icon = icon("arrow-up"), color = "blue", fill = FALSE)
  })

  output$num_cat <- renderInfoBox({
    validate(need(input$file1, 'Please upload your dataset.'))
    infoBox(title = "Categories", value = (max(my_data()) - min(my_data()) + 1), subtitle = "The number of categories", icon = icon("sort"), color = "yellow", fill = FALSE)
  })

  # Assumptions TAB
  output$desc_table <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    ass <- assumptions(my_data())
    ass$descriptives
  }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "400px")

  output$download_desc <- downloadHandler(
    filename = function() {
      paste("decriptive_statistics.csv", sep = "")
    },
    content = function(file) {
      ass <- assumptions(my_data())
      utils::write.csv(ass$descriptives, file)
    }
  )

  output$collinearity <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    ass <- assumptions(my_data())
    ass$multicollinearity
  })

  output$m_normality <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    mardia_normality <- mvnormalTest::mardia(my_data())
    energy_normality <- energy::mvnorm.test(my_data(), R = 100)
    normality_results <- data.frame(matrix(NA, 3, 3, dimnames = list(c("mardia_skewness", "mardia_kurtosis", "energy_test"), c("Statistic", "p_value", "Result"))))

    normality_results[1, 1] <- round(as.numeric(as.character(mardia_normality$mv.test[1, 2])), 2)
    normality_results[1, 2] <- formatC(as.numeric(as.character(mardia_normality$mv.test[1, 3])), digits = 3, format = "f")
    normality_results[1, 3] <- as.character(mardia_normality$mv.test[1, 4])

    normality_results[2, 1] <- round(as.numeric(as.character(mardia_normality$mv.test[2, 2])), 2)
    normality_results[2, 2] <- formatC(as.numeric(as.character(mardia_normality$mv.test[2, 3])), digits = 3, format = "f")
    normality_results[2, 3] <- as.character(mardia_normality$mv.test[2, 4])

    normality_results[3, ] <- data.frame(Statistic = round(energy_normality$statistic, 2), p_value = formatC(energy_normality$p.value, digits = 3, format = "f"), Result = ifelse(energy_normality$p.value >= 0.05, "YES", "NO"))

    normality_results
  }, rownames = TRUE)

  # Exploratory Graph Analysis TAB EGA
  output$ega_network <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    ega_results <- EGAnet::EGA.estimate(data = my_data(), model = input$est_method_ega)
    ega_results$network
  }, rownames = TRUE)

  output$ega_network_results <- downloadHandler(
    filename = function() {
      paste("ega_network_results.csv", sep = "")
    },
    content = function(file) {
      ega_results <- EGAnet::EGA.estimate(data = my_data(), model = input$est_method_ega)
      utils::write.csv(ega_results$network, file, row.names = TRUE, col.names = TRUE)
    }
  )

  output$ega_network_plot <- renderPlot({
    validate(need(input$file1, 'Please upload your dataset.'))
    plot_ega <- EGAnet::EGA(data = my_data(), model = input$est_method_ega, plot.EGA = TRUE, plot.type = "qgraph")
    plot_ega$Plot.EGA
  })

  output$ega_network_download <- downloadHandler(
    filename = function() {
      paste("ega_diagram.svg")
    },
    content = function(file) {
      svg(file)
      EGAnet::EGA(data = my_data(), model = input$est_method_ega, plot.EGA = TRUE, plot.type = "qgraph")
      dev.off()
    }
  )

  # Exploratory Factor Analysis TAB
  output$scree_plot <- renderPlot({
    validate(need(input$file1, 'Please upload your dataset.'))
    scree_plot <- EFAtools::SCREE(my_data())
    plot(scree_plot)
  })

  output$dim_ret_results <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    factor_ret <- function(x, method) {
      if (method == "pa_mrfa") {
        op_pa_analysis <- EFA.MRFA::parallelMRFA(x)
        op_pa_result <- op_pa_analysis$N_factors_percentiles
        return(data.frame(optimal_pa = op_pa_result, row.names = "Suggested Factors"))
      } else if (method == "pa_traditional") {
        tra_pa_analysis <- psych::fa.parallel(x, fa = "pc")
        tra_pa_result <- tra_pa_analysis$ncomp
        return(data.frame(traditional_pa = tra_pa_result, row.names = "Suggested Factors"))
      } else if (method == "hull_method") {
        hull_analysis <- EFA.MRFA::hullEFA(x, display = FALSE)
        hull_result <- hull_analysis$n_factors
        return(data.frame(hull = hull_result, row.names = "Suggested Factors"))
      } else if (method == "map_method_tra") {
        map_analysis <- EFA.dimensions::MAP(x)
        tra_map_result <- map_analysis$NfactorsMAP
        return(data.frame(traditional_map = tra_map_result, row.names = "Suggested Factors"))
      } else if (method == "map_method_rev") {
        map_analysis <- EFA.dimensions::MAP(x)
        rev_map_result <- map_analysis$NfactorsMAP4
        return(data.frame(revised_map = rev_map_result, row.names = "Suggested Factors"))
      } else if (method == "EGA_tmfg") {
        ega_analysis_tmfg <- EGAnet::EGA(data = x, model = "TMFG")
        ega_tmfg_result <- ega_analysis_tmfg$n.dim
        return(data.frame(EGA_TMFG = ega_tmfg_result, row.names = "Suggested Factors"))
      } else if (method == "EGA_glasso") {
        ega_analysis_glasso <- EGAnet::EGA(data = x, model = "glasso")
        ega_glasso_result <- ega_analysis_glasso$n.dim
        return(data.frame(EGA_Glasso = ega_glasso_result, row.names = "Suggested Factors"))
      } else if (method == "EK_C") {
        emkpc_analysis <- EFA.dimensions::EMPKC(x)
        emkpc_result <- emkpc_analysis$NfactorsEMPKC
        return(data.frame(Emprical_Kaiser = emkpc_result, row.names = "Suggested Factors"))
      } else if (method == "comp_data_method") {
        cd_analysis <- EFAtools::CD(x)
        cd_result <- cd_analysis$n_factors
        return(data.frame(Comparison_Data = cd_result, row.names = "Suggested Factors"))
      } else if (method == "scree_method") {
        EFAtools::SCREE(x)
      }
    }

    factor_ret(x = my_data(), method = input$dimension_methods)
  }, rownames = TRUE)

  output$efa_result_str <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    efa_of_data_set <- psych::fa(r = my_data(), nfactors = input$number_factor, rotate = input$rotating_method, fm = input$fact_method, cor = ifelse(input$cor_kind == "pea", "cor", "poly"))
    str_result <- data.frame(efa_of_data_set$Structure[, 1:input$number_factor])
    colnames(str_result) <- colnames(efa_of_data_set$Structure)
    str_result
  }, rownames = TRUE)

  output$download_efa_loadings <- downloadHandler(
    filename = function() {
      paste("efa_factor_loadings.csv", sep = "")
    },
    content = function(file) {
      efa_of_data_set <- psych::fa(r = my_data(), nfactors = input$number_factor, rotate = input$rotating_method, fm = input$fact_method, cor = ifelse(input$cor_kind == "pea", "cor", "poly"))
      str_result <- data.frame(efa_of_data_set$Structure[, 1:input$number_factor])
      colnames(str_result) <- colnames(efa_of_data_set$Structure)
      utils::write.csv(str_result, file, row.names = TRUE, col.names = FALSE)
    }
  )

  output$efa_result_interf_cor <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    if (input$number_factor == 1) {
      print("Owing to the unidimensional structure, there is no interfactor correlation.")
    } else {
      efa_of_data_set <- psych::fa(r = my_data(), nfactors = input$number_factor, rotate = input$rotating_method, fm = input$fact_method, cor = ifelse(input$cor_kind == "pea", "cor", "poly"))
      int_fact_result <- data.frame(as.matrix(efa_of_data_set$score.cor))
      rownames(int_fact_result) <- paste("f", seq(1:input$number_factor), sep = "")
      colnames(int_fact_result) <- paste("f", seq(1:input$number_factor), sep = "")
      int_fact_result
    }
  }, rownames = TRUE)

  output$efa_result_expl_var <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    efa_of_data_set <- psych::fa(r = my_data(), nfactors = input$number_factor, rotate = input$rotating_method, fm = input$fact_method, cor = ifelse(input$cor_kind == "pea", "cor", "poly"))
    exp_var_result <- data.frame(matrix(as.numeric(efa_of_data_set$Vaccounted), ncol = input$number_factor, nrow = 5, byrow = FALSE), row.names = c("SS loadings", "Proportion Var", "Cumulative Var", "Proportion Explained", "Cumulative Proportion"))
    colnames(exp_var_result) <- colnames(efa_of_data_set$Vaccounted)
    exp_var_result
  }, rownames = TRUE)

  output$kmo_result <- renderText({
    validate(need(input$file1, 'Please upload your dataset.'))
    kmo_analysis <- psych::KMO(my_data())
    kmo_results <- data.frame(KMO = kmo_analysis$MSA)
    kmo_interpret <- dplyr::case_when(
      kmo_analysis$MSA < 0.49 ~ "Unacceptable",
      kmo_analysis$MSA < 0.59 ~ "Miserable",
      kmo_analysis$MSA < 0.69 ~ "Mediocre",
      kmo_analysis$MSA < 0.79 ~ "Middling",
      kmo_analysis$MSA < 0.89 ~ "Meritorious",
      kmo_analysis$MSA < 1.00 ~ "Marvelous"
    )
    paste("<b>Your KMO value =", round(kmo_analysis$MSA, 3), "(", kmo_interpret, ")</b>", sep = "")
  })

  output$bartlett <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    bartlett_test <- if (input$cor_kind == "pea") {
      psych::cortest.bartlett(cor(my_data()), n = nrow(my_data()))
    } else {
      psych::cortest.bartlett(psych::polychoric(my_data())$rho, n = nrow(my_data()))
    }
    bartlett_result <- data.frame(matrix(c(bartlett_test$chisq, bartlett_test$df, bartlett_test$p.value), byrow = TRUE))
    rownames(bartlett_result) <- c("Chi Square", "Degrees of Freedom", "p value")
    colnames(bartlett_result) <- "Value"
    bartlett_result
  }, rownames = TRUE)

  output$robust_msa <- renderTable({
    validate(need(input$file1, 'Please upload your dataset.'))
    kmo_analysis <- psych::KMO(my_data())
    msa_results <- data.frame(MSA = kmo_analysis$MSAi)
    interpret_msa <- ifelse(kmo_analysis$MSAi < 0.50, "Remove", "Good")
    msa_results$Interpretation <- interpret_msa
    rownames(msa_results) <- paste("Item", seq(1:ncol(my_data())), sep = "_")
    msa_results
  }, rownames = TRUE)

  output$heat_map <- renderPlot({
    validate(need(input$file1, 'Please upload your dataset.'))
    if (ncol(my_data()) < 21) {
      cor_plot <- if (input$cor_kind == "pea") {
        ggcorrplot::ggcorrplot(corr = cor(my_data()), hc.order = TRUE, type = "lower", lab = TRUE)
      } else {
        ggcorrplot::ggcorrplot(corr = psych::polychoric(my_data())$rho, hc.order = TRUE, type = "lower", lab = TRUE)
      }
    } else {
      cor_plot <- if (input$cor_kind == "pea") {
        ggcorrplot::ggcorrplot(corr = cor(my_data()), hc.order = TRUE, method = "circle")
      } else {
        ggcorrplot::ggcorrplot(corr = psych::polychoric(my_data())$rho, hc.order = TRUE, method = "circle")
      }
    }
    plot(cor_plot)
  })

  # Confirmatory Factor Analysis TAB
  observeEvent(input$define_structure, {
    model <- input$cfa_define

    output$cfa_results <- renderTable({
      validate(need(input$file1, 'Please upload your dataset.'))
      cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
      fit_measures <- data.frame(
        "Chi_Sqaure" = lavaan::fitmeasures(cfa_result_first)[c("chisq")],
        "df" = lavaan::fitmeasures(cfa_result_first)[c("df")],
        "Chi/df" = lavaan::fitmeasures(cfa_result_first)[c("chisq")] / lavaan::fitmeasures(cfa_result_first)[c("df")],
        "p_value" = lavaan::fitmeasures(cfa_result_first)[c("pvalue")],
        "CFI" = lavaan::fitmeasures(cfa_result_first)[c("cfi")],
        "TLI" = lavaan::fitmeasures(cfa_result_first)[c("tli")],
        "RFI" = lavaan::fitmeasures(cfa_result_first)[c("rfi")],
        "SRMR" = lavaan::fitmeasures(cfa_result_first)[c("srmr")],
        "RMSEA" = lavaan::fitmeasures(cfa_result_first)[c("rmsea")],
        "RMSEA_ci_lower" = lavaan::fitmeasures(cfa_result_first)[c("rmsea.ci.lower")],
        "RMSEA_ci_upper" = lavaan::fitmeasures(cfa_result_first)[c("rmsea.ci.upper")]
      )
      fit_results <- as.data.frame(t(as.matrix(fit_measures)))
      colnames(fit_results) <- c("Value")
      fit_results
    }, rownames = TRUE)

    output$download_fits <- downloadHandler(
      filename = function() {
        paste("fit_results.csv", sep = "")
      },
      content = function(file) {
        colnames(fit_results) <- c("Value")
        utils::write.csv(fit_results, file, row.names = TRUE, col.names = FALSE)
      }
    )

    output$cfa_factor_loadings <- renderTable({
      validate(need(input$file1, 'Please upload your dataset.'))
      cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
      lv_std <- lavaan::standardizedsolution(cfa_result_first)
      lv_results <- lv_std[1:ncol(my_data()), -c(1, 2)]
      colnames(lv_results) <- c("Variable", "factor_loading", "std_error", "fl/std_error", "p_value", "ci.lower", "ci.upper")
      lv_results
    }, rownames = TRUE)

    output$download_cfa_loadings <- downloadHandler(
      filename = function() {
        paste("cfa_factor_loadings.csv", sep = "")
      },
      content = function(file) {
        validate(need(input$file1, 'Please upload your dataset.'))
        cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
        lv_std <- lavaan::standardizedsolution(cfa_result_first)
        lv_results <- lv_std[1:ncol(my_data()), -c(1, 2)]
        colnames(lv_results) <- c("Variable", "factor_loading", "std_error", "fl/std_error", "p_value", "ci.lower", "ci.upper")
        utils::write.csv(lv_results, file, row.names = TRUE, col.names = TRUE)
      }
    )

    output$cfa_modification <- renderTable({
      validate(need(input$file1, 'Please upload your dataset.'))
      cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
      lv_mod <- lavaan::modificationindices(cfa_result_first, sort. = TRUE, power = TRUE, maximum.number = 20)
      lv_mod
    }, rownames = TRUE)

    output$path_diagram <- renderPlot({
      validate(need(input$file1, 'Please upload your dataset.'))
      cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
      semPlot::semPaths(object = cfa_result_first, what = "std", intercepts = FALSE, layout = "spring", edge.label.cex = 1.2, sizeMan = 5, sizeLat = 8, edge.color = "black", thresholds = FALSE, esize = 2)
    })

    output$save_diagram <- downloadHandler(
      filename = function() {
        paste("path_diagram.svg")
      },
      content = function(file) {
        svg(file)
        validate(need(input$file1, 'Please upload your dataset.'))
        cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
        semPlot::semPaths(object = cfa_result_first, what = "std", intercepts = FALSE, layout = "spring", edge.label.cex = 1.2, sizeMan = 5, sizeLat = 8, edge.color = "black", thresholds = FALSE, esize = 2)
        dev.off()
      }
    )
  })

  # Reliability Analysis TAB
  output$reliability_result <- renderText({
    validate(need(input$file1, 'Please upload your dataset.'))
    reliability_func <- function(x, method) {
      if (method == "alpha") {
        alpha_analysis <- psych::alpha(x)
        return(sprintf("%.3f", as.numeric(alpha_analysis$total[1])))
      } else if (method == "omega") {
        omega_analysis <- MBESS::ci.reliability(x)
        return(sprintf("%.3f", omega_analysis$est))
      } else if (method == "theta") {
        armor_theta <- function(x, method = "PCA", correlation_type = c("cor", "poly")) {
          number_item = ncol(x)
          if (method == "PCA") {
            pca_result = psych::principal(x, cor = correlation_type)
            eigen_values = pca_result$Vaccounted[1, 1]
            armor_theta_value = (number_item / (number_item - 1)) * (1 - 1 / eigen_values)
          } else {
            efa_result = psych::fa(x, fm = method, cor = correlation_type)
            eigen_values = efa_result$Vaccounted[1, 1]
            armor_theta_value = (number_item / (number_item - 1)) * (1 - 1 / eigen_values)
          }
          return(data.frame(armor_theta = armor_theta_value))
        }
        armor_result <- armor_theta(x, method = "PCA", correlation_type = input$cor_kind)
        return(sprintf("%.3f", as.numeric(armor_result$armor_theta)))
      } else if (method == "structure") {
        model <- input$defined_structure
        model_fit <- lavaan::cfa(model = model, data = my_data())
        structural_reliability <- semTools::reliability(model_fit)
        return(sprintf("%.3f", structural_reliability[5, 1]))
      } else if (method == "s_alpha") {
        user_strata <- input$strata_define
        user_strata_definition <- as.numeric(unlist(strsplit(user_strata, ",")))
        stratas <- cbind(colnames(my_data()), user_strata_definition)
        result_s_alpha <- sirt::stratified.cronbach.alpha(my_data(), itemstrata = stratas)
        return(sprintf("%.3f", result_s_alpha$alpha.stratified[1]))
      }
    }

    user_strata <- input$strata_define
    user_strata_definition <- as.numeric(unlist(strsplit(user_strata, ",")))
    if (input$reliability_coeff == "s_alpha" & length(user_strata_definition) != ncol(my_data())) {
      paste("Please define all of the variables correctly. You defined ", length(user_strata_definition), " variables but your dataset has ", ncol(my_data()), " variables.", sep = "")
    } else {
      result_rel <- reliability_func(my_data(), method = input$reliability_coeff)
      paste("The ", input$reliability_coeff, " is ", result_rel, sep = "")
    }
  })

  # Item Weighting TAB
  observeEvent(input$weight_item, {
    output$weighted_scores <- renderTable({
      validate(need(input$file1, 'Please upload your dataset.'))
      item_weighting <- function(x) {
        item_stats <- psychometric::item.exam(x = x, discrim = TRUE)
        item_diff <- item_stats$Difficulty / max(x)
        ind_averages <- rowSums(x) / (ncol(x) * max(x))
        new_data <- data.frame(matrix(NA, nrow = nrow(x), ncol = ncol(x)))
        colnames(new_data) <- paste("V", seq(1:ncol(x)), sep = "")
        weighted_data <- data.frame(matrix(NA, nrow = nrow(x), ncol = ncol(x)))
        colnames(weighted_data) <- paste("V", seq(1:ncol(x)), sep = "")
        for (i in 1:ncol(x)) {
          for (t in 1:nrow(x)) {
            new_data[t, i] = ind_averages[t] + item_diff[i]
          }
        }
        for (i in 1:ncol(x)) {
          for (t in 1:nrow(x)) {
            if (new_data[t, i] >= 1) {
              weighted_data[t, i] = x[t, i] + item_stats$Item.Rel.woi[i]
            } else {
              weighted_data[t, i] = x[t, i]
            }
          }
        }
        return(weighted_data)
      }
      rv$weighted_scores <- item_weighting(my_data())
      head(rv$weighted_scores, n = 10)
    }, rownames = TRUE)
  })

  output$download_weighted_scores <- downloadHandler(
    filename = function() {
      paste("Weighted_scores.dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$weighted_scores, file, row.names = FALSE, col.names = FALSE)
    }
  )

  # About TAB
  output$about <- renderText({
    paste(
      p(strong("This application's name is Factor Analysis for All (FAfA)")),
      p(strong("Aim of the App:"),
        "This app has been prepared for researchers to use while performing exploratory and confirmatory factor analysis.
       Examination of the EFA and CFA assumptions allows the dataset to be divided into two randomly,
       EFA in one part and CFA in the other part, and reliability analysis done through a single program.
       In addition, with the help of this app, in which the item weighting method is integrated,
       item weighting can be done to examine whether the construct validity has improved or not.")
    )
  })

  output$developer <- renderText({
    paste(
      p(strong("About App Developer:"), br(),
        "Abdullah Faruk KILIC, PhD, Assoc. Prof.", br(),
        "Trakya University, Faculty of Education", br(),
        "Department of Educational Science", br(),
        "Division of Measurement and Evaluation in Education"), br(),
      p(strong("e-mail-1:"), tags$a(href = "mailto:abdullahfarukkilic@gmail.com", "abdullahfarukkilic@gmail.com")),
      p(strong("e-mail-2:"), tags$a(href = "mailto:afarukkilic@trakya.edu.tr", "afarukkilic@trakya.edu.tr")),
      p(strong("RG:"), tags$a(href = "https://www.researchgate.net/profile/Abdullah-Kilic-2", "Click Here for Research Gate Profile!")),
      p(strong("Scholar:"), tags$a(href = "https://scholar.google.com/citations?user=AP7LlpoAAAAJ&hl=tr", "Click Here for Google Schoolar Profile!")),
      p(strong("Please contact me for all errors encountered in the application."))
    )
  })

  output$contributer <- renderText({
    paste(
      p(strong("I would like to thank the researchers whose names are written below for their feedback on the first version of this application.")), br(),
      p(strong("Seda Nur SAKAR:"), tags$a(href = "https://avesis.hacettepe.edu.tr/sedasakar", "Click Here for Researcher's Profile!")),
      p(strong("Tugay KACAK:"), tags$a(href = "https://personel.trakya.edu.tr/tugaykacak/", "Click Here for Researcher's Profile!")),
      p(strong("Basak ERDEM KARA:"), tags$a(href = "https://avesis.anadolu.edu.tr/basakerdem", "Click Here for Researcher's Profile!")),
      p(strong("Meltem ACAR GUVENDIR"), tags$a(href = "https://personel.trakya.edu.tr/meltemacar/", "Click Here for Researcher's Profile!")),
      p(strong("Alperen YANDI"), tags$a(href = "https://www.linkedin.com/in/alperen-yandi-36404891?originalSubdomain=tr", "Click Here for Researcher's Profile!")),
      p(strong("Murat Dogan SAHIN"), tags$a(href = "https://avesis.anadolu.edu.tr/mdsahin", "Click Here for Researcher's Profile!")), br(),

      br(),
      strong("New in this version:", br(),
             "Some bugs were fixed", br(),
             "EGA analysis was added", br(),
             "Added some information in the analysis sections.", br(),
             "Reporting of CFA factor loadings has been improved.", br(),
             "Added modification suggestions by sorted order.", br()),
      em("Version: 0.2")
    )
  })
}




################################TURKISH VERSION OF SERVER################################################
app_server_tr <- function(input, output, session) {

  load_required_packages <- function() {
    packages <- c(
      "EFA.MRFA", "EFA.dimensions", "EFAtools", "EGAnet", "MBESS",
      "dplyr", "energy", "ggcorrplot", "golem", "lavaan", "mctest",
      "moments", "mvnormalTest", "pastecs", "psych", "psychometric",
      "semPlot", "semTools", "sirt", "stats", "utils", "grDevices"
    )

    lapply(packages, function(pkg) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(paste(pkg, "'paketi gerekli ancak yuklu degil.", sep = ""))
      }
    })
  }

  load_required_packages()


  # Reactive Values for data
  rv <- reactiveValues(
    excluded_data_set = NULL,
    splitted_efa = NULL,
    splitted_cfa = NULL,
    data_without_outlier = NULL,
    weighted_scores = NULL,
    MD_p = NULL,
    ass = NULL,
    kmo_analysis = NULL,
    cfa_result_first= NULL

  )

  # Reading the user uploaded data
  my_data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- utils::read.table(inFile$datapath)
    data
  })

  # Assumption Function
  assumptions <- function(x) {
    # create a data frame for descriptives.
    descr <- as.data.frame(matrix(NA, nrow = ncol(x), ncol = 8))
    colnames(descr) <- c("N", "N (missing)", "Min", "Max", "Median", "Mean", "Skewness", "Kurtosis")

    # create a function to obtain mean values with missing data
    missing_mean <- function(x) {
      mean(x, na.rm = TRUE)
    }

    descriptives <- pastecs::stat.desc(x)
    descr[, c(1, 3, 4)] <- t(descriptives[c(1, 4, 5), ])
    descr[, 2] <- colSums(apply(x, 2, is.na))
    descr[, 5] <- t(descriptives[8, ])
    descr[, 6] <- apply(x, 2, missing_mean)
    descr[, 7] <- moments::skewness(x, na.rm = TRUE)
    descr[, 8] <- moments::kurtosis(x, na.rm = TRUE) - 3

    # Define the model to obtain VIF & TV values
    x <- stats::na.omit(x)
    x_new <- x
    x_new$rn <- 1:nrow(x)
    model_for_collinearity <- stats::lm(as.formula(paste(colnames(x_new)[ncol(x_new)], "~", paste(colnames(x_new)[1:(ncol(x_new) - 1)], collapse = "+"), sep = "")), data = x_new)

    # Calculate VIF & TV values
    mc_VIF_TOL <- as.data.frame(mctest::mctest(model_for_collinearity, type = "i")$idiags[, 1:2])
    mc_CI <- mctest::eigprop(mod = model_for_collinearity)$ci # CI values

    # Obtain multicollinearity statistics
    mc_control <- data.frame(
      VIF_min = min(mc_VIF_TOL$VIF),
      VIF_max = max(mc_VIF_TOL$VIF),
      TOL_min = min(mc_VIF_TOL$TOL),
      TOL_max = max(mc_VIF_TOL$TOL),
      CI_min = min(mc_CI),
      CI_max = max(mc_CI)
    )

    # Mahalanobis Distance
    distance <- as.matrix(stats::mahalanobis(x, colMeans(x), cov = stats::cov(x)))
    Mah_significant <- x %>%
      dplyr::transmute(Row_Number = 1:nrow(x), MD = distance, MD_p = stats::pchisq(distance, df = (ncol(x) - 1), lower.tail = FALSE)) %>%
      dplyr::filter(MD_p <= 0.001)

    # Mardia's kurtosis and skewness
    mardia_kurt <- mvnormalTest::mardia(x)$mv.test[2, ]
    mardia_skew <- mvnormalTest::mardia(x)$mv.test[1, ]

    # Summary for all statistics
    return(list(
      descriptives = round(descr, 2),
      multicollinearity = round(mc_control, 2),
      Mah_significant = Mah_significant,
      n_outlier = nrow(Mah_significant),
      Mardia_Kurtosis = mardia_kurt,
      Mardia_Skewness = mardia_skew
    ))
  }

  # Exclude the variables
  observeEvent(input$exclude_the_variables, {
    variables_defined <- input$excluded_variables
    user_excluded_variables <- as.numeric(unlist(strsplit(variables_defined, ",")))
    rv$excluded_data_set <- my_data()[, -user_excluded_variables]
    showNotification(paste("Degiskenler veri setinden cikarilacaktir. Veri setinde su anda ", ncol(rv$excluded_data_set), " degisken ve ", nrow(rv$excluded_data_set), " birey yer almaktadir.", sep = ""))
  })

  output$excluded_data <- downloadHandler(
    filename = function() {
      paste("excluded_data_set.dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$excluded_data_set, file, row.names = FALSE, col.names = FALSE)
    }
  )

  # Splitting data
  observeEvent(input$split_the_data, {
    selected <- sample.int(n = nrow(my_data()), size = floor(.50 * nrow(my_data())), replace = FALSE)
    rv$splitted_efa <- my_data()[selected, ]
    rv$splitted_cfa <- my_data()[-selected, ]
    showNotification(paste("Veri seti ikiye bolundu. ilk veri setindeki katilimci sayisi ", nrow(rv$splitted_efa), " iken ikinci veri setinde ", nrow(rv$splitted_cfa), " birey yer almaktadir.", sep = ""))
  })

  output$split_download_efa <- downloadHandler(
    filename = function() {
      paste("first_half_efa.dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$splitted_efa, file, row.names = FALSE, col.names = FALSE)
    }
  )

  output$split_download_cfa <- downloadHandler(
    filename = function() {
      paste("second_half_cfa.dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$splitted_cfa, file, row.names = FALSE, col.names = FALSE)
    }
  )

  # Outliers
  output$mah <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    ass <- assumptions(my_data())
    ass$Mah_significant
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  output$n_mah <- renderText({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    ass <- assumptions(my_data())
    ass$n_outlier
  })

  observeEvent(input$remove_outliers, {
    ass <- assumptions(my_data())
    if (nrow(ass$Mah_significant) == 0) {
      rv$data_without_outlier <- my_data()
      showNotification("Veri setinde uc deger yoktur. Bu nedenle tum veri setini kullandik.")
    } else {
      rv$data_without_outlier <- my_data()[-ass$Mah_significant$Row_Number, ]
      showNotification(paste("Uc degerler veri setinden cikarilmistir. Son durumda veri setinizde,", nrow(rv$data_without_outlier), " birey yer almaktadir.", sep = ""))
    }
  })

  output$dl <- downloadHandler(
    filename = function() {
      paste("my_data_without_outliers_", Sys.Date(), ".dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$data_without_outlier, file, row.names = FALSE, col.names = FALSE)
    }
  )

  # Select Data TAB
  output$mydatatable <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    head(my_data(), 10)
  })

  # InfoBoxes
  output$n_var <- renderInfoBox({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    infoBox(title = "NDeg", value = ncol(my_data()), subtitle = "Degisken Sayisi", color = "green", fill = FALSE, icon = icon("bolt-lightning"))
  })

  output$n <- renderInfoBox({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    infoBox(title = "n", value = nrow(my_data()), subtitle = "Orneklem Buyuklugu", color = "green", fill = FALSE, icon = icon("people-roof"))
  })

  output$min_value <- renderInfoBox({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    infoBox(title = "en_kucuk", value = min(my_data()), subtitle = "Degiskenin aldigi en kucuk deger", icon = icon("arrow-down"), color = "blue", fill = FALSE)
  })

  output$max_value <- renderInfoBox({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    infoBox(title = "en_buyuk", value = max(my_data()), subtitle = "Degiskenin aldigi en buyuk deger", icon = icon("arrow-up"), color = "blue", fill = FALSE)
  })

  output$num_cat <- renderInfoBox({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    infoBox(title = "Kategori", value = (max(my_data()) - min(my_data()) + 1), subtitle = "Kategori Sayisi", icon = icon("sort"), color = "yellow", fill = FALSE)
  })

  # Assumptions TAB
  output$desc_table <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    ass <- assumptions(my_data())
    ass$descriptives
  }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "400px")

  output$download_desc <- downloadHandler(
    filename = function() {
      paste("decriptive_statistics.csv", sep = "")
    },
    content = function(file) {
      ass <- assumptions(my_data())
      utils::write.csv(ass$descriptives, file)
    }
  )

  output$collinearity <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    ass <- assumptions(my_data())
    ass$multicollinearity
  })

  output$m_normality <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    mardia_normality <- mvnormalTest::mardia(my_data())
    energy_normality <- energy::mvnorm.test(my_data(), R = 100)
    normality_results <- data.frame(matrix(NA, 3, 3, dimnames = list(c("mardia_skewness", "mardia_kurtosis", "energy_test"), c("Statistic", "p_value", "Result"))))

    normality_results[1, 1] <- round(as.numeric(as.character(mardia_normality$mv.test[1, 2])), 2)
    normality_results[1, 2] <- formatC(as.numeric(as.character(mardia_normality$mv.test[1, 3])), digits = 3, format = "f")
    normality_results[1, 3] <- as.character(mardia_normality$mv.test[1, 4])

    normality_results[2, 1] <- round(as.numeric(as.character(mardia_normality$mv.test[2, 2])), 2)
    normality_results[2, 2] <- formatC(as.numeric(as.character(mardia_normality$mv.test[2, 3])), digits = 3, format = "f")
    normality_results[2, 3] <- as.character(mardia_normality$mv.test[2, 4])

    normality_results[3, ] <- data.frame(Statistic = round(energy_normality$statistic, 2), p_value = formatC(energy_normality$p.value, digits = 3, format = "f"), Result = ifelse(energy_normality$p.value >= 0.05, "YES", "NO"))

    normality_results
  }, rownames = TRUE)

  # Exploratory Graph Analysis TAB EGA
  output$ega_network <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    ega_results <- EGAnet::EGA.estimate(data = my_data(), model = input$est_method_ega)
    ega_results$network
  }, rownames = TRUE)

  output$ega_network_results <- downloadHandler(
    filename = function() {
      paste("ega_network_results.csv", sep = "")
    },
    content = function(file) {
      ega_results <- EGAnet::EGA.estimate(data = my_data(), model = input$est_method_ega)
      utils::write.csv(ega_results$network, file, row.names = TRUE, col.names = TRUE)
    }
  )

  output$ega_network_plot <- renderPlot({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    plot_ega <- EGAnet::EGA(data = my_data(), model = input$est_method_ega, plot.EGA = TRUE, plot.type = "qgraph")
    plot_ega$Plot.EGA
  })

  output$ega_network_download <- downloadHandler(
    filename = function() {
      paste("ega_diagram.svg")
    },
    content = function(file) {
      svg(file)
      EGAnet::EGA(data = my_data(), model = input$est_method_ega, plot.EGA = TRUE, plot.type = "qgraph")
      dev.off()
    }
  )

  # Exploratory Factor Analysis TAB
  output$scree_plot <- renderPlot({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    scree_plot <- EFAtools::SCREE(my_data())
    plot(scree_plot)
  })

  output$dim_ret_results <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    factor_ret <- function(x, method) {
      if (method == "pa_mrfa") {
        op_pa_analysis <- EFA.MRFA::parallelMRFA(x)
        op_pa_result <- op_pa_analysis$N_factors_percentiles
        return(data.frame(optimal_pa = op_pa_result, row.names = "Onerilen Boyut Sayisi"))
      } else if (method == "pa_traditional") {
        tra_pa_analysis <- psych::fa.parallel(x, fa = "pc")
        tra_pa_result <- tra_pa_analysis$ncomp
        return(data.frame(traditional_pa = tra_pa_result, row.names = "Onerilen Boyut Sayisi"))
      } else if (method == "hull_method") {
        hull_analysis <- EFA.MRFA::hullEFA(x, display = FALSE)
        hull_result <- hull_analysis$n_factors
        return(data.frame(hull = hull_result, row.names = "Onerilen Boyut Sayisi"))
      } else if (method == "map_method_tra") {
        map_analysis <- EFA.dimensions::MAP(x)
        tra_map_result <- map_analysis$NfactorsMAP
        return(data.frame(traditional_map = tra_map_result, row.names = "Onerilen Boyut Sayisi"))
      } else if (method == "map_method_rev") {
        map_analysis <- EFA.dimensions::MAP(x)
        rev_map_result <- map_analysis$NfactorsMAP4
        return(data.frame(revised_map = rev_map_result, row.names = "Onerilen Boyut Sayisi"))
      } else if (method == "EGA_tmfg") {
        ega_analysis_tmfg <- EGAnet::EGA(data = x, model = "TMFG")
        ega_tmfg_result <- ega_analysis_tmfg$n.dim
        return(data.frame(EGA_TMFG = ega_tmfg_result, row.names = "Onerilen Boyut Sayisi"))
      } else if (method == "EGA_glasso") {
        ega_analysis_glasso <- EGAnet::EGA(data = x, model = "glasso")
        ega_glasso_result <- ega_analysis_glasso$n.dim
        return(data.frame(EGA_Glasso = ega_glasso_result, row.names = "Onerilen Boyut Sayisi"))
      } else if (method == "EK_C") {
        emkpc_analysis <- EFA.dimensions::EMPKC(x)
        emkpc_result <- emkpc_analysis$NfactorsEMPKC
        return(data.frame(Emprical_Kaiser = emkpc_result, row.names = "Onerilen Boyut Sayisi"))
      } else if (method == "comp_data_method") {
        cd_analysis <- EFAtools::CD(x)
        cd_result <- cd_analysis$n_factors
        return(data.frame(Comparison_Data = cd_result, row.names = "Onerilen Boyut Sayisi"))
      } else if (method == "scree_method") {
        EFAtools::SCREE(x)
      }
    }

    factor_ret(x = my_data(), method = input$dimension_methods)
  }, rownames = TRUE)

  output$efa_result_str <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    efa_of_data_set <- psych::fa(r = my_data(), nfactors = input$number_factor, rotate = input$rotating_method, fm = input$fact_method, cor = ifelse(input$cor_kind == "pea", "cor", "poly"))
    str_result <- data.frame(efa_of_data_set$Structure[, 1:input$number_factor])
    colnames(str_result) <- colnames(efa_of_data_set$Structure)
    str_result
  }, rownames = TRUE)

  output$download_efa_loadings <- downloadHandler(
    filename = function() {
      paste("efa_factor_loadings.csv", sep = "")
    },
    content = function(file) {
      efa_of_data_set <- psych::fa(r = my_data(), nfactors = input$number_factor, rotate = input$rotating_method, fm = input$fact_method, cor = ifelse(input$cor_kind == "pea", "cor", "poly"))
      str_result <- data.frame(efa_of_data_set$Structure[, 1:input$number_factor])
      colnames(str_result) <- colnames(efa_of_data_set$Structure)
      utils::write.csv(str_result, file, row.names = TRUE, col.names = FALSE)
    }
  )

  output$efa_result_interf_cor <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    if (input$number_factor == 1) {
      print("Tek boyutlu yapiyi incelediginiz icin boyutlar arasi korelasyon incelenemez.")
    } else {
      efa_of_data_set <- psych::fa(r = my_data(), nfactors = input$number_factor, rotate = input$rotating_method, fm = input$fact_method, cor = ifelse(input$cor_kind == "pea", "cor", "poly"))
      int_fact_result <- data.frame(as.matrix(efa_of_data_set$score.cor))
      rownames(int_fact_result) <- paste("f", seq(1:input$number_factor), sep = "")
      colnames(int_fact_result) <- paste("f", seq(1:input$number_factor), sep = "")
      int_fact_result
    }
  }, rownames = TRUE)

  output$efa_result_expl_var <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    efa_of_data_set <- psych::fa(r = my_data(), nfactors = input$number_factor, rotate = input$rotating_method, fm = input$fact_method, cor = ifelse(input$cor_kind == "pea", "cor", "poly"))
    exp_var_result <- data.frame(matrix(as.numeric(efa_of_data_set$Vaccounted), ncol = input$number_factor, nrow = 5, byrow = FALSE), row.names = c("SS loadings", "Proportion Var", "Cumulative Var", "Proportion Explained", "Cumulative Proportion"))
    colnames(exp_var_result) <- colnames(efa_of_data_set$Vaccounted)
    exp_var_result
  }, rownames = TRUE)

  output$kmo_result <- renderText({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    kmo_analysis <- psych::KMO(my_data())
    kmo_results <- data.frame(KMO = kmo_analysis$MSA)
    kmo_interpret <- dplyr::case_when(
      kmo_analysis$MSA < 0.49 ~ "Kabul edilemez",
      kmo_analysis$MSA < 0.59 ~ "Yetersiz",
      kmo_analysis$MSA < 0.69 ~ "Vasat",
      kmo_analysis$MSA < 0.79 ~ "Orta",
      kmo_analysis$MSA < 0.89 ~ "Iyi",
      kmo_analysis$MSA < 1.00 ~ "Mukemmel"
    )
    paste("<b>KMO degeriniz =", round(kmo_analysis$MSA, 3), "(", kmo_interpret, ")</b>", sep = "")
  })

  output$bartlett <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    bartlett_test <- if (input$cor_kind == "pea") {
      psych::cortest.bartlett(cor(my_data()), n = nrow(my_data()))
    } else {
      psych::cortest.bartlett(psych::polychoric(my_data())$rho, n = nrow(my_data()))
    }
    bartlett_result <- data.frame(matrix(c(bartlett_test$chisq, bartlett_test$df, bartlett_test$p.value), byrow = TRUE))
    rownames(bartlett_result) <- c("Chi Square", "Degrees of Freedom", "p value")
    colnames(bartlett_result) <- "Value"
    bartlett_result
  }, rownames = TRUE)

  output$robust_msa <- renderTable({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    kmo_analysis <- psych::KMO(my_data())
    msa_results <- data.frame(MSA = kmo_analysis$MSAi)
    interpret_msa <- ifelse(kmo_analysis$MSAi < 0.50, "Madde Atilmali", "Madde Yeterli")
    msa_results$Interpretation <- interpret_msa
    rownames(msa_results) <- paste("Madde", seq(1:ncol(my_data())), sep = "_")
    msa_results
  }, rownames = TRUE)

  output$heat_map <- renderPlot({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    if (ncol(my_data()) < 21) {
      cor_plot <- if (input$cor_kind == "pea") {
        ggcorrplot::ggcorrplot(corr = cor(my_data()), hc.order = TRUE, type = "lower", lab = TRUE)
      } else {
        ggcorrplot::ggcorrplot(corr = psych::polychoric(my_data())$rho, hc.order = TRUE, type = "lower", lab = TRUE)
      }
    } else {
      cor_plot <- if (input$cor_kind == "pea") {
        ggcorrplot::ggcorrplot(corr = cor(my_data()), hc.order = TRUE, method = "circle")
      } else {
        ggcorrplot::ggcorrplot(corr = psych::polychoric(my_data())$rho, hc.order = TRUE, method = "circle")
      }
    }
    plot(cor_plot)
  })

  # Confirmatory Factor Analysis TAB
  observeEvent(input$define_structure, {
    model <- input$cfa_define

    output$cfa_results <- renderTable({
      validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
      cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
      fit_measures <- data.frame(
        "Chi_Sqaure" = lavaan::fitmeasures(cfa_result_first)[c("chisq")],
        "df" = lavaan::fitmeasures(cfa_result_first)[c("df")],
        "Chi/df" = lavaan::fitmeasures(cfa_result_first)[c("chisq")] / lavaan::fitmeasures(cfa_result_first)[c("df")],
        "p_value" = lavaan::fitmeasures(cfa_result_first)[c("pvalue")],
        "CFI" = lavaan::fitmeasures(cfa_result_first)[c("cfi")],
        "TLI" = lavaan::fitmeasures(cfa_result_first)[c("tli")],
        "RFI" = lavaan::fitmeasures(cfa_result_first)[c("rfi")],
        "SRMR" = lavaan::fitmeasures(cfa_result_first)[c("srmr")],
        "RMSEA" = lavaan::fitmeasures(cfa_result_first)[c("rmsea")],
        "RMSEA_ci_lower" = lavaan::fitmeasures(cfa_result_first)[c("rmsea.ci.lower")],
        "RMSEA_ci_upper" = lavaan::fitmeasures(cfa_result_first)[c("rmsea.ci.upper")]
      )
      fit_results <- as.data.frame(t(as.matrix(fit_measures)))
      colnames(fit_results) <- c("Value")
      fit_results
    }, rownames = TRUE)

    output$download_fits <- downloadHandler(
      filename = function() {
        paste("fit_results.csv", sep = "")
      },
      content = function(file) {
        colnames(fit_results) <- c("Value")
        utils::write.csv(fit_results, file, row.names = TRUE, col.names = FALSE)
      }
    )

    output$cfa_factor_loadings <- renderTable({
      validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
      cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
      lv_std <- lavaan::standardizedsolution(cfa_result_first)
      lv_results <- lv_std[1:ncol(my_data()), -c(1, 2)]
      colnames(lv_results) <- c("Variable", "factor_loading", "std_error", "fl/std_error", "p_value", "ci.lower", "ci.upper")
      lv_results
    }, rownames = TRUE)

    output$download_cfa_loadings <- downloadHandler(
      filename = function() {
        paste("cfa_factor_loadings.csv", sep = "")
      },
      content = function(file) {
        validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
        cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
        lv_std <- lavaan::standardizedsolution(cfa_result_first)
        lv_results <- lv_std[1:ncol(my_data()), -c(1, 2)]
        colnames(lv_results) <- c("Variable", "factor_loading", "std_error", "fl/std_error", "p_value", "ci.lower", "ci.upper")
        utils::write.csv(lv_results, file, row.names = TRUE, col.names = TRUE)
      }
    )

    output$cfa_modification <- renderTable({
      validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
      cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
      lv_mod <- lavaan::modificationindices(cfa_result_first, sort. = TRUE, power = TRUE, maximum.number = 20)
      lv_mod
    }, rownames = TRUE)

    output$path_diagram <- renderPlot({
      validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
      cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
      semPlot::semPaths(object = cfa_result_first, what = "std", intercepts = FALSE, layout = "spring", edge.label.cex = 1.2, sizeMan = 5, sizeLat = 8, edge.color = "black", thresholds = FALSE, esize = 2)
    })

    output$save_diagram <- downloadHandler(
      filename = function() {
        paste("path_diagram.svg")
      },
      content = function(file) {
        svg(file)
        validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
        cfa_result_first <- lavaan::cfa(model = model, data = my_data(), ordered = ifelse(input$cfa_cor_mat == "pea", FALSE, TRUE), estimator = input$cfa_est_method)
        semPlot::semPaths(object = cfa_result_first, what = "std", intercepts = FALSE, layout = "spring", edge.label.cex = 1.2, sizeMan = 5, sizeLat = 8, edge.color = "black", thresholds = FALSE, esize = 2)
        dev.off()
      }
    )
  })

  # Reliability Analysis TAB
  output$reliability_result <- renderText({
    validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
    reliability_func <- function(x, method) {
      if (method == "alpha") {
        alpha_analysis <- psych::alpha(x)
        return(sprintf("%.3f", as.numeric(alpha_analysis$total[1])))
      } else if (method == "omega") {
        omega_analysis <- MBESS::ci.reliability(x)
        return(sprintf("%.3f", omega_analysis$est))
      } else if (method == "theta") {
        armor_theta <- function(x, method = "PCA", correlation_type = c("cor", "poly")) {
          number_item = ncol(x)
          if (method == "PCA") {
            pca_result = psych::principal(x, cor = correlation_type)
            eigen_values = pca_result$Vaccounted[1, 1]
            armor_theta_value = (number_item / (number_item - 1)) * (1 - 1 / eigen_values)
          } else {
            efa_result = psych::fa(x, fm = method, cor = correlation_type)
            eigen_values = efa_result$Vaccounted[1, 1]
            armor_theta_value = (number_item / (number_item - 1)) * (1 - 1 / eigen_values)
          }
          return(data.frame(armor_theta = armor_theta_value))
        }
        armor_result <- armor_theta(x, method = "PCA", correlation_type = input$cor_kind)
        return(sprintf("%.3f", as.numeric(armor_result$armor_theta)))
      } else if (method == "structure") {
        model <- input$defined_structure
        model_fit <- lavaan::cfa(model = model, data = my_data())
        structural_reliability <- semTools::reliability(model_fit)
        return(sprintf("%.3f", structural_reliability[5, 1]))
      } else if (method == "s_alpha") {
        user_strata <- input$strata_define
        user_strata_definition <- as.numeric(unlist(strsplit(user_strata, ",")))
        stratas <- cbind(colnames(my_data()), user_strata_definition)
        result_s_alpha <- sirt::stratified.cronbach.alpha(my_data(), itemstrata = stratas)
        return(sprintf("%.3f", result_s_alpha$alpha.stratified[1]))
      }
    }

    user_strata <- input$strata_define
    user_strata_definition <- as.numeric(unlist(strsplit(user_strata, ",")))
    if (input$reliability_coeff == "s_alpha" & length(user_strata_definition) != ncol(my_data())) {
      paste("Lutfen maddelerin hangi boyutlara ait oldugunu dogru bir sekilde tanimlayiniz. Siz ", length(user_strata_definition), " degisken tanimladiniz. Ancak veri setinizde ", ncol(my_data()), " degisken bulunmaktadir.", sep = "")
    } else {
      result_rel <- reliability_func(my_data(), method = input$reliability_coeff)
      paste("Guvenirlik Katasiyiniz: ", input$reliability_coeff, result_rel, sep = "")
    }
  })

  # Item Weighting TAB
  observeEvent(input$weight_item, {
    output$weighted_scores <- renderTable({
      validate(need(input$file1, 'Lutfen veri setinizi yukleyiniz.'))
      item_weighting <- function(x) {
        item_stats <- psychometric::item.exam(x = x, discrim = TRUE)
        item_diff <- item_stats$Difficulty / max(x)
        ind_averages <- rowSums(x) / (ncol(x) * max(x))
        new_data <- data.frame(matrix(NA, nrow = nrow(x), ncol = ncol(x)))
        colnames(new_data) <- paste("V", seq(1:ncol(x)), sep = "")
        weighted_data <- data.frame(matrix(NA, nrow = nrow(x), ncol = ncol(x)))
        colnames(weighted_data) <- paste("V", seq(1:ncol(x)), sep = "")
        for (i in 1:ncol(x)) {
          for (t in 1:nrow(x)) {
            new_data[t, i] = ind_averages[t] + item_diff[i]
          }
        }
        for (i in 1:ncol(x)) {
          for (t in 1:nrow(x)) {
            if (new_data[t, i] >= 1) {
              weighted_data[t, i] = x[t, i] + item_stats$Item.Rel.woi[i]
            } else {
              weighted_data[t, i] = x[t, i]
            }
          }
        }
        return(weighted_data)
      }
      rv$weighted_scores <- item_weighting(my_data())
      head(rv$weighted_scores, n = 10)
    }, rownames = TRUE)
  })

  output$download_weighted_scores <- downloadHandler(
    filename = function() {
      paste("Weighted_scores.dat", sep = "")
    },
    content = function(file) {
      utils::write.table(rv$weighted_scores, file, row.names = FALSE, col.names = FALSE)
    }
  )

  # About TAB
  output$about <- renderText({
      paste(
        p(strong("Bu uygulamanin adi Herkes icin Faktor Analizi (Factor Analysis for All [FAfA])'dir.")),
        p(strong("Uygulamanin Amaci:"),
          "Bu uygulama arastirmacilara acimlayici ve dogrulayici faktor analizi gerceklestirirken tum surecleri tek bir uygulamada yapabilmelerini saglamaktir.
        AFA ve DFA varsayimlarinin kontrolu, veri setini rassal olarak ikiye bolme (AFA ve DFA icin), farkli guvenirlik katsayilari bu uygulama ile incelenebilir.
        Buna ek olarak madde agirliklandirmasi de bu uygulamaya eklenmistir. Bu yontem bazi durumlarda yapi gecerligini yukseltebilmektedir."
        )
      )
    })

  output$developer <- renderText({
      paste(
        p(strong("Gelistirici Hakkinda:"), br(),
          "Abdullah Faruk KILIC, PhD, Doc. Dr.", br(),
          "Trakya Universitesi, Egitim Fakultesi", br(),
          "Egitim Bilimleri Bolumu", br(),
          "Egitimde Olcme ve Degerlendirme ABD"), br(),
        p(strong("e-mail-1:"), tags$a(href = "mailto:abdullahfarukkilic@gmail.com", "abdullahfarukkilic@gmail.com")),
        p(strong("e-mail-2:"), tags$a(href = "mailto:afarukkilic@trakya.edu.tr", "afarukkilic@trakya.edu.tr")),
        p(strong("RG:"), tags$a(href = "https://www.researchgate.net/profile/Abdullah-Kilic-2", "Click Here for Research Gate Profile!")),
        p(strong("Scholar:"), tags$a(href = "https://scholar.google.com/citations?user=AP7LlpoAAAAJ&hl=tr", "Click Here for Google Scholar Profile!")),
        p(strong("Her turlu problemi bildirmek icin mail atabilirsiniz."))
      )
    })

  output$contributer <- renderText({
      paste(
        p(strong("Bu uygulamanin gelistirilmesinde gorus, oneri ve dusuncelerini esirgemeyen, uygulamanin gelismesinde kiymetli katkilari olan arastirmaci arkadaslarima tesekkur ederim.")), br(),
        p(strong("Seda Nur SAKAR:"), tags$a(href = "https://avesis.hacettepe.edu.tr/sedasakar", "Click Here for Researcher's Profile!")),
        p(strong("Tugay KACAK:"), tags$a(href = "https://personel.trakya.edu.tr/tugaykacak/", "Click Here for Researcher's Profile!")),
        p(strong("Basak ERDEM KARA:"), tags$a(href = "https://avesis.anadolu.edu.tr/basakerdem", "Click Here for Researcher's Profile!")),
        p(strong("Meltem ACAR GUVENDIR"), tags$a(href = "https://personel.trakya.edu.tr/meltemacar/", "Click Here for Researcher's Profile!")),
        p(strong("Alperen YANDI"), tags$a(href = "https://www.linkedin.com/in/alperen-yandi-36404891?originalSubdomain=tr", "Click Here for Researcher's Profile!")),
        p(strong("Murat Dogan SAHIN"), tags$a(href = "https://avesis.anadolu.edu.tr/mdsahin", "Click Here for Researcher's Profile!")), br(),

        br(),
        strong("Bu versiyonda yeni olanlar:", br(),
               "Bazi hatalar duzeltildi", br(),
               "EGA analizi eklendi", br(),
               "Analiz sekmesine ekstra bilgi eklendi.", br(),
               "DFA faktor yuklerinin raporlanmasi kismi gelistirildi.", br(),
               "Modifikasyon onerileri onem duzeyine gore siralandi."
        ), br(),
        em("Version: 0.2")
      )
    })
}

