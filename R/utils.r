# utils.R
 
#' Common Utility Functions for the Factor Analysis Shiny Application

# ---------------------------------------------------------------------------
# Helper for default NULL values (coalesce)
# ---------------------------------------------------------------------------
#' Coalesce for NULL/NA or empty values
#'
#' Replaces x with y if x is NULL, has zero length, or all its elements are NA.
#' @param x The primary value.
#' @param y The fallback value.
#' @return x if valid, otherwise y.
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# ---------------------------------------------------------------------------
# clean_missing_data
# ---------------------------------------------------------------------------
#' Clean Missing Data
#'
#' Handles various NA representations, converts columns to numeric, and removes
#' rows with NAs.
#' @param data A data frame.
#' @return A list with `cleaned_data` (data.frame) and `removed_rows` (integer).
#' @importFrom stats na.omit
#' @noRd
clean_missing_data <- function(data) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  data_std_na <- as.data.frame(lapply(data, function(col_item) {
    if(is.character(col_item)) {
        col_item[col_item %in% c("", " ", "NA", "N/A", "na", "n/a", ".", "-", "?", "missing")] <- NA
    } else if (is.factor(col_item)) {
        char_col <- as.character(col_item)
        char_col[char_col %in% c("", " ", "NA", "N/A", "na", "n/a", ".", "-", "?", "missing")] <- NA
        col_item <- char_col
    }
    if(is.character(col_item) || is.factor(col_item)){
        potential_na_mask <- is.na(col_item) | suppressWarnings(is.na(as.numeric(as.character(col_item))))
        col_item[potential_na_mask & !is.na(col_item) & !(as.character(col_item) %in% c("", " "))] <- NA
    }
    return(col_item)
  }))

  data_numeric <- as.data.frame(lapply(data_std_na, function(col_item) {
      suppressWarnings(as.numeric(as.character(col_item)))
  }))

  original_nrow <- nrow(data_numeric)
  cleaned_data_final <- stats::na.omit(data_numeric)
  removed_rows_count <- original_nrow - nrow(cleaned_data_final)

  return(list(
    cleaned_data = cleaned_data_final,
    removed_rows = removed_rows_count
  ))
}

# ---------------------------------------------------------------------------
# assumptions
# ---------------------------------------------------------------------------
#' Calculate Statistical Assumptions
#'
#' Calculates descriptives, multicollinearity, Mahalanobis distance, and Mardia's tests.
#' @param x A numeric data frame.
#' @param mah_p_threshold P-value threshold for Mahalanobis outliers.
#' @return A list of assumption check results.
#' @importFrom pastecs stat.desc
#' @importFrom moments skewness kurtosis
#' @importFrom mctest mctest eigprop
#' @importFrom mvnormalTest mardia
#' @importFrom dplyr mutate filter select arrange
#' @importFrom magrittr %>%
#' @importFrom stats na.omit lm as.formula model.matrix mahalanobis pchisq cov median cor
#' @noRd
assumptions <- function(x, mah_p_threshold = 0.001) { # Added mah_p_threshold argument
  message("DEBUG: assumptions() function (USER LOGIC VERSION WITH CORRECTIONS) called.")
nrow(a)
  # Ensure input is a data.frame and has columns
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }
  if (ncol(x) == 0) {
    stop("Input data 'x' has 0 columns for assumptions checking.")
  }

  # Ensure data is numeric (as per previous robust versions)
  x_numeric <- x
  if (!all(sapply(x_numeric, is.numeric))) {
    warning("Not all columns are numeric in data passed to assumptions(). Attempting conversion.")
    x_numeric <- as.data.frame(lapply(x, function(col) {
      if(!is.numeric(col)) suppressWarnings(as.numeric(as.character(col))) else col
    }))
    if (!all(sapply(x_numeric, is.numeric))) {
      stop("Failed to convert all columns to numeric for assumptions checking.")
    }
  }
  x <- x_numeric # Use the numeric version

  # --- Descriptive Statistics (User's Logic) ---
  descr <- as.data.frame(matrix(NA, nrow = ncol(x), ncol = 8 ))
  colnames(descr) <- c("N", "N (missing)", "Min", "Max", "Median", "Mean", "Skewness", "Kurtosis")
  rownames(descr) <- colnames(x) # Assign row names for clarity

  if (nrow(x) > 0) { # Proceed only if data has rows
    missing_mean <- function(var) mean(var, na.rm = TRUE) # Corrected to take single var

    # Use tryCatch for pastecs in case of issues with very small/odd data
    descriptives_pastecs <- tryCatch(
      pastecs::stat.desc(x, basic = TRUE, desc = TRUE, norm = FALSE),
      error = function(e) {
        warning(paste("pastecs::stat.desc failed:", e$message)); NULL
      }
    )

    if (!is.null(descriptives_pastecs) && is.matrix(descriptives_pastecs)) {
      if("nbr.val" %in% rownames(descriptives_pastecs)) descr[,"N"] <- descriptives_pastecs["nbr.val", ]
      if("min" %in% rownames(descriptives_pastecs)) descr[,"Min"] <- descriptives_pastecs["min", ]
      if("max" %in% rownames(descriptives_pastecs)) descr[,"Max"] <- descriptives_pastecs["max", ]
      if("median" %in% rownames(descriptives_pastecs)) descr[,"Median"] <- descriptives_pastecs["median", ]
    }

    descr[,"N"] <- nrow(x)
    descr[,"N (missing)"] <- colSums(apply(x, 2, is.na))
    descr[,"Min"] <- apply(na.omit(x), 2, min)
    descr[,"Max"] <- apply(na.omit(x), 2, max)
    descr[,"Median"] <- apply(na.omit(x), 2, median)
    descr[,"Mean"] <- apply(x, 2, missing_mean) # User's original

    # Skewness and Kurtosis from 'moments' package
    if(nrow(x) >= 1 && ncol(x) >=1){
      skew_vals <- tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) rep(NA_real_, ncol(x)))
      kurt_vals <- tryCatch(moments::kurtosis(x, na.rm = TRUE) - 3, error = function(e) rep(NA_real_, ncol(x))) #kurtosis
      if(length(skew_vals) == ncol(x)) descr[,"Skewness"] <- as.numeric(skew_vals)
      if(length(kurt_vals) == ncol(x)) descr[,"Kurtosis"] <- as.numeric(kurt_vals)
    }
  } else {
    warning("Input data for 'assumptions' has 0 rows. Descriptive statistics will be NA.")
  }

  # --- Data for multicollinearity, Mahalanobis, Mardia needs to be complete cases ---
  x_complete <- stats::na.omit(x) # Use the already numeric 'x'

  # Initialize results for parts that depend on x_complete
  mc_control <- data.frame(VIF_min=NA_real_, VIF_max=NA_real_, TOL_min=NA_real_, TOL_max=NA_real_, CI_min=NA_real_, CI_max=NA_real_)
  Mah_significant <- data.frame(Row_Number=integer(), MD=numeric(), MD_p=numeric(), stringsAsFactors = FALSE)

  # Default structure for mardia results
  mardia_default_row <- data.frame(Test="N/A", Statistic=NA_real_, "p-value"=NA_real_, Result=NA_character_, check.names = FALSE, stringsAsFactors = FALSE)
  mardia_kurt_result_df <- mardia_default_row[1, , drop=FALSE]; mardia_kurt_result_df$Test <- "Kurtosis"
  mardia_skew_result_df <- mardia_default_row[1, , drop=FALSE]; mardia_skew_result_df$Test <- "Skewness"
  n_outlier <- 0

  if (nrow(x_complete) >= 2 && ncol(x_complete) >= 1 && nrow(x_complete) > ncol(x_complete)) {
    # --- Multicollinearity (User's Logic) ---
    if (ncol(x_complete) >= 2) {
      x_new <- x_complete
      x_new$rn_dummy_dependent_var <- 1:nrow(x_new) # User's variable name
      model_for_collinearity <- tryCatch(
        lm(stats::as.formula(paste(colnames(x_new)[ncol(x_new)], "~",
                                   paste(colnames(x_new)[1:(ncol(x_new)-1)], collapse = "+"))),
           data=x_new),
        error = function(e) {warning(paste("LM for multicollinearity failed:", e$message)); NULL}
      )

      if (!is.null(model_for_collinearity)) {
        mc_VIF_TOL_list <- tryCatch(mctest::mctest(model_for_collinearity, type = "i"), error = function(e) NULL)
        mc_CI_list <- tryCatch(mctest::eigprop(mod = model_for_collinearity), error = function(e) NULL)

        if (!is.null(mc_VIF_TOL_list) && is.list(mc_VIF_TOL_list) && "idiags" %in% names(mc_VIF_TOL_list)) {
          mc_VIF_TOL_df <- as.data.frame(mc_VIF_TOL_list$idiags[,1:2]) # Ensure it's a data frame
          mc_control$VIF_min <- if("VIF" %in% colnames(mc_VIF_TOL_df) && sum(!is.na(mc_VIF_TOL_df$VIF)) > 0) min(mc_VIF_TOL_df$VIF, na.rm=TRUE) else NA_real_
          mc_control$VIF_max <- if("VIF" %in% colnames(mc_VIF_TOL_df) && sum(!is.na(mc_VIF_TOL_df$VIF)) > 0) max(mc_VIF_TOL_df$VIF, na.rm=TRUE) else NA_real_
          mc_control$TOL_min <- if("TOL" %in% colnames(mc_VIF_TOL_df) && sum(!is.na(mc_VIF_TOL_df$TOL)) > 0) min(mc_VIF_TOL_df$TOL, na.rm=TRUE) else NA_real_
          mc_control$TOL_max <- if("TOL" %in% colnames(mc_VIF_TOL_df) && sum(!is.na(mc_VIF_TOL_df$TOL)) > 0) max(mc_VIF_TOL_df$TOL, na.rm=TRUE) else NA_real_
        }

        if (!is.null(mc_CI_list) && is.list(mc_CI_list) && "ci" %in% names(mc_CI_list) && length(mc_CI_list$ci) > 0) {
          mc_control$CI_min <- min(mc_CI_list$ci, na.rm=TRUE)
          mc_control$CI_max <- max(mc_CI_list$ci, na.rm=TRUE)
        }
      }
    }

    # --- Mahalanobis Distance (User's Logic, adapted for mah_p_threshold) ---
    if (nrow(x_complete) > ncol(x_complete) && ncol(x_complete) > 0) {
      distance <- tryCatch(as.matrix(stats::mahalanobis(x_complete, colMeans(x_complete), cov(x_complete))), error = function(e) NULL)
      if (!is.null(distance)) {
        Mah_significant <- x_complete %>% # Use x_complete here
          dplyr::mutate(Row_Number_In_Complete_Data = 1:nrow(x_complete), # Row number within x_complete
                        MD = as.numeric(distance), # Ensure MD is numeric
                        MD_p = stats::pchisq(MD, df = (ncol(x_complete)), lower.tail = FALSE)) %>%
          dplyr::filter(MD_p <= mah_p_threshold) %>%
          dplyr::select(Row_Number_In_Complete_Data, MD, MD_p) %>% # Select only relevant columns
          dplyr::arrange(MD_p,  dplyr::desc(MD))
        n_outlier <- nrow(Mah_significant)
      }
    } else {
      warning("Not enough observations relative to variables (N <= P) for Mahalanobis distance.")
    }

    # --- Mardia's Kurtosis and Skewness (User's Logic with Correction) ---
    if (nrow(x_complete) >= 2 && ncol(x_complete) >= 2) {
      mardia_test_output <- tryCatch(mvnormalTest::mardia(as.matrix(x_complete)), error = function(e) {
        warning(paste("Mardia test failed:", e$message)); NULL
      })
      if (!is.null(mardia_test_output) && !is.null(mardia_test_output$mv.test) && is.data.frame(mardia_test_output$mv.test)) {
        mardia_full_df <- mardia_test_output$mv.test

        # Process Skewness
        skew_row <- mardia_full_df[mardia_full_df$Test == "Skewness",, drop = FALSE]
        if (nrow(skew_row) == 1) {
          mardia_skew_result_df <- data.frame(
            Test = "Skewness",
            Statistic = as.numeric(as.character(skew_row[1, "Statistic"])),
            "p-value" = as.numeric(as.character(skew_row[1, "p-value"])),
            Result = as.character(skew_row[1, "Result"]),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }

        # Process Kurtosis
        kurt_row <- mardia_full_df[mardia_full_df$Test == "Kurtosis", , drop = FALSE]
        if (nrow(kurt_row) == 1) {
          mardia_kurt_result_df <- data.frame(
            Test = "Kurtosis",
            Statistic = as.numeric(as.character(kurt_row[1, "Statistic"])),
            "p-value" = as.numeric(as.character(kurt_row[1, "p-value"])),
            Result = as.character(kurt_row[1, "Result"]),
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }
      }
    } else {
      warning("Not enough data (N<2 or P<2) for Mardia's tests.")
    }
  } else {
    warning("Not enough complete cases (N < 2 or N <= P) for some assumption checks.")
  }

  # --- Return List (User's Naming Convention for Mardia) ---
  return(list(
    descriptives = round(descr, 2),
    multicollinearity = round(mc_control, 2),
    Mah_significant = Mah_significant, # This is already a data.frame
    n_outlier = n_outlier,             # This is nrow(Mah_significant)
    Mardia_Kurtosis = mardia_kurt_result_df, # This is the 1-row data.frame
    Mardia_Skewness = mardia_skew_result_df  # This is the 1-row data.frame
  ))
}


# ---------------------------------------------------------------------------
# factor_ret
# ---------------------------------------------------------------------------
#' Factor Retention Methods
#'
#' Applies methods to suggest the number of factors to retain.
#' @param x A numeric data frame or matrix.
#' @param method Character string for the retention method.
#' @return A data frame with suggested number of factors.
#' @importFrom stats na.omit
#' @importFrom EFA.MRFA parallelMRFA hullEFA
#' @importFrom psych fa.parallel principal
#' @importFrom EFA.dimensions MAP EMPKC
#' @importFrom EGAnet EGA
#' @importFrom EFAtools CD
#' @noRd
factor_ret <- function(x, method = "hull_method") {
  if (!is.data.frame(x) && !is.matrix(x)) {
    x <- as.data.frame(x)
  }
  if (!all(sapply(x, is.numeric))) {
      x_numeric_cols <- sapply(x, is.numeric)
      non_numeric_cols <- colnames(x)[!x_numeric_cols]
      warning(paste0("DEBUG factor_ret: Non-numeric columns found and will be excluded: ", paste(non_numeric_cols, collapse=", ")))
      x <- x[, x_numeric_cols, drop=FALSE]
      if(ncol(x) < 2) {
        return(data.frame(Suggested_Factors = NA, row.names = paste("Error in", method, ": Requires at least 2 numeric columns.")))
      }
  }

  x_complete <- stats::na.omit(x)

  if (nrow(x_complete) < max(3, ncol(x_complete) +1 ) || ncol(x_complete) < 2 ) {
    return(data.frame(Suggested_Factors = NA, row.names = paste("Error in", method, ": Insufficient data (N=",nrow(x_complete),", P=",ncol(x_complete),")")))
  }

  col_variances <- apply(x_complete, 2, var)
  if (any(col_variances == 0, na.rm = TRUE)) {
      zero_var_cols <- colnames(x_complete)[col_variances == 0]
      return(data.frame(Suggested_Factors = NA, row.names = paste("Error in", method, ": Zero variance in column(s)")))
  }

  if (method == "pa_mrfa") {
    return(tryCatch({
      op_pa_analysis <- EFA.MRFA::parallelMRFA(x_complete)
      data.frame(Suggested_Factors = op_pa_analysis$N_factors_percentiles, row.names = "Optimal Parallel Analysis (MRFA)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in pa_mrfa:", conditionMessage(e)))
    }))
  } else if (method == "pa_traditional") {
    return(tryCatch({
      tra_pa_analysis <- psych::fa.parallel(x_complete, fa = "fa", plot = FALSE, show.legend = FALSE, error.bars = FALSE, fm="pa")
      data.frame(Suggested_Factors = tra_pa_analysis$nfact, row.names = "Traditional Parallel Analysis (FA based)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in pa_traditional:", conditionMessage(e)))
    }))
  } else if (method == "hull_method") {
    return(tryCatch({
      hull_analysis <- EFA.MRFA::hullEFA(x_complete, display = FALSE)
      data.frame(Suggested_Factors = hull_analysis$n_factors, row.names = "Hull Method (EFA)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in hull_method:", conditionMessage(e)))
    }))
  } else if (method == "map_method_tra") {
    return(tryCatch({
      map_analysis <- suppressMessages(EFA.dimensions::MAP(x_complete, corkind = "pearson", verbose = FALSE))
      data.frame(Suggested_Factors = map_analysis$NfactorsMAP, row.names = "Minimum Average Partial (MAP - Original)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in map_method_tra:", conditionMessage(e)))
    }))
  } else if (method == "map_method_rev") {
    return(tryCatch({
      map_analysis <- suppressMessages(EFA.dimensions::MAP(x_complete, corkind = "pearson", verbose = FALSE))
      data.frame(Suggested_Factors = map_analysis$NfactorsMAP4, row.names = "Minimum Average Partial (MAP - Revised)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in map_method_rev:", conditionMessage(e)))
    }))
  } else if (method == "EGA_tmfg") {
    return(tryCatch({
      ega_analysis_tmfg <- EGAnet::EGA(data = x_complete, model = "TMFG", plot.EGA = FALSE, verbose = FALSE)
      data.frame(Suggested_Factors = ega_analysis_tmfg$n.dim, row.names = "Exploratory Graph Analysis (TMFG)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in EGA_tmfg:", conditionMessage(e)))
    }))
  } else if (method == "EGA_glasso") {
    return(tryCatch({
      ega_analysis_glasso <- EGAnet::EGA(data = x_complete, model = "glasso", plot.EGA = FALSE, verbose = FALSE)
      data.frame(Suggested_Factors = ega_analysis_glasso$n.dim, row.names = "Exploratory Graph Analysis (Glasso)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in EGA_glasso:", conditionMessage(e)))
    }))
  } else if (method == "EK_C") {
    return(tryCatch({
      emkpc_analysis <- suppressMessages(EFA.dimensions::EMPKC(x_complete, corkind = "pearson", verbose = FALSE))
      data.frame(Suggested_Factors = emkpc_analysis$NfactorsEMPKC, row.names = "Empirical Kaiser Criterion (EKC)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in EK_C:", conditionMessage(e)))
    }))
  } else if (method == "comp_data_method") {
     return(tryCatch({
      cd_analysis <- EFAtools::CD(x_complete)
      data.frame(Suggested_Factors = cd_analysis$n_factors, row.names = "Comparison Data (CD)")
    }, error = function(e) {
      data.frame(Suggested_Factors = NA, row.names = paste("Error in comp_data_method:", conditionMessage(e)))
    }))
  } else {
    return(data.frame(Suggested_Factors = NA, row.names = paste("Error: Unknown method", method)))
  }
}

# ---------------------------------------------------------------------------
# reliability_func
# ---------------------------------------------------------------------------
#' Calculate Reliability Coefficients
#'
#' Calculates various reliability coefficients like Alpha, Omega, Theta, etc.
#' @param x A numeric data frame or matrix.
#' @param method Reliability method ("alpha", "omega", "theta", "structure", "s_alpha").
#' @param cor_kind Correlation type (for "theta").
#' @param defined_structure Lavaan model syntax (for "structure").
#' @param strata_define Strata definition string (for "s_alpha").
#' @return A string with the formatted reliability coefficient or an error message.
#' @importFrom psych alpha omega principal
#' @importFrom MBESS ci.reliability
#' @importFrom lavaan lavaanify cfa
#' @importFrom semTools reliability
#' @importFrom sirt stratified.cronbach.alpha
#' @importFrom stats na.omit cov
#' @noRd
reliability_func <- function(x, method = "alpha", cor_kind = "cor", defined_structure = NULL, strata_define = NULL) {
  if (!is.data.frame(x) && !is.matrix(x)) x <- as.data.frame(x)
    if (!all(sapply(x, is.numeric))) {
      x_numeric_cols <- sapply(x, is.numeric)
      warning(paste0("Non-numeric columns found and will be excluded from reliability analysis: ", paste(colnames(x)[!x_numeric_cols], collapse=", ")))
      x <- x[, x_numeric_cols, drop=FALSE]
      if(ncol(x) < 2) stop("At least two numeric columns required for reliability analysis.")
  }
  x_complete <- stats::na.omit(x)
  if (nrow(x_complete) < 2 || ncol(x_complete) < 2 ) {
    stop("Not enough data (rows/columns) after NA removal and numeric filtering for reliability analysis.")
  }

  result_value <- tryCatch({
    if (method == "alpha") {
      alpha_analysis <- psych::alpha(x_complete, check.keys = FALSE)
      alpha_total_df <- as.data.frame(alpha_analysis$total)
      std_alpha_val <- alpha_total_df["std.alpha",1] %||% alpha_total_df["raw_alpha",1] %||% alpha_total_df[1,1] %||% NA_real_
      as.numeric(std_alpha_val)
    } else if (method == "omega") {
      omega_psych <- tryCatch(psych::omega(x_complete, plot=FALSE, fm="pa"), error = function(e) {
          warning(paste("psych::omega failed:", e$message, "Trying MBESS if N > P.")); NULL
          })
      if(!is.null(omega_psych) && "omega.tot" %in% names(omega_psych)){
          return(omega_psych$omega.tot)
      } else if (nrow(x_complete) > ncol(x_complete)) {
          warning("psych::omega did not provide omega.tot, attempting MBESS::ci.reliability (requires N > P).")
          cov_matrix <- stats::cov(x_complete)
          omega_analysis_mbess <- MBESS::ci.reliability(S = cov_matrix, N = nrow(x_complete), type = "omega")
          return(omega_analysis_mbess$est)
      } else {
          warning("Omega calculation failed with psych::omega and N <= P for MBESS.")
          return(NA_real_)
      }
    } else if (method == "theta") {
      armor_theta_calc <- function(data_in, correlation_type_internal = "cor") {
        num_items <- ncol(data_in)
        if (num_items < 2) return(NA_real_)
        valid_cor_types <- c("cor", "cov", "poly", "tet")
        if(!(correlation_type_internal %in% valid_cor_types)) {
            warning(paste("Invalid correlation_type '", correlation_type_internal, "' for psych::principal. Defaulting to 'cor'."))
            correlation_type_internal <- "cor"
        }
        pca_res <- psych::principal(data_in, nfactors = 1, rotate = "none", cor = correlation_type_internal)
        first_eigenvalue <- pca_res$Vaccounted[1, 1]
        if (is.na(first_eigenvalue) || (first_eigenvalue <= 1 && num_items > 1) || num_items <= 1) {
            return(NA_real_)
        }
        theta_val <- (num_items / (num_items - 1)) * (1 - (1 / first_eigenvalue))
        return(theta_val)
      }
      armor_theta_calc(x_complete, correlation_type_internal = cor_kind)
    } else if (method == "structure") {
      if (is.null(defined_structure) || nchar(trimws(defined_structure)) == 0) {
        stop("CFA model structure must be defined for 'structural' reliability.")
      }
      manifest_vars_rel <- unique(unlist(lavaan::lavaanify(defined_structure)$rhs[lavaan::lavaanify(defined_structure)$op == "=~"]))
      ordered_arg_rel <- if (cor_kind == "poly" && length(manifest_vars_rel) > 0) manifest_vars_rel else FALSE
      estimator_rel <- ifelse(any(as.logical(ordered_arg_rel)), "WLSMV", "ML")

      cfa_model_fit <- lavaan::cfa(model = defined_structure, data = x_complete, ordered = ordered_arg_rel, estimator = estimator_rel, warn = FALSE)
      structural_rel_matrix <- semTools::reliability(cfa_model_fit)
      return(sprintf("%.3f", structural_rel_matrix[5, 1]))
    } else if (method == "s_alpha") {
      if (is.null(strata_define) || nchar(trimws(strata_define)) == 0) {
        stop("Strata definition must be provided for 'stratified alpha'.")
      }
      strata_num_vector <- suppressWarnings(as.numeric(unlist(strsplit(strata_define, ","))))
      if(any(is.na(strata_num_vector))) stop("Strata definition contains non-numeric values.")
      if (length(strata_num_vector) != ncol(x_complete)) {
        stop("Length of strata definition does not match the number of items.")
      }
      item_names_strata <- colnames(x_complete)
      strata_matrix_sirt <- data.frame(item = item_names_strata, stratum = strata_num_vector)

      s_alpha_results <- sirt::stratified.cronbach.alpha(dat = x_complete, itemstrata = strata_matrix_sirt)
      s_alpha_results$alpha.stratified[1]
    } else {
      stop(paste("Unknown reliability method:", method))
    }
  }, error = function(e) {
    warning(paste("Error in reliability_func method '", method, "': ", e$message, sep=""))
    return(NA_real_)
  })

  if (is.na(result_value)) {
    return("Calculation failed or N/A.")
  } else {
    return(sprintf("%.3f", as.numeric(result_value)))
  }
}

# ---------------------------------------------------------------------------
# item_weighting
# ---------------------------------------------------------------------------
#' Item Weighting Function
#'
#' Applies a specific item weighting algorithm.
#' @param x A numeric data frame.
#' @return A data frame with weighted scores.
#' @importFrom psychometric item.exam
#' @importFrom stats na.omit
#' @importFrom psychometric item.exam
#' @noRd
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
      new_data[t, i] <- ind_averages[t] + item_diff[i]
    }
  }
  for (i in 1:ncol(x)) {
    for (t in 1:nrow(x)) {
      if (new_data[t, i] >= 1) {
        weighted_data[t, i] <- x[t, i] + item_stats$Item.Rel.woi[i]
      } else {
        weighted_data[t, i] <- x[t, i]
      }
    }
  }
  return(weighted_data)
}
