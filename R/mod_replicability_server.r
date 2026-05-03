#' EFA Replicability Server Module
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @noRd
efa_server_replicability <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL)

    clean_numeric_complete <- function(d) {
      if (is.null(d)) return(NULL)
      d <- as.data.frame(d)
      d <- d[, sapply(d, is.numeric), drop = FALSE]
      if (ncol(d) < 3) return(NULL)
      d <- stats::na.omit(d)
      if (nrow(d) < 60) return(NULL)
      zero_var <- vapply(d, stats::var, numeric(1), na.rm = TRUE) == 0
      if (any(zero_var)) d <- d[, !zero_var, drop = FALSE]
      if (ncol(d) < 3) return(NULL)
      d
    }

    run_efa <- function(d, nfactors, rotate, fm, cor_kind) {
      psych::fa(
        r        = d,
        nfactors = nfactors,
        rotate   = rotate,
        fm       = fm,
        cor      = if (cor_kind == "pea") "cor" else "poly"
      )
    }

    loading_matrix <- function(fa_obj, nfactors) {
      loads <- as.matrix(unclass(fa_obj$loadings))
      loads <- loads[, seq_len(nfactors), drop = FALSE]
      colnames(loads) <- paste0("F", seq_len(nfactors))
      loads
    }

    align_loadings <- function(load_1, load_2) {
      nfactors <- ncol(load_1)
      if (nfactors == 1) {
        if (sum(load_1[, 1] * load_2[, 1], na.rm = TRUE) < 0) {
          load_2[, 1] <- -load_2[, 1]
        }
        return(list(load_2 = load_2, congruence = data.frame(Factor = "F1", Congruence = psych::factor.congruence(load_1, load_2)[1, 1])))
      }

      congr <- psych::factor.congruence(load_1, load_2)
      pool <- as.data.frame(as.table(abs(congr)), stringsAsFactors = FALSE)
      names(pool) <- c("Factor_1", "Factor_2", "Abs_Congruence")
      pool <- pool[order(pool$Abs_Congruence, decreasing = TRUE), , drop = FALSE]

      matched_1 <- character(0)
      matched_2 <- character(0)
      pairs <- data.frame(Factor_1 = character(0), Factor_2 = character(0), stringsAsFactors = FALSE)
      for (i in seq_len(nrow(pool))) {
        p1 <- as.character(pool$Factor_1[i])
        p2 <- as.character(pool$Factor_2[i])
        if (!(p1 %in% matched_1) && !(p2 %in% matched_2)) {
          pairs <- rbind(pairs, data.frame(Factor_1 = p1, Factor_2 = p2, stringsAsFactors = FALSE))
          matched_1 <- c(matched_1, p1)
          matched_2 <- c(matched_2, p2)
        }
      }

      ordered_2 <- load_2[, pairs$Factor_2, drop = FALSE]
      colnames(ordered_2) <- pairs$Factor_1
      ordered_2 <- ordered_2[, colnames(load_1), drop = FALSE]

      for (factor_name in colnames(load_1)) {
        if (sum(load_1[, factor_name] * ordered_2[, factor_name], na.rm = TRUE) < 0) {
          ordered_2[, factor_name] <- -ordered_2[, factor_name]
        }
      }

      aligned_congr <- psych::factor.congruence(load_1, ordered_2)
      congr_tbl <- data.frame(
        Factor = colnames(load_1),
        Congruence = diag(aligned_congr),
        stringsAsFactors = FALSE
      )

      list(load_2 = ordered_2, congruence = congr_tbl)
    }

    build_comparison <- function(load_1, load_2) {
      out <- data.frame(Item = rownames(load_1), stringsAsFactors = FALSE)
      diffs <- matrix(NA_real_, nrow = nrow(load_1), ncol = ncol(load_1))

      for (j in seq_len(ncol(load_1))) {
        factor_name <- colnames(load_1)[j]
        s1_name <- paste0("Split1_", factor_name)
        s2_name <- paste0("Split2_", factor_name)
        diff_name <- paste0("Diff_", factor_name)

        out[[s1_name]] <- load_1[, j]
        out[[s2_name]] <- load_2[, j]
        out[[diff_name]] <- abs(load_1[, j] - load_2[, j])
        diffs[, j] <- out[[diff_name]]
      }

      out$MaxAbsDiff <- apply(diffs, 1, max, na.rm = TRUE)
      out$Primary_Split1 <- colnames(load_1)[max.col(abs(load_1), ties.method = "first")]
      out$Primary_Split2 <- colnames(load_2)[max.col(abs(load_2), ties.method = "first")]
      out$Same_Primary <- out$Primary_Split1 == out$Primary_Split2
      out
    }

    observeEvent(list(data(), input$cor_kind, input$number_factor, input$fact_method,
                      input$rotating_method, input$split_prop, input$seed), {
      result(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$run_replicability, {
      d <- clean_numeric_complete(data())
      if (is.null(d)) {
        showNotification(
          "Replicability requires at least 60 complete cases and 3 numeric variables after cleaning.",
          type = "error", duration = 8
        )
        return()
      }

      nfactors <- as.numeric(input$number_factor)
      if (nfactors < 1 || nfactors >= ncol(d)) {
        showNotification("Number of factors must be smaller than the number of variables.", type = "error", duration = 8)
        return()
      }

      split_n <- floor(nrow(d) * as.numeric(input$split_prop))
      if (min(split_n, nrow(d) - split_n) < 30) {
        showNotification("Each split must contain at least 30 complete cases.", type = "error", duration = 8)
        return()
      }

      showNotification("Running EFA replicability analysis...", type = "message", duration = NULL, id = "rep_progress")

      tryCatch({
        set.seed(as.integer(input$seed))
        idx_1 <- sample(seq_len(nrow(d)), split_n)
        d1 <- d[idx_1, , drop = FALSE]
        d2 <- d[-idx_1, , drop = FALSE]

        fa_1 <- run_efa(d1, nfactors, input$rotating_method, input$fact_method, input$cor_kind)
        fa_2 <- run_efa(d2, nfactors, input$rotating_method, input$fact_method, input$cor_kind)

        load_1 <- loading_matrix(fa_1, nfactors)
        load_2 <- loading_matrix(fa_2, nfactors)
        aligned <- align_loadings(load_1, load_2)
        comp <- build_comparison(load_1, aligned$load_2)

        summary <- data.frame(
          Metric = c(
            "Total complete cases",
            "Split 1 cases",
            "Split 2 cases",
            "Variables",
            "Factors",
            "Correlation",
            "Extraction",
            "Rotation",
            "Mean absolute loading difference",
            "Items with same primary factor"
          ),
          Value = c(
            nrow(d),
            nrow(d1),
            nrow(d2),
            ncol(d),
            nfactors,
            if (input$cor_kind == "pea") "Pearson" else "Polychoric",
            input$fact_method,
            input$rotating_method,
            round(mean(comp$MaxAbsDiff, na.rm = TRUE), 3),
            paste0(sum(comp$Same_Primary, na.rm = TRUE), " / ", nrow(comp))
          ),
          stringsAsFactors = FALSE
        )

        result(list(
          comparison = comp,
          summary = summary,
          congruence = aligned$congruence
        ))

        removeNotification("rep_progress")
        showNotification("Replicability analysis complete.", type = "message", duration = 4)
      }, error = function(e) {
        removeNotification("rep_progress")
        result(NULL)
        showNotification(paste("Replicability failed:", e$message), type = "error", duration = 10)
      })
    })

    output$loadings_comparison <- renderTable({
      req(result())
      result()$comparison
    }, digits = 3, striped = TRUE, bordered = TRUE)

    output$summary_table <- renderTable({
      req(result())
      result()$summary
    }, striped = TRUE, bordered = TRUE)

    output$factor_congruence <- renderTable({
      req(result())
      congr <- result()$congruence
      congr$Congruence <- round(congr$Congruence, 3)
      congr
    }, striped = TRUE, bordered = TRUE)

    output$download_replicability <- downloadHandler(
      filename = "efa_replicability_loadings.csv",
      content = function(file) {
        req(result())
        utils::write.csv(result()$comparison, file, row.names = FALSE)
      }
    )
  })
}
