# Internal ItemRest-style item removal engine for FAfA.
# Required imports: psych, gtools, qgraph, stats, utils.

.efa_cache <- new.env(hash = TRUE, parent = emptyenv())
.alpha_cache <- new.env(hash = TRUE, parent = emptyenv())

clear_efa_cache <- function() {
  rm(list = ls(envir = .efa_cache), envir = .efa_cache)
  rm(list = ls(envir = .alpha_cache), envir = .alpha_cache)
}

howard <- function(primary, secondary) {
  !(primary >= 0.40 && secondary < 0.30 && (primary - secondary) >= 0.20)
}

descriptive_stats <- function(data) {
  list(
    n_items   = ncol(data),
    n_obs     = nrow(data),
    min_value = min(data, na.rm = TRUE),
    max_value = max(data, na.rm = TRUE)
  )
}

cor_matrix_custom <- function(data, method = "polychoric") {
  if (method == "polychoric") {
    suppressMessages(suppressWarnings(qgraph::cor_auto(data)))
  } else {
    stats::cor(data, use = "pairwise.complete.obs", method = method)
  }
}

get_cor_matrix <- function(data, cor_method = "pearson") {
  cor_method <- match.arg(tolower(cor_method), c("pearson", "spearman", "polychoric"))
  colnames(data) <- gsub("\\.", "_", colnames(data))
  cor_matrix_custom(data, cor_method)
}

determine_n_factors <- function(data, cor_method = "pearson", cor_mat = NULL) {
  if (is.null(cor_mat)) {
    cor_mat <- cor_matrix_custom(data, cor_method)
  }
  fa_parallel <- suppressMessages(suppressWarnings(
    psych::fa.parallel(
      cor_mat,
      fa = "fa",
      n.iter = 20,
      n.obs = nrow(data),
      show.legend = FALSE,
      plot = FALSE,
      main = NULL
    )
  ))
  fa_parallel$nfact
}

alpha_custom <- function(data, use_cache = TRUE) {
  cache_key <- paste(sort(colnames(data)), collapse = "|")
  if (use_cache && exists(cache_key, envir = .alpha_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .alpha_cache, inherits = FALSE))
  }

  alpha_obj <- NULL
  invisible(utils::capture.output({
    alpha_obj <- suppressMessages(suppressWarnings(
      psych::alpha(data, check.keys = FALSE)
    ))
  }))
  alpha_val <- alpha_obj$total$raw_alpha
  if (use_cache) {
    assign(cache_key, alpha_val, envir = .alpha_cache, inherits = FALSE)
  }
  alpha_val
}

efa_custom <- function(data, n_factors = 1, cor_method = "polychoric",
                       extract = "uls", rotate = "oblimin",
                       use_cache = TRUE, cor_mat = NULL) {
  if (use_cache) {
    cache_key <- paste(
      n_factors, cor_method, extract, rotate,
      paste(sort(colnames(data)), collapse = "|"),
      sep = "||"
    )
    if (exists(cache_key, envir = .efa_cache, inherits = FALSE)) {
      return(get(cache_key, envir = .efa_cache, inherits = FALSE))
    }
  }

  if (is.null(cor_mat)) {
    cor_mat <- cor_matrix_custom(data, cor_method)
  } else {
    cor_mat <- cor_mat[colnames(data), colnames(data), drop = FALSE]
  }

  efa <- NULL
  invisible(utils::capture.output({
    efa <- psych::fa(
      r = cor_mat,
      nfactors = n_factors,
      rotate = rotate,
      fm = extract
    )
  }))

  alpha_val <- alpha_custom(data, use_cache = use_cache)
  loading <- efa$loadings[]
  explained_var <- sum(efa$Vaccounted["SS loadings", seq_len(n_factors)]) / ncol(data)

  result <- list(
    efa = efa,
    alpha = alpha_val,
    explained_var = explained_var,
    loadings = loading
  )

  if (use_cache) {
    assign(cache_key, result, envir = .efa_cache, inherits = FALSE)
  }
  result
}

sort_item_ids <- function(x) {
  gtools::mixedsort(x)
}

identify_problem_items <- function(efa_res, min_loading = 0.30,
                                   loading_diff = 0.10) {
  loadings <- abs(efa_res$loadings)
  items <- rownames(loadings)
  cross_3 <- character(0)
  cross_2 <- character(0)
  low_loading <- character(0)

  if (ncol(loadings) == 0) {
    return(list(cross_3 = character(0), cross_2 = character(0), low_loading = character(0)))
  }

  for (i in seq_len(nrow(loadings))) {
    item_loads <- loadings[i, ]
    primary <- max(item_loads, na.rm = TRUE)

    if (all(item_loads < min_loading, na.rm = TRUE)) {
      low_loading <- c(low_loading, items[i])
      next
    }

    if (ncol(loadings) > 1) {
      sorted_loads <- sort(item_loads, decreasing = TRUE)
      secondary <- sorted_loads[2]

      if (identical(loading_diff, "howard")) {
        is_cross <- howard(primary, secondary)
      } else {
        is_cross <- (primary - secondary) <= as.numeric(loading_diff)
      }

      if (is_cross) {
        n_sig <- sum(item_loads >= min_loading, na.rm = TRUE)
        if (n_sig >= 3) {
          cross_3 <- c(cross_3, items[i])
        } else if (n_sig == 2) {
          cross_2 <- c(cross_2, items[i])
        } else {
          low_loading <- c(low_loading, items[i])
        }
      }
    }
  }

  list(
    cross_3 = unique(cross_3),
    cross_2 = unique(cross_2),
    low_loading = unique(low_loading)
  )
}

get_combinations <- function(items, max_size = NULL) {
  n <- length(items)
  if (n == 0) return(list())
  upper <- if (is.null(max_size)) n else min(n, max_size)
  unlist(
    lapply(seq_len(upper), function(i) utils::combn(items, i, simplify = FALSE)),
    recursive = FALSE
  )
}

build_summary_row <- function(remaining_items, removed_items, efa_out,
                              min_loading, loading_diff,
                              iteration = NA_integer_,
                              prior_removed = character(0)) {
  all_removed <- sort_item_ids(unique(c(prior_removed, removed_items)))
  prob <- identify_problem_items(efa_out, min_loading, loading_diff)
  cross_items <- sort_item_ids(unique(c(prob$cross_2, prob$cross_3)))
  low_items <- sort_item_ids(unique(prob$low_loading))

  load_mat <- as.matrix(efa_out$loadings)
  valid_loads <- abs(load_mat[abs(load_mat) >= min_loading])
  load_min <- if (length(valid_loads) > 0) formatC(min(valid_loads), digits = 2, format = "f") else NA_character_
  load_max <- if (length(valid_loads) > 0) formatC(max(valid_loads), digits = 2, format = "f") else NA_character_
  loading_range <- if (is.na(load_min)) "N/A" else paste0(load_min, "/", load_max)

  data.frame(
    Iteration = iteration,
    Removed_This_Step = if (length(removed_items) == 0) "None" else paste(sort_item_ids(removed_items), collapse = "-"),
    Removed_Items = if (length(all_removed) == 0) "None" else paste(all_removed, collapse = "-"),
    N_Removed = length(all_removed),
    Remaining_Items = paste(sort_item_ids(remaining_items), collapse = "-"),
    N_Remaining = length(remaining_items),
    Total_Explained_Var = efa_out$explained_var,
    Factor_Loading_Range = loading_range,
    Cronbachs_Alpha = efa_out$alpha,
    Has_Cross_Loading = ifelse(length(cross_items) > 0, "Yes", "No"),
    Cross_Loading_Items = ifelse(length(cross_items) > 0, paste(cross_items, collapse = ", "), "None"),
    Has_Low_Loading = ifelse(length(low_items) > 0, "Yes", "No"),
    Low_Loading_Items = ifelse(length(low_items) > 0, paste(low_items, collapse = ", "), "None"),
    stringsAsFactors = FALSE
  )
}

test_removals <- function(data, base_items, combs, n_factors, cor_method,
                          extract, rotate, min_loading, loading_diff,
                          min_items_per_factor = 3, early_exit = FALSE,
                          iteration = NA_integer_,
                          prior_removed = character(0),
                          cor_mat = NULL) {
  min_required <- n_factors * min_items_per_factor
  summary_list <- vector("list", length(combs))

  for (i in seq_along(combs)) {
    removed_items <- unlist(combs[[i]])
    remaining_items <- setdiff(base_items, removed_items)

    if (length(remaining_items) >= min_required) {
      efa_out <- tryCatch(
        efa_custom(
          data[, remaining_items, drop = FALSE],
          n_factors,
          cor_method,
          extract,
          rotate,
          use_cache = TRUE,
          cor_mat = cor_mat
        ),
        error = function(e) NULL
      )

      if (!is.null(efa_out)) {
        row <- build_summary_row(
          remaining_items,
          removed_items,
          efa_out,
          min_loading,
          loading_diff,
          iteration,
          prior_removed = prior_removed
        )

        summary_list[[i]] <- row

        if (early_exit && row$Has_Cross_Loading == "No" && row$Has_Low_Loading == "No") {
          return(row)
        }
      }
    }
  }

  result <- do.call(rbind, summary_list[!vapply(summary_list, is.null, logical(1))])
  if (is.null(result) || nrow(result) == 0) return(data.frame())

  result <- result[!duplicated(result$Removed_Items), ]
  result <- result[order(
    result$Has_Cross_Loading,
    result$Has_Low_Loading,
    -result$Total_Explained_Var,
    result$N_Removed
  ), ]
  row.names(result) <- NULL
  result
}

.parse_item_string <- function(s) {
  if (is.na(s) || s == "None" || nchar(trimws(s)) == 0) return(character(0))
  unlist(strsplit(s, "-"))
}

clean_result_rows <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0) return(data.frame())
  tbl[tbl$Has_Cross_Loading == "No" & tbl$Has_Low_Loading == "No", , drop = FALSE]
}

strategy_row_to_list <- function(row) {
  if (is.null(row) || nrow(row) == 0) return(NULL)
  row <- row[1, , drop = FALSE]
  list(
    row = row,
    removed_items = .parse_item_string(row$Removed_Items),
    remaining_items = .parse_item_string(row$Remaining_Items),
    n_removed = row$N_Removed,
    n_remaining = row$N_Remaining,
    explained_var = row$Total_Explained_Var,
    cronbachs_alpha = row$Cronbachs_Alpha,
    has_cross_loading = row$Has_Cross_Loading,
    cross_loading = .parse_item_string(gsub(", ", "-", row$Cross_Loading_Items)),
    has_low_loading = row$Has_Low_Loading,
    low_loading = .parse_item_string(gsub(", ", "-", row$Low_Loading_Items))
  )
}

build_recommended_strategy <- function(clean_solution, results_all,
                                       initial_efa, current_items) {
  if (!is.null(clean_solution) && !is.null(clean_solution$summary_row)) {
    out <- strategy_row_to_list(clean_solution$summary_row)
    out$source <- "clean_solution"
    return(out)
  }

  results_clean <- clean_result_rows(results_all)
  if (nrow(results_clean) > 0) {
    out <- strategy_row_to_list(results_clean[1, , drop = FALSE])
    out$source <- "clean_results"
    return(out)
  }

  if (!is.null(clean_solution)) {
    efa_out <- if (!is.null(clean_solution$efa)) clean_solution$efa else initial_efa
    return(list(
      row = NULL,
      source = "initial_or_iterative_clean",
      removed_items = clean_solution$removed_items,
      remaining_items = clean_solution$remaining_items,
      explained_var = efa_out$explained_var,
      cronbachs_alpha = efa_out$alpha
    ))
  }

  NULL
}

all_subsets_search <- function(data, base_items, problem_items, n_factors,
                               cor_method, extract, rotate, min_loading,
                               loading_diff, max_removed_per_iter,
                               min_items_per_factor, cor_mat = NULL) {
  if (length(problem_items) == 0) {
    return(list(summary = data.frame(), clean_solution = NULL))
  }

  combs <- get_combinations(problem_items, max_size = max_removed_per_iter)
  tbl <- test_removals(
    data, base_items, combs, n_factors, cor_method, extract, rotate,
    min_loading, loading_diff, min_items_per_factor,
    early_exit = FALSE, iteration = 1L, cor_mat = cor_mat
  )

  clean <- NULL
  if (nrow(tbl) > 0) {
    cand <- tbl[tbl$Has_Cross_Loading == "No" & tbl$Has_Low_Loading == "No", , drop = FALSE]
    if (nrow(cand) > 0) clean <- cand[1, , drop = FALSE]
  }

  list(summary = tbl, clean_solution = clean)
}

stepwise_search <- function(data, current_items, n_factors, cor_method,
                            extract, rotate, min_loading, loading_diff,
                            max_iter, max_removed_total, max_removed_per_iter,
                            min_items_per_factor, cor_mat = NULL) {
  already_removed <- character(0)
  all_iter_results <- list()
  clean_solution <- NULL
  visited <- character(0)

  for (iter in seq_len(max_iter)) {
    current_key <- paste(sort_item_ids(current_items), collapse = "-")
    if (current_key %in% visited) break
    visited <- c(visited, current_key)

    if (!is.null(max_removed_total) && length(already_removed) >= max_removed_total) break
    if (length(current_items) < n_factors * min_items_per_factor) break

    efa_now <- tryCatch(
      efa_custom(data[, current_items, drop = FALSE], n_factors, cor_method, extract, rotate, use_cache = TRUE, cor_mat = cor_mat),
      error = function(e) NULL
    )
    if (is.null(efa_now)) break

    prob <- identify_problem_items(efa_now, min_loading, loading_diff)
    problem_now <- sort_item_ids(unique(c(prob$cross_2, prob$cross_3, prob$low_loading)))

    if (length(problem_now) == 0) {
      clean_solution <- list(
        iteration = iter,
        removed_items = already_removed,
        remaining_items = current_items,
        efa = efa_now
      )
      break
    }

    max_this_round <- max_removed_per_iter
    if (!is.null(max_removed_total)) {
      quota <- max_removed_total - length(already_removed)
      if (quota <= 0) break
      max_this_round <- min(max_removed_per_iter, quota)
    }

    combs <- get_combinations(problem_now, max_size = max_this_round)
    if (length(combs) == 0) break

    iter_tbl <- test_removals(
      data, current_items, combs, n_factors, cor_method, extract, rotate,
      min_loading, loading_diff, min_items_per_factor,
      early_exit = FALSE, iteration = as.integer(iter),
      prior_removed = already_removed, cor_mat = cor_mat
    )

    if (nrow(iter_tbl) == 0) break
    all_iter_results[[iter]] <- iter_tbl

    cand <- iter_tbl[iter_tbl$Has_Cross_Loading == "No" & iter_tbl$Has_Low_Loading == "No", , drop = FALSE]
    if (nrow(cand) > 0) {
      best_clean <- cand[1, , drop = FALSE]
      clean_solution <- list(
        iteration = iter,
        summary_row = best_clean,
        removed_items = .parse_item_string(best_clean$Removed_Items),
        remaining_items = .parse_item_string(best_clean$Remaining_Items)
      )
      break
    }

    best_mid <- iter_tbl[1, , drop = FALSE]
    already_removed <- .parse_item_string(best_mid$Removed_Items)
    current_items <- .parse_item_string(best_mid$Remaining_Items)
  }

  list(
    clean_solution = clean_solution,
    iteration_results = all_iter_results,
    final_removed = already_removed,
    final_remaining = current_items
  )
}

backward_search <- function(data, current_items, problem_items, n_factors,
                            cor_method, extract, rotate, min_loading,
                            loading_diff, max_iter, max_removed_total,
                            min_items_per_factor, cor_mat = NULL) {
  already_removed <- character(0)
  all_iter_results <- list()
  clean_solution <- NULL
  visited <- character(0)

  for (iter in seq_len(max_iter)) {
    current_key <- paste(sort_item_ids(current_items), collapse = "-")
    if (current_key %in% visited) break
    visited <- c(visited, current_key)

    if (!is.null(max_removed_total) && length(already_removed) >= max_removed_total) break
    if (length(current_items) < n_factors * min_items_per_factor) break

    efa_now <- tryCatch(
      efa_custom(data[, current_items, drop = FALSE], n_factors, cor_method, extract, rotate, use_cache = TRUE, cor_mat = cor_mat),
      error = function(e) NULL
    )
    if (is.null(efa_now)) break

    prob_now <- identify_problem_items(efa_now, min_loading, loading_diff)
    problem_now <- sort_item_ids(intersect(
      unique(c(prob_now$cross_2, prob_now$cross_3, prob_now$low_loading)),
      current_items
    ))

    if (length(problem_now) == 0) {
      clean_solution <- list(
        iteration = iter,
        removed_items = already_removed,
        remaining_items = current_items,
        efa = efa_now
      )
      break
    }

    round_tbl <- test_removals(
      data, current_items, as.list(problem_now),
      n_factors, cor_method, extract, rotate,
      min_loading, loading_diff, min_items_per_factor,
      early_exit = FALSE, iteration = as.integer(iter),
      prior_removed = already_removed, cor_mat = cor_mat
    )

    if (nrow(round_tbl) == 0) break
    all_iter_results[[iter]] <- round_tbl
    best <- round_tbl[1, , drop = FALSE]

    if (best$Has_Cross_Loading == "No" && best$Has_Low_Loading == "No") {
      clean_solution <- list(
        iteration = iter,
        summary_row = best,
        removed_items = .parse_item_string(best$Removed_Items),
        remaining_items = .parse_item_string(best$Remaining_Items)
      )
      break
    }

    newly <- .parse_item_string(best$Removed_This_Step)
    if (length(newly) == 0 || any(newly %in% already_removed)) break

    already_removed <- c(already_removed, newly)
    current_items <- .parse_item_string(best$Remaining_Items)
  }

  list(
    clean_solution = clean_solution,
    iteration_results = all_iter_results,
    final_removed = already_removed,
    final_remaining = current_items
  )
}

itemrest <- function(data,
                     cor_method = "pearson",
                     n_factors = NULL,
                     extract = "uls",
                     rotate = "oblimin",
                     min_loading = 0.30,
                     loading_diff = 0.10,
                     search = "all_subsets",
                     max_iter = 20,
                     max_removed_total = NULL,
                     max_removed_per_iter = 3,
                     min_items_per_factor = 3) {
  search <- match.arg(search, c("all_subsets", "stepwise", "backward"))
  cor_method <- match.arg(tolower(cor_method), c("pearson", "spearman", "polychoric"))

  data <- as.data.frame(data)
  colnames(data) <- gsub("\\.", "_", colnames(data))

  clear_efa_cache()
  full_cor_mat <- cor_matrix_custom(data, cor_method)

  auto_n_factors <- is.null(n_factors)
  if (auto_n_factors) {
    n_factors <- determine_n_factors(data, cor_method, cor_mat = full_cor_mat)
  }
  n_factors <- as.integer(n_factors)

  descriptives <- descriptive_stats(data)
  initial_efa <- efa_custom(data, n_factors, cor_method, extract, rotate, use_cache = TRUE, cor_mat = full_cor_mat)
  problem_items <- identify_problem_items(initial_efa, min_loading, loading_diff)
  all_problem <- sort_item_ids(unique(c(problem_items$cross_3, problem_items$cross_2, problem_items$low_loading)))

  repro_snippet <- if (cor_method == "polychoric") {
    paste0(
      "cor_mat <- get_cor_matrix(data, cor_method = \"polychoric\")\n",
      "psych::fa(r = cor_mat, nfactors = ", n_factors, ", ",
      "rotate = \"", rotate, "\", fm = \"", extract, "\")"
    )
  } else {
    NULL
  }

  removal_summary <- NULL
  clean_solution <- NULL
  search_result <- NULL
  current_items <- colnames(data)

  if (length(all_problem) > 0) {
    if (search == "all_subsets") {
      search_result <- all_subsets_search(
        data, current_items, all_problem, n_factors, cor_method,
        extract, rotate, min_loading, loading_diff,
        max_removed_per_iter, min_items_per_factor, cor_mat = full_cor_mat
      )
      removal_summary <- search_result$summary
      clean_solution <- search_result$clean_solution
    } else if (search == "stepwise") {
      search_result <- stepwise_search(
        data, current_items, n_factors, cor_method, extract, rotate,
        min_loading, loading_diff, max_iter, max_removed_total,
        max_removed_per_iter, min_items_per_factor, cor_mat = full_cor_mat
      )
      removal_summary <- do.call(rbind, search_result$iteration_results)
      clean_solution <- search_result$clean_solution
    } else if (search == "backward") {
      search_result <- backward_search(
        data, current_items, all_problem, n_factors, cor_method,
        extract, rotate, min_loading, loading_diff,
        max_iter, max_removed_total, min_items_per_factor, cor_mat = full_cor_mat
      )
      removal_summary <- do.call(rbind, search_result$iteration_results)
      clean_solution <- search_result$clean_solution
    }
  } else {
    clean_solution <- list(
      iteration = 0L,
      removed_items = character(0),
      remaining_items = current_items,
      efa = initial_efa
    )
  }

  if (is.null(removal_summary)) {
    removal_summary <- data.frame()
  }

  results_all <- removal_summary
  results_clean <- clean_result_rows(results_all)
  recommended <- build_recommended_strategy(clean_solution, results_all, initial_efa, current_items)

  initial_summary <- list(
    cronbachs_alpha = initial_efa$alpha,
    explained_var = initial_efa$explained_var,
    cross_loading_items = sort_item_ids(unique(c(problem_items$cross_2, problem_items$cross_3))),
    low_loading_items = sort_item_ids(problem_items$low_loading),
    all_problem_items = all_problem
  )

  output <- list(
    all = results_all,
    clean = recommended,
    results_all = results_all,
    results_clean = results_clean,
    recommended = recommended,
    initial = initial_summary,
    descriptive_stats = descriptives,
    initial_efa = initial_efa,
    problem_items = problem_items,
    all_problem_items = all_problem,
    all_problem_items_combined = all_problem,
    removal_summary = results_all,
    clean_solution = clean_solution,
    search_result = search_result,
    replication_code = repro_snippet,
    settings = list(
      n_factors = n_factors,
      auto_n_factors = auto_n_factors,
      cor_method = cor_method,
      extract = extract,
      rotate = rotate,
      min_loading = min_loading,
      loading_diff = loading_diff,
      search = search,
      max_iter = max_iter,
      max_removed_total = max_removed_total,
      max_removed_per_iter = max_removed_per_iter,
      min_items_per_factor = min_items_per_factor
    )
  )
  class(output) <- "itemrest_result"
  output
}

#' Print an itemrest result.
#' @param x Object of class itemrest_result.
#' @param report Report scope: clean or all.
#' @param ... Not used.
#' @return Invisibly returns x.
#' @export
print.itemrest_result <- function(x, report = "clean", ...) {
  report <- match.arg(report, c("clean", "all"))
  results_all <- if (!is.null(x$results_all)) x$results_all else x$removal_summary

  if (report == "clean") {
    if (is.null(x$recommended)) {
      cat("No clean strategy was found.\n")
    } else if (is.null(x$recommended$row)) {
      cat("Recommended strategy:\n")
      cat("Items removed:", if (length(x$recommended$removed_items) == 0) "None" else paste(sort_item_ids(x$recommended$removed_items), collapse = ", "), "\n")
      cat("Alpha:", formatC(x$recommended$cronbachs_alpha, digits = 3, format = "f"), "\n")
      cat("Explained var:", paste0(formatC(x$recommended$explained_var * 100, format = "f", digits = 2), "%"), "\n")
    } else {
      print(x$recommended$row, row.names = FALSE)
    }
  } else if (is.null(results_all) || nrow(results_all) == 0) {
    cat("No removal strategies evaluated.\n")
  } else {
    print(results_all, row.names = FALSE)
  }

  invisible(x)
}
