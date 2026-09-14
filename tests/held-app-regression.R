# Focused server checks for the held bootstrap and bagging apps.
# Run from the repository root with: Rscript tests/held-app-regression.R

library(shiny)

assert_close <- function(actual, expected, tolerance = 1e-10) {
  stopifnot(length(actual) == length(expected))
  stopifnot(max(abs(actual - expected)) <= tolerance)
}

expected_sap_roe <- c(
  21.1, 15.8, 1.2, 61.2, 15.2, 26.4, 66.9, 5.4, 12.2, 4.9, 20.8, 23.3, 13.2,
  14.5, 27.3, 17, -41.1, 35.9, 116.4, 8.1, 7.9, 6.7, 32.7, 11.6, 17.1, -47.5,
  23.8, 1.1, 26.7, 6.9, 13.4, 14.9, 14.9, 14.6, -15.4, 4.6, 43.9, 15.8, 6.4,
  13.1, 14.6, 8.4, 8.9, 8.8, 23.1, -91.8, 45.8, 7.3, 18.9, 22.8, -7.7, 6.8,
  -9.2, -62.6, 18.7, 27.3, 8.3, 16.7, 18.4, 28.8, 25.6, 16.9, 0.3, -0.7,
  -38.5, 8.3, 8.3, 8.3, 8.3, 6.2, 6.2, -0.5, 18.6, 0.9, 47.4, 27.3, 4.7, 2.8,
  41.4, 26, 14.6
)

run_bootstrap_checks <- function() {
  e <- new.env(parent = globalenv())
  source("apps/sap_bootstrap/app.R", local = e)
  stopifnot(identical(e$SAP_ROE, expected_sap_roe))
  stopifnot(length(e$SAP_ROE) == 81L)

  testServer(e$server, {
    session$setInputs(
      k = 2, bw_means = 0.5, show_normal = FALSE,
      sort_resample = TRUE, color_original = TRUE, color_resample = TRUE
    )
    trigger <- 0L
    batch <- function(k) {
      trigger <<- trigger + 1L
      session$setInputs(k = k)
      session$setInputs(draw_many = trigger)
    }
    count <- function() length(values$mean_history)

    session$setInputs(draw_one = 1)
    stopifnot(count() == 1L, length(values$current_idx) == 81L)
    before <- values$mean_history
    for (bad in list(NA_real_, NULL, NaN, Inf, -Inf, c(1, 2), "2")) {
      batch(bad)
      stopifnot(identical(values$mean_history, before))
    }

    batch(2.4)
    stopifnot(count() == 3L)
    batch(0)
    stopifnot(count() == 4L)
    batch(10001)
    stopifnot(count() == 10004L)
    batch(1)
    stopifnot(count() == 10005L)

    before <- values$mean_history
    session$setInputs(show_normal = TRUE, bw_means = 1)
    stopifnot(identical(values$mean_history, before))

    values$mean_history <- c(min(e$SAP_ROE), max(e$SAP_ROE))
    values$current_idx <- rep(which.max(e$SAP_ROE), e$N_S)
    session$setInputs(bw_means = 1.1)
    plotted <- output$dist_plot
    domain <- plotted$coordmap$panels[[1]]$domain
    stopifnot(domain$left <= min(e$SAP_ROE), domain$right >= max(e$SAP_ROE))

    session$setInputs(clear = 1)
    stopifnot(count() == 0L, length(values$current_idx) == 0L)
    batch(1)
    stopifnot(count() == 1L)
    session$setInputs(clear = 2)
    stopifnot(count() == 0L, length(values$current_idx) == 0L)
  })
  cat("apps/sap_bootstrap/app.R: held server, data, input, range, toggle, reset PASS\n")
}

reference_bagging <- function(seed, data, x_pred, n_trees) {
  set.seed(seed)
  n <- nrow(data)
  predictions <- matrix(NA_real_, nrow = length(x_pred), ncol = n_trees)
  oob_sum <- numeric(n)
  oob_n <- integer(n)
  oob_mse <- numeric(n_trees)

  for (b in seq_len(n_trees)) {
    indices <- sample(seq_len(n), n, replace = TRUE)
    counts <- tabulate(indices, nbins = n)
    fit <- rpart::rpart(
      y ~ x, data = data[indices, ],
      control = rpart::rpart.control(cp = 0.01, minsplit = 5)
    )
    predictions[, b] <- predict(fit, newdata = data.frame(x = x_pred))
    oob <- counts == 0
    if (any(oob)) {
      oob_sum[oob] <- oob_sum[oob] + predict(fit, newdata = data[oob, , drop = FALSE])
      oob_n[oob] <- oob_n[oob] + 1L
    }
    have <- oob_n > 0
    oob_mse[b] <- mean((data$y[have] - oob_sum[have] / oob_n[have])^2)
  }
  list(predictions = predictions, oob_mse = oob_mse)
}

run_bagging_checks <- function(file) {
  e <- new.env(parent = globalenv())
  source(file, local = e)
  stopifnot(1L %in% e$tree_axis_ticks(1),
            all(diff(e$tree_axis_ticks(1101)) > 0))

  seed <- 2468
  expected <- reference_bagging(seed, e$data, e$x_pred, 3)
  testServer(e$server, {
    session$setInputs(k = 3)
    set.seed(seed)
    session$setInputs(draw_many = 1)
    stopifnot(ncol(values$pred_mat) == 3L, length(values$oob_mse) == 3L)
    assert_close(as.vector(values$pred_mat), as.vector(expected$predictions))
    assert_close(values$oob_mse, expected$oob_mse)
    stopifnot(nchar(output$resample_plot$src) > 0,
              nchar(output$ensemble_plot$src) > 0,
              nchar(output$oob_plot$src) > 0)

    trigger <- 1L
    batch <- function(k) {
      trigger <<- trigger + 1L
      session$setInputs(k = k)
      session$setInputs(draw_many = trigger)
    }
    tree_count <- function() ncol(values$pred_mat)
    before <- values$pred_mat
    before_mse <- values$oob_mse
    for (bad in list(NA_real_, NULL, NaN, Inf, -Inf, c(1, 2), "2")) {
      batch(bad)
      stopifnot(identical(values$pred_mat, before))
      stopifnot(identical(values$oob_mse, before_mse))
    }

    batch(2.4)
    stopifnot(tree_count() == 5L, length(values$oob_mse) == 5L)
    batch(0)
    stopifnot(tree_count() == 6L)
    batch(1001)
    stopifnot(tree_count() == 1006L, length(values$oob_mse) == 1006L)
    oob_plot <- output$oob_plot
    stopifnot(is.null(oob_plot$coordmap$panels[[1]]$log$x))
    batch(1)
    stopifnot(tree_count() == 1007L)

    session$setInputs(clear = 1)
    stopifnot(is.null(values$pred_mat), length(values$oob_mse) == 0L,
              is.null(values$current_counts))
    batch(1)
    stopifnot(tree_count() == 1L, length(values$oob_mse) == 1L)
    session$setInputs(clear = 2)
    stopifnot(is.null(values$pred_mat), length(values$oob_mse) == 0L)
  })
  cat(file, ": held server, direct predictions/OOB, input, cap, reset PASS\n")
}

run_bootstrap_checks()
for (file in c("apps/bagging/app.R", "apps/bagging/app_shinylive.R")) {
  run_bagging_checks(file)
}
