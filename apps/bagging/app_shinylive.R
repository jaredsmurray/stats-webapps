# Bagging a regression tree, one resample at a time — shinylive port
#
# This base-graphics implementation is embedded by bagging.qmd. The native
# ggplot2 variant lives in app.R and shares the simulation and UI behavior.
#
# Run locally with shiny::runApp("apps/bagging/app_shinylive.R").

library(shiny)
library(bslib)
library(rpart)

# --- Data ---------------------------------------------------------------
# Same DGP and seed as the deck, so the scatterplot matches its figures.
set.seed(1)
n <- 100
x <- runif(n, 0, 10)
y <- sin(x) + 0.3 * x + rnorm(n, 0, 0.5)
data <- data.frame(x = x, y = y)

x_pred <- seq(0, 10, length.out = 200)
Y_LIM  <- range(y) + c(-0.3, 0.3)
MAX_K  <- 1000

tree_axis_ticks <- function(total) {
  upper <- max(10, total)
  ticks <- pretty(c(1, upper), n = 6)
  ticks <- ticks[is.finite(ticks) & ticks >= 1 & ticks <= upper]
  spacing <- if (length(ticks) > 1) min(diff(ticks)) else max(1, upper - 1)
  endpoint_gap <- 0.75 * spacing
  if (!any(ticks == 1) && (total <= 10 || length(ticks) == 0 ||
                           ticks[1] - 1 >= endpoint_gap)) {
    ticks <- c(1, ticks)
  }
  if (!any(ticks == total) && total > 1 &&
      (length(ticks) == 0 || total - max(ticks) >= endpoint_gap)) {
    ticks <- c(ticks, total)
  }
  sort(unique(as.integer(ticks)))
}

# --- Colors -------------------------------------------------------------
tree_color <- "#DE8F05"   # orange: the current tree's fit
prev_color <- "#F2CD9B"   # light orange tint: earlier trees' fits
bag_color  <- "#029E73"   # green: the bagged (averaged) fit
oob_color  <- "gray70"    # points left out of the current resample
data_color <- "gray30"    # background data in the ensemble panel

# --- Resampling and fitting ---------------------------------------------
create_bootstrap_sample <- function(data) {
  n <- nrow(data)
  sample_indices <- sample(1:n, n, replace = TRUE)
  sample_counts <- table(factor(sample_indices, levels = 1:n))
  list(
    sampled_data  = data[sample_indices, ],
    sample_counts = as.numeric(sample_counts)
  )
}

fit_bagged_tree <- function(sampled_data) {
  rpart(y ~ x, data = sampled_data,
        control = rpart.control(cp = 0.01, minsplit = 5))
}

ui <- page_sidebar(
  title = "Bagging regression trees",

  tags$head(
    tags$style(HTML("
      * { font-family: 'Arial', 'Helvetica', sans-serif !important; }
      .bslib-page-main { min-width: 0 !important; }
      #resample_plot, #resample_plot img,
      #ensemble_plot, #ensemble_plot img { min-height: 350px !important; }
      #oob_plot, #oob_plot img { min-height: 260px !important; }
    "))
  ),

  sidebar = sidebar(
    width = 300,

    p(style = "font-size: 0.92em; margin-bottom: 10px;",
      "The app uses a simulated dataset of 100 observations."),

    div(
      style = "margin-top: 4px;",
      actionButton("draw_one", "Take 1 resample",
                   class = "btn-primary",
                   style = "margin-bottom: 5px; width: 100%;"),
      div(
        style = "margin-bottom: 10px;",
        numericInput("k", label = "Number of trees", value = 50,
                     min = 1, max = MAX_K, width = "100%"),
        actionButton("draw_many", "Take many resamples",
                     style = "width: 100%;")
      ),
      actionButton("clear", "Clear history",
                   class = "btn-outline-secondary",
                   style = "width: 100%; margin-bottom: 10px;")
    ),

    hr(style = "margin: 10px 0;"),
    div(style = "font-size: 0.9em;", textOutput("n_trees"))
  ),

  layout_columns(
    col_widths = breakpoints(
      xs = c(12, 12, 12), sm = c(12, 12, 12), md = c(12, 12, 12),
      lg = c(6, 6, 12)
    ),
    fill = FALSE,
    fillable = FALSE,

    card(
      card_header("Current resample and its tree"),
      card_body(plotOutput("resample_plot", height = "350px", fill = FALSE),
                fill = FALSE),
      card_footer(
        style = "font-size: 0.85em;",
        "Grey points are absent from this resample. Larger points appear more often in this resample."
      ),
      fill = FALSE
    ),

    card(
      card_header("The ensemble and the bagged fit"),
      card_body(plotOutput("ensemble_plot", height = "350px", fill = FALSE),
                fill = FALSE),
      card_footer(
        style = "font-size: 0.85em;",
        "Orange: individual trees; current tree highlighted. Green: their average."
      ),
      fill = FALSE
    ),

    card(
      card_header("Out-of-bag error"),
      card_body(plotOutput("oob_plot", height = "260px", fill = FALSE),
                fill = FALSE),
      card_footer(
        style = "font-size: 0.85em;",
        "OOB predictions average only trees fitted without that observation."
      ),
      fill = FALSE
    )
  )
)

server <- function(input, output, session) {

  # The browser can report a zero width for an instant at startup (before the
  # layout settles), which crashes the graphics device. Fall back to a sane
  # default until a real measurement arrives.
  safe_width <- function(id, default = 420) {
    function() {
      w <- session$clientData[[paste0("output_", id, "_width")]]
      if (is.null(w) || !is.finite(w) || w < 50) default else w
    }
  }

  values <- reactiveValues(
    pred_mat       = NULL,         # 200 x B: each tree's grid predictions
    current_counts = NULL,         # inclusion counts of the latest resample
    oob_sum        = numeric(n),   # running sum of OOB predictions per point
    oob_n          = integer(n),   # number of trees for which point was OOB
    oob_mse        = numeric(0)    # OOB MSE after each tree
  )

  reset_history <- function() {
    values$pred_mat       <- NULL
    values$current_counts <- NULL
    values$oob_sum        <- numeric(n)
    values$oob_n          <- integer(n)
    values$oob_mse        <- numeric(0)
  }
  observeEvent(input$clear, reset_history())

  add_trees <- function(k) {
    new_cols <- matrix(NA_real_, nrow = length(x_pred), ncol = k)
    new_mse  <- numeric(k)
    oob_sum  <- values$oob_sum
    oob_n    <- values$oob_n
    counts   <- NULL

    for (b in seq_len(k)) {
      bs  <- create_bootstrap_sample(data)
      fit <- fit_bagged_tree(bs$sampled_data)
      new_cols[, b] <- predict(fit, newdata = data.frame(x = x_pred))

      oob <- bs$sample_counts == 0
      if (any(oob)) {
        oob_sum[oob] <- oob_sum[oob] +
          predict(fit, newdata = data[oob, , drop = FALSE])
        oob_n[oob] <- oob_n[oob] + 1L
      }
      have <- oob_n > 0
      new_mse[b] <- mean((y[have] - oob_sum[have] / oob_n[have])^2)
      counts <- bs$sample_counts
    }

    values$pred_mat       <- cbind(values$pred_mat, new_cols)
    values$oob_sum        <- oob_sum
    values$oob_n          <- oob_n
    values$oob_mse        <- c(values$oob_mse, new_mse)
    values$current_counts <- counts
  }

  observeEvent(input$draw_one, add_trees(1))
  observeEvent(input$draw_many, {
    k <- input$k
    if (length(k) != 1L || !is.numeric(k) || !is.finite(k)) {
      showNotification("Enter a number of trees from 1 to 1,000.",
                       type = "warning")
      return()
    }
    k <- max(1, min(MAX_K, round(k)))
    updateNumericInput(session, "k", value = k)
    add_trees(k)
  })

  # Shared scaffold: the scatter axes every panel draws on
  open_panel <- function() {
    par(mar = c(4.2, 4.2, 1.2, 0.8))
    plot(NULL, xlim = c(0, 10), ylim = Y_LIM,
         xlab = "x", ylab = "y")
  }

  empty_panel <- function(msg) {
    open_panel()
    points(x, y, pch = 19, cex = 0.9,
           col = adjustcolor(data_color, 0.6))
    mtext(msg, side = 3, line = 0.1, col = "grey50", cex = 0.95)
  }

  # Panel 1: the current resample and the tree fit to it
  output$resample_plot <- renderPlot(width = safe_width("resample_plot"), {
    if (is.null(values$pred_mat)) {
      return(empty_panel("Take a resample to see it here"))
    }
    counts  <- values$current_counts
    sampled <- counts > 0
    # cex 0.9 to 1.9 stands in for ggplot's size range c(1, 3)
    cex_in <- 0.9 + (counts[sampled] - 1) / max(1, max(counts) - 1)

    open_panel()
    points(x[!sampled], y[!sampled], pch = 19, cex = 0.9,
           col = adjustcolor(oob_color, 0.5))
    points(x[sampled], y[sampled], pch = 19, cex = cex_in,
           col = adjustcolor("black", 0.7))
    B <- ncol(values$pred_mat)
    lines(x_pred, values$pred_mat[, B], type = "s", col = tree_color, lwd = 2)
  })

  # Panel 2: all trees so far, plus their average
  output$ensemble_plot <- renderPlot(width = safe_width("ensemble_plot"), {
    if (is.null(values$pred_mat)) {
      return(empty_panel("The bagged fit builds up here"))
    }
    B <- ncol(values$pred_mat)

    open_panel()
    points(x, y, pch = 19, cex = 0.9, col = adjustcolor(data_color, 0.6))
    if (B > 1) {
      matlines(x_pred, values$pred_mat[, -B, drop = FALSE], type = "s",
               col = adjustcolor(prev_color, 0.5), lty = 1, lwd = 1)
    }
    lines(x_pred, values$pred_mat[, B], type = "s", col = tree_color, lwd = 2)
    lines(x_pred, rowMeans(values$pred_mat), type = "s", col = bag_color, lwd = 4)
  })

  # Panel 3: OOB MSE against the number of trees, on a linear x scale
  output$oob_plot <- renderPlot(width = safe_width("oob_plot"), {
    if (length(values$oob_mse) == 0) {
      par(mar = c(0.5, 0.5, 0.5, 0.5))
      plot.new()
      text(0.5, 0.5, "Take resamples to track\nthe out-of-bag error",
           col = "grey50", cex = 1.2)
      return(invisible())
    }
    mse <- values$oob_mse
    Bs  <- seq_along(mse)

    max_trees <- max(10, max(Bs))
    tree_ticks <- tree_axis_ticks(max(Bs))

    par(mar = c(4.2, 4.2, 1.2, 0.8))
    plot(Bs, mse, type = "o", pch = 19, cex = 0.7,
         col = bag_color, lwd = 2, axes = FALSE,
         xlab = "Number of trees", ylab = "Out-of-bag MSE",
         xlim = c(1, max_trees))
    axis(1, at = tree_ticks, labels = tree_ticks)
    axis(2)
    box(bty = "l")
  })

  output$n_trees <- renderText({
    B <- if (is.null(values$pred_mat)) 0 else ncol(values$pred_mat)
    paste("Resamples (trees) so far:", B)
  })
}

shinyApp(ui = ui, server = server)
