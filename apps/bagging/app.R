# Bagging a regression tree, one resample at a time
#
# Companion app for the bagging section. The data are the 1D nonlinear
# example from the bagging/random-forest deck: n = 100, x ~ Unif(0, 10),
# y = sin(x) + 0.3 x + noise. Each click draws a bootstrap resample, fits a
# regression tree to it, and adds that tree to the ensemble. Three panels:
#
#   1. The current resample: out-of-bag points fade to grey, included points
#      grow with the number of times they were drawn, and the tree fit to
#      this resample is the orange step function.
#   2. The ensemble: every tree so far in faint orange, the current tree
#      highlighted in orange, and the bagged fit -- the average of all trees -- in
#      green on top.
#   3. Out-of-bag MSE as a function of the number of trees.
#
# This native app uses ggplot2 for local inspection. The browser page reads
# app_shinylive.R during rendering; both variants share the simulation and UI.
#
# Run locally with shiny::runApp("apps/bagging/app.R").

library(shiny)
library(bslib)
library(rpart)
library(ggplot2)

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

theme_bag <- theme_minimal() +
  theme(axis.title = element_text(size = 13),
        axis.text  = element_text(size = 11),
        legend.position = "none")

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

  empty_panel <- function(msg) {
    ggplot(data, aes(x = x, y = y)) +
      geom_point(size = 1.5, alpha = 0.6, color = data_color) +
      coord_cartesian(ylim = Y_LIM) +
      labs(subtitle = msg) +
      theme_bag +
      theme(plot.subtitle = element_text(color = "grey50", size = 12))
  }

  # Panel 1: the current resample and the tree fit to it
  output$resample_plot <- renderPlot(width = safe_width("resample_plot"), {
    if (is.null(values$pred_mat)) {
      return(empty_panel("Take a resample to see it here"))
    }
    plot_data <- data
    plot_data$sample_count <- values$current_counts
    plot_data$sampled <- plot_data$sample_count > 0

    B <- ncol(values$pred_mat)
    tree_data <- data.frame(x = x_pred, y_pred = values$pred_mat[, B])

    ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(data = subset(plot_data, !sampled),
                 color = oob_color, size = 1.5, alpha = 0.5) +
      geom_point(data = subset(plot_data, sampled),
                 aes(size = sample_count), alpha = 0.7) +
      geom_step(data = tree_data, aes(x = x, y = y_pred), direction = "hv",
                color = tree_color, linewidth = 1, inherit.aes = FALSE) +
      scale_size_continuous(range = c(1, 3)) +
      coord_cartesian(ylim = Y_LIM) +
      theme_bag
  })

  # Panel 2: all trees so far, plus their average
  output$ensemble_plot <- renderPlot(width = safe_width("ensemble_plot"), {
    if (is.null(values$pred_mat)) {
      return(empty_panel("The bagged fit builds up here"))
    }
    B <- ncol(values$pred_mat)

    p <- ggplot(data, aes(x = x, y = y)) +
      geom_point(size = 1.5, alpha = 0.6, color = data_color)

    if (B > 1) {
      prev <- data.frame(
        x = rep(x_pred, B - 1),
        y_pred = as.vector(values$pred_mat[, -B, drop = FALSE]),
        tree_id = rep(seq_len(B - 1), each = length(x_pred))
      )
      p <- p + geom_step(data = prev,
                         aes(x = x, y = y_pred, group = tree_id),
                         color = prev_color, alpha = 0.5,
                         linewidth = 0.4, direction = "hv",
                         inherit.aes = FALSE)
    }

    current <- data.frame(x = x_pred, y_pred = values$pred_mat[, B])
    bagged  <- data.frame(x = x_pred, y_pred = rowMeans(values$pred_mat))

    p +
      geom_step(data = current, aes(x = x, y = y_pred), direction = "hv",
                color = tree_color, linewidth = 1, inherit.aes = FALSE) +
      geom_step(data = bagged, aes(x = x, y = y_pred), direction = "hv",
                color = bag_color, linewidth = 2, inherit.aes = FALSE) +
      coord_cartesian(ylim = Y_LIM) +
      theme_bag
  })

  # Panel 3: OOB MSE against the number of trees
  output$oob_plot <- renderPlot(width = safe_width("oob_plot"), {
    if (length(values$oob_mse) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Take resamples to track the out-of-bag error",
                   color = "grey50", size = 4.5) +
          theme_void()
      )
    }
    oob_data <- data.frame(B = seq_along(values$oob_mse),
                           mse = values$oob_mse)
    total <- max(oob_data$B)
    tree_ticks <- tree_axis_ticks(total)
    p <- ggplot(oob_data, aes(x = B, y = mse)) +
      geom_point(color = bag_color, size = 1.5)
    if (total > 1) {
      p <- p + geom_line(color = bag_color, linewidth = 1)
    }
    p +
      scale_x_continuous(breaks = tree_ticks,
                         limits = c(1, max(10, total))) +
      labs(x = "Number of trees", y = "Out-of-bag MSE") +
      theme_bag
  })

  output$n_trees <- renderText({
    B <- if (is.null(values$pred_mat)) 0 else ncol(values$pred_mat)
    paste("Resamples (trees) so far:", B)
  })
}

shinyApp(ui = ui, server = server)
