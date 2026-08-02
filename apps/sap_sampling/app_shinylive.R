# Sampling distribution of the sample mean: return on equity (ROE) at SAP
#
# Base-graphics port of app.R for shinylive (the site's shinylive bundle can
# only include CRAN packages, and this project's ggplot2 is a GitHub install).
# Keep the two apps in sync by hand: same population, controls, and panels.
# This file is embedded verbatim in sap_sampling_app.qmd at the repo root.
#
# Run locally with shiny::runApp("webapps/sap_sampling_app/app_shinylive.R").

library(shiny)
library(bslib)

# --- Data ---------------------------------------------------------------
# 81 ROE values, a frozen bootstrap resample of SAP customers drawn by
# working/sapsim.R from working/sap.csv. The resample was generated without a
# seed and cannot be regenerated, so the values are inlined here verbatim (full
# precision, extracted from working/sap_resample.csv). Do NOT regenerate.
SAP_ROE <- c(
  -0.120396544, -0.100691091, 0.167590054, 0.137871816, -0.008553021, 0.137871816,
  0.125869109, 0.029765192, -0.044352879, 0.252452635, 0.181038292, -0.023422169,
  0.048956074, 0.127671050, 0.410341235, 0.258652282, 0.034964973, 0.258652282,
  0.256746830, -0.100691091, 0.129724225, 0.034964973, 0.803444112, 0.258652282,
  0.803444112, 0.167590054, -0.018913042, 0.509901026, 0.388392828, 0.148384173,
  0.167590054, 0.627228636, -0.083626775, 0.357408018, 0.188396498, 0.009950306,
  0.012788839, 0.448264021, -0.083626775, 0.033139821, 0.129724225, 0.380798900,
  -0.095886669, 0.009950306, -0.018913042, 0.048956074, -0.045643574, 0.027237541,
  0.200929684, 0.006652830, -0.023422169, 0.076985554, 0.027237541, -0.045643574,
  0.137871816, 0.048956074, -0.038963332, 0.127671050, 0.033574695, -0.074997475,
  0.129724225, 0.240981928, 0.001447413, 0.181038292, 0.188396498, -0.100691091,
  0.132512837, 0.236645754, 0.252452635, 0.200929684, 0.803444112, -0.042335084,
  0.034964973, 0.258652282, -0.044352879, -0.097850051, 0.188643477, 0.015643796,
  0.057244759, -0.038963332, -0.014643583
)
stopifnot(length(SAP_ROE) == 81)

# --- Fake population (built once at startup) ---------------------------
# Smooth the 81 resampled values with a Gaussian kernel (bandwidth from the
# Silverman default) to make a plausible continuous population. Any draw below
# -1 (a hard ROE floor) is rejected and redrawn; the loop essentially never
# fires -- it is only a guard.
set.seed(2026)
KDE_BW <- density(SAP_ROE)$bw            # about 0.0601
M_POP  <- 30000
pop <- sample(SAP_ROE, M_POP, replace = TRUE) + rnorm(M_POP, 0, KDE_BW)
bad <- which(pop < -1)
while (length(bad) > 0) {
  pop[bad] <- sample(SAP_ROE, length(bad), replace = TRUE) +
    rnorm(length(bad), 0, KDE_BW)
  bad <- which(pop < -1)
}
# Display in percent (12.6 rather than 0.126), matching the chapter's units.
pop <- 100 * pop
POP_MEAN <- mean(pop)
POP_SD   <- sd(pop)
POP_XLIM <- range(pop)

# --- Colors -------------------------------------------------------------
bar_color    <- "#0173B2"   # blue: distribution / sample bars
mean_color   <- "grey30"    # solid line at the population mean
normal_color <- "#9370DB"   # purple: normal approximation
mark_color   <- "#CC0000"   # red X marking the current sample mean
hit_color    <- "#2E8B57"   # sea green: this sample's CI captures mu
miss_color   <- "#CC0000"   # red: this sample's CI misses mu

pct_axis <- function() {
  at <- pretty(POP_XLIM, n = 6)
  axis(1, at = at, labels = paste0(at, "%"))
}

stats_box <- function(lines) {
  usr <- par("usr")
  text(usr[2] - 0.02 * diff(usr[1:2]), usr[4] - 0.05 * diff(usr[3:4]),
       paste(lines, collapse = "\n"), adj = c(1, 1), cex = 1.05, font = 2,
       col = "gray20")
}

ui <- page_sidebar(
  title = "Sampling Distribution of the Sample Mean — SAP Customer ROE",

  tags$head(
    tags$style(HTML("
      * { font-family: 'Arial', 'Helvetica', sans-serif !important; }
    "))
  ),

  sidebar = sidebar(
    width = 300,

    sliderInput("n", "Sample size (n):",
                min = 1, max = 100, value = 25, step = 1, ticks = FALSE),
    numericInput("n_typed", "...or type a sample size:",
                 value = 25, min = 1, max = 100, step = 1),

    div(
      style = "margin-top: 10px;",
      actionButton("take_one", "Take 1 sample",
                   class = "btn-primary",
                   style = "margin-bottom: 5px; width: 100%;"),
      div(
        style = "display: flex; gap: 5px; margin-bottom: 5px;",
        numericInput("k", label = NULL, value = 50, min = 1, max = 10000,
                     width = "80px"),
        actionButton("take_many", "Take many samples",
                     style = "flex: 1;")
      ),
      actionButton("clear", "Clear history",
                   class = "btn-outline-secondary",
                   style = "width: 100%; margin-bottom: 10px;")
    ),

    checkboxInput("show_normal", "Show normal approximation", value = FALSE),
    checkboxInput("show_ci", "Show 95% confidence interval", value = FALSE),

    sliderInput("bw_means", "Bin width (sample means, % points):",
                min = 0.1, max = 5, value = 0.5, step = 0.1,
                ticks = FALSE)
  ),

  layout_columns(
    col_widths = c(12),

    card(
      card_header("Population of ROE values (simulated)"),
      plotOutput("pop_plot", height = "260px")
    ),

    card(
      card_header("Current sample"),
      plotOutput("sample_plot", height = "230px")
    ),

    card(
      card_header("Sampling distribution of the sample mean"),
      plotOutput("dist_plot", height = "300px"),
      card_footer(
        style = "font-size: 0.9em;",
        textOutput("n_samples"),
        conditionalPanel(
          condition = "input.show_ci",
          style = "font-size: 0.85em; margin-top: 4px;",
          HTML("<span style='color: #2E8B57;'>Green</span>: this sample's 95% CI
                captures &mu;; <span style='color: #CC0000;'>red</span>: it misses")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # The browser can report a zero width for an instant at startup (before the
  # layout settles), which crashes the graphics device. Fall back to a sane
  # default until a real measurement arrives.
  safe_width <- function(id, default = 750) {
    function() {
      w <- session$clientData[[paste0("output_", id, "_width")]]
      if (is.null(w) || !is.finite(w) || w < 50) default else w
    }
  }

  values <- reactiveValues(
    mean_history   = numeric(0),   # one sample mean per sample taken
    current_sample = numeric(0)    # the n values of the most recent sample
  )

  n_size <- reactive(as.integer(input$n))

  # The interval is x-bar +/- 2 SE using the KNOWN population sd, so the margin
  # is the same for every sample: a sample captures mu exactly when its mean
  # lands within one margin of it. That makes coverage a function of the stored
  # means alone -- no need to track hits as they happen.
  ci_margin <- reactive(2 * POP_SD / sqrt(n_size()))

  # Keep the slider and the typed sample size in sync: typing a value (rounded
  # to an integer and clamped to 1-100 for safety) moves the slider, and moving
  # the slider fills the numeric box. Updates with no change don't re-fire, so
  # this can't loop.
  observeEvent(input$n_typed, {
    v <- input$n_typed
    if (is.null(v) || is.na(v)) return()
    v <- max(1, min(100, round(v)))
    updateNumericInput(session, "n_typed", value = v)
    updateSliderInput(session, "n", value = v)
  })
  observeEvent(input$n, {
    updateNumericInput(session, "n_typed", value = input$n)
  })

  reset_history <- function() {
    values$mean_history   <- numeric(0)
    values$current_sample <- numeric(0)
  }

  # Changing n invalidates the accumulated distribution
  observeEvent(input$n, reset_history(), ignoreInit = TRUE)
  observeEvent(input$clear, reset_history())

  # Take 1 sample: append its mean and keep the sample for the middle panel
  observeEvent(input$take_one, {
    s <- sample(pop, n_size(), replace = TRUE)
    values$current_sample <- s
    values$mean_history <- c(values$mean_history, mean(s))
  })

  # Take many samples: fast path for the bulk (means only), plus one full draw
  # so the current-sample panel has something to show.
  observeEvent(input$take_many, {
    n <- n_size()
    k <- max(1, min(10000, round(input$k)))
    if (k > 1) {
      bulk <- colMeans(matrix(sample(pop, n * (k - 1), replace = TRUE), nrow = n))
      values$mean_history <- c(values$mean_history, bulk)
    }
    s <- sample(pop, n, replace = TRUE)
    values$current_sample <- s
    values$mean_history <- c(values$mean_history, mean(s))
  })

  # Top panel: the population
  output$pop_plot <- renderPlot(width = safe_width("pop_plot"), {
    par(mar = c(4.2, 4.2, 0.5, 0.5))
    hist(pop, breaks = seq(POP_XLIM[1], POP_XLIM[2], length.out = 61),
         col = bar_color, border = "white", main = "", freq = TRUE,
         xlim = POP_XLIM, xlab = "Return on equity (ROE, %)", ylab = "Count",
         axes = FALSE)
    pct_axis(); axis(2)
    abline(v = POP_MEAN, col = mean_color, lwd = 2.5)
    stats_box(c(
      sprintf("Population size (N) = %s", format(M_POP, big.mark = ",")),
      sprintf("Population mean (mu) = %.1f%%", POP_MEAN),
      sprintf("Population SD (sigma) = %.1f%%", POP_SD)
    ))
  })

  # Middle panel: the current sample, on the same x-limits as the population
  output$sample_plot <- renderPlot(width = safe_width("sample_plot"), {
    if (length(values$current_sample) == 0) {
      par(mar = c(0.5, 0.5, 0.5, 0.5))
      plot.new()
      text(0.5, 0.5, "Take a sample to see it here", col = "grey50", cex = 1.3)
      return(invisible())
    }

    s <- values$current_sample
    xbar <- mean(s)

    par(mar = c(4.2, 4.2, 0.5, 0.5))
    hist(s, breaks = seq(5 * floor(POP_XLIM[1] / 5),
                         5 * ceiling(POP_XLIM[2] / 5), by = 5),
         col = bar_color, border = "white", main = "", freq = TRUE,
         xlim = POP_XLIM, xlab = "Return on equity (ROE, %)", ylab = "Count",
         axes = FALSE)
    pct_axis(); axis(2)
    rug(s, col = bar_color)
    abline(v = xbar, col = mark_color, lwd = 2.5)
    stats_box(c(
      sprintf("Sample size (n) = %d", length(s)),
      sprintf("Sample mean (x-bar) = %.1f%%", xbar),
      sprintf("Sample SD (s) = %s",
              if (length(s) >= 2) sprintf("%.1f%%", sd(s)) else "-")
    ))
  })

  # Bottom panel: sampling distribution of the sample mean, on the density
  # scale so the normal overlay needs no rescaling.
  output$dist_plot <- renderPlot(width = safe_width("dist_plot", 950), {
    if (length(values$mean_history) == 0) {
      par(mar = c(0.5, 0.5, 0.5, 0.5))
      plot.new()
      text(0.5, 0.5, "Take samples to build up the sampling distribution",
           col = "grey50", cex = 1.3)
      return(invisible())
    }

    n <- n_size()
    se <- POP_SD / sqrt(n)
    m <- values$mean_history
    bw <- input$bw_means

    # Bins aligned so that POP_MEAN falls on a bin boundary
    lo <- POP_MEAN - bw * ceiling((POP_MEAN - min(m)) / bw + 1)
    hi <- POP_MEAN + bw * ceiling((max(m) - POP_MEAN) / bw + 1)
    h <- hist(m, breaks = seq(lo, hi, by = bw), plot = FALSE)

    y_hi <- max(c(h$density,
                  if (input$show_normal) dnorm(POP_MEAN, POP_MEAN, se))) * 1.12

    par(mar = c(4.2, 4.2, 0.5, 0.5))
    plot(h, freq = FALSE, col = bar_color, border = "white", main = "",
         xlim = POP_XLIM, ylim = c(0, y_hi),
         xlab = "Sample mean ROE (%)", ylab = "Density", axes = FALSE)
    pct_axis(); axis(2)
    abline(v = POP_MEAN, col = mean_color, lty = 2, lwd = 2)

    if (input$show_normal) {
      xs <- seq(POP_XLIM[1], POP_XLIM[2], length.out = 400)
      lines(xs, dnorm(xs, POP_MEAN, se), col = normal_color, lwd = 2.5)
    }

    # X on the axis marking the current sample's mean, with its interval when
    # requested. Green when the interval covers mu, red when it misses.
    if (length(values$current_sample) > 0) {
      xbar <- mean(values$current_sample)
      x_color <- if (input$show_ci) {
        if (abs(xbar - POP_MEAN) <= ci_margin()) hit_color else miss_color
      } else {
        mark_color
      }

      if (input$show_ci) {
        lo_ci <- xbar - ci_margin()
        hi_ci <- xbar + ci_margin()
        cap <- 0.02 * y_hi
        segments(lo_ci, 0, hi_ci, 0, col = x_color, lwd = 4, xpd = NA)
        segments(c(lo_ci, hi_ci), -cap, c(lo_ci, hi_ci), cap,
                 col = x_color, lwd = 3, xpd = NA)
      }

      points(xbar, 0, pch = 4, cex = 1.6, lwd = 3, col = x_color, xpd = NA)
    }
  })

  output$n_samples <- renderText({
    total <- length(values$mean_history)
    if (total > 0 && input$show_ci) {
      coverage <- mean(abs(values$mean_history - POP_MEAN) <= ci_margin())
      sprintf("Samples taken: %d   |   CI coverage: %.1f%%", total, 100 * coverage)
    } else {
      paste("Samples taken:", total)
    }
  })
}

shinyApp(ui = ui, server = server)
