# Sampling distribution of the sample mean: return on equity (ROE) at SAP
#
# Companion app for the sampling chapter. A fake population of ROE values is
# built once at startup by smoothing a small set of frozen bootstrap ROE
# values with a Gaussian kernel. Each "sample" of n customers yields a mean
# ROE; the app accumulates the sampling distribution of the sample mean across
# samples, with an optional normal overlay N(mu, sigma / sqrt(n)).
#
# Run from the webapps project with shiny::runApp("apps/sap_sampling").

library(shiny)
library(bslib)
library(ggplot2)

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
# -1 (a hard ROE floor) is rejected and redrawn; with min ROE about -0.12 and
# bandwidth about 0.06 the loop essentially never fires -- it is only a guard.
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
# All displayed values and normal overlays use the realized population mean and
# sd (the kernel inflates sd slightly over the raw data; using the realized
# values keeps the app internally consistent).
POP_MEAN <- mean(pop)
POP_SD   <- sd(pop)
POP_XLIM <- range(pop)

# --- Colors -------------------------------------------------------------
bar_color    <- "#0173B2"   # blue: distribution / sample bars
mean_color   <- "grey30"    # solid line at the population mean
sd_color     <- "grey55"    # dashed lines at +/- 1 sd
normal_color <- "#9370DB"   # purple: normal approximation
mark_color   <- "#CC0000"   # red X marking the current sample mean
hit_color    <- "#2E8B57"   # sea green: this sample's CI captures mu
miss_color   <- "#CC0000"   # red: this sample's CI misses mu

# --- Population panel (built once; renderPlot just returns it) ---------
pop_df <- data.frame(roe = pop)
pop_plot <- ggplot(pop_df, aes(x = roe)) +
  geom_histogram(bins = 60, fill = bar_color, alpha = 0.85) +
  geom_vline(xintercept = POP_MEAN, color = mean_color, linewidth = 1.1) +
  geom_label(
    data = data.frame(x = Inf, y = Inf, lab = sprintf(
      "Population size (N) = %s\nPopulation mean (mu) = %.1f%%\nPopulation SD (sigma) = %.1f percentage points",
      format(M_POP, big.mark = ","), POP_MEAN, POP_SD)),
    aes(x = x, y = y, label = lab), inherit.aes = FALSE,
    hjust = 1.02, vjust = 1.1, size = 4.5, color = "gray20",
    lineheight = 1.1, fontface = "bold",
    fill = alpha("white", 0.65), label.size = 0) +
  coord_cartesian(xlim = POP_XLIM, expand = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = "Return on equity (ROE, %)", y = "Count") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 10)
  )

ui <- page_sidebar(
  title = "Sampling distributions: SAP customer ROE",

  # Consistent font across the app and its plots
  tags$head(
    tags$style(HTML("
      * { font-family: 'Arial', 'Helvetica', sans-serif !important; }
      text { font-family: 'Arial', 'Helvetica', sans-serif !important; }
    "))
  ),

  sidebar = sidebar(
    width = 300,

    sliderInput("n", "Sample size (n):",
                min = 1, max = 100, value = 25, step = 1, ticks = FALSE),
    numericInput("n_typed", "Sample size (typed):",
                 value = 25, min = 1, max = 100, step = 1),

    div(
      style = "margin-top: 10px;",
      actionButton("take_one", "Take 1 sample",
                   class = "btn-primary",
                   style = "margin-bottom: 5px; width: 100%;"),
      div(
        style = "margin-bottom: 10px;",
        numericInput("k", label = "Number of samples", value = 50, min = 1, max = 10000,
                     width = "100%"),
        actionButton("take_many", "Take many samples",
                     style = "width: 100%;")
      ),
      actionButton("clear", "Clear history",
                   class = "btn-outline-secondary",
                   style = "width: 100%; margin-bottom: 10px;")
    ),

    checkboxInput("show_normal", "Show normal approximation", value = FALSE),
    checkboxInput("show_ci", "Show approximate 95% confidence interval", value = FALSE),

    sliderInput("bw_means", "Bin width (percentage points):",
                min = 0.1, max = 5, value = 0.5, step = 0.1,
                ticks = FALSE)
  ),

  layout_columns(
    col_widths = c(12),
    row_heights = c("32%", "28%", "40%"),

    card(
      card_header("Population of ROE values (simulated)"),
      plotOutput("pop_plot", height = "100%")
    ),

    card(
      card_header("Current sample"),
      plotOutput("sample_plot", height = "100%")
    ),

    card(
      card_header("Sampling distribution of the sample mean"),
      plotOutput("dist_plot", height = "100%"),
      card_footer(
        style = "font-size: 0.9em;",
        textOutput("n_samples"),
        conditionalPanel(
          condition = "input.show_ci",
          style = "font-size: 0.85em; margin-top: 4px;",
          HTML("<span style='color: #2E8B57;'>Green</span>: this sample's interval
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
    if (length(v) != 1L || !is.finite(v)) return()
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
    k <- input$k
    if (length(k) != 1L || !is.finite(k)) {
      showNotification("Enter a number of samples from 1 to 10,000.", type = "warning")
      return()
    }
    k <- max(1, min(10000, round(k)))
    updateNumericInput(session, "k", value = k)
    if (k > 1) {
      bulk <- colMeans(matrix(sample(pop, n * (k - 1), replace = TRUE), nrow = n))
      values$mean_history <- c(values$mean_history, bulk)
    }
    s <- sample(pop, n, replace = TRUE)
    values$current_sample <- s
    values$mean_history <- c(values$mean_history, mean(s))
  })

  # Top panel: the population, pre-built once at startup
  output$pop_plot <- renderPlot(width = safe_width("pop_plot"), {
    pop_plot
  })

  # Middle panel: the current sample, on the same x-limits as the population
  output$sample_plot <- renderPlot(width = safe_width("sample_plot"), {
    if (length(values$current_sample) == 0) {
      return(
        ggplot() +
          theme_void() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Take a sample to see it here",
                   size = 5, color = "gray60")
      )
    }

    s <- values$current_sample
    xbar <- mean(s)
    df <- data.frame(roe = s)

    ggplot(df, aes(x = roe)) +
      geom_histogram(binwidth = 5, fill = bar_color, alpha = 0.85,
                     boundary = 0) +
      geom_rug(sides = "b", color = bar_color, alpha = 0.7) +
      geom_vline(xintercept = xbar, color = mark_color, linewidth = 1.1) +
      geom_label(
        data = data.frame(x = Inf, y = Inf, lab = sprintf(
          "Sample size (n) = %d\nSample mean (x-bar) = %.1f%%\nSample SD (s) = %s",
          length(s), xbar,
          if (length(s) >= 2) sprintf("%.1f percentage points", sd(s)) else "-")),
        aes(x = x, y = y, label = lab), inherit.aes = FALSE,
        hjust = 1.02, vjust = 1.1, size = 4.5, color = "gray20",
        lineheight = 1.1, fontface = "bold",
        fill = alpha("white", 0.65), label.size = 0) +
      coord_cartesian(xlim = POP_XLIM, expand = FALSE) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Return on equity (ROE, %)", y = "Count") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10)
      )
  })

  # Bottom panel: sampling distribution of the sample mean, on the density
  # scale so the normal overlay needs no rescaling.
  output$dist_plot <- renderPlot(width = safe_width("dist_plot", 950), {
    if (length(values$mean_history) == 0) {
      return(
        ggplot() +
          theme_void() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Take samples to build up the sampling distribution",
                   size = 5, color = "gray60")
      )
    }

    n <- n_size()
    se <- POP_SD / sqrt(n)

    df <- data.frame(m = values$mean_history)

    p <- ggplot(df, aes(x = m)) +
      geom_histogram(aes(y = after_stat(density)),
                     binwidth = input$bw_means, boundary = POP_MEAN,
                     fill = bar_color, alpha = 0.85) +
      geom_vline(xintercept = POP_MEAN, color = mean_color,
                 linetype = "dashed", linewidth = 1.0) +
      # clip = "off" so the interval and its end caps, drawn at y = 0, are not
      # cut in half by the panel floor (the base-graphics port uses xpd = NA)
      coord_cartesian(xlim = POP_XLIM, expand = FALSE, clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Sample mean ROE (%)", y = "Density") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10)
      )

    if (input$show_normal) {
      p <- p + stat_function(
        fun = function(x) dnorm(x, POP_MEAN, se),
        color = normal_color, linewidth = 1.2, alpha = 0.9
      )
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
        ci <- data.frame(lo = xbar - ci_margin(), hi = xbar + ci_margin())
        p <- p +
          geom_segment(
            data = ci, aes(x = lo, xend = hi, y = 0, yend = 0),
            color = x_color, linewidth = 2, alpha = 0.9
          ) +
          # End caps, sized in mm so they need no y-axis units
          geom_point(
            data = data.frame(x = c(ci$lo, ci$hi), y = 0),
            aes(x = x, y = y),
            shape = 124, size = 4, color = x_color, stroke = 1.5
          )
      }

      p <- p + geom_point(
        data = data.frame(x = xbar, y = 0),
        aes(x = x, y = y),
        shape = 4, size = 4, color = x_color, stroke = 2
      )
    }

    p
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
