# The bootstrap: resampling the 81 SAP customer ROEs
#
# Companion app for the bootstrap section of the sampling distributions
# chapter. The top panel shows the observed sample of 81 firms as a numbered
# grid (index + ROE); each "resample" draws 81 firms from those 81 with
# replacement. The middle panel shows the current resample (duplicates
# highlighted), and the bottom panel accumulates the histogram of resample
# means -- the bootstrap approximation to the sampling distribution of the
# sample mean -- with an optional CLT overlay N(x-bar, s^2/n).
#
# The browser page reads this file during rendering, so it is the maintained
# browser implementation as well as the native base-graphics app.
#
# Run locally with shiny::runApp("apps/sap_bootstrap/app.R").

library(shiny)
library(bslib)

# --- Data ---------------------------------------------------------------
# The 81 ROE values (in percent) from data/sap_roe/sap_roe.csv, in row order:
# firm i in the app is row i of that file, the real Nucleus sample the
# chapter bootstraps. Inlined so the app is standalone under shinylive.
SAP_ROE <- c(
  21.1, 15.8, 1.2, 61.2, 15.2, 26.4, 66.9, 5.4, 12.2, 4.9, 20.8, 23.3, 13.2,
  14.5, 27.3, 17, -41.1, 35.9, 116.4, 8.1, 7.9, 6.7, 32.7, 11.6, 17.1, -47.5,
  23.8, 1.1, 26.7, 6.9, 13.4, 14.9, 14.9, 14.6, -15.4, 4.6, 43.9, 15.8, 6.4,
  13.1, 14.6, 8.4, 8.9, 8.8, 23.1, -91.8, 45.8, 7.3, 18.9, 22.8, -7.7, 6.8,
  -9.2, -62.6, 18.7, 27.3, 8.3, 16.7, 18.4, 28.8, 25.6, 16.9, 0.3, -0.7,
  -38.5, 8.3, 8.3, 8.3, 8.3, 6.2, 6.2, -0.5, 18.6, 0.9, 47.4, 27.3, 4.7, 2.8,
  41.4, 26, 14.6
)
N_S <- length(SAP_ROE)
stopifnot(N_S == 81)

SAMP_MEAN <- mean(SAP_ROE)             # 12.6: plays the role of mu
SAMP_SD   <- sd(SAP_ROE)               # 25.7: plays the role of sigma
SAMP_SE   <- SAMP_SD / sqrt(N_S)       # 2.85: the CLT standard error
DIST_XLIM <- SAMP_MEAN + c(-12, 12)

# --- Colors -------------------------------------------------------------
bar_color    <- "#0173B2"   # blue: firm tiles / histogram bars
dup_color    <- "#DE8F05"   # orange: firms drawn more than once
out_color    <- "grey88"    # faded tile: firm not in the current resample
mean_color   <- "grey30"    # dashed line at the original sample mean
normal_color <- "#9370DB"   # purple: CLT normal approximation
mark_color   <- "#CC0000"   # red X marking the current resample mean

# --- Grid panels --------------------------------------------------------
# Both grid panels draw 81 tiles in a 9 x 9 grid, each tile labeled with a
# firm's index (bold) and its ROE. Statistics appear in the card footer.
GRID_XMAX <- 9

draw_grid <- function(idx, fill, text_col) {
  par(mar = c(0.3, 0.3, 0.3, 0.3))
  plot.new()
  plot.window(xlim = c(0, GRID_XMAX), ylim = c(0, 9))
  k  <- seq_along(idx)
  cc <- (k - 1) %% 9
  rr <- (k - 1) %/% 9
  xl <- cc + 0.05; xr <- cc + 0.95
  yt <- 9 - rr - 0.05; yb <- 9 - rr - 0.95
  rect(xl, yb, xr, yt, col = fill, border = "white")
  text((xl + xr) / 2, yb + 0.60, labels = idx,
       cex = 1.08, font = 2, col = text_col)
  text((xl + xr) / 2, yb + 0.26, labels = sprintf("%.1f", SAP_ROE[idx]),
       cex = 0.84, col = text_col)
}

ui <- page_sidebar(
  title = "Bootstrap distributions: SAP customer ROE",

  tags$head(
    tags$style(HTML("
      * { font-family: 'Arial', 'Helvetica', sans-serif !important; }
      .bslib-page-main { min-width: 0 !important; }
      #sample_plot, #sample_plot img,
      #resample_plot, #resample_plot img { min-height: 360px !important; }
      #dist_plot, #dist_plot img { min-height: 300px !important; }
    "))
  ),

  sidebar = sidebar(
    width = 300,

    p(style = "font-size: 0.92em; margin-bottom: 10px;",
      "Each resample draws 81 firms with replacement from the observed sample."),

    div(
      style = "margin-top: 4px;",
      actionButton("draw_one", "Draw 1 resample",
                   class = "btn-primary",
                   style = "margin-bottom: 5px; width: 100%;"),
      div(
        style = "margin-bottom: 10px;",
        numericInput("k", label = "Number of resamples", value = 50,
                     min = 1, max = 10000, width = "100%"),
        actionButton("draw_many", "Draw many resamples",
                     style = "width: 100%;")
      ),
      actionButton("clear", "Clear history",
                   class = "btn-outline-secondary",
                   style = "width: 100%; margin-bottom: 10px;")
    ),

    hr(style = "margin: 10px 0;"),
    div(style = "font-size: 0.85em; font-weight: 600; margin-bottom: 4px;",
        "Grid display"),
    checkboxInput("sort_resample", "Sort resample by firm index",
                  value = TRUE),
    checkboxInput("color_original", "Grey out firms left out (top panel)",
                  value = TRUE),
    checkboxInput("color_resample", "Color duplicate draws (resample)",
                  value = TRUE),

    hr(style = "margin: 10px 0;"),
    checkboxInput("show_normal", "Show normal approximation",
                  value = FALSE),

    sliderInput("bw_means", "Bin width (percentage points):",
                min = 0.1, max = 5, value = 0.5, step = 0.1,
                ticks = FALSE)
  ),

  layout_columns(
    col_widths = c(12),
    fill = FALSE,
    fillable = FALSE,

    card(
      card_header("Observed sample: 81 firms with ROE (%)"),
      card_body(plotOutput("sample_plot", height = "360px", fill = FALSE),
                fill = FALSE),
      card_footer(
        style = "font-size: 0.9em;",
        textOutput("sample_stats"),
        textOutput("sample_note")
      ),
      fill = FALSE
    ),

    card(
      card_header("Current bootstrap resample (81 draws with replacement)"),
      card_body(plotOutput("resample_plot", height = "360px", fill = FALSE),
                fill = FALSE),
      card_footer(
        style = "font-size: 0.9em;",
        textOutput("resample_stats"),
        textOutput("resample_note")
      ),
      fill = FALSE
    ),

    card(
      card_header("Bootstrap distribution of the resample mean"),
      card_body(plotOutput("dist_plot", height = "300px", fill = FALSE),
                fill = FALSE),
      card_footer(
        style = "font-size: 0.9em;",
        textOutput("n_resamples")
      ),
      fill = FALSE
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
    mean_history = numeric(0),   # one resample mean per resample drawn
    current_idx  = integer(0)    # firm indices of the most recent resample
  )

  reset_history <- function() {
    values$mean_history <- numeric(0)
    values$current_idx  <- integer(0)
  }
  observeEvent(input$clear, reset_history())

  # Draw 1 resample: keep the indices in draw order (sorting is a display
  # choice, applied in the resample panel), append the mean
  observeEvent(input$draw_one, {
    idx <- sample(N_S, N_S, replace = TRUE)
    values$current_idx  <- idx
    values$mean_history <- c(values$mean_history, mean(SAP_ROE[idx]))
  })

  # Draw many: fast path for the bulk (means only), plus one full draw so the
  # grid panels have something to show.
  observeEvent(input$draw_many, {
    k <- input$k
    if (length(k) != 1L || !is.numeric(k) || !is.finite(k)) {
      showNotification("Enter a number of resamples from 1 to 10,000.",
                       type = "warning")
      return()
    }
    k <- max(1, min(10000, round(k)))
    updateNumericInput(session, "k", value = k)
    if (k > 1) {
      bulk <- colMeans(matrix(sample(SAP_ROE, N_S * (k - 1), replace = TRUE),
                              nrow = N_S))
      values$mean_history <- c(values$mean_history, bulk)
    }
    idx <- sample(N_S, N_S, replace = TRUE)
    values$current_idx  <- idx
    values$mean_history <- c(values$mean_history, mean(SAP_ROE[idx]))
  })

  # Top panel: the observed sample, playing the role of the population. With
  # highlighting on, firms missing from the current resample fade to grey.
  output$sample_plot <- renderPlot(width = safe_width("sample_plot"), {
    idx <- 1:N_S
    if (length(values$current_idx) > 0 && input$color_original) {
      drawn <- idx %in% values$current_idx
      fill  <- ifelse(drawn, bar_color, out_color)
      tcol  <- ifelse(drawn, "white", "grey55")
    } else {
      fill <- bar_color
      tcol <- "white"
    }
    draw_grid(idx, fill, tcol)
  })

  output$sample_stats <- renderText({
    paste(
      sprintf("Sample size (n) = %d", N_S),
      sprintf("Sample mean (x-bar) = %.1f%%", SAMP_MEAN),
      sprintf("Sample SD (s) = %.1f percentage points", SAMP_SD),
      sep = "   |   "
    )
  })

  output$sample_note <- renderText({
    if (length(values$current_idx) > 0 && isTRUE(input$color_original)) {
      "Grey firms are absent from this resample."
    } else {
      ""
    }
  })

  # Middle panel: the current resample, duplicates in orange
  output$resample_plot <- renderPlot(width = safe_width("resample_plot"), {
    idx <- values$current_idx
    if (length(idx) == 0) {
      par(mar = c(0.5, 0.5, 0.5, 0.5))
      plot.new()
      text(0.5, 0.5, "Draw a resample to see it here",
           col = "grey50", cex = 1.3)
      return(invisible())
    }

    disp   <- if (input$sort_resample) sort(idx) else idx
    counts <- table(disp)
    dup    <- counts[as.character(disp)] > 1
    rs     <- SAP_ROE[disp]
    fill   <- if (input$color_resample) ifelse(dup, dup_color, bar_color) else bar_color
    draw_grid(disp, fill, "white")
  })

  output$resample_stats <- renderText({
    idx <- values$current_idx
    if (length(idx) == 0) {
      return("")
    }
    rs <- SAP_ROE[idx]
    paste(
      sprintf("Resample size (n) = %d", length(idx)),
      sprintf("Resample mean = %.1f%%", mean(rs)),
      sprintf("Resample SD = %.1f percentage points", sd(rs)),
      sep = "   |   "
    )
  })

  output$resample_note <- renderText({
    if (length(values$current_idx) > 0 && isTRUE(input$color_resample)) {
      "Orange firms appear more than once."
    } else {
      ""
    }
  })

  # Bottom panel: histogram of resample means on the density scale, so the
  # normal overlay needs no rescaling.
  output$dist_plot <- renderPlot(width = safe_width("dist_plot", 950), {
    if (length(values$mean_history) == 0) {
      par(mar = c(0.5, 0.5, 0.5, 0.5))
      plot.new()
      text(0.5, 0.5, "Draw resamples to build up the bootstrap distribution",
           col = "grey50", cex = 1.3)
      return(invisible())
    }

    m  <- values$mean_history
    bw <- input$bw_means

    # Bins aligned so that the original sample mean falls on a bin boundary.
    lo <- SAMP_MEAN - bw * ceiling((SAMP_MEAN - min(m)) / bw + 1)
    hi <- SAMP_MEAN + bw * ceiling((max(m) - SAMP_MEAN) / bw + 1)
    h  <- hist(m, breaks = seq(lo, hi, by = bw), plot = FALSE)

    current_mean <- if (length(values$current_idx) > 0) {
      mean(SAP_ROE[values$current_idx])
    } else {
      NA_real_
    }
    plot_xlim <- range(c(DIST_XLIM, h$breaks, m, current_mean), finite = TRUE)

    y_hi <- max(c(h$density,
                  if (input$show_normal) dnorm(SAMP_MEAN, SAMP_MEAN, SAMP_SE))) * 1.12

    par(mar = c(4.2, 4.2, 0.5, 0.5))
    plot(h, freq = FALSE, col = bar_color, border = "white", main = "",
         xlim = plot_xlim, ylim = c(0, y_hi),
         xlab = "Resample mean ROE (%)", ylab = "Density", axes = FALSE)
    at <- pretty(plot_xlim, n = 6)
    axis(1, at = at, labels = paste0(at, "%"))
    axis(2)
    abline(v = SAMP_MEAN, col = mean_color, lty = 2, lwd = 2)

    if (input$show_normal) {
      xs <- seq(plot_xlim[1], plot_xlim[2], length.out = 400)
      lines(xs, dnorm(xs, SAMP_MEAN, SAMP_SE), col = normal_color, lwd = 2.5)
    }

    # Red X on the axis marking the current resample's mean
    if (length(values$current_idx) > 0) {
      points(mean(SAP_ROE[values$current_idx]), 0, pch = 4, cex = 1.6, lwd = 3,
             col = mark_color, xpd = NA)
    }
  })

  output$n_resamples <- renderText({
    paste("Resamples taken:", length(values$mean_history))
  })
}

shinyApp(ui = ui, server = server)
