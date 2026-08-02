# The Thaler bet: N independent investments, each paying +$2M with
# probability 1/2 or -$1M with probability 1/2. This app simulates whole
# portfolios one at a time (or many at once) and builds up the distribution
# of total payoffs (or of the proportion of profitable investments).
# The number of investments N is adjustable. Run locally with shiny::runApp().

library(shiny)
library(bslib)
library(ggplot2)

# Fixed per-investment payoffs
WIN  <-  2              # payoff of a profitable investment, in $M
LOSS <- -1              # payoff of an unprofitable investment, in $M
SPACING <- 3            # gap between adjacent possible totals ($M): WIN - LOSS

# How many tiles to show per row in the current-portfolio strip
TILES_PER_ROW <- 25

# Colorblind-safe colors
win_color  <- "#009E73"   # green for +$2M
loss_color <- "#D55E00"   # red for -$1M
bar_color  <- "#0173B2"   # blue for the distribution bars
normal_color <- "#9370DB" # purple for the normal curve

ui <- page_sidebar(
  title = "Thaler Example: Simulating N Independent Investments",

  # Consistent font across the app and its plots
  tags$head(
    tags$style(HTML("
      * {
        font-family: 'Arial', 'Helvetica', sans-serif !important;
      }
      text {
        font-family: 'Arial', 'Helvetica', sans-serif !important;
      }
    "))
  ),

  sidebar = sidebar(
    width = 300,

    # Number of independent investments
    sliderInput("n_proj", "Number of investments (N):",
                min = 1, max = 50, value = 23, step = 1, ticks = FALSE),

    # Required wins for an overall profit
    uiOutput("required_wins"),

    # Simulate a single portfolio
    actionButton("sim_one", "Simulate 1 portfolio",
                 class = "btn-primary",
                 style = "margin-bottom: 5px; width: 100%;"),

    # Simulate many portfolios at once
    div(
      style = "display: flex; gap: 5px; margin-bottom: 5px;",
      numericInput("k", label = NULL, value = 50, min = 1, max = 10000,
                   width = "80px"),
      actionButton("sim_many", "Simulate many",
                   style = "flex: 1;")
    ),

    # Clear everything and start over
    actionButton("clear", "Clear history",
                 class = "btn-outline-secondary",
                 style = "width: 100%; margin-bottom: 10px;"),

    # Overlay the normal approximation on the distribution plot
    checkboxInput("show_normal", "Show normal approximation", value = FALSE),

    # Switch the bottom panel to the proportion of profitable investments
    checkboxInput("show_prop", "Show proportion profitable", value = FALSE),
    conditionalPanel(
      condition = "input.show_prop == true",
      div(
        style = "margin-left: 22px;",
        checkboxInput("as_percent", "Label axis as percent", value = FALSE)
      )
    ),

    # Highlight the portfolios that lost money
    checkboxInput("show_loss", "Highlight losing portfolios", value = FALSE)
  ),

  # Main area: current portfolio on top, distribution below
  layout_columns(
    col_widths = c(12),
    row_heights = c("32%", "68%"),

    card(
      card_header("This portfolio"),
      div(
        style = "font-size: 0.95em; padding: 2px 8px; min-height: 1.3em; color: #333;",
        textOutput("portfolio_summary")
      ),
      plotOutput("portfolio_plot", height = "100%")
    ),

    card(
      card_header(textOutput("dist_title")),
      plotOutput("dist_plot", height = "100%"),
      card_footer(
        style = "font-size: 0.9em;",
        textOutput("n_simulated")
      )
    )
  )
)

server <- function(input, output, session) {

  # State: wins count for every simulated portfolio (an integer per
  # simulation), plus the WIN/LOSS outcomes of the most recent portfolio
  # (so we can draw the tile strip). Totals are re-derived as 3*wins - N
  # and the proportion profitable as wins / N.
  values <- reactiveValues(
    history = integer(0),          # number of profitable investments per sim
    current_outcomes = numeric(0)  # the N payoffs of the current portfolio
  )

  # Changing N invalidates the accumulated distribution
  reset_history <- function() {
    values$history <- integer(0)
    values$current_outcomes <- numeric(0)
  }
  observeEvent(input$n_proj, reset_history(), ignoreInit = TRUE)
  observeEvent(input$clear, reset_history())

  # Draw one portfolio: N independent coin flips between +$2M and -$1M
  simulate_portfolio <- function() {
    sample(c(WIN, LOSS), size = input$n_proj, replace = TRUE)
  }

  # Simulate 1 portfolio: record its win count, show its outcomes
  observeEvent(input$sim_one, {
    outcomes <- simulate_portfolio()
    values$current_outcomes <- outcomes
    values$history <- c(values$history, sum(outcomes == WIN))
  })

  # Simulate many portfolios: record k win counts, display the last one.
  # Fast path: the bulk of the win counts come straight from rbinom(); only
  # the final portfolio is drawn in full so the tile panel can show it.
  observeEvent(input$sim_many, {
    k <- input$k
    if (is.na(k) || k < 1) return()
    k <- min(round(k), 10000)
    N <- input$n_proj

    if (k > 1) {
      values$history <- c(values$history, rbinom(k - 1, N, 0.5))
    }
    outcomes <- simulate_portfolio()
    values$current_outcomes <- outcomes
    values$history <- c(values$history, sum(outcomes == WIN))
  })

  # Smallest number of profitable investments that yields an overall profit
  output$required_wins <- renderUI({
    req <- floor(input$n_proj / 3) + 1
    div(
      style = "font-size: 0.85em; color: #555; margin-bottom: 12px;",
      sprintf("%d profitable %s required for overall profit",
              req, if (req == 1) "project" else "projects")
    )
  })

  # Header for the bottom card, depends on the view
  output$dist_title <- renderText({
    if (isTRUE(input$show_prop)) {
      "Distribution of the proportion profitable"
    } else {
      "Distribution of total payoffs"
    }
  })

  # One-line summary of the current portfolio
  output$portfolio_summary <- renderText({
    if (length(values$current_outcomes) == 0) return("")
    N <- length(values$current_outcomes)
    wins <- sum(values$current_outcomes == WIN)
    total <- SPACING * wins - N
    sprintf("Profitable: %d of %d (%.1f%%)  |  Total profit: $%dM",
            wins, N, 100 * wins / N, total)
  })

  # Top card: strip of the N investments in the current portfolio,
  # wrapping at TILES_PER_ROW per row
  output$portfolio_plot <- renderPlot({

    if (length(values$current_outcomes) == 0) {
      # Placeholder before the first simulation
      return(
        ggplot() +
          theme_void() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Click 'Simulate 1 portfolio' to begin",
                   size = 5, color = "gray60")
      )
    }

    outcomes <- values$current_outcomes
    idx <- seq_along(outcomes)
    x <- (idx - 1) %% TILES_PER_ROW + 1
    y <- -((idx - 1) %/% TILES_PER_ROW) * 1.7

    tiles <- data.frame(
      x = x,
      y = y,
      idx = idx,
      label = ifelse(outcomes == WIN, "+$2M", "-$1M"),
      fill = ifelse(outcomes == WIN, win_color, loss_color)
    )

    ggplot(tiles) +
      # Investment number above each tile
      geom_text(aes(x = x, y = y + 0.62, label = idx),
                size = 3, color = "gray30") +
      # One colored tile per investment, labeled with its payoff
      geom_tile(aes(x = x, y = y, fill = fill),
                width = 0.92, height = 0.85) +
      geom_text(aes(x = x, y = y, label = label),
                color = "white", size = 3.0, fontface = "bold") +
      scale_fill_identity() +
      coord_cartesian(xlim = c(0.4, TILES_PER_ROW + 0.6),
                      ylim = c(min(y) - 0.7, 0.95), expand = FALSE) +
      theme_void()
  })

  # Bottom card: bar plot of the accumulated distribution
  output$dist_plot <- renderPlot({

    N <- input$n_proj

    if (length(values$history) == 0) {
      return(
        ggplot() +
          theme_void() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Simulate a portfolio to see the distribution",
                   size = 5, color = "gray60")
      )
    }

    n_sims <- length(values$history)
    wins <- 0:N
    counts <- tabulate(values$history + 1, nbins = N + 1)
    prop <- counts / n_sims

    # A losing portfolio has total 3*wins - N < 0 (a total of exactly 0 is
    # not a loss). Equivalent to wins < N/3.
    is_loss <- (SPACING * wins - N) < 0

    prop_mode <- isTRUE(input$show_prop)
    as_pct <- isTRUE(input$as_percent)
    show_loss <- isTRUE(input$show_loss)

    has_current <- length(values$current_outcomes) > 0
    current_wins <- if (has_current) sum(values$current_outcomes == WIN) else NA

    if (prop_mode) {
      # Proportion of investments profitable: support at wins/N, spacing 1/N.
      xvals <- wins / N
      barwidth <- 0.6 / N
      mu <- 0.5
      sd_x <- sqrt(0.25 / N)
      overlay_scale <- 1 / N
      current_x <- if (has_current) current_wins / N else NA
      x_label <- if (as_pct) "Percent of investments profitable"
                 else "Proportion of investments profitable"
    } else {
      # Total payoff: support at 3*wins - N, spacing SPACING ($3M).
      xvals <- SPACING * wins - N
      barwidth <- 0.6 * SPACING
      # Total = 3*wins - N; mean wins is 0.5N, so mean total = 0.5N and
      # var total = 9 * var(wins) = 9 * 0.25 * N = 2.25N.
      mu <- 0.5 * N
      sd_x <- sqrt(2.25 * N)
      overlay_scale <- SPACING
      current_x <- if (has_current) SPACING * current_wins - N else NA
      x_label <- "Total payoff ($M)"
    }

    bars <- data.frame(
      x = xvals,
      prop = prop,
      fill = ifelse(show_loss & is_loss, loss_color, bar_color)
    )

    y_expand <- expansion(mult = c(0.02, if (show_loss) 0.14 else 0.1))

    p <- ggplot(bars, aes(x = x, y = prop)) +
      geom_col(aes(fill = fill), width = barwidth, alpha = 0.85) +
      scale_fill_identity() +
      scale_y_continuous(expand = y_expand) +
      labs(x = x_label, y = "Proportion of simulations") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10)
      )

    if (prop_mode) {
      if (as_pct) {
        p <- p + scale_x_continuous(
          labels = function(x) paste0(round(100 * x), "%")
        )
      }
    } else {
      p <- p + scale_x_continuous(breaks = scales::breaks_pretty())
    }

    # Normal approximation, scaled by the support spacing so density matches
    # the proportion heights.
    if (isTRUE(input$show_normal)) {
      p <- p + stat_function(
        fun = function(x) dnorm(x, mean = mu, sd = sd_x) * overlay_scale,
        color = normal_color, linewidth = 1.2, alpha = 0.9
      )
    }

    # Annotation summarizing how often money was lost
    if (show_loss) {
      n_loss <- sum(values$history < N / 3)
      prop_loss <- n_loss / n_sims
      p <- p + annotate(
        "text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.5,
        label = sprintf("Lost money in %.1f%% of simulations (%d of %d)",
                        100 * prop_loss, n_loss, n_sims),
        color = loss_color, size = 4
      )
    }

    # Red X marking the current portfolio's value
    if (has_current) {
      p <- p + geom_point(
        data = data.frame(x = current_x, y = 0),
        aes(x = x, y = y),
        shape = 4, size = 4, color = "red", stroke = 2,
        inherit.aes = FALSE
      )
    }

    p
  })

  # Footer: running count of simulated portfolios
  output$n_simulated <- renderText({
    paste("Portfolios simulated:", length(values$history))
  })
}

shinyApp(ui = ui, server = server)
