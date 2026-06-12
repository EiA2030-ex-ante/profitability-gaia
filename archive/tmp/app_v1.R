# =========================================================
# Interactive Lime Profitability Explorer (Final Integrated)
# =========================================================
library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)
library(bslib)
library(tidyr)
library(purrr)
library(scales)
library(stringr)

# ---------- Load precomputed model outputs ----------
bundle <- readRDS("tmp/yield_models_bundle.rds")
all_models_df <- bundle$predictions
message("Loaded columns: ", paste(names(all_models_df), collapse = ", "))
perf_summary  <- bundle$metrics

# ---- Normalize column names ----
all_models_df <- all_models_df %>%
  rename_with( ~ gsub("emmean", "fit", .x)) %>%
  rename_with( ~ gsub("lwr|lower.CL", "lower.CL", .x)) %>%
  rename_with( ~ gsub("upr|upper.CL", "upper.CL", .x)) %>%
  mutate(
    yield_tha = coalesce(fit, yield_tha),
    country     = factor(country),
    admin2_gadm = factor(admin2_gadm),
    crop        = factor(crop),
    model       = factor(model, levels = c("ols", "lmm", "rf", "xgb"))
  )

catalog <- all_models_df %>%
  distinct(country, admin2_gadm, crop) %>%
  arrange(country, admin2_gadm, crop)

bar_colors <- c(
  "#0E3065",
  "#FFBE00",
  "#FC3400",
  "#00640D",
  "#454283",
  "#490000",
  "#4E5E77",
  "#8F2D56"
)

extrafont::loadfonts(quiet = F)
my_font_2 <- "Muli"
my_font <- "Frutiger"

#==========================================================
# For data section
#==========================================================

df <- readr::read_csv("tmp/data_y1.csv") |>
  select(-1) |>
  distinct() |>
  mutate(
    crop = as.factor(crop),
    country = as.factor(country),
    treatment = as.factor(treatment),
    lime_factor = factor(lime_tha)
  ) |>
  drop_na(yield_tha, lime_tha, fid, crop, country) |>
  mutate(treatment2 = paste0(treatment, " \n(", lime_tha, " t/ha)"))


# 1) Counts per country × site × crop × treatment
counts <- df %>%
  distinct() %>%
  count(country, admin2_gadm, crop, treatment)

# 1.1) heatmap of counts
# --- add totals per crop and per site ---
crop_totals <- counts %>%
  filter(treatment == "T1") %>%
  group_by(crop) %>%
  summarise(total_crop = sum(n), .groups = "drop")

site_totals <- counts %>%
  filter(treatment == "T1") %>%
  group_by(admin2_gadm) %>%
  summarise(total_site = sum(n), .groups = "drop")

# --- join labels ---
counts_labs <- counts %>%
  filter(treatment == "T1") %>%
  left_join(crop_totals, by = "crop") %>%
  left_join(site_totals, by = "admin2_gadm") %>%
  mutate(
    crop_lab = paste0(crop, " \n(N=", total_crop, ")"),
    site_lab = paste0(admin2_gadm, " \n(N=", total_site, ")")
  )




# 2) Summary stats per country × site × crop × treatment
# 2) Yield summaries by crop × treatment
yield_summary <- df %>%
  group_by(crop, treatment, treatment2, admin2_gadm) %>%
  summarise(
    n = n(),
    mean_yield = mean(yield_tha, na.rm = TRUE),
    sd_yield = sd(yield_tha, na.rm = TRUE),
    se = sd_yield / sqrt(n),
    ci95 = 1.96 * se,
    .groups = "drop"
  )




# 4) Yield respose plots
# --- compute yield response relative to T1 ---
yield_resp <- df %>%
  group_by(country, admin2_gadm, fid, crop) %>%
  mutate(yield_T1 = mean(yield_tha[treatment == "T1"], na.rm = TRUE),
         yield_response = yield_tha - yield_T1) %>%
  ungroup() %>%
  filter(treatment != "T1") %>% # exclude T1 (response = 0 by definition))
  mutate(treatment2 = paste0(treatment, " \n(", lime_tha, " t/ha)"))

resp_summary <- yield_resp %>%
  group_by(country, admin2_gadm, crop, treatment, treatment2) %>%
  summarise(
    n = n(),
    mean_resp = mean(yield_response, na.rm = TRUE),
    sd_resp = sd(yield_response, na.rm = TRUE),
    se = sd_resp / sqrt(n),
    ci95 = 1.96 * se,
    .groups = "drop"
  )


# Map
map_1 <- df |>
  select(fid, country, admin2_gadm, lat, lng, crop) |>
  distinct() |>
  filter(!is.na(lat)) |>
  group_by(fid, country, admin2_gadm, lat, lng) |>
  summarise(
    n_crops = n(),
    crops   = paste(unique(crop), collapse = " | ")  # crop names separated by |
  ) |>
  ungroup() |>
  distinct()
admin2_gadm <- unique(map_1$admin2_gadm)
# remove NA

admin2_gadm <- admin2_gadm[!is.na(admin2_gadm)]

color_plasma <- setNames(viridis::plasma(length(admin2_gadm) + 2), admin2_gadm)
cols <- hcl.colors(length(unique(admin2_gadm)), "Zissou 1")
setNames(cols, admin2_gadm)

pal <- colorFactor(
  palette = cols,
  # or use cols
  domain  = map_1$admin2_gadm
)



# =========================================================
# Helper: compute yield response (robust version)
# =========================================================
respify <- function(dat) {
  if (nrow(dat) == 0 ||
      !"yield_tha" %in% names(dat) || !"lime_tha" %in% names(dat)) {
    return(
      tibble(
        lime_tha = numeric(),
        yield_tha = numeric(),
        yield_resp = numeric(),
        lower_resp = numeric(),
        upper_resp = numeric()
      )
    )
  }
  
  if (any(dat$lime_tha == 0, na.rm = TRUE)) {
    y0 <- dat$yield_tha[which.min(abs(dat$lime_tha - 0))]
  } else {
    o <- dat[order(dat$lime_tha), ]
    y0 <- approx(
      x = o$lime_tha,
      y = o$yield_tha,
      xout = 0,
      rule = 2
    )$y
  }
  
  dat %>%
    mutate(
      yield_resp = yield_tha - y0,
      lower_resp = if ("lower.CL" %in% names(dat))
        lower.CL - y0
      else
        NA_real_,
      upper_resp = if ("upper.CL" %in% names(dat))
        upper.CL - y0
      else
        NA_real_
    )
}

# --- Apply respify() group-wise ---
all_models_df <- all_models_df %>%
  group_by(country, admin2_gadm, crop, model) %>%
  group_modify( ~ respify(.x)) %>%
  ungroup()
# =========================================================
# Theme
# =========================================================
my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Poppins"),
  heading_font = font_google("Poppins"),
  primary = "#1ABC9C",
  secondary = "#F39C12",
  success = "#1871B8",
  font_scale = 1.05
)

# =========================================================
# UI
# =========================================================
ui <- fluidPage(theme = my_theme,
                withMathJax(),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    selectInput("country", "Country", choices = levels(droplevels(catalog$country))),
                    uiOutput("site_ui"),
                    uiOutput("crop_ui"),
                    hr(),
                    tags$h5("Model settings"),
                    selectInput(
                      "model_type",
                      "Yield Response Model",
                      choices = c(
                        "Unconditional (OLS)" = "ols",
                        "Mixed Effects (LMM)" = "lmm",
                        "Random Forest" = "rf",
                        "XGBoost" = "xgb"
                      ),
                      selected = "lmm"
                    ),
                    hr(),
                    sliderInput(
                      "price_crop",
                      "Output price [USD/ton]",
                      min = 100,
                      max = 500,
                      value = 160
                    ),
                    sliderInput(
                      "price_lime",
                      "Lime price [USD/ton]",
                      min = 0,
                      max = 200,
                      value = 55
                    ),
                    sliderInput(
                      "discount",
                      "Discount rate [%]",
                      min = 0,
                      max = 20,
                      value = 10,
                      step = 1
                    ),
                    sliderInput(
                      "years",
                      "Years",
                      min = 1,
                      max = 10,
                      value = 5
                    ),
                    hr(),
                    tags$h5("Monte Carlo simulation settings"),
                    sliderInput(
                      "sd_crop",
                      "Crop price SD (USD/ton)",
                      min = 0,
                      max = 150,
                      value = 30
                    ),
                    sliderInput(
                      "sd_lime",
                      "Lime price SD (USD/ton)",
                      min = 0,
                      max = 100,
                      value = 20
                    ),
                    numericInput(
                      "n_sim",
                      "Number of simulations",
                      value = 1000,
                      min = 100,
                      max = 10000,
                      step = 500
                    ),
                    checkboxInput("ci", "Show confidence intervals", TRUE),
                    downloadButton("download", "Download Dataset")
                  ),
                  mainPanel(
                    width = 9,
                    tabsetPanel(
                    tabPanel(
                      "Data Overview",
                      fluidRow(column(width = 6, card(
                        card_header("Observation Counts"), card_body(plotlyOutput("p_counts", height = "650px"))
                      )), column(width = 6, card(
                        card_header("Mean Yield ± 95% CI"), card_body(plotlyOutput("p_means", height = "650px"))
                      ))),
                      fluidRow(column(width = 6, card(
                        card_header("Mean Yield Response ± 95% CI"),
                        card_body(plotlyOutput("p_means_resp", height = "650px"))
                      )), column(width = 6, card(
                        card_header("Map of Trial Fields"),
                        card_body(
                          leafletOutput("map_sites", height = "650px")
                        )
                      ))),
                      br(),
                      tags$div(
                        style = "font-size:15px; background-color:#F9FAFB; border-radius:10px; padding:15px;",
                        tags$h4("Interpretation Guide"),
                        tags$ul(
                          tags$li("Top-left: Number of observations (fields) per crop and site."),
                          tags$li(
                            "Top-right: Mean yields across treatments, with 95% confidence intervals."
                          ),
                          tags$li(
                            "Bottom-left: Yield responses relative to control (T1), by treatment and crop."
                          ),
                          tags$li(
                            "Bottom-right: Map of trial fields."
                          )
                        )
                      )
                    )
                    
                    ,
                    # -----------------------------------------------------
                    # 1. Agronomic Responses
                    # -----------------------------------------------------
                    tabPanel(
                      "Agronomic Responses",
                      plotlyOutput("plot_resp", height = "600px", width = "80%"),
                      br(),
                      includeMarkdown("docs/explanation_agronomic.md") # optional external md
                    ),
                    # -----------------------------------------------------
                    # 2. Economic (NR + NPV)
                    # -----------------------------------------------------
                    tabPanel(
                      "Net Revenue and Net Present Value",
                      plotlyOutput("plot_profit", height = "600px", width = "80%"),
                      br(),
                      includeMarkdown("docs/explanation_npv.md")
                    ),
                    # -----------------------------------------------------
                    # 3. Monte Carlo
                    # -----------------------------------------------------
                    tabPanel(
                      "Monte Carlo Simulation",
                      plotlyOutput("plot_sim", height = "550px", width = "80%"),
                      br(),
                      plotlyOutput("plot_npv_dist", height = "550px", width = "80%"),
                      br(),
                      includeMarkdown("docs/explanation_mc.md")
                    ),
                    # -----------------------------------------------------
                    # 4. Model Performance
                    # -----------------------------------------------------
                    tabPanel(
                      "Model Performance",
                      plotlyOutput("plot_perf", height = "500px", width = "80%"),
                      dataTableOutput("table_perf")
                    )
                  )
                )))


# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  # Dynamic selectors
  output$site_ui <- renderUI({
    req(input$country)
    sites <- catalog %>%
      filter(country == input$country) %>%
      pull(admin2_gadm) %>%
      unique()
    selectInput("site", "Site (Admin2)", choices = sites)
  })
  
  output$crop_ui <- renderUI({
    req(input$country, input$site)
    crops <- catalog %>%
      filter(country == input$country, admin2_gadm == input$site) %>%
      pull(crop) %>%
      unique()
    selectInput("crop", "Crop", choices = crops)
  })
  
  # =========================================================
  # Reactive: Subset by selection, always with yield_resp
  # =========================================================
  df_sel <- reactive({
    req(input$country, input$site, input$crop, input$model_type)
    
    dat <- all_models_df %>%
      filter(
        model == input$model_type,
        country == input$country,
        admin2_gadm == input$site,
        crop == input$crop
      ) %>%
      arrange(lime_tha)
    
    # Defensive fallback: if subset has no yield_resp, compute it
    if (!"yield_resp" %in% names(dat) &&
        "yield_tha" %in% names(dat)) {
      dat <- dat %>%
        mutate(yield_resp = yield_tha - first(yield_tha))
    }
    
    # Guarantee structure even if empty
    if (nrow(dat) == 0) {
      dat <- tibble(
        lime_tha = numeric(),
        yield_resp = numeric(),
        lower_resp = numeric(),
        upper_resp = numeric()
      )
    }
    
    dat
  })
  
  #-----------------------------------------------------------
  # Data Overview tab
  #-----------------------------------------------------------
  # --- Data overview plots ---
  output$p_counts <- renderPlotly({
    # --- plot with new labels ---
    p_counts <- counts_labs %>%
      ggplot(aes(x = crop_lab, y = site_lab, fill = n)) +
      geom_tile(color = "white", width = 0.5) +
      geom_text(
        aes(label = n),
        color = "gold",
        size = 5,
        family = my_font
      ) +
      # scale_fill_gradient(low = "lightyellow", high = "gold") +
      # scale_fill_gradientn(colors = cols) +
      scale_fill_gradient2() +
      labs(
        title = "Number of farmer fields by Site × Crop",
        x = "Crop (total N)",
        y = "Sites (total N)",
        fill = "Observations"
      ) +
      ggthemes::theme_pander(base_size = 16, base_family = my_font) +
      theme(
        legend.position = "none",
        plot.title = element_text(
          hjust = 0.5,
          size = 16,
          face = "plain"
        )
      )
    
    ggplotly(p_counts)
  })
  
  output$p_means <- renderPlotly({
    p_means <- yield_summary %>%
      ggplot(aes(
        x = treatment2,
        y = mean_yield,
        color = admin2_gadm,
        group = admin2_gadm
      )) +
      geom_point(position = position_dodge(width = 0.4), size = 2) +
      geom_errorbar(
        aes(ymin = mean_yield - ci95, ymax = mean_yield + ci95),
        width = 0.1,
        position = position_dodge(width = 0.4)
      ) +
      facet_wrap( ~ crop, scales = "free_y", ncol = 2) +
      scale_color_manual(values = bar_colors) +
      guides(color = guide_legend(ncol = 2)) +
      labs(
        x = "",
        y = "Mean yield (t/ha)",
        title = "Mean yield ± 95% CI by treatment",
        color = ""
      ) +
      ggthemes::theme_clean(base_size = 18, base_family = my_font) +
      theme(
        legend.position = c(0.8, 0.05),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(fill = NA, color = NA),
        # legend font
        legend.text = element_text(size = 12, family = my_font),
        plot.title = element_text(
          hjust = 0.5,
          size = 18,
          face = "plain"
        ),
        strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
        panel.grid.major.y = element_line(linewidth = 0.1)
      )
    ggplotly(p_means)
  })
  
  output$p_means_resp <- renderPlotly({
    p_means_resp <- resp_summary %>%
      ggplot(aes(
        x = treatment2,
        y = mean_resp,
        color = admin2_gadm,
        group = admin2_gadm
      )) +
      geom_point(position = position_dodge(width = 0.4), size = 2) +
      geom_errorbar(
        aes(ymin = mean_resp - ci95, ymax = mean_resp + ci95),
        width = 0.1,
        position = position_dodge(width = 0.4)
      ) +
      facet_wrap( ~ crop, scales = "free_y", ncol = 2) +
      guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
      scale_color_manual(values = bar_colors) +
      labs(
        x = "",
        y = "Mean yield response (t/ha)",
        title = "Mean yield response ± 95% CI by treatment",
        color = ""
      ) +
      theme_minimal(base_size = 18, base_family = my_font) +
      theme(
        legend.position = c(0.8, 0.05),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(fill = NA, color = NA),
        # legend font
        legend.text = element_text(size = 12, family = my_font),
        plot.title = element_text(
          hjust = 0.5,
          size = 18,
          face = "plain"
        ),
        strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white")
      )
    
    
    ggplotly(p_means_resp)
  })
  
  output$map_sites <- renderLeaflet({
    # --- Basic map centered on East Africa ---
    leaflet(map_1) |>
      addTiles() |>
      setView(lng = 35, lat = -3, zoom = 5) |>
      addCircleMarkers(
        ~lng,
        ~lat,
        radius = ~n_crops * 5,
        color = ~pal(admin2_gadm),
        stroke = FALSE,
        fillOpacity = 0.5,
        popup = ~ htmltools::HTML(
          paste0(
            "<table style='width:200px; border-collapse:collapse;'>",
            "<tr><th style='text-align:left;'>FID</th><td>", fid, "</td></tr>",
            "<tr><th style='text-align:left;'>Admin2</th><td>", admin2_gadm, "</td></tr>",
            "<tr><th style='text-align:left;'># Crops</th><td>", n_crops, "</td></tr>",
            "<tr><th style='text-align:left;'>Crops</th><td>", crops, "</td></tr>",
            "</table>"
          )
        )
      ) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addLegend(
        "bottomright",
        pal = pal,
        values = ~ admin2_gadm,
        title = "Admin2"
      )
  })
  
  
  # ----------------------------------------------------------
  # Plot 1: Agronomic Response
  # ----------------------------------------------------------
  
  output$plot_resp <- renderPlotly({
    dat <- df_sel()
    req(nrow(dat) > 0)
    message("Reactive data columns: ", paste(names(dat), collapse = ", "))
    if (nrow(dat) == 0 || !"yield_resp" %in% names(dat)) {
      return(
        plotly_empty(
          type = "scatter",
          mode = "text",
          text = "No yield response data available"
        )
      )
    }
    crop_price <- as.numeric(input$price_crop)
    lime_price <- as.numeric(input$price_lime)
    
    ymax_limit <- max(dat$yield_resp, na.rm = TRUE)
    if (!is.finite(ymax_limit) || ymax_limit <= 0)
      ymax_limit <- 1
    ymax_limit <- ymax_limit * 1.2
    
    df_break <- tibble(lime_tha = seq(0, max(dat$lime_tha, na.rm = TRUE), length.out = 100)) %>%
      mutate(req_resp = lime_tha * lime_price / crop_price)
    p <- ggplot(data = dat, aes(x = .data$lime_tha, y = .data$yield_resp)) +
      # --- Red region: unprofitable zone ---
      geom_ribbon(
        data = df_break,
        aes(x = lime_tha, ymin = 0, ymax = req_resp),
        fill = "red",
        alpha = 0.15,
        inherit.aes = FALSE
      ) +
      # --- Green region: profitable zone ---
      geom_ribbon(
        data = df_break,
        aes(x = lime_tha, ymin = req_resp, ymax = ymax_limit),
        fill = "green",
        alpha = 0.1,
        inherit.aes = FALSE
      ) +
      # --- Breakeven line ---
      geom_line(
        data = df_break,
        aes(x = lime_tha, y = req_resp),
        color = "red",
        linetype = "dashed",
        linewidth = 0.7,
        inherit.aes = FALSE
      ) +
      # --- Model confidence interval ribbon ---
      {
        if (input$ci &&
            all(c("lower_resp", "upper_resp") %in% names(dat))) {
          geom_ribbon(
            aes(
              x = .data$lime_tha,
              ymin = .data$lower_resp,
              ymax = .data$upper_resp
            ),
            alpha = 0.20,
            fill = "grey70"
          )
        }
      } +
      # --- Yield response curve ---
      geom_line(
        aes(x = .data$lime_tha, y = .data$yield_resp),
        color = "black",
        linewidth = 1.1
      ) +
      geom_point(
        aes(x = .data$lime_tha, y = .data$yield_resp),
        size = 2,
        color = "black"
      ) +
      labs(
        x = "Lime application rate (t/ha)",
        y = "Yield response (t/ha, vs control)",
        title = paste0(
          input$crop,
          " | ",
          input$site,
          ", ",
          input$country,
          " — Model: ",
          toupper(input$model_type)
        )
      ) +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # ----------------------------------------------------------
  # Plot 2: Net Revenue & NPV
  # ----------------------------------------------------------
  output$plot_profit <- renderPlotly({
    dat <- df_sel()
    req(nrow(dat) > 0)
    crop_price <- input$price_crop
    lime_price <- input$price_lime
    r <- input$discount / 100
    T <- input$years
    disc_factor <- sum(1 / (1 + r)^(1:T))
    
    df_econ <- dat %>%
      mutate(
        net_revenue = yield_resp * crop_price - lime_tha * lime_price,
        npv = net_revenue * disc_factor
      )
    
    p2 <- ggplot(df_econ, aes(x = lime_tha)) +
      geom_line(aes(y = net_revenue, color = "Net Revenue"), linewidth = 1.2) +
      geom_line(aes(y = npv, color = "NPV"),
                linewidth = 1.2,
                linetype = "dashed") +
      geom_vline(
        xintercept = df_econ$lime_tha[which.max(df_econ$npv)],
        color = "grey40",
        linetype = "dotted"
      ) +
      scale_color_manual(values = c(
        "Net Revenue" = "#0072B2",
        "NPV" = "#009E73"
      )) +
      labs(
        x = "Lime rate (t/ha)",
        y = "Profit (USD/ha)",
        color = "",
        title = "Net Revenue and Net Present Value (USD/ha)"
      ) +
      theme_minimal(base_size = 14)
    ggplotly(p2)
  })
  
  # ----------------------------------------------------------
  # Monte Carlo simulations
  # ----------------------------------------------------------
  output$plot_sim <- renderPlotly({
    dat <- df_sel()
    req(nrow(dat) > 0)
    n_sim <- input$n_sim
    mu_crop <- input$price_crop
    mu_lime <- input$price_lime
    sd_crop <- input$sd_crop
    sd_lime <- input$sd_lime
    crop_prices <- rnorm(n_sim, mu_crop, sd_crop)
    lime_prices <- rnorm(n_sim, mu_lime, sd_lime)
    
    sim_df <- dat %>%
      mutate(prob_profit = map_dbl(lime_tha, \(L) mean((yield_resp * crop_prices - L * lime_prices) > 0, na.rm = TRUE
      )))
    if (nrow(sim_df) == 0)
      return(plotly_empty(text = "No data"))
    p_prob <- ggplot(sim_df, aes(lime_tha, prob_profit)) +
      geom_line(color = "#1871B8", linewidth = 1.3) +
      geom_point(size = 2, color = "#1871B8") +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(
        x = "Lime rate (t/ha)",
        y = "P(Profit > 0)",
        title = paste0("Monte Carlo Profitability — ", input$crop, " | ", input$site)
      ) +
      theme_minimal(base_size = 14)
    ggplotly(p_prob)
  })
  
  output$plot_npv_dist <- renderPlotly({
    dat <- df_sel()
    req(nrow(dat) > 0)
    n_sim <- input$n_sim
    mu_crop <- input$price_crop
    mu_lime <- input$price_lime
    sd_crop <- input$sd_crop
    sd_lime <- input$sd_lime
    r <- input$discount / 100
    T <- input$years
    disc_factor <- sum(1 / (1 + r)^(1:T))
    crop_prices <- rnorm(n_sim, mu_crop, sd_crop)
    lime_prices <- rnorm(n_sim, mu_lime, sd_lime)
    
    npv_sims <- replicate(n_sim, {
      pc <- sample(crop_prices, 1)
      pl <- sample(lime_prices, 1)
      df_tmp <- dat %>% mutate(net_revenue = yield_resp * pc - lime_tha * pl,
                               npv = net_revenue * disc_factor)
      max(df_tmp$npv, na.rm = TRUE)
    })
    npv_df <- tibble(NPV = npv_sims)
    p_npv <- ggplot(npv_df, aes(NPV)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 40,
        fill = "#009E73",
        alpha = 0.8
      ) +
      geom_vline(xintercept = 0,
                 color = "red",
                 linetype = "dashed") +
      geom_density(color = "#004D40") +
      labs(
        x = "NPV (USD/ha)",
        y = "Density",
        title = paste0("NPV distribution — ", input$crop, " | ", input$site),
        subtitle = paste0(
          "Mean = ",
          round(mean(npv_sims), 1),
          " | P(NPV>0) = ",
          round(mean(npv_sims > 0) * 100, 1),
          "%"
        )
      ) +
      theme_minimal(base_size = 14)
    ggplotly(p_npv)
  })
  
  # ----------------------------------------------------------
  # Model performance tab
  # ----------------------------------------------------------
  output$plot_perf <- renderPlotly({
    req(!is.null(perf_summary))
    p <- perf_summary %>%
      mutate(facet_label = paste(country, admin2_gadm, sep = " | ")) %>%
      filter(crop == input$crop) %>%
      ggplot(aes(model, R2, fill = model)) +
      geom_col() +
      facet_wrap( ~ facet_label) +
      labs(y = "R²", title = "Model performance by site") +
      scale_fill_manual(values = bar_colors) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$table_perf <- renderDataTable({
    req(!is.null(perf_summary))
    perf_summary %>% filter(crop == input$crop) %>% arrange(desc(R2)) %>%
      mutate(R2 = round(R2, 4), RMSE = round(RMSE, 4))
  })
  
  # ----------------------------------------------------------
  # Download
  # ----------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0(
        "lime_profit_",
        input$country,
        "_",
        input$site,
        "_",
        input$crop,
        "_",
        input$model_type,
        ".csv"
      )
    },
    content = function(file) {
      write.csv(df_sel(), file, row.names = FALSE)
    }
  )
}

# =========================================================
# Run app
# =========================================================
shinyApp(ui, server)