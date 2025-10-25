# R/server_main.R
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(plotly)
  library(leaflet)
  library(ggplot2)
  library(scales)
})

server <- function(input, output, session) {
  
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) my_theme_dark else my_theme_light
  ))
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

  eff_prices <- reactive({
    req(input$country, input$site, input$crop)
    base_crop_price <- base_crop_prices %>%
      filter(
        country == input$country,
        admin2_gadm == input$site,
        crop == input$crop
      ) %>%
      pull(crop_price_base) %>%
      first()
    base_lime <- base_lime_price %>%
      filter(country == input$country) %>%
      pull(lime_price_base) %>%
      first()
    if (is.na(base_crop_price)) {
      base_crop_price <- 160
    }
    if (is.na(base_lime)) {
      base_lime <- 55
    }
    tibble::tibble(
      crop_price_eff = as.numeric(base_crop_price) * (1 + input$pct_crop_price / 100),
      lime_price_eff = as.numeric(base_lime) * (1 + input$pct_lime_price / 100)
    )
  })

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
    if (!"yield_resp" %in% names(dat) &&
      "yield_tha" %in% names(dat)) {
      dat <- dat %>% mutate(yield_resp = yield_tha - dplyr::first(yield_tha))
    }
    if (nrow(dat) == 0) {
      dat <- tibble::tibble(
        lime_tha = numeric(),
        yield_resp = numeric(),
        lower_resp = numeric(),
        upper_resp = numeric()
      )
    }
    dat
  })
  
  # --- Reactive effective prices (used across all modules) ---
  p_eff <- reactive({
    req(input$country, input$site, input$crop)
    eff_prices()
  })
  
  crop_price <- reactive({
    req(p_eff())
    as.numeric(p_eff()$crop_price_eff[1])
  })
  
  lime_price <- reactive({
    req(p_eff())
    as.numeric(p_eff()$lime_price_eff[1])
  })
  
  disc_factor <- reactive({
    r <- input$discount / 100
    T <- ifelse(is.null(input$years) || length(input$years) == 0, 5, input$years)
    decay <- ifelse(is.null(input$benefit_decay), 0, input$benefit_decay) / 100
    pv_factor(T, r, decay)
  })

  # Data Overview
  output$p_counts <- renderPlotly({
    p <- counts_labs %>%
      ggplot(aes(x = crop_lab, y = site_lab, fill = n)) +
      geom_tile(color = "white", width = 0.5) +
      geom_text(
        aes(label = n),
        color = "gold",
        size = 5,
        family = my_font
      ) +
      scale_fill_gradient2() +
      labs(
        title = "",
        x = "Crop (total N)",
        y = "Sites (total N)",
        fill = "Observations"
      ) +
      theme_minimal(base_size = 16, base_family = my_font) +
      theme(legend.position = "none")
    ggplotly(p)
  })

  output$p_means <- renderPlotly({
    p <- yield_summary %>%
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
      facet_wrap(~crop, scales = "free_y", ncol = 2) +
      scale_color_manual(values = bar_colors) +
      guides(color = guide_legend(ncol = 2)) +
      labs(
        x = "",
        y = "Mean yield (t/ha)",
        title = "",
        color = ""
      ) +
      theme_minimal(base_size = 18, base_family = my_font) +
      theme(
        legend.position = c(0.8, 0.05),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(fill = NA, color = NA),
        strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white")
      )
    ggplotly(p)
  })

  output$p_means_resp <- renderPlotly({
    p <- resp_summary %>%
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
      facet_wrap(~crop, scales = "free_y", ncol = 2) +
      scale_color_manual(values = bar_colors) +
      labs(
        x = "",
        y = "Mean yield response (t/ha)",
        title = "",
        color = ""
      ) +
      theme_minimal(base_size = 18, base_family = my_font) +
      theme(
        legend.position = c(0.8, 0.05),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(fill = NA, color = NA),
        strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white")
      )
    ggplotly(p)
  })

  output$map_sites <- renderLeaflet({
    leaflet(map_1) |>
      addTiles() |>
      setView(
        lng = 35,
        lat = -3,
        zoom = 4
      ) |>
      addCircleMarkers(
        ~lng,
        ~lat,
        radius = ~ n_crops * 5,
        color = ~ pal(admin2_gadm),
        stroke = FALSE,
        fillOpacity = 0.5,
        popup = ~ htmltools::HTML(
          paste0(
            "<table style='width:200px; border-collapse:collapse;'>",
            "<tr><th style='text-align:left;'>FID</th><td>",
            fid,
            "</td></tr>",
            "<tr><th style='text-align:left;'>Admin2</th><td>",
            admin2_gadm,
            "</td></tr>",
            "<tr><th style='text-align:left;'># Crops</th><td>",
            n_crops,
            "</td></tr>",
            "<tr><th style='text-align:left;'>Crops</th><td>",
            crops,
            "</td></tr>",
            "</table>"
          )
        )
      ) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addLegend(
        "bottomright",
        pal = pal,
        values = ~admin2_gadm,
        title = "Admin2"
      )
  })

  # Agronomic Responses
  output$plot_resp <- renderPlotly({
    dat <- df_sel()
    req(nrow(dat) > 0)
    p_eff <- eff_prices()
    crop_price <- as.numeric(p_eff$crop_price_eff[1])
    lime_price <- as.numeric(p_eff$lime_price_eff[1])
    ymax_limit <- max(dat$yield_resp, na.rm = TRUE)
    if (!is.finite(ymax_limit) || ymax_limit <= 0) {
      ymax_limit <- 1
    }
    ymax_limit <- ymax_limit * 1.2
    df_break <- tibble::tibble(lime_tha = seq(0, max(dat$lime_tha, na.rm = TRUE), length.out = 100)) %>%
      dplyr::mutate(req_resp = lime_tha * lime_price / crop_price)
    p <- ggplot(dat, aes(x = .data$lime_tha, y = .data$yield_resp)) +
      geom_ribbon(
        data = df_break,
        aes(x = lime_tha, ymin = 0, ymax = req_resp),
        fill = "red",
        alpha = 0.15,
        inherit.aes = FALSE
      ) +
      geom_ribbon(
        data = df_break,
        aes(x = lime_tha, ymin = req_resp, ymax = ymax_limit),
        fill = "green",
        alpha = 0.1,
        inherit.aes = FALSE
      ) +
      geom_line(
        data = df_break,
        aes(x = lime_tha, y = req_resp),
        color = "red",
        linetype = "dashed",
        linewidth = 0.7,
        inherit.aes = FALSE
      ) +
      {
        if (isTRUE(input$ci) &&
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

  # Economic (NR + NPV)
  output$plot_profit <- renderPlotly({
    dat <- df_sel()
    req(nrow(dat) > 0)
    p_eff <- eff_prices()
    crop_price <- as.numeric(p_eff$crop_price_eff[1])
    lime_price <- as.numeric(p_eff$lime_price_eff[1])
    r <- input$discount / 100
    T <- ifelse(is.null(input$years) ||
      length(input$years) == 0,
    5,
    input$years
    )
    decay <- ifelse(is.null(input$benefit_decay), 0, input$benefit_decay) / 100
    disc_factor <- pv_factor(T, r, decay)

    df_econ <- dat %>%
      mutate(
        net_revenue = yield_resp * crop_price - lime_tha * lime_price,
        npv = (yield_resp * crop_price) * disc_factor - lime_tha * lime_price
      )

    p <- ggplot(df_econ, aes(x = lime_tha)) +
      geom_line(aes(y = net_revenue, color = "Net Revenue"), linewidth = 1.2) +
      geom_line(aes(y = npv, color = "NPV"),
        linewidth = 1.2,
        linetype = "dashed"
      ) +
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
    ggplotly(p)
  })

  # Monte Carlo: Profitability
  output$plot_sim <- renderPlotly({
    dat <- df_sel()
    req(nrow(dat) > 0)
    p_eff <- eff_prices()
    mu_crop <- as.numeric(p_eff$crop_price_eff[1])
    mu_lime <- as.numeric(p_eff$lime_price_eff[1])
    sd_crop <- mu_crop * (input$sd_crop_pct / 100)
    sd_lime <- mu_lime * (input$sd_lime_pct / 100)
    n_sim <- input$n_sim

    r <- input$discount / 100
    T <- ifelse(is.null(input$years) ||
      length(input$years) == 0,
    5,
    input$years
    )
    decay <- ifelse(is.null(input$benefit_decay), 0, input$benefit_decay) / 100
    disc_factor <- pv_factor(T, r, decay)

    crop_prices <- pmax(rnorm(n_sim, mu_crop, sd_crop), 0)
    lime_prices <- pmax(rnorm(n_sim, mu_lime, sd_lime), 0)

    sim_df <- dat %>%
      mutate(prob_npv_pos = purrr::map_dbl(lime_tha, \(L) {
        npv_vec <- (yield_resp * crop_prices) * disc_factor - L * lime_prices
        mean(npv_vec > 0, na.rm = TRUE)
      }))

    p_prob <- ggplot(sim_df, aes(lime_tha, prob_npv_pos)) +
      geom_line(color = "#1871B8", linewidth = 1.3) +
      geom_point(size = 2, color = "#1871B8") +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1)
      ) +
      labs(
        x = "Lime rate (t/ha)",
        y = "P(NPV > 0)",
        title = paste0("Monte Carlo Profitability — ", input$crop, " | ", input$site),
        subtitle = paste0(
          "Baseline P(C)=",
          round(mu_crop, 1),
          " USD/t (±",
          input$sd_crop_pct,
          "%),  ",
          "P(L)=",
          round(mu_lime, 1),
          " USD/t (±",
          input$sd_lime_pct,
          "%); ",
          "Decay=",
          input$benefit_decay,
          "%/yr"
        )
      ) +
      theme_minimal(base_size = 14)

    ggplotly(p_prob)
  })

  # Monte Carlo: NPV distribution + optimal rate
  output$plot_npv_dist <- renderPlotly({
    dat <- df_sel()
    req(nrow(dat) > 0)
    p_eff <- eff_prices()
    mu_crop <- as.numeric(p_eff$crop_price_eff[1])
    mu_lime <- as.numeric(p_eff$lime_price_eff[1])
    sd_crop <- mu_crop * (input$sd_crop_pct / 100)
    sd_lime <- mu_lime * (input$sd_lime_pct / 100)
    n_sim <- input$n_sim
    r <- input$discount / 100
    T <- ifelse(is.null(input$years) ||
      length(input$years) == 0,
    5,
    input$years
    )
    disc_factor <- discount_factor(T, r)
    crop_prices <- pmax(rnorm(n_sim, mu_crop, sd_crop), 0)
    lime_prices <- pmax(rnorm(n_sim, mu_lime, sd_lime), 0)
    npv_sims <- numeric(n_sim)
    opt_rate <- numeric(n_sim)

    for (i in seq_len(n_sim)) {
      pc <- crop_prices[i]
      pl <- lime_prices[i]
      df_tmp <- dat %>%
        mutate(
          npv = (yield_resp * pc) * disc_factor - lime_tha * pl,
          net_revenue = yield_resp * pc - lime_tha * pl
        )
      j <- which.max(df_tmp$npv)
      npv_sims[i] <- df_tmp$npv[j]
      opt_rate[i] <- df_tmp$lime_tha[j]
    }
    sim_df <- tibble::tibble(NPV = npv_sims, OptimalRate = opt_rate)
    p1 <- ggplot(sim_df, aes(NPV)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 40,
        fill = "#009E73",
        alpha = 0.8,
        color = "white"
      ) +
      geom_vline(
        xintercept = 0,
        color = "red",
        linetype = "dashed"
      ) +
      geom_density(color = "#004D40") +
      labs(
        x = "NPV (USD/ha)",
        y = "Density",
        title = paste0("NPV distribution — ", input$crop, " | ", input$site),
        subtitle = paste0(
          "Mean NPV = ",
          round(mean(sim_df$NPV), 1),
          " | P(NPV>0) = ",
          round(mean(sim_df$NPV > 0) * 100, 1),
          "%"
        )
      ) +
      theme_minimal(base_size = 14)

    ggplotly(p1)
  })

  output$plot_rar <- renderPlotly({
    res <- mod_risk_analysis(
      dat = df_sel(),
      crop_price = crop_price(),
      lime_price = lime_price(),
      disc_factor = disc_factor()
    )
    res$plot_rar
  })
  
  output$plot_var <- renderPlotly({
    res <- mod_risk_analysis(
      dat = df_sel(),
      crop_price = crop_price(),
      lime_price = lime_price(),
      disc_factor = disc_factor()
    )
    res$plot_var
  })
  
  output$plot_shocks <- renderPlotly({
    mod_shocks(
      dat = df_sel(),
      crop_price = crop_price(),
      lime_price = lime_price(),
      disc_factor = disc_factor()
    )
  })
  
  output$plot_scaling <- renderPlotly({
    mod_scaling(
      df_map = map_1,
      dat = df_sel(),
      crop_price = crop_price(),
      lime_price = lime_price(),
      disc_factor = disc_factor()
    )
  })
  
  output$plot_adoption <- renderPlotly({
    mod_adoption(
      df_econ = df_sel(),
      max_adopt = 0.8,
      T = input$years,
      r = input$discount / 100,
      decay = (input$benefit_decay %||% 0) / 100
    )
  })

  # Model performance
  output$plot_perf <- renderPlotly({
    req(!is.null(perf_summary))
    p <- perf_summary %>%
      mutate(facet_label = paste(country, admin2_gadm, sep = " | ")) %>%
      filter(crop == input$crop) %>%
      ggplot(aes(model, R2, fill = model)) +
      geom_col() +
      facet_wrap(~facet_label) +
      labs(y = "R²", title = "Model performance by site") +
      scale_fill_manual(values = bar_colors) +
      theme_minimal()
    ggplotly(p)
  })
  output$table_perf <- renderDataTable({
    req(!is.null(perf_summary))
    perf_summary %>%
      filter(crop == input$crop) %>%
      arrange(desc(R2)) %>%
      mutate(R2 = round(R2, 4), RMSE = round(RMSE, 4))
  })
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