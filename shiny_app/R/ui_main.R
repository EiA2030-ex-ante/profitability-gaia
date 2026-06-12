# R/ui_main.R
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})
ui <- fluidPage(theme = my_theme_light,
                checkboxInput("dark_mode", "Dark mode"),
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
                    tags$h5("Prices (relative to baseline)"),
                    sliderInput(
                      "pct_crop_price",
                      "Crop price adjustment (%)",
                      min = -80,
                      max = 200,
                      value = 0,
                      step = 5
                    ),
                    sliderInput(
                      "pct_lime_price",
                      "Lime price adjustment (%)",
                      min = -80,
                      max = 200,
                      value = 0,
                      step = 5
                    ),
                    hr(),
                    tags$h5("Monte Carlo (price volatility as % SD)"),
                    sliderInput(
                      "sd_crop_pct",
                      "Crop price SD (%)",
                      min = 0,
                      max = 100,
                      value = 15,
                      step = 1
                    ),
                    sliderInput(
                      "sd_lime_pct",
                      "Lime price SD (%)",
                      min = 0,
                      max = 100,
                      value = 10,
                      step = 1
                    ),
                    hr(),
                    sliderInput(
                      "years",
                      "Years",
                      min = 1,
                      max = 10,
                      value = 5
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
                      "benefit_decay",
                      "Benefit decay per year (%):",
                      min = 0,
                      max = 50,
                      value = 0,
                      step = 1
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
                        fluidRow(column(
                          width = 6, bslib::card(
                            bslib::card_header("Observation Counts"),
                            bslib::card_body(plotlyOutput("p_counts", height = "500px"))
                          )
                        ), column(
                          width = 6, bslib::card(
                            bslib::card_header("Mean Yield ± 95% CI"),
                            bslib::card_body(plotlyOutput("p_means", height = "500px"))
                          )
                        )),
                        fluidRow(column(
                          width = 6, bslib::card(
                            bslib::card_header("Mean Yield Response ± 95% CI"),
                            bslib::card_body(plotlyOutput("p_means_resp", height = "500px"))
                          )
                        ), column(
                          width = 6, bslib::card(
                            bslib::card_header("Map of Trial Fields"),
                            bslib::card_body(leafletOutput("map_sites", height = "500px"))
                          )
                        )),
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
                            tags$li("Bottom-right: Map of trial fields.")
                          )
                        )
                      ),
                      tabPanel(
                        "Agronomic Responses",
                        plotlyOutput("plot_resp", height = "600px", width = "50%"),
                        br(),
                        includeMarkdown("docs/explanation_agronomic.md")
                      ),
                      tabPanel(
                        "Net Revenue and Net Present Value",
                        plotlyOutput("plot_profit", height = "600px", width = "50%"),
                        br(),
                        includeMarkdown("docs/explanation_npv.md")
                      ),
                      tabPanel(
                        "Monte Carlo Simulation",
                        plotlyOutput("plot_sim", height = "550px", width = "50%"),
                        br(),
                        plotlyOutput("plot_npv_dist", height = "550px", width = "50%"),
                        br(),
                        includeMarkdown("docs/explanation_mc.md")
                      ),
                      tabPanel(
                        "Model Performance",
                        plotlyOutput("plot_perf", height = "500px", width = "50%"),
                        dataTableOutput("table_perf")
                      ),
                      tabPanel(
                        "Advanced Analysis",
                        tabsetPanel(
                          tabPanel("Risk & VaR", plotlyOutput("plot_rar"), plotlyOutput("plot_var")),
                          tabPanel("Shock Scenarios", plotlyOutput("plot_shocks")),
                          tabPanel("Scaling Curve", plotlyOutput("plot_scaling")),
                          tabPanel("Dynamic Adoption", plotlyOutput("plot_adoption"))
                        )
                      )
                    )
                  )
                ))
