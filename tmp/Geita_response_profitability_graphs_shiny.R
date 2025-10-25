library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Yes, but is it profitable?"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("p_m",
                  "Maize price (USD/MT)",
                  min = 50,
                  max = 300,
                  value = 150,
                  step=5),
      sliderInput("p_l",
                  "Lime price (USD/MT)",
                  min = 50,
                  max = 300,
                  value = 75,
                  step = 5)
    ),
    mainPanel(
      plotOutput("ResponseProfitability"),
      #textOutput("description") # Text output for the description
      uiOutput("description") # Use uiOutput for HTML content
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$ResponseProfitability <- renderPlot({

    # define price ratio from inputs
    ## ratio p_m/p_l
    pratio <- input$p_m / input$p_l
    
    
    fun1 <- function(x) (y = x/pratio)
    
    x <- 0:10
    y <- fun1(x)
    df.pr <- data.frame(x,y)
    
      # response data 
      df.ar <- as.data.frame(matrix(
        c(1.727, 7.5,
          1.706, 5.5,
          1.500, 4.5,
          1.313, 3,  
          0.860, 2, 
          0.479, 1,  
          0,   0), byrow=TRUE, ncol=2))
      names(df.ar) <-c("y", "x")
      df.ar
      
      annotation <- data.frame(
        l1 = c(5.25, 5.25),
        l2 = c(0.8, 0.65),
        label = c(paste("Maize price =", p_m,"USD/MT"), paste("Lime price =", p_l,"USD/MT "))
      )   
        
      ggplot(df.ar, aes(x, y), ylim(0, 3), xlim(0, 8)) + 
      geom_line(data = df.ar, col = "green4") + geom_point(data = df.ar, col = "green4", size=3) +
      geom_ribbon(data = df.pr, ymin=-Inf, aes(ymax=y), fill='red', alpha=0.2) +
      geom_ribbon(data = df.pr, aes(ymin=y), ymax=Inf, fill='green', alpha=0.2) +
      coord_cartesian(ylim = c(0,3), xlim = c(0, 8)) +
      ylab("Yield response attributed to liming (MT/HA)") + xlab("Lime application rate (MT/HA)") +
      geom_text(data=annotation, aes( x=l1, y=l2, label=label), 
                color="white", 
                size=3 , angle=0, fontface="bold" )
  })

  # Render text for the description
  
  # output$description <- renderText({
  #   return("The values in green are estimated responses to lime treatments, where the additional yield (relative to yield wiht zero lime) is plotted on the y-axis and the amount of lime is plotted on the x-axis. Use the sliders to adjust the assumed farmgate maize and lime prices and observe how the profitability of the lime investments change in response. The goal is to allow users to explore how positive agronomic responses to lime may or may not actually be profitable for a farmer.")
  # })
  
  output$description <- renderUI({
    HTML("The values plotted in <span style='color:green;font-weight:bold;'>green</span> are estimated responses to lime treatments, where the additional yield (relative to yield wiht zero lime) is plotted on the y-axis and the amount of lime is plotted on the x-axis. Use the sliders to adjust the assumed farmgate maize and lime prices and observe how the profitability of the lime investments change in response. The goal is to allow users to explore how positive agronomic responses to lime may or may not actually be profitable for a farmer.")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
