source("func.R")

#####################################
# Random Person App - Shiny Driver  #
# D. Russel                         #
#####################################

########## UI #############
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  mobileDetect('isMobile'),
  
  # Application title
  tags$div(class = "title", "The Identity Generator"),
  
  tags$div(
    class = "body",
    
    tags$br(),
    uiOutput("identity"),
    
    # Button
    actionButton("generate", "Generate an identity"),
    textOutput("the_counter"),
    tags$br(),
    
    # Global Graph - Desktop
    conditionalPanel("input.isMobile == false",
      plotlyOutput("global_plot")
    ),
    
    tags$br(),
    
    # Global Graph - Mobile
    conditionalPanel("input.isMobile == true",
      tags$label(
        tags$input(id="show_global", type="checkbox"),
        tags$span(id="check_text", "Show how this compares to the global distribution")
      ),
      conditionalPanel("input.show_global == true",
                       plotOutput("global_plot.mobile"))
    ),
    tags$label(
      tags$input(id="show_info", type ="checkbox"),
      tags$span(id="check_text2", "Show me your math and assumptions")
    ),
    tags$br(),
    conditionalPanel("input.show_info == true",
                     tags$p(id="information",
                            "This is app version 0.3. It attempts to generate a random person and their income using the World Bank income and population data.
                            As is widely noted - income data has large uncertainty. Income data is collected in many different ways in different countries and can represent
                            different things. For simplicity, we rely entirely on the World Bank data. This is not intended to be a perfectly technical analysis of global incomes.
                            To generate our estimates, we first sample a country, weighted by population. Then sample the in-country quantile from a uniform distribution.
                            We estimate an individual at that quantile's income by plugging this uniform random variable into the CDF of the country's Lorenz Curve
                            and subtracting that value by the value of the cdf of the lorenz curve at quantile-(1/pop). The Lorenz curve CDF is
                            estimated by a Pareto distribution with parameters based-on the country's median income and gini coefficient. The use of the Pareto distribution is informed by:",
                            tags$a(href="http://www.cgeh.nl/sites/default/files/WorkingPapers/CGEH.WP_.No1_.Vanzandenetal.jan2011_0.pdf",
                                   "http://www.cgeh.nl/sites/default/files/WorkingPapers/CGEH.WP_.No1_.Vanzandenetal.jan2011_0.pdf"))
    )
  )
)

#### Server ####
server <- function(input, output) {
  
  values <- reactiveValues()
  values$the_counter <- 0
  values$country <- NULL
  values$income <- -100000
  
  observeEvent(input$generate, {
    ident <- get_person()
    values$the_counter <- values$the_counter + 1
    values$country <- ident[[1]]
    values$income <- ident[[2]]
  })
  
  output$identity <- renderUI({
    if(is.null(values$country)) ""
    else{
      tags$div("You are from", tags$b(values$country), 
             "and your annual income (purchasing-power-adjusted to US$) is", 
             tags$b(dollar_format()(round(values$income))))
    }
  })
  
  output$the_counter <- renderText({
    paste("Number of identities generated =", values$the_counter)
  })
  
  output$global_plot <- renderPlotly({
    if(values$income < 70000){
      global_plot %>%
        layout(shapes = list(vline(values$income)),
               xaxis = list(title = 'Annual Income (PPP Adjusted to US$)', range=c(0,70000)))
    }
    else{
      global_plot %>%
        layout(shapes = list(vline(values$income)),
               xaxis = list(title = 'Annual Income (PPP Adjusted to US$)', range=c(0,values$income+5000)))
    }
  })
  
  output$global_plot.mobile <- renderPlot({
    if(values$income < 70000){
      global_plot.mobile + 
        geom_vline(xintercept = values$income, color='black', linetype = 'dashed', size=1)
    }
    else{
      global_plot.mobile + 
        geom_vline(xintercept = values$income, color='black', linetype = 'dashed', size=1) +
        scale_x_continuous(labels = dollar, limits = c(0,values$income))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)