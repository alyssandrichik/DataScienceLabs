# Libraries
library(shiny)
library(tidyverse)
library(babynames)
library(DT)
library(fredr)
library(purrr)
library(ggplot2)
library(lubridate)
fredr_set_key("eabb390665ab70b057e87529f8a547b2")


# User interface
ui <- fluidPage(
  titlePanel(title = "U.S. Economic Data Explorer (FRED)"),
  tags$div(
    tags$h4("Look at key variables (Unemployment Rate, Federal Funds Interest Rate, GDP per capita growth, 5 year inflation, 5 year expected inflation) from the Federal Reserve Economic Data (FRED). Ask interesting questions such as: what is the relationship of the different rates of change in the U.S. economy over time? Consider looking at short time spans for more clear depictions of the data."), 
    tags$h5("By: Blaise Albis-Burdige & Alyssa Andrichik"),
    tags$a(href="https://cran.r-project.org/web/packages/fredr/index.html", "Source: https://cran.r-project.org/web/packages/fredr/index.html"),),
  sidebarLayout(
    sidebarPanel(
      tags$em("The following input is for the first output plot that looks at variables over time. Put single space between the ID's."),
      p(""),
      selectizeInput(inputId = "names",
                     label = "Enter FRED Variable ID",
                     choices = NULL,
                     multiple = TRUE),
      tags$em("The following inputs are for the second output plot that compares the relationship between two economic rates."),
      p(""),
      selectizeInput(inputId = "x_var",
                     label = "Enter x Variable Choice for Interaction",
                     choices = NULL,
                     multiple = FALSE),
      selectizeInput(inputId = "y_var",
                     label = "Enter Y Variable Choice for Interaction",
                     choices = NULL,
                     multiple = FALSE),
      sliderInput("year_range", "Range of Years:",
                  min = 1980, 
                  max = 2021,
                  value = c(min,max),
                  sep = ""),
      tags$em("Note that the year range is for both plots."),
      p(""),
      
      submitButton("Update Results!")
    ),
    mainPanel(
      plotOutput(outputId = "line_chart"),
      plotOutput(outputId = "scatter_graph")
    )
  )
)
# Server function
server <- function(input, output, session){
  updateSelectizeInput(session, 'names', 
                       choices = c("UNRATE", "FEDFUNDS","CPGDPAI","T5YIE","T5YIFR"), 
                       server = TRUE)
  
  updateSelectizeInput(session, 'x_var', 
                       choices = c("UNRATE", "FEDFUNDS","CPGDPAI","T5YIE","T5YIFR"), 
                       server = TRUE)
  
  updateSelectizeInput(session, 'y_var', 
                       choices = c("UNRATE", "FEDFUNDS","CPGDPAI","T5YIE","T5YIFR"), 
                       server = TRUE)
  dat_names <- reactive({
    #dropdown for time series
    # slider for time selection
    map_dfr(c("UNRATE", "FEDFUNDS","CPGDPAI","T5YIE","T5YIFR"), fredr) %>% mutate(date=ymd(date)) %>%
      filter(series_id %in% c(unlist(str_split(input$names, " "))),
             year(date) > input$year_range[1], year(date) < input$year_range[2]) 
  })
  output$line_chart <- renderPlot({
   # dat_names %>%
      ggplot(data=(dat_names()), mapping = aes(x = date, y = value, color = series_id)) +
      geom_line() +
      labs(x = "Observation Date", y = "Rate", color = "Series")
  })
  dat_names_agg <- reactive({ 
    map_dfr(c("UNRATE", "FEDFUNDS","CPGDPAI","T5YIE","T5YIFR"), fredr) %>% mutate(date=ymd(date)) %>%
      filter(
             year(date) > input$year_range[1], year(date) < input$year_range[2]) %>%  pivot_wider(names_from = series_id, values_from = value) 
  })
  output$scatter_graph <-  renderPlot({
    dat_names_agg() %>% ggplot( aes_string(x={input$x_var}, y={input$y_var})) +
      geom_point()
  })
}
# Creates app
shinyApp(ui = ui, server = server)