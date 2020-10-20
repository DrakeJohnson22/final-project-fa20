#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)

# Define UI for application that draws a histogram

x <- read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_ev_probabilities_2020.csv",
              col_types = cols(cycle = col_double(),
                               branch = col_character(),
                               model = col_character(),
                               modeldate = col_character(),
                               candidate_inc = col_character(),
                               candidate_chal = col_character(),
                               candidate_3rd = col_logical(),
                               evprob_inc = col_double(),
                               evprob_chal = col_double(),
                               evprob_3rd = col_logical(),
                               total_ev = col_double(),
                               timestamp = col_character(),
                               simulations = col_double()))

x4 <- read_csv("raw_data/mmALL_073119_csv.csv")

ui <- navbarPage(
    "Final Project Title",
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Drake and I study Government and African American Studies. 
             You can reach me at drakejohnson@college.harvard.edu."),
             a("Dan made me link Google", href = "https://www.google.com"),
    ),
    
    
    tabPanel("Model",
             fluidPage(
                 selectInput("x", "Pick your x variable", choices = names(x)),
                 selectInput("y", "Pick your y variable", choices = names(x)),
                 selectInput("geom", "drake", c("point", "jitter", "smooth", "line", "col")),
                 plotOutput("plot1")
             )),
    
    tabPanel("Milestone 4",
             fluidPage(
                 selectInput("x", "Pick your x variable", choices = names(x4)),
                 selectInput("y", "Pick your y variable", choices = names(x4)),
                 selectInput("geom", "drake", c("point", "jitter", "smooth", "line", "col")),
                 plotOutput("plot2")
             )),
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu."))
)

# Define server logic required to draw a histogram

server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               jitter = geom_jitter(),
               smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               line = geom_line(),
               col = geom_col())
    })
    
    output$plot1 <- renderPlot({
        ggplot(x, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
    
    output$plot2 <- renderPlot({
        ggplot(x4, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)