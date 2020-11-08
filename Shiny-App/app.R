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
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)
library(ggthemes)

# Define UI for application that draws a histogram

d <- read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_ev_probabilities_2020.csv",
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

d4 <- read_csv("raw_data/mmALL_073119_csv.csv") %>%
  select(-c(id, ccode, protestnumber, location, participants,
            protesteridentity, sources, notes)) %>%
  group_by(region, protest) %>%
  mutate(level = sum(protest)) %>%
  select(level, region, protest)

d6 <- read_excel("raw_data/Master_court_revise_12_3_05.xls") %>%
  select(Case_Year, School_Segregation_Case, Court_Desegregation_Plan) %>%
  group_by(Case_Year) %>%
  filter(School_Segregation_Case == "Yes") %>%
  mutate(total_cases = n()) %>%
  pivot_wider(names_from = Court_Desegregation_Plan,
              values_from = Court_Desegregation_Plan) %>%
  mutate(ruled_deseg = table(Yes)) %>%
  mutate(pct_ruled_deseg = ruled_deseg/total_cases) %>%
  mutate(pct_ruled_segregate = (1-pct_ruled_deseg)) %>%
  select(Case_Year, pct_ruled_deseg, pct_ruled_segregate)

d6_state <- read_excel("raw_data/Master_court_revise_12_3_05.xls") %>%
  select(State, Case_Year, School_Segregation_Case, Court_Desegregation_Plan) %>%
  group_by(State, Case_Year) %>%
  filter(School_Segregation_Case == "Yes") %>%
  mutate(total_cases = n()) %>%
  pivot_wider(names_from = Court_Desegregation_Plan,
              values_from = Court_Desegregation_Plan) %>%
  mutate(ruled_deseg = map_dbl(Yes, ~ ifelse(is.null(.), 0, table(.)))) %>%
  mutate(pct_ruled_deseg = ruled_deseg/total_cases) %>%
  mutate(pct_ruled_segregate = (1-pct_ruled_deseg)) %>%
  select(State, Case_Year, total_cases, pct_ruled_deseg, pct_ruled_segregate) %>%
  rename(`Case Year` = Case_Year,
         `Case Rulings for Desegregation` = pct_ruled_deseg,
         `Case Rulings For Segregation` = pct_ruled_segregate)

# You need to use str_replace to replace all the "Null" values with something
# else, or you need to find a function that counts the number of items in a
# vector that is more useful than table() (make sure that you are able to
# input a value of 0 for vectors that don't have any values, maybe by using an
# if_else() function and a is.na = 0 kind of replace thing)

# You got this Drake

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
             selectInput("x", "Pick your x variable", choices = names(d)),
             selectInput("y", "Pick your y variable", choices = names(d)),
             selectInput("geom", "drake", c("point", "jitter", "smooth", "line", "col")),
             plotOutput("plot1")
           )),
  
  tabPanel("Milestone 4",
           fluidPage(
             selectInput("x2", "Click the Region variable", choices = names(d4)),
             selectInput("y2", "Click the Level variable", choices = names(d4)),
             selectInput("geom", "drake", c("point", "jitter", "smooth", "line", "col")),
             plotOutput("plot2")
           )),
  
  tabPanel("Milestone 5",
           fluidPage(
             selectInput("x3", "(These choices don't affect the plot)", choices = names(d4)),
             selectInput("y3", "(These choices don't affect the plot)", choices = names(d4)),
             selectInput("geom", "(These choices don't affect the plot)", c("point", "jitter", "smooth", "line", "col")),
             plotOutput("plot3")
           )),
  
  tabPanel("Milestone 6",
           fluidPage(
             selectInput("x4", "Pick your x variable", choices = names(d6[,1])),
             selectInput("y4", "Pick your y variable", choices = names(c(d6[,2], d6[,3]))),
             selectInput("geom", "Pick your plot", c("point", "jitter", "smooth", "line", "col", "density")),
             plotOutput("plot4")
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
           col = geom_col(),
           density = geom_density())
  })
  
  output$plot1 <- renderPlot({
    ggplot(d, aes(.data[[input$x]], .data[[input$y]])) +
      plot_geom()
  }, res = 96)
  
  output$plot2 <- renderPlot({
    ggplot(d4, aes(.data[[input$x2]], .data[[input$y2]])) +
      plot_geom() +
      theme_clean() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(size = 9)) +
      labs(title = "Number of Protests by Region from 1990-2019",
           x = "Region",
           y = "Number of Protests")
  }, res = 96)
  
  output$plot3 <- renderPlot({
    ggplot(d4, aes(x = fct_rev(fct_reorder(region, level)), y = protest, fill = region)) +
      geom_col() +
      theme_clean() +
      theme(legend.position = "bottom", axis.text.x = element_text(size = 9)) +
      labs(title = "Number of Protests by Region from 1990-2019",
           x = "Region",
           y = "Number of Protests")
  }, res = 96)
  
  output$plot4 <- renderPlot({
    ggplot(d6, aes(.data[[input$x4]], .data[[input$y4]])) +
      plot_geom() +
      theme_clean() +
      scale_x_discrete() +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(size = 6)) +
      labs(title = "Desegregation Cases from 1952-2002",
           x = "State",
           y = "Percentage")
  }, res = 96)
  
}

# Run the application 
shinyApp(ui = ui, server = server)