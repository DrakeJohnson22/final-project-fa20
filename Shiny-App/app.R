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
library(rstanarm)

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

d4_raw <- read_csv("raw_data/mmALL_073119_csv.csv")

us_protest_data <- read_excel("raw_data/USA_2020_Nov28.xlsx")

us_protest_clean <- us_protest_data %>%
    select(EVENT_TYPE, SUB_EVENT_TYPE:INTERACTION, ADMIN1) %>%
    group_by(ADMIN1, SUB_EVENT_TYPE, EVENT_TYPE) %>%
    mutate(value = n())

stan_glm(data = us_protest_data, FATALITIES ~ ADMIN1 - 1, refresh = 0, family = gaussian())

ui <- navbarPage(
    "Protest Risk in Different States",
    
    tabPanel("Overview", 
             titlePanel("Global Protest Response"),
             h3("Summer of 2020"),
             p("The Summer of 2020 was full of widespread pandemic histeria,
               tragedy from lost families and lost jobs, and an international
               racial reckoning. The Armed Conflict Location & Event Data Project 
               recorded over 10,000 protests and demonstrations in the United
               States following the murder of George Floyd, along with an equal
               number of demonstrations abroad.
               
               Racial Reckoning has become a defining issue during the year of
               2020, and with it the associated police brutality and civil unrest
               response concerns. There were a number of violent responses to
               protesters over the course of this year, both from tear gas and
               riot shields to mass shootings and military-sanctioned brutal engagements."),
             h3("About the Project"),
             p("This project aims to analyze how dangerous it may be to protest in different states.
               As we continue to live in a constantly-moving world, and as racial
               reckoning will continue to stretch far beyond the limits of this
               past summer, it is beneficial to understand which states warrant additional caution
               as we engage in civil protest and activism around our cause."),
             h3("About Me"),
             p("My name is Drake Johnson, and I am a student at Harvard College
             studying Government and African American Studies. My particular
             areas of interest are urban inequalities and fundamental social change,
             with intentions to work in a policy capacity to fix these inequities.
             You can reach me at drakejohnson@college.harvard.edu if you have any further questions."),
             a("Dan made me link Google", href = "https://www.google.com"),
    ),
    
    
    tabPanel("Model",
             h3("Under Construction"),
             p("This page is still being put together. Please check back later!"),
             h3(" "),
             p(" "),
             fluidPage(
                 selectInput("x", "Pick your x variable", choices = names(d)),
                 selectInput("y", "Pick your y variable", choices = names(d)),
                 selectInput("geom", "drake", c("point", "jitter", "smooth", "line", "col")),
                 plotOutput("plot1")
             )),
    
    tabPanel("Mapping",
             h3("Interactive Mapping"),
             p("This is also under construction, so please bear with it!"),
             h3(" "),
             p(" "),
             fluidPage(
                 selectInput("x2", "Click the Region variable", choices = names(d4)),
                 selectInput("y2", "Click the Level variable", choices = names(d4)),
                 selectInput("geom", "drake", c("point", "jitter", "smooth", "line", "col")),
                 plotOutput("plot2")
             )),
    
    tabPanel("Example",
             h3("Data Visualization"),
             p("Below is a visual of the number of protests, by region, between 1990 and 2019"),
             h3(" "),
             p(" "),
             fluidPage(
                 selectInput("x3", "(These choices don't affect the plot)", choices = names(d4)),
                 selectInput("y3", "(These choices don't affect the plot)", choices = names(d4)),
                 selectInput("geom", "(These choices don't affect the plot)", c("point", "jitter", "smooth", "line", "col")),
                 plotOutput("plot3")
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
