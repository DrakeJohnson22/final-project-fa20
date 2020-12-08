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
library(tidymodels)



# Data Wrangling ----------------------------------------------------------



global_protest_data <- read_csv("raw_data/mmALL_073119_csv.csv")

global_protest_clean <- global_protest_data %>%
    filter(protest == 1) %>%
    select(country, year, protesterviolence, participants) %>%
    group_by(country, year) %>%
    mutate(count = n()) %>%
    mutate(num_violent = sum(protesterviolence)) %>%
    mutate(pct_violent = num_violent/count)

us_protest_data <- read_excel("raw_data/USA_2020_Nov28.xlsx")

names(us_protest_data) <- tolower(names(us_protest_data))

us_protest_clean <- us_protest_data %>%
    select(event_date, event_type, actor1, admin1, fatalities) %>%
    mutate(event_date = format(event_date, "%m")) %>%
    rename(event_month = event_date) %>%
    group_by(event_month, admin1) %>%
    mutate(count = n()) %>%
    mutate(fatalities = ifelse(fatalities == 0, 0, 1)) %>%
    mutate(fatalities = as.logical(fatalities)) %>%
    group_by(event_month, admin1, fatalities) %>%
    mutate(num_fatal = sum(fatalities)) %>%
    mutate(pct_fatal = num_fatal/count)



# Shiny App ---------------------------------------------------------------



ui <- navbarPage(
    "Protest Risk",
    
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
    
    
    tabPanel("Protests by State",
             h3("Under Construction"),
             p("This page is still being put together. Please check back later!"),
             h3(" "),
             p(" "),
             fluidPage(
                 selectInput("x", "Pick your x variable", choices = names(us_protest_clean)),
                 selectInput("y", "Pick your y variable", choices = names(us_protest_clean)),
                 selectInput("geom", "drake", c("point", "jitter", "smooth", "line", "col")),
                 plotOutput("plot1")
             )),
    
    tabPanel("Protests by Country",
             h3("Interactive Mapping"),
             p("This is also under construction, so please bear with it!"),
             h3(" "),
             p(" "),
             fluidPage(
                 selectInput("z2", "drake", choices = sort(unique(global_protest_clean$country))),
                 plotOutput("plot2")
             )),
    
    tabPanel("Protest Risk",
             h3("Data Visualization"),
             p("Below is a visual of the number of protests, by region, between 1990 and 2019"),
             h3(" "),
             p(" "),
             fluidPage(
                 selectInput("x3", "(These choices don't affect the plot)", choices = names(us_protest_data)),
                 selectInput("y3", "(These choices don't affect the plot)", choices = names(us_protest_data)),
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



# Plotting ----------------------------------------------------------------



server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom2,
               point = geom_point(),
               jitter = geom_jitter(),
               smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               line = geom_line(),
               col = geom_col(),
               density = geom_density())
    })
    
    output$plot1 <- renderPlot({
        ggplot(us_protest_clean, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
    
    output$plot2 <- renderPlot({
        global_protest_clean %>%
            filter(country == input$z2) %>%
        ggplot(aes(year, count)) +
            geom_line() +
            theme_clean() +
            theme(legend.position = "bottom",
                  axis.text.x = element_text(size = 9)) +
            labs(title = "Number of Protests per Year from 1990-2019",
                 x = "Year",
                 y = "Number of Protests")
    }, res = 96)
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
