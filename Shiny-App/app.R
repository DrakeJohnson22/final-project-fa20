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
             h3("Protests by State"),
             p("This tab will provide information on statewide protesting during
               the summer and fall of 2020. This will be beneficial in anticipating
               protest frequency and, if we expect to be in a particular
               location, likelihood that a protest may occur."),
             h3("Protests by Country"),
             p("This tab will provide information on protest trends in different countries
               between 1990 and 2019. Data for the United States is not included.
               This will be beneficial in anticipating protest frequency in potential
               international travel destinations."),
             h3("Protest Risk Model"),
             p("This tab will provide insight into a model that analyses protest
               violence risk and protest fatality risk for select states and countries
               respectively. This model will give insight into factors that may
               impact both protest frequency and protest risk in different locations."),
             h3("About"),
             p("This tab will provide further insight regarding this project and myself.")
    ),
    
    
    tabPanel("Protests by State",
             h3("A Summer of Social Unrest"),
             p("During this past summer there were over 10,000 protests across
               the United States, and demonstrations have continued to persist.
               California leads the country with over 2,000 demonstrations, almost
               twice as much as the next most active state. Following California
               is New York, Florida, Pennsylvania, Texas, and Illinois. North and
               South Dakota have been the least active states, each documenting
               only a little over 40 demonstrations. Following them at the bottom
               of the list are Wyoming, New Hampshire, Alaska, Delaware, and Hawaii.
               The District of Columbia falls at rank 31 on our list with 180 demonstrations."),
              p("The interactive mapping graph below represents monthly protest data
               for each of the 50 state and the District of Columbia. Data used for
               this visualization spans the day of George Floyd's death to
               November 28, 2020. There is a visible spike in social unrest in
               July - the month directly following Floyd's murder. Due to the
                nature of the pandemic and recent political elections, we can
                expect demonstration frequency to slowly deteriorate while remaining
                higher than previous years. Future instances of racial injustice
                or dismissive attitudes by public officials will have a positive
                effect on protest frequency."),
             p(" "),
             fluidPage(
                 selectInput("z1", "State", choices = sort(unique(us_protest_clean$admin1))),
                 plotOutput("plot1")
             )),
    
    tabPanel("Protests by Country",
             h3("Interactive Mapping"),
             fluidPage(
                 selectInput("z2", "Country", choices = sort(unique(global_protest_clean$country))),
                 selectInput("geom2", "Plot Type", choices = c("Line Graph", "Bar Graph")),
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
    
    tabPanel("About", 
             titlePanel("About"),
             h3("About the Project"),
             p("This project aims to analyze how dangerous it may be to protest in different states.
               As we continue to live in a constantly-moving world, and as racial
               reckoning will continue to stretch far beyond the limits of this
               past summer, it is beneficial to understand which states warrant additional caution
               as we engage in civil protest and activism around our cause."),
             h3("About the Data"),
             p(" "),
             h3("About Me"),
             p("My name is Drake Johnson, and I am a student at Harvard College
             studying Government and African American Studies. My particular
             areas of interest are urban inequalities and fundamental social change,
             with intentions to work in a policy capacity to fix these inequities.
             You can reach me at drakejohnson@college.harvard.edu if you have any further questions."),
             a("Contact Me", href = "mailto:drakejohnson@college.harvard.edu"))
)



# Plotting ----------------------------------------------------------------



server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom2,
               point = geom_point(),
               jitter = geom_jitter(),
               smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               'Line Graph' = geom_line(),
               'Bar Graph' = geom_col(),
               density = geom_density())
    })
    
    
    
    output$plot1 <- renderPlot({
        us_protest_clean %>%
            filter(admin1 == input$z1) %>%
        ggplot(aes(event_month, count)) +
            geom_col(fill = "deepskyblue3") +
            theme_clean() +
            theme(legend.position = "bottom",
                  axis.text.x = element_text(size = 9),
                  axis.title.x = element_text(face = "bold"),
                  axis.title.y = element_text(face = "bold")) +
            scale_x_discrete(labels = c("May", "June", "July", "August", "September", "October", "November")) +
            labs(title = "Number of Protests per Month in 2020",
                 subtitle = "Data begins following the murder of George Floyd through 11/28/2020",
                 caption = "Source: Armed Conflict Location & Event Data Project",
                 x = "Month",
                 y = "Number of Protests")
    }, res = 96)
    
    
    
    output$plot2 <- renderPlot({
        global_protest_clean %>%
            filter(country == input$z2) %>%
        ggplot(aes(year, count)) +
            plot_geom() +
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
