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
             titlePanel(strong("Global Protest Response")),
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
             h3("Tab Descriptions"),
             h4("Protests by State"),
             p("The Protests by State tab will provide information on statewide protesting during
               the summer and fall of 2020. This will be beneficial in anticipating
               protest frequency and, if we expect to be in a particular
               location, likelihood that a protest may occur."),
             h4("Protests by Country"),
             p("The Protests by Country tab will provide information on protest trends in different countries
               between 1990 and 2019. Data for the United States is not included.
               This will be beneficial in anticipating protest frequency in potential
               international travel destinations."),
             h4("Protest Risk Model"),
             p("The Protest Risk Model tab will provide insight into a model that analyses protest
               violence risk and protest fatality risk for select states and countries
               respectively. This model will give insight into factors that may
               impact both protest frequency and protest risk in different locations."),
             h4("About"),
             p("The About tab will provide further insight regarding this project and myself.")
    ),
    
    
    tabPanel("Protests by State",
             titlePanel(strong("A Summer of Social Unrest")),
             p("During this past summer there were over 10,000 protests across
               the United States, and demonstrations have continued to persist.
               California leads the country with over 2,000 demonstrations, almost
               twice as much as the next most active state. Following California
               is New York, Florida, Pennsylvania, Texas, and Illinois. North and
               South Dakota have been the least active states, each documenting
               only a little over 40 demonstrations. Following them at the bottom
               of the list are Wyoming, New Hampshire, Alaska, Delaware, and Hawaii.
               The District of Columbia falls at rank 31 on our list with 180 demonstrations."),
              p("The interactive mapping plot below represents monthly protest data
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
                 plotOutput("plot1"),
                 plotOutput("plot4")
             )),
    
    tabPanel("Protests by Country",
             titlePanel(strong("Demonstrations Abroad")),
             p("The social unrest that dominated public attention in teh United States
               this past summer was not conducted in isolation. Over the same summer-fall
               time period, and equal 10,000 demonstrations were held oversees across
               74 other countries. Racial reckoning was not solely an American issue,
               but became a global movement for justice."),
             p("Data regarding 2020 demonstrations abroad is much more decentralized.
               However, there is very useful documentation of protests information
               for different countries from 1990 to 2019. These generational
               trends - which have the limitation of not demonstrating increased
               engagement under the unique circumstances of coronavirus and the
               current political climate - provide insight into annual protest
               rates in various countries. Some of the countries with the most
               protests during this time frame include the United Kingdom,
               South Korea, Kenya and Venezuela, while some of the countries with
               the fewest protests include Czechoslovakia, South Sudan, Kosovo, and Qatar."),
             p("The interactive mapping plot below illustrates country trends in protests over the last 20 years."),
             fluidPage(
                 selectInput("z2", "Country", choices = sort(unique(global_protest_clean$country))),
                 selectInput("geom2", "Plot Type", choices = c("Line Graph", "Bar Graph", "Smooth Line Graph")),
                 plotOutput("plot2"),
                 plotOutput("plot3")
             )),

    
    tabPanel("Protest Risk Model",
             h3("Data Visualization"),
             p("Below is a visual of the number of protests, by region, between 1990 and 2019"),
             h3(" "),
             p(" "),
    tbl_regression(dim1_model, intercept = TRUE) %>%
        as_gt() %>%
        tab_header(title = "Regression of Congressional Ideology", 
                   subtitle = "The Effect of Party on DW-NOMINATE Percentile") %>%
        tab_source_note(md("Source: https://voteview.com/data"))),
            
    
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
               'Smooth Line Graph' = geom_smooth(se = FALSE, na.rm = TRUE),
               'Line Graph' = geom_line(),
               'Bar Graph' = geom_col(),
               density = geom_density())
    })
    
    
    
    output$plot1 <- renderPlot({
        us_protest_clean %>%
            filter(admin1 == input$z1) %>%
        ggplot(aes(event_month, count)) +
            geom_col(fill = "deepskyblue3") +
            theme_minimal() +
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
            theme_minimal() +
            theme(legend.position = "bottom",
                  axis.text.x = element_text(size = 9)) +
            labs(title = "Number of Protests per Year",
                 subtitle = "Data recorded from 1990-2019",
                 x = "Year",
                 y = "Number of Protests")
    }, res = 96)
    
    
    
    output$plot3 <- renderPlot({
        global_protest_clean %>%
            filter(country == input$z2) %>%
            ggplot(aes(year, pct_violent), ) +
            plot_geom() +
            theme_minimal() +
            theme(legend.position = "bottom",
                  axis.text.x = element_text(size = 9)) +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(title = "Rate of Protest Violence per Year",
                 subtitle = "Based on percentage of protests that were violent each year",
                 caption = "Source: Armed Conflict Location & Event Data Project",
                 x = "Year",
                 y = "Percentage of Protests that are Violent")
    }, res = 96)

    
    
    output$plot4 <- renderPlot({
        us_protest_clean %>%
            filter(admin1 == input$z1) %>%
            ggplot(aes(event_month, pct_fatal)) +
            geom_col(fill = "deeppink4") +
            theme_minimal() +
            theme(legend.position = "bottom",
                  axis.text.x = element_text(size = 9),
                  axis.title.x = element_text(face = "bold"),
                  axis.title.y = element_text(face = "bold")) +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(title = "Monthly Protest Fatality Rates",
                 subtitle = "Blank charts represent 0% fatality rate",
                 caption = "Source: Armed Conflict Location & Event Data Project",
                 x = "Month",
                 y = "Percentage of Protests that included Fatalities")
    }, res = 96)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
