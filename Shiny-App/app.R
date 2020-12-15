#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)
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

set.seed(2020)

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



# Model Wrangling ---------------------------------------------------------



global_protest_small <- global_protest_clean %>%
    filter(country %in% c("Guinea", "Kenya", "Ukraine", "Romania", "Bangladesh",
                          "Ireland", "Madagascar", "India", "Colombia", "Togo"))

us_protest_small <- us_protest_clean %>%
    filter(admin1 %in% c("California", "New York", "Kentucky", "Texas", "Florida",
                         "Oklahoma", "Minnesota", "Wisconsin", "Illinois", "Hawaii"))


global_year <- stan_glm(data = global_protest_small, pct_violent ~ year,
                             refresh = 0, family = gaussian())

global_year_count <- stan_glm(data = global_protest_small, count ~ year,
                        refresh = 0, family = gaussian())

global_country <- stan_glm(data = global_protest_small, pct_violent ~ country,
                                refresh = 0, family = gaussian())

us_month <- stan_glm(data = us_protest_small, fatalities ~ event_month,
                          refresh = 0, family = gaussian())

us_admin <- stan_glm(data = us_protest_small, fatalities ~ admin1,
                          refresh = 0, family = gaussian())




# Shiny App ---------------------------------------------------------------



ui <- navbarPage(theme = shinytheme("cosmo"),
    "Protest Risk",
    
    tabPanel(theme = shinytheme("cosmo"), "Overview", 
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
    
    
    tabPanel(theme = shinytheme("cosmo"), "Protests by State",
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
    
    tabPanel(theme = shinytheme("cosmo"), "Protests by Country",
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

    
    tabPanel(theme = shinytheme("cosmo"), "Risk Models",
             titlePanel(strong("Country Violence Model")),
             p("Below is a regression table showing the impact of the country where a protest begins on
               the likelihood that that protest will be violent. This is an explanatory model illustrating
               the different protest violence rates for select countries. These specific countries were
               selected because they all had a significant number of documented protests, whereas some
               countries had very few protests but they were all violent (skewing the usefulness of this table)."),
             p(" "),
             tbl_regression(global_country, intercept = TRUE) %>%
                as_gt() %>%
                tab_header(title = "Regression of Country Protest Risk", 
                   subtitle = "The Effects of Origin Country on Protest Violence") %>%
                   tab_source_note(md("Source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HTTWYL")),
             p(" "),
             p("The intercept that is listed represents information from the posterior distribution for Bangledesh. This data
               is used as a reference against which other countries are compared as being \"more likely to have protests become
               violent\" or \"less likely to have protest become violent\"."),
             p("This regression table is interpretted by looking at the Beta value for each country. The (Intercept) Beta value
               represents the percentage chance that a protest in Bangladesh will be violent. This value is used as a basis against
               which the other values - representing percentage chances of protest violence for other countries - will be calculated.
               To calculate the protest violence chance for a country, you add that country's Beta value to the Beta value listed for
               the (Intercept). For example, Guinea has a 67% chance for a protest to be violent, because 0.50 + 0.17 = 0.67. Ireland
               has a 5% chance that a protest will be violent, because 0.50 + -0.45 = 0.05."),
             p("It is important to note that Beta values with 95% Confidence Intervals - represented by the rightmost column of our model - that
               cross 0 are considered to be not statistically significant. This is because 0 represents a non-influence of a country on the
               likelihood of a have protest being violent, and if 0 falls within that confidence interval it is highly possible that this
               may be the case. In our model, the Beta values for Togo and India are not
               considered statistically significant, while the rest of the Beta values are. In these cases, we can ignore the Beta value's potential
               influence on protest risk because we can not be comfortably certain that it is accurate."),
             p("We can additionally look at the influence of year generally on the likelihood that a protest will become violent.
               While year doesn't have much of an influence on the likelihood of a protest to become violent, it does influence the
               number of protests there are in a given year. The second table below illustrates that every year there is an expected
               average increase of 1.4 protests across our 10 countries."),
             p(" "),
             p(" "),
             tbl_regression(global_year, intercept = TRUE) %>%
                 as_gt() %>%
                 tab_header(title = "Regression of Country Protest Risk", 
                            subtitle = "The Effects of Year on Protest Violence") %>%
                 tab_source_note(md("Source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HTTWYL")),
             p(" "),
             p(" "),
             tbl_regression(global_year_count, intercept = TRUE) %>%
                 as_gt() %>%
                 tab_header(title = "Regression of Country Protest Risk", 
                            subtitle = "Every year there are projected to be 1.4 more protests in each country") %>%
                 tab_source_note(md("Source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HTTWYL")),
             p(" "),
             p(" "),
             titlePanel(strong("State Fatality Model")),
             p("Below is a regression table showing the impact of the state where a protest begins on
               the likelihood that that protest will end with 1-3 fatalities. I began
               this project with the hypothesis that the state which you are protesting
               in has substantial influence on whether your protest may or may not end with fatalities.
               Unfortunately, according to the explanatory model below there is no significant influence of protest state
               on risk of fatalities. Because our state protest data is only from the summer and fall of 2020, Wisdom tells
               us that it would not make sense to use a model based on this data to predict the influence of month on
               risk for fatalities. Because this year's circumstances allowed for extraordinary social unrest, and because
               there was a substantial increase demonstrations during the month of June, our model will likely not be useful
               for estimating the relationship between month and protest risk in any given year."),
             p(" "),
             p(" "),
             tbl_regression(us_admin, intercept = TRUE) %>%
                 as_gt() %>%
                 tab_header(title = "Regression of State Protest Risk", 
                            subtitle = "There is no meaningful correlation between state and fatality risk") %>%
                 tab_source_note(md("Source: https://acleddata.com/special-projects/us-crisis-monitor/")),
             p(" "),
             tbl_regression(us_month, intercept = TRUE) %>%
                 as_gt() %>%
                 tab_header(title = "Regression of State Protest Risk", 
                            subtitle = "There is no meaningful correlation between month and fatality risk in 2020") %>%
                 tab_source_note(md("Source: https://acleddata.com/special-projects/us-crisis-monitor/"))
             ),
            
    
    tabPanel(theme = shinytheme("cosmo"), "About", 
             titlePanel(strong("About")),
             h3("About the Project"),
             p("This project aims to analyze how dangerous it may be to protest in different states.
               As we continue to live in a constantly-moving world, and as racial
               reckoning will continue to stretch far beyond the limits of this
               past summer, it is beneficial to understand which states warrant additional caution
               as we engage in civil protest and activism around our cause."),
             h3("About the Data"),
             p("The Armed Conflict Location & Event Data Project (ACLED) is a collection of data and analyses on
               conflict and social unrest across the globe. Operating as a nonprofit, ACLED has launched
               a US Crisis Monitor in partnership with the Bridging Divides Initiative at Princeton University.
               This crisis monitor records instances of political violence and protest within the United States and
               compiled it into a  maleable data set."),
             p("Data from the ACLED US Crisis Monitor was used for evaluation of social unrest and demonstrations in the United States.
               The interactive mapping from the \"Protests by State\" tab utilized this data, as did the U.S.-based models represented on the \"Risk Models\" tab."),
             a("For more information on the ACLED US Crisis Monitor, click this link.", href = "https://acleddata.com/special-projects/us-crisis-monitor/"),
             p(" "),
             p("The Harvard Dataverse Repository is a repository of data accessible to researchers globally. The data I have used for global protest analyses was accessed
               through the harvard Dataverse Repository and comes from the Mass Mobilization Project (MM). MM, authored by David H. Clark (Binghamton University) and Patrick M.
               Regan (University of Notre Dame), is an effort to understand citizen movements against governments globally. The project is sponsored by the Political Instability
               Task Force, which is funded by the CIA."),
             p("Data from the Mass Mobilization Project was used for evaluation of global protests trends displayed on the \"Protests by Country\" tab, as well as for the development of models explaining both the influence
               of different countries on protest violence as well as predicting how time impacts protest frequency. These models are visible on the \"Risk Models\" tab."),
             a("For more information on the Mass Mobilization Project, click this link.", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HTTWYL"),
             p(""),
             h3("About Me"),
             p("My name is Drake Johnson, and I am a student at Harvard College
             studying Government and African American Studies. My particular
             areas of interest are urban inequalities and fundamental social change,
             with intentions to work in a policy capacity to fix these inequities.
             You can reach me at drakejohnson@college.harvard.edu if you have any further questions."),
             a("Contact Me", href = "mailto:drakejohnson@college.harvard.edu"),
             a("Access this Project", href = "https://github.com/DrakeJohnson22/protest-risk"))
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
            ungroup() %>%
            select(event_month, admin1, count) %>%
            unique() %>%
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
