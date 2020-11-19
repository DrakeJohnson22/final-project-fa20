

d4 <- read_csv("Shiny-App/raw_data/mmALL_073119_csv.csv") %>%
  select(-c(id, ccode, protestnumber, location, participants,
            protesteridentity, sources, notes)) %>%
  group_by(region, protest) %>%
  mutate(level = sum(protest))

d4_raw <- read_csv("Shiny-App/raw_data/mmALL_073119_csv.csv")


d4_accomodation <- d4_raw %>%
  mutate(accommodate = ifelse(stateresponse1 == "accomodation", 1, 0))

stan_glm(data = d4_accomodation, accommodate ~ year, refresh = 0, family = binomial())

library(readr)
library(rstanarm)
library(tidyverse)

d4_raw$stateresponse1
