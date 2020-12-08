
# Global Protest Data -----------------------------------------------------


global_protest_data <- read_csv("Shiny-App/raw_data/mmALL_073119_csv.csv")

global_protest_clean <- protests_abroad_data %>%
  filter(protest == 1) %>%
  select(country, year, protesterviolence, participants) %>%
  group_by(country, year) %>%
  mutate(count = n()) %>%
  mutate(num_violent = sum(protesterviolence)) %>%
  mutate(pct_violent = num_violent/count)

ggplot(d4, aes(x = fct_rev(fct_reorder(region, level)), y = protest, fill = region)) +
  geom_col() +
  theme_clean() +
  theme(legend.position = "bottom", axis.text.x = element_text(size = 9)) +
  labs(title = "Number of Protests by Region from 1990-2019",
       x = "Region",
       y = "Number of Protests")

set.seed(2020)

global_split <- initial_split(global_protest_clean, prob = 0.80)
global_train <- training(global_split)
global_test <- testing(global_split)

global_stan_country_year <- stan_glm(data = global_train, pct_violent ~ country + year + country*year,
         refresh = 0, family = gaussian())

global_stan_country <- stan_glm(data = global_train, pct_violent ~ country,
         refresh = 0, family = gaussian())

global_count_stan_country <- stan_glm(data = global_train, count ~ country,
                                      refresh = 0, family = gaussian())

global_count_stan_country_year <- stan_glm(data = global_train, count ~ country + year + country*year,
                                           refresh = 0, family = gaussian())


# U.S. Protest Data -------------------------------------------------------


us_protest_data <- read_excel("Shiny-App/raw_data/USA_2020_Nov28.xlsx")

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

set.seed(2020)

us_split <- initial_split(us_protest_clean, prob = 0.80)
us_train <- training(us_split)
us_test <- testing(us_split)

us_stan_admin_month <- stan_glm(data = us_train, pct_fatal ~ admin1 + event_month + admin1*event_month,
         refresh = 0, family = gaussian())

us_stan_admin <- stan_glm(data = us_train, pct_fatal ~ admin1,
         refresh = 0, family = gaussian())

us_count_stan_admin <- stan_glm(data = us_train, count ~ admin1,
                                refresh = 0, family = gaussian())

us_count_stan_admin_month <- stan_glm(data = us_train, count ~ admin1 + event_month + admin1*event_month,
                                      refresh = 0, family = gaussian())






