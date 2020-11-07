
d4_raw <- read_csv("raw_data/mmALL_073119_csv.csv")

d4 <- read_csv("raw_data/mmALL_073119_csv.csv") %>%
  select(-c(id, ccode, protestnumber, location, participants,
            protesteridentity, sources, notes)) %>%
  group_by(region, protest) %>%
  mutate(level = sum(protest))

ggplot(d4, aes(x = fct_rev(fct_reorder(region, level)), y = protest, fill = region)) +
  geom_col() +
  theme_clean() +
  theme(legend.position = "bottom", axis.text.x = element_text(size = 9)) +
  labs(title = "Number of Protests by Region from 1990-2019",
       x = "Region",
       y = "Number of Protests")

d6_check <- read_excel("Shiny-App/raw_data/Master_court_revise_12_3_05.xls") %>%
  select(State, Case_Year, School_Segregation_Case, Court_Desegregation_Plan) %>%
  group_by(State, Case_Year) %>%
  filter(School_Segregation_Case == "Yes") %>%
  mutate(total_cases = n())
