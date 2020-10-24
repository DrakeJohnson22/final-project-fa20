
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

