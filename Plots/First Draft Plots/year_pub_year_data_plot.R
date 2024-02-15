library(tidyverse)

den_ya_yp <- review_csv %>%
  mutate(years_used = strsplit(years_used, split = ", ")) %>%
  mutate(time = ifelse(one_year_only == "yes", "Cross-Section", "Longitudinal")) %>%
  unnest(cols = c(years_used)) %>%
  group_by(year, years_used) %>%
  summarise("count" = n()) %>%
  ggplot(data = ., aes(x = year, y = years_used, fill = count)) +
  geom_tile() +
  scale_fill_binned() +
  labs(x = "Publication Year",
       y = "Civil Rights Data Collection Year") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

ggsave(plot = den_ya_yp,
       filename = "Exports//February 7//den_ya_yp.png",
       height = 4,
       dpi = 300)
