library(tidyverse)
library(extrafont)
font_import()

### Put historical at the bottom
den_ya_yp <- review_csv %>%
  mutate(years_used = strsplit(years_used, split = ", ")) %>%
  unnest(cols = c(years_used)) %>%
  mutate(years_used = str_to_title(years_used)) %>%
  filter(years_used != "Unclear") %>%
  group_by(year, years_used) %>%
  summarise("count" = n()) %>%
  ggplot(data = ., aes(x = year, y = years_used, fill = count)) +
  geom_tile() +
  scale_fill_binned(name = 
"Number of Occurrences") +
  scale_y_discrete(limits = c("Historical", "2000", "2002", "2004", "2006", "2009-10",
                              "2011-12", "2013-14", "2015-16", "2017-18", "2020-21")) +
  labs(x = "Publication Year",
       y = "Civil Rights Data Collection Year") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "bottom",
        text = element_text(family = "Seaford"))

ggsave(plot = den_ya_yp,
       filename = "Exports//den_ya_yp.png",
       height = 4,
       dpi = 300)
