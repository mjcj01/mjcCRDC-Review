library(tidyverse)

### What to do with more than 5 colors?
review_csv %>%
  mutate(school_lvls = gsub("elementary, middle, high", "K-12", school_lvls),
         school_lvls = gsub("middle, elementary, high", "K-12", school_lvls),
         school_lvls = gsub("middle, high", "secondary", school_lvls)) %>%
  mutate(school_lvls = strsplit(school_lvls, split = ", ")) %>%
  unnest(cols = c(school_lvls)) %>%
  ggplot(data = ., aes(x = year, fill = school_lvls)) +
  geom_bar() +
  labs(x = "Publication Year",
       y = "Occurrences") +
  scale_fill_discrete(breaks = c("preK", "elementary", "middle", "high", "K-12", "secondary", "other"),
                      name = "School Levels") +
  theme_minimal()

review_csv %>%
  mutate(geographic_coverage = strsplit(geographic_coverage, split = ", ")) %>%
  unnest(cols = c(geographic_coverage)) %>%
  ggplot(data = ., aes(x = year, fill = geographic_coverage)) +
  geom_bar() +
  labs(x = "Publication Year",
       y = "Occurrences") +
  scale_fill_manual(values = three_color, name = "Geographic Coverage") +
  theme_minimal()
