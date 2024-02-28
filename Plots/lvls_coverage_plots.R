library(tidyverse)

### What to do with more than 5 colors?
noa_lvls_py <- review_csv %>%
  mutate(school_lvls = gsub("elementary, middle, high", "K-12", school_lvls),
         school_lvls = gsub("middle, elementary, high", "K-12", school_lvls),
         school_lvls = gsub("middle, high", "secondary", school_lvls)) %>%
  mutate(school_lvls = strsplit(school_lvls, split = ", ")) %>%
  unnest(cols = c(school_lvls)) %>%
  mutate(school_lvls = str_to_sentence(school_lvls)) %>%
  mutate(school_lvls = gsub("Prek", "Pre-K", school_lvls)) %>%
  ggplot(data = ., aes(x = year, fill = school_lvls)) +
  geom_bar() +
  labs(x = "Publication Year",
       y = "Occurrences") +
  scale_fill_discrete(breaks = c("Pre-K", "Elementary", "Middle", "High", "K-12", "Secondary", "Other"),
                      name = "School Levels") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        text = element_text(family = "Seaford"))

ggsave(plot = noa_lvls_py,
       filename = "Exports//noa_lvls_py.png",
       height = 4,
       dpi = 300)

noa_cov_py <- review_csv %>%
  mutate(geographic_coverage = strsplit(geographic_coverage, split = ", ")) %>%
  unnest(cols = c(geographic_coverage)) %>%
  mutate(geographic_coverage = str_to_title(geographic_coverage)) %>%
  ggplot(data = ., aes(x = year, fill = geographic_coverage)) +
  geom_bar() +
  labs(x = "Publication Year",
       y = "Occurrences") +
  scale_fill_manual(values = three_color, name = "Geographic Coverage") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        text = element_text(family = "Seaford"))

ggsave(plot = noa_cov_py,
       filename = "Exports//noa_cov_py.png",
       height = 4,
       dpi = 300)
