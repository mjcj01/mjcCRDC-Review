library(tidyverse)

### Number of articles published with proportional color with proportional color for 
### cross-section or longitudinal, by publish year
noa_csl_py_prj <- review_csv %>%
  mutate(time = ifelse(one_year_only == "yes", "Cross-Section", "Longitudinal")) %>%
  filter(work_type %in% cat1) %>%
  ggplot(data = ., aes(x = year, fill = time)) +
  geom_bar() +
  labs(title = 
"Number of Peer-Reviewed Journal Articles Analyzing
Cross-Sectional Data vs. Longitudinal Data",
       x = "Publication Year",
       y = "Number of Articles") +
  scale_fill_manual(values = two_color) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = "white"),
        text = element_text(family = "Seaford"))

ggsave(plot = noa_csl_py_prj,
       filename = "Exports//noa_csl_py_prj.png",
       height = 4,
       dpi = 300)

noa_csl_py_all <- review_csv %>%
  mutate(time = ifelse(one_year_only == "yes", "Cross-Section", "Longitudinal")) %>%
  ggplot(data = ., aes(x = year, fill = time)) +
  geom_bar() +
  labs(title = 
"Number of Peer-Reviewed Journal Articles, 
Book Chapters, and Reports Analyzing
Cross-Sectional Data vs. Longitudinal Data",
       x = "Publication Year",
       y = "Number of Articles") +
  scale_fill_manual(values = two_color) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = "white"),
        text = element_text(family = "Seaford"))

ggsave(plot = noa_csl_py_all,
       filename = "Exports//noa_csl_py_all.png",
       height = 4,
       dpi = 300)

### Articles published with proportional colors for cross-section or longitudinal, by
### years analyzed
noa_csl_ya <- review_csv %>%
  mutate(years_used = strsplit(years_used, split = ", ")) %>%
  mutate(time = ifelse(one_year_only == "yes", "Cross-Section", "Longitudinal")) %>%
  unnest(cols = c(years_used)) %>%
  ggplot(data = ., aes(x = years_used, fill = time)) +
  geom_bar() +
  scale_fill_manual(values = two_color, name = "") +
  #coord_flip() +
  labs(x = "Civil Rights Data Collection Year",
       y = "Occurances") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 315, vjust = 1, hjust = 0),
        text = element_text(family = "Seaford"))

ggsave(plot = noa_csl_ya,
       filename = "Exports//noa_csl_ya.png",
       height = 4,
       dpi = 300)

