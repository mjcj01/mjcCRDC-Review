library(tidyverse)

### Number of articles published with proportional color for work category, by publish year
noa_wc_py <- review_csv %>%
  select(title, year, work_type) %>%
  mutate("Category" = ifelse(work_type %in% cat1, "Journal Article",
                             ifelse(work_type %in% cat2, "Book Chapter",
                                    ifelse(work_type %in% cat3, "Report", "Other")))) %>%
  ggplot(data = ., aes(x = year, fill = Category)) +
  geom_bar() +
  labs(x = "Publication Year",
       y = "Number of Articles") +
  scale_fill_manual(values = four_color, name = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

ggsave(plot = noa_wc_py,
       filename = "Exports//February 7//noa_wc_py.png",
       height = 4,
       dpi = 300)

### Articles published with proportional colors for work category, by years analyzed
noa_wc_ya <- review_csv %>%
  mutate(years_used = strsplit(years_used, split = ", ")) %>%
  mutate("Category" = ifelse(work_type %in% cat1, "Journal Article",
                             ifelse(work_type %in% cat2, "Book Chapter",
                                    ifelse(work_type %in% cat3, "Report", "Other")))) %>%
  unnest(cols = c(years_used)) %>%
  ggplot(data = ., aes(x = years_used, fill = Category)) +
  geom_bar() +
  labs(x = "Year Analyzed",
       y = "Number of Articles") +
  scale_fill_manual(values = four_color, name = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 315, vjust = 1, hjust = 0),
        plot.background = element_rect(fill = "white"))

ggsave(plot = noa_wc_ya,
       filename = "Exports//February 7//noa_wc_ya.png",
       height = 4,
       dpi = 300)
