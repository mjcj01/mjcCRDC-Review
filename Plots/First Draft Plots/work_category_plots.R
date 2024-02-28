library(tidyverse)

### Number of articles published with proportional color for work category, by publish year

### For-loop to create a data frame that has each category per year
for(i in c(2008:2023)) {
  df <- data.frame("year" = i,
                   "Category" = c("Book Chapter", "Journal Article", "Other", "Report"))
  df1 <- rbind(df, df1)
}

noa_wc_py_peryear <- review_csv %>%
  drop_na(year) %>%
  select(title, year, work_type) %>%
  mutate("Category" = ifelse(work_type %in% cat1, "Journal Article",
                             ifelse(work_type %in% cat2, "Book Chapter",
                                    ifelse(work_type %in% cat3, "Report", "Other")))) %>%
  group_by(Category, year) %>%
  summarise("count" = n()) %>%
  merge(.,
        df1,
        by = c("year", "Category"),
        all = TRUE) %>%
  replace(., is.na(.), 0) %>%
  ggplot(data = ., aes(x = year, y = count, color = Category)) +
  geom_line(size = 1) +
  labs(x = "Publication Year",
       y = "Number of Articles") +
  scale_color_manual(values = four_color, name = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        text = element_text(family = "Seaford"))

ggsave(plot = noa_wc_py_peryear,
       filename = "Exports//noa_wc_py_peryear.png",
       height = 4,
       dpi = 300)

noa_wc_py_cumsum <- review_csv %>%
  drop_na(year) %>%
  select(title, year, work_type) %>%
  mutate("Category" = ifelse(work_type %in% cat1, "Journal Article",
                             ifelse(work_type %in% cat2, "Book Chapter",
                                    ifelse(work_type %in% cat3, "Report", "Other")))) %>%
  group_by(Category, year) %>%
  summarise("count" = n()) %>%
  merge(.,
        df1,
        by = c("year", "Category"),
        all = TRUE) %>%
  replace(., is.na(.), 0) %>% 
  ungroup() %>% 
  group_by(Category) %>% 
  mutate("sum" = cumsum(count)) %>%
  ggplot(data = ., aes(x = year, y = sum, color = Category)) +
  geom_line(size = 1) +
  labs(x = "Publication Year",
       y = "Number of Articles") +
  scale_color_manual(values = four_color, name = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        text = element_text(family = "Seaford"))

### Articles published with proportional colors for work category, by years analyzed
noa_wc_ya <- review_csv %>%
  mutate(years_used = strsplit(years_used, split = ", ")) %>%
  mutate("Category" = ifelse(work_type %in% cat1, "Journal Article",
                             ifelse(work_type %in% cat2, "Book Chapter",
                                    ifelse(work_type %in% cat3, "Report", "Working Paper")))) %>%
  unnest(cols = c(years_used)) %>%
  mutate(years_used = str_to_title(years_used)) %>%
  ggplot(data = ., aes(x = years_used, fill = Category)) +
  geom_bar() +
  labs(title = 
"Number of Articles, Book Chapters, Reports, and Working 
Papers by Civil Rights Data Collection Year",
       x = "Civil Rights Data Collection Year",
       y = "Number of Articles") +
  scale_fill_manual(values = four_color, name = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 315, vjust = 1, hjust = 0),
        plot.background = element_rect(fill = "white"),
        text = element_text(family = "Seaford"))

ggsave(plot = noa_wc_ya,
       filename = "Exports//noa_wc_ya.png",
       height = 4,
       dpi = 300)


