library(tidyverse)
library(treemap)

review_csv %>%
  mutate("Category" = ifelse(work_type %in% cat1, "Journal Article",
                      ifelse(work_type %in% cat2, "Book Chapter",
                      ifelse(work_type %in% cat3, "Report", "Other")))) %>%
  mutate(work_type = str_to_title(work_type)) %>%
  group_by(work_type, Category) %>%
  summarise("Number of Articles" = n()) %>%
  ggplot(data = ., aes(area = `Number of Articles`, fill = Category, subgroup = Category)) +
  treemapify::geom_treemap(layout = "squarified")
