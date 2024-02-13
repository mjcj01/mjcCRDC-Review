library(tidyverse)
library(ggsankey)

### Color by "work_type"
### Also color by "combination vs. only 1 year used"
data_frame("Year" = c("Historical", "2000", "2002", "2004", "2006", "2009-2010", "2011-2012", 
                      "2013-2014", "2015-2016", "2017-2018", "2020-2021", "Unclear"),
           "Occurances" = c(sum(grepl("historical", review_csv$years_used)),
                            sum(grepl("2000", review_csv$years_used)),
                            sum(grepl("2002", review_csv$years_used)),
                            sum(grepl("2004", review_csv$years_used)),
                            sum(grepl("2006", review_csv$years_used)),
                            sum(grepl("2009-10", review_csv$years_used)),
                            sum(grepl("2011-12", review_csv$years_used)),
                            sum(grepl("2013-14", review_csv$years_used)),
                            sum(grepl("2015-16", review_csv$years_used)),
                            sum(grepl("2017-18", review_csv$years_used)),
                            sum(grepl("2020-21", review_csv$years_used)),
                            sum(grepl("unclear", review_csv$years_used)))) %>%
  ggplot(data = ., aes(x = Year, y = Occurances)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

data_frame("Civil Rights Focus" = c("Race & Ethnicity", "Sex & Gender", "Disability", "English Language Learners", "Other"),
           "Occurances" = c(sum(grepl("race/ethnicity", review_csv$civil_rights_focus)),
                            sum(grepl("sex/gender", review_csv$civil_rights_focus)),
                            sum(grepl("disability", review_csv$civil_rights_focus)),
                            sum(grepl("ELL", review_csv$civil_rights_focus)),
                            sum(grepl("other", review_csv$civil_rights_focus)))) %>%
  arrange(Occurances) %>%
  mutate(`Civil Rights Focus` = factor(`Civil Rights Focus`, levels = `Civil Rights Focus`)) %>%
  ggplot(data = ., aes(x = `Civil Rights Focus`, y = Occurances)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

data_frame("Substantive Topic" = c("Discipline", "Special Education", "English Language Learners", "Staff",
                                   "Access to Opportunity", "Achievement", "Gifted Education", "Segregation",
                                   "Sports", "School Funding", "Health", "Other"),
           "Occurances" = c(sum(grepl("discipline", review_csv$topic)),
                            sum(grepl("sped", review_csv$topic)),
                            sum(grepl("ELL", review_csv$topic)),
                            sum(grepl("staff", review_csv$topic)),
                            sum(grepl("access to opportunity", review_csv$topic)),
                            sum(grepl("achievement", review_csv$topic)),
                            sum(grepl("G/T", review_csv$topic)),
                            sum(grepl("sports", review_csv$topic)),
                            sum(grepl("segregation", review_csv$topic)),
                            sum(grepl("school funding", review_csv$topic)),
                            sum(grepl("health", review_csv$topic)),
                            sum(grepl("other", review_csv$topic)))) %>%
  arrange(Occurances) %>%
  mutate(`Substantive Topic` = factor(`Substantive Topic`, levels = `Substantive Topic`)) %>%
  ggplot(data = ., aes(x = `Substantive Topic`, y = Occurances)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

review_csv %>%
  drop_na(year) %>%
  filter(year > 2000) %>%
  ggplot(data = ., aes(x = year)) +
  geom_histogram() +
  labs(caption = "Two articles were published prior to 2000; one was published in 1971 and one in 1973.") +
  theme_minimal()

### Requires library(ggsankey)
review_csv %>%
  drop_na(special_school_focus) %>%
  make_long(crdc_headline_only, special_school_focus, schools_analyzed) %>%
  ggplot(., aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                fill = factor(node))) +
  geom_sankey() +
  theme_void()

adjusted_lvls <- review_csv %>%
  select(title, school_lvls) %>%
  mutate(adjusted_school_lvls = gsub("elementary, middle, high", "K-12", school_lvls))

data_frame("Level" = c("Pre-K", "Elementary", "Middle", "High", "K-12", "Other"),
           "Occurances" = c(sum(grepl("preK", adjusted_lvls$adjusted_school_lvls)),
                            sum(grepl("elementary", adjusted_lvls$adjusted_school_lvls)),
                            sum(grepl("middle", adjusted_lvls$adjusted_school_lvls)),
                            sum(grepl("high", adjusted_lvls$adjusted_school_lvls)),
                            sum(grepl("K-12", adjusted_lvls$adjusted_school_lvls)),
                            sum(grepl("other", adjusted_lvls$adjusted_school_lvls)))) %>%
  arrange(Occurances) %>%
  mutate(Level = factor(Level, levels = Level)) %>%
  ggplot(data = ., aes(x = Level, y = Occurances)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

### Graph depicting “methods” used in papers or any other coding schema we used (like was CRDC merged with other data? Cross-sectional vs. longitudinal, etc.)
### CRDC merged with other datasets by year, color by proportional yes or no
### Cross-sectional vs. longitudinal by year
