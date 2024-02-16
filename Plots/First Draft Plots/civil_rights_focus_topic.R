library(tidyverse)

noa_crf <- data_frame("Civil Rights Focus" = c("Race & Ethnicity", "Sex & Gender", "Disability", 
                                               "English Language Learners", "Other"),
           "Occurrences" = c(sum(grepl("race/ethnicity", review_csv$civil_rights_focus)),
                            sum(grepl("sex/gender", review_csv$civil_rights_focus)),
                            sum(grepl("disability", review_csv$civil_rights_focus)),
                            sum(grepl("ELL", review_csv$civil_rights_focus)),
                            sum(grepl("other", review_csv$civil_rights_focus)))) %>%
  arrange(Occurrences) %>%
  mutate(`Civil Rights Focus` = factor(`Civil Rights Focus`, levels = `Civil Rights Focus`)) %>%
  ggplot(data = ., aes(x = `Civil Rights Focus`, y = Occurrences)) +
  geom_col() +
  #scale_fill_manual(values = five_color, name = "") +
  labs(caption = "Note: The substantive focus of an article/report is shown above. 
       If an article/report had multiple foci, they were all captured separately 
       and shown above as separate occurrences.") +
  #coord_flip() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 315, vjust = 1, hjust = 0),
        legend.position = "bottom")

ggsave(plot = noa_crf,
       filename = "Exports//February 7//noa_crf.png",
       height = 4,
       dpi = 300)

noa_crt <- data_frame("Substantive Topic" = c("Discipline", "Special Education", "English Language Learners", "Staff",
                                   "Access to Opportunity", "Achievement", "Gifted Education", "Segregation",
                                   "Sports", "School Funding", "Health", "Other"),
           "Occurrences" = c(sum(grepl("discipline", review_csv$topic)),
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
  arrange(Occurrences) %>%
  mutate(`Substantive Topic` = factor(`Substantive Topic`, levels = `Substantive Topic`)) %>%
  ggplot(data = ., aes(x = `Substantive Topic`, y = Occurrences)) +
  geom_col() +
  labs(caption = "Note: The substantive focus of an article/report is shown above. 
       If an article/report had multiple foci, they were all captured separately 
       and shown above as separate occurrences.") +
  #coord_flip() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 315, vjust = 1, hjust = 0))

ggsave(plot = noa_crt,
       filename = "Exports//February 7//noa_crt.png",
       height = 4,
       dpi = 300)
