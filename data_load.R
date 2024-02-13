library(tidyverse)

### Must have locale = locale(encoding="latin1") in read_csv(), otherwise it will not read in
review_csv <- read_csv("full_text_review_raw.csv", locale = locale(encoding = "latin1"))

colnames(review_csv) <- c("authors", "title", "year", "source", "org_type", "org_lvl", "other_org_lvl", "publisher",
                          "article_url", "cites_url", "abtract", "full_text_url", "related_url", "how_find", "work_type",
                          "crdc_empirical", "accessible", "other_data", "first_reader", "crdc_headline_only",
                          "one_year_only", "years_used", "combined_data", "other_data_sources", "geographic_coverage",
                          "subnational_coverage", "civil_rights_focus", "other_focus", "topic", "other_topic",
                          "analysis_type", "school_lvls", "other_lvls", "special_school_focus", "schools_analyzed",
                          "other_schools_analyzed", "notes", "another_reader_req", "second_reader")

cat1 <- c("peer-reviewed journal article", "commentary in peer-reviewed journal", "invited journal article")
cat2 <- c("book chapter")
cat3 <- c("report")
cat4 <- c("working paper")

review_csv <- review_csv %>%
  filter(crdc_headline_only == "no") %>%
  filter(work_type %in% cat1 | work_type %in% cat2 | work_type %in% cat3 | work_type %in% cat4)
