# read data ----
# call read xsl
library(readxl)
library(janitor)
rawdataxl <- readxl::read_xlsx("data/2022-08-15__SurveyResults_raw.xlsx") %>%
  janitor::clean_names()

# clean data ----
library(tidyverse)

data <- rawdataxl
# # recommended by: https://youtu.be/qUgVhpg-lP4
library(tidyverse)

important_characteristics__coder_vs_non_coder <- data %>% 
  select(respondent_id, starts_with("how_important_are")) %>% 
  pivot_longer(cols = starts_with("how_important_are"),
               names_to = "questionX",
                 values_to = "answer",
                 values_drop_na = TRUE) %>%
  mutate(questionX, question = str_remove(questionX, "how_important_are_the_following_characteristics_when_you_are_searching_or_selecting_for_code_research_software_for_a_particular_task_the_rows_are_presented_in_random_order_if_you_havent_considered_one_of_the_listed_attributes_please_skip_the_row_")) %>% 
  select(-questionX) %>% 
  arrange(respondent_id, question) %>% 
  count(question, answer) %>% 
  mutate(question_updated = case_when( 
    str_detect(question,"_very_important")~ str_remove(question,"_very_important"),
    str_detect(question,"_somewhat_important") ~ str_remove(question,"_somewhat_important"),
    str_detect(question,"_unimportant")~ str_remove(question,"_unimportant"))) %>% 
  select(-question) 

totals_per_how_important <-
  important_characteristics__coder_vs_non_coder %>%
  group_by(question_updated) %>% 
  summarise(total = sum(n))
   
important_characteristics__coder_vs_non_coder1 <-
  inner_join(
  important_characteristics__coder_vs_non_coder,
  totals_per_how_important) %>% 
  mutate(pct = 
           round(((n*100)/total), 2)
         ) %>% 
  arrange(answer)
  

