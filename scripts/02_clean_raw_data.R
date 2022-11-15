# check survey data ----

library(readxl)
library(tidyverse)
library(janitor)

# add theme ----
#devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)
ggthemr('fresh')

# Figure 1 researchers ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                            selection_string = 
                              "are_you_a_researcher")

researchers <- as_tibble(myres[[1]])
total_researchers <- as.integer(myres[[2]]) #119

source("scripts/helper_functions.R")
simple_barplot_report(data = researchers,
                      tit = "Are you a researcher?",
                      subtit = "",
                      fignum = "Fig1",
                      fignam = "are_you_a_researcher")
  
  
 

# Figure 1 RSE ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                                     selection_string = 
                                       "do_you_consider_yourself")

rse <- as_tibble(myres[[1]])
total_rse <- as.integer(myres[[2]]) #156

simple_barplot_report(data = rse,
                      tit = "Do you consider yourself an RSE?",
                      subtit = "",
                      fignum = "Fig1",
                      fignam = "do_you_consider_yourself")

# Figure 1 Do you write code? ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                                     selection_string = 
                                       "do_you_write_code")

write_code <- as_tibble(myres[[1]])
total_write_code <- as.integer(myres[[2]]) #156

simple_barplot_report(data = rse,
                      tit = "Do you write code or / Research software?",
                      subtit = "",
                      fignum = "Fig1",
                      fignam = "do_you_write_code")


# Figure 1 and 2 Audience ----
table_audience <- rawdataxl %>% 
  select(starts_with("do_you_consider_yourself"),
         starts_with("are_you_a_researcher"),
         starts_with("do_you_write_code")) %>%
  unite(col = "researcher", c("are_you_a_researcher_yes", "are_you_a_researcher_no"),
        sep = "", na.rm = TRUE) %>%
  unite(col = "write_code", c("do_you_write_code_research_software_research_software_is_software_created_or_used_in_research_from_macros_and_small_scale_scripts_to_big_complex_suites_of_software_and_software_services_source_code_files_algorithms_scripts_computational_workflows_and_executables_yes",
                              "do_you_write_code_research_software_research_software_is_software_created_or_used_in_research_from_macros_and_small_scale_scripts_to_big_complex_suites_of_software_and_software_services_source_code_files_algorithms_scripts_computational_workflows_and_executables_no"),
        sep = "", na.rm = TRUE) %>% 
  unite(col = "rse", c("do_you_consider_yourself_a_research_software_engineer_a_research_software_engineer_is_defined_as_someone_who_combines_software_engineering_expertise_with_an_intimate_understanding_of_research_if_yes_join_the_rse_aunz_org_community_yes",
                       "do_you_consider_yourself_a_research_software_engineer_a_research_software_engineer_is_defined_as_someone_who_combines_software_engineering_expertise_with_an_intimate_understanding_of_research_if_yes_join_the_rse_aunz_org_community_no"),
        sep = "", na.rm = TRUE) %>% 
  count(researcher, write_code, rse) %>% 
  mutate(pct = round((n / sum(n))*100, 2)) %>% 
  print(n = Inf)

writexl::write_xlsx(
  x = list("question" = table_audience,
           "total_responses" = as.data.frame(nrow(rawdataxl))),
  path = paste0("dataperquestion/",
                Sys.Date(), "_", "researchers_write_code_rse",
                ".xlsx"),
  format_headers = FALSE)

# plot three columns
rawdataxl %>% 
  select(starts_with("do_you_consider_yourself"),
         starts_with("are_you_a_researcher"),
         starts_with("do_you_write_code")) %>%
  unite(col = "researcher", c("are_you_a_researcher_yes", "are_you_a_researcher_no"),
        sep = "", na.rm = TRUE) %>%
  unite(col = "write_code", c("do_you_write_code_research_software_research_software_is_software_created_or_used_in_research_from_macros_and_small_scale_scripts_to_big_complex_suites_of_software_and_software_services_source_code_files_algorithms_scripts_computational_workflows_and_executables_yes",
                              "do_you_write_code_research_software_research_software_is_software_created_or_used_in_research_from_macros_and_small_scale_scripts_to_big_complex_suites_of_software_and_software_services_source_code_files_algorithms_scripts_computational_workflows_and_executables_no"),
        sep = "", na.rm = TRUE) %>% 
  unite(col = "rse", c("do_you_consider_yourself_a_research_software_engineer_a_research_software_engineer_is_defined_as_someone_who_combines_software_engineering_expertise_with_an_intimate_understanding_of_research_if_yes_join_the_rse_aunz_org_community_yes",
                       "do_you_consider_yourself_a_research_software_engineer_a_research_software_engineer_is_defined_as_someone_who_combines_software_engineering_expertise_with_an_intimate_understanding_of_research_if_yes_join_the_rse_aunz_org_community_no"),
        sep = "", na.rm = TRUE) %>% 
  filter(write_code != "", rse != "") %>% 
  #View()
  ggplot(aes(x = write_code, fill = researcher)) +
  geom_bar() +
  facet_grid(. ~ rse,
             labeller=label_both)

ggsave(filename = paste0("plots/",Sys.Date(), "_", 
                         "Fig1","_",
                         "researchers_write_code_rse", ".png"),
       width = 15, height = 10, units = "in")
  
  
# Figure 2 FOR codes ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                                     selection_string = 
                                       "what_are_the_two")

for_codes <- as_tibble(myres[[1]])
total_for_codes <- as.integer(myres[[2]]) #151

simple_barplot_report(data = for_codes,
                      tit = "What are the Fields of Research (FOR)",
                      subtit = "in which you work",
                      fignum = "Fig2",
                      fignam = "what_are_the_two")


# Figure 5 years of experience ----
total_experience_responses <- rawdataxl %>% 
  select(starts_with("for_how_many_years")) %>% 
  filter(if_any(.cols = everything(), ~ !is.na(.))) %>% 
  nrow()

experience_years <- rawdataxl %>% 
      select(starts_with("for_how_many_years")) %>% 
      filter(if_any(.cols = everything(), ~ !is.na(.))) %>% 
      ## Reformat the data
      pivot_longer(cols = starts_with("for_how_many_years"),
                   names_to = "question",
                   values_to = "experience",
                   values_drop_na = TRUE) %>% 
      ## calculate the height of the bars
      count(experience) %>% 
      #View()
      group_by(experience) %>% 
      summarise(n = sum(n)) %>% 
      #View()
      mutate(pct = (n/total_experience_responses)*100) %>% 
      mutate(mean_pct = mean(pct)) 

writexl::write_xlsx(
  x = list("question" = experience_years,
           "total_responses" = as.data.frame(total_experience_responses)),
  path = paste0("dataperquestion/",
                Sys.Date(), "_", "experience_years",
                ".xlsx"),
  format_headers = FALSE)

## Plot
experience_years %>% 
  mutate(experience = factor(experience, 
                             levels = c("<1 year", "1-2 years",
                                        "2-5 years","5-10 years",
                                        "10-20 years","20-40 years",
                                        ">40 years"))) %>%
  ggplot(aes(x = experience, y = pct, size = 15)) +
  geom_point() +
  geom_hline(yintercept = unique(experience_years$mean_pct),
             linetype="dashed", col= "gray2") +
  labs(title = "How many years of experience coding?",
       x = "years",
       y = "Percentage")+
  theme(legend.position = "none")

ggsave(filename = paste0("plots/",Sys.Date(), "_", 
                         "Fig5","_",
                         "years_of_experience", ".png"),
       width = 15, height = 10, units = "in")

# Figure 6 How important is code to your research? ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                                     selection_string = 
                                       "how_important_is_code")

software_importance <- as_tibble(myres[[1]])
total_software_importance <- as.integer(myres[[2]]) #142

software_importance %>% 
  mutate(essential = "X") %>%
  ggplot(aes(x = essential, y = pct, fill = 
               how_important_is_code)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct,2), "%"),
                angle = 45),
            position = position_stack(vjust = 0.5), 
            size = 1.8,
            show.legend = NA) +
  scale_fill_brewer(palette = "Blues", 
                    name="",
                    breaks = 1:5,
                    labels=c("Not important", "Less important", 
                             "Important", "Very important", "Essential"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(title = "How important is code / research software to your research?",
       x = "",
       y = "Percentage") +
  coord_flip() 


ggsave(filename = paste0("plots/",Sys.Date(), "_", 
                         "Fig6","_",
                         "how_important_is_code", ".png"),
       width = 15, height = 10, units = "in")

# Figure 7. Reasons respondents search for code ----  
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                                     selection_string = 
                                       "what_are_some_of_the_reasons")

reasons_to_search <- as_tibble(myres[[1]])
total_reasons_to_search <- as.integer(myres[[2]]) #157

simple_barplot_report(data = reasons_to_search,
                      tit = "What are some of the reasons",
                      subtit ="to search for code / research software?",
                      fignum = "Fig7",
                      fignam = "what_are_some_of_the_reasons_to_search")

# Figure 8. Mechanisms respondents use to find RS ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                                     selection_string = 
                                       "how_do_you_go_about_finding")

mechanisms_to_find <- as_tibble(myres[[1]])
total_mechanisms_to_find  <- as.integer(myres[[2]]) #157

simple_barplot_report(data = mechanisms_to_find,
                      tit = "How do you go about finding",
                      subtit ="code / research software?",
                      fignum = "Fig8",
                      fignam = "how_do_you_go_about_finding")


# Figure 10. The differences between how coders and non-coders search ----
mechanisms_to_find_write_code <- rawdataxl %>% 
  select(starts_with("how_do_you_go_about_finding"),
         starts_with("do_you_write_code")) %>% 
  filter(if_any(.cols = everything(), ~ !is.na(.))) 

total_responses <- nrow(mechanisms_to_find_write_code)

mechanisms_to_find_write_code %>% 
  unite(col = "write_code", c("do_you_write_code_research_software_research_software_is_software_created_or_used_in_research_from_macros_and_small_scale_scripts_to_big_complex_suites_of_software_and_software_services_source_code_files_algorithms_scripts_computational_workflows_and_executables_yes",
                              "do_you_write_code_research_software_research_software_is_software_created_or_used_in_research_from_macros_and_small_scale_scripts_to_big_complex_suites_of_software_and_software_services_source_code_files_algorithms_scripts_computational_workflows_and_executables_no"),
        sep = "", na.rm = TRUE) %>% 
  # the person who didn't respond the question of write code, does write code
  mutate(write_code = case_when(write_code == "" ~ "Yes",
                                TRUE ~ as.character(write_code))) %>%
  pivot_longer(cols = starts_with("how_do_you_go_about_finding"),
               names_to = "question",
               values_to = "mechanisms",
               values_drop_na = TRUE) %>% 
  ## calculate the height of the bars
  count(mechanisms, write_code) %>%
  mutate(mechanisms = case_when(mechanisms == "Search \"Research Data Australia\" https://researchdata.edu.au/" ~ as.character(mechanisms),
                                n == 1 ~ "Other",
                                TRUE ~ as.character(mechanisms))) %>% 
  group_by(mechanisms, write_code) %>% 
  summarise(n = sum(n)) %>% 
  mutate(score = (n*100)/total_responses) %>% # do I need to score for each group?
  arrange(desc(score)) %>% 
  #View()
  ggplot(aes(x = forcats::fct_reorder(mechanisms, 
                                      score,
                                      max),
             y = score,
             fill = write_code)) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_text(aes(label = paste0(round(score, 2), "%")),
            position = position_dodge(width = 0.9),    # move to center of bars
            vjust = 0.5,    # nudge above top of bar
            hjust = -.2,
            size = 5) + 
  coord_flip() +
  labs(title = "How do those who write code vs those who don't",
       subtitle =" go about finding code / research software?",
       x = "Options",
       y = "Percentage") +
  theme(axis.text = element_text(size = 9)) 
ggsave(filename = paste0("plots/",Sys.Date(), "_Fig10_", 
"mechanisms_to_find_write_code", ".png"), 
    width = 15, height = 10, units = "in")

# Figure 12. The hindrances to finding code and research software ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                                     selection_string = 
                                       "what_factors_have_hindered")

hindrances_to_find <- as_tibble(myres[[1]])
total_hindrances_to_find  <- as.integer(myres[[2]]) #157

simple_barplot_report(data = hindrances_to_find,
                      tit = "What are the hinderances to find",
                      subtit ="code / research software?",
                      fignum = "Fig12",
                      fignam = "what_factors_have_hindered")


# Figure 17. The information in a catalogue ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                                     selection_string = 
                                       "what_information_would_you_find_most_useful_in")

catalogue_info <- as_tibble(myres[[1]])
total_catalogue_info  <- as.integer(myres[[2]]) #157

simple_barplot_report(data = catalogue_info,
                      tit = "What information would you find most useful in a catalogue of",
                      subtit ="code / research software?",
                      fignum = "Fig17",
                      fignam = "what_information_would_you_find_most_useful_in")

# Figure 19. How respondents go about sharing their code ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
                            selection_string = 
                              "how_do_you_go_about_making_code")

researchers_share <- as_tibble(myres[[1]])
total_researchers_share <- as.integer(myres[[2]]) #119

simple_barplot_report(data = researchers_share,
                      tit = "How do you go about making code / research software",
                      subtit = "you have developed available for re-use by others?",
                      fignum = "Fig20",
                      fignam = "How_researchers_share")

# Figure 22. Programming languages ----
source("scripts/helper_functions.R")
myres <- check_data_for_one_question(data = rawdataxl, 
           selection_string = "which_programming_and")

programming_lang <- as_tibble(myres[[1]])
total_programming_lang <- as.integer(myres[[2]]) #119

View(programming_lang)
# needs to merge stata with different spellings
programming_lang <- programming_lang %>% 
  mutate(which_programming_and = case_when(
    str_detect(which_programming_and, "STATA") ~ "Stata",
               TRUE ~ as.character(which_programming_and))
    ) %>% 
  group_by(which_programming_and) %>%
  summarise(across(c(n, pct), sum)) %>% 
  arrange(desc(pct)) 

  View(programming_lang)  

writexl::write_xlsx(
    x = list("question" = programming_lang,
             "total_responses" = as.data.frame(total_programming_lang)),
    path = paste0("dataperquestion/",
                  Sys.Date(), "_", "programming_lang",
                  ".xlsx"),
    format_headers = FALSE)


source("scripts/helper_functions.R")
simple_barplot_report(data = programming_lang,
                      tit = "Which programming and/or scripting language(s) do you use the most?",
                      subtit = "pick up to 3?",
                      fignum = "Fig21",
                      fignam = "programming_lang")

# Figure 22. Search Frequency ----
source("scripts/helper_functions.R")
myres <-
  check_data_for_one_question(data = rawdataxl, 
  selection_string = "how_often_do_you_search_online")

search_freq <- as_tibble(myres[[1]])
total_search_freq <- as.integer(myres[[2]]) #119

View(search_freq)
print(total_search_freq) #157

simple_barplot_report(data = search_freq,
                      tit = "How often you search online for",
                      subtit = "code / research software",
                      fignum = "Fig22",
                      fignam = "search_freq")

# Figure 24. freedom to choose  ----
source("scripts/helper_functions.R")
myres <-
  check_data_for_one_question(data = rawdataxl, 
      selection_string = "how_much_freedom_do")

freedom_to_choose <- as_tibble(myres[[1]])
total_freedom_to_choose <- as.integer(myres[[2]]) #157

simple_barplot_report(data = freedom_to_choose,
                      tit = "How much freedom do you have to search for",
                      subtit = "code / research software",
                      fignum = "Fig25",
                      fignam = "freedom_to_choose")

# Figure 25. Factors inhibiting reuse of the code ----
source("scripts/helper_functions.R")
myres <-
  check_data_for_one_question(data = rawdataxl, 
       selection_string = "if_you_searched_and_found")

inhibiting_reuse <- as_tibble(myres[[1]])
total_inhibiting_reuse <- as.integer(myres[[2]]) #151

View(inhibiting_reuse)
print(total_inhibiting_reuse) #151

simple_barplot_report(data = inhibiting_reuse,
                      tit = "What factors have inhibited you from reusing",
                      subtit = "code / research software",
                      fignum = "Fig25",
                      fignam = "inhibiting_reuse")

# Figure 26. As a person who writes code ----
source("scripts/helper_functions.R")
myres <-
  check_data_for_one_question(data = rawdataxl, 
                              selection_string = "as_a_person_who_codes")

write_code_when_share <- as_tibble(myres[[1]])
total_write_code_when_share <- as.integer(myres[[2]]) #151

View(write_code_when_share)
print(total_write_code_when_share) #119

simple_barplot_report(data = write_code_when_share,
                      tit = "What's the purpose for writing",
                      subtit = "code / research software",
                      fignum = "Fig26",
                      fignam = "as_a_person_who_codes")
