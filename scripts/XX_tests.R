# read raw data ----
rawdataxl <- readxl::read_xlsx("data/2022-08-15__SurveyResults_raw.xlsx") %>%
  janitor::clean_names()

colnames(rawdataxl)

# add theme ----
#devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)
ggthemr('fresh')

# Fig 1 researchers ----
ggplot(researchers, aes(x = are_you_a_researcher, y = pct,
                        label = round(pct, 2))) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Are you a researcher?",
       x = "",
       y = "Percentage")

# Fig 1 RSEs ----

rse <- rawdataxl %>%
    select(starts_with("do_you_consider_yourself")) %>%
    unite(col = "rse", c("do_you_consider_yourself_a_research_software_engineer_a_research_software_engineer_is_defined_as_someone_who_combines_software_engineering_expertise_with_an_intimate_understanding_of_research_if_yes_join_the_rse_aunz_org_community_yes",
                         "do_you_consider_yourself_a_research_software_engineer_a_research_software_engineer_is_defined_as_someone_who_combines_software_engineering_expertise_with_an_intimate_understanding_of_research_if_yes_join_the_rse_aunz_org_community_no"),
          sep = "", na.rm = TRUE) %>%
    count(rse) %>%
    mutate(pct = prop.table(n)) %>%
    print(n = Inf)

  ggplot(rse, aes(x = rse, y = pct,
                          label = scales::percent(accuracy = 0.01,
                                                  pct))) +
    geom_col(position = 'dodge') +
    geom_text(position = position_dodge(width = .9),    # move to center of bars
              vjust = -0.5,    # nudge above top of bar
              size = 3) +
    scale_y_continuous(labels = scales::percent)+
    labs(title = "Do you consider yourself an RSE?",
         x = "",
         y = "Percentage")
# Figure 7. Reasons respondents search for code ---- 
why_search_code <- rawdataxl %>%
  select(starts_with("what_are_some_of_the_reasons")) %>%
  filter(if_any(.cols = everything(), ~ !is.na(.)))

total_responses <- nrow(why_search_code)

why_search_code %>%
  pivot_longer(cols = starts_with("what_are_some_of_the_reasons"),
               names_to = "question",
               values_to = "reasons",
               values_drop_na = TRUE) %>%
  ## calculate the height of the bars
  count(reasons) %>%
  filter(n > 1) %>%
  mutate(score = (n*100)/total_responses) %>%
  arrange(desc(score)) %>%
  ggplot(aes(x = forcats::fct_reorder(reasons,
                                      score,
                                      max),
             y = score)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(score,2), "%")),
            position = position_dodge(width = .1),    # move to center of bars
            vjust = 0.5,    # nudge above top of bar
            hjust = 1,
            size = 2.8) +
  coord_flip() +
  labs(title = "What are some of the reasons",
       subtitle ="why you search for code / research software?",
       x = "Reasons",
       y = "Percentage") +
  theme(axis.text = element_text(size = 8))

# Figure 1 Do you write code? ----
write_code <- rawdataxl %>%
  select(starts_with("do_you_write_code")) %>%
  unite(col = "write_code", c("do_you_write_code_research_software_research_software_is_software_created_or_used_in_research_from_macros_and_small_scale_scripts_to_big_complex_suites_of_software_and_software_services_source_code_files_algorithms_scripts_computational_workflows_and_executables_yes",
                              "do_you_write_code_research_software_research_software_is_software_created_or_used_in_research_from_macros_and_small_scale_scripts_to_big_complex_suites_of_software_and_software_services_source_code_files_algorithms_scripts_computational_workflows_and_executables_no"),
        sep = "", na.rm = TRUE) %>%
  count(write_code) %>%
    mutate(pct = prop.table(n)) %>%
    print(n = Inf)

  ggplot(write_code, aes(x = write_code, y = pct,
                  label = scales::percent(accuracy = 0.01,
                                          pct))) +
    geom_col(position = 'dodge') +
    geom_text(position = position_dodge(width = .9),    # move to center of bars
              vjust = -0.5,    # nudge above top of bar
              size = 3) +
    scale_y_continuous(labels = scales::percent)+
    labs(title = "Do you write code/research software?",
         x = "",
         y = "Percentage")
  
# Figure 2 FOR codes ----
  nparticipant <- nrow(rawdataxl)

  codes_for <- rawdataxl %>%
      select(starts_with("what_are_the_two")) %>%
      ## Reformat the data
      pivot_longer(cols = starts_with("what_are_the_two"),
                   names_to = "question",
                   values_to = "FOR_codes",
                   values_drop_na = TRUE) %>%
      ## calculate the height of the bars
      group_by(FOR_codes) %>%
      summarise(score = length(FOR_codes)/nparticipant) %>%
      arrange(desc(score))

      ## Plot
      codes_for %>%
        mutate(FOR_codes = factor(FOR_codes, levels = unique(FOR_codes))) %>%
      ggplot(aes(x = FOR_codes, y = (score*100))) +
      geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "What are the Fields of Research (FOR)",
             x = "FOR codes",
             y = "Percentage")

# Figure 6 How important is code to your research? ----
software_importance <- rawdataxl %>%
  select(how_important_is_code_research_software_to_your_research =
           starts_with("how_important_is_code")) %>%
  drop_na()

#  plot
respondents_importance <- nrow(software_importance)
software_importance <- rawdataxl %>%
  select(how_important_is_code_research_software_to_your_research =
           starts_with("how_important_is_code")) %>%
  drop_na() %>%
  group_by(how_important_is_code_research_software_to_your_research) %>%
  count() %>%
  mutate(score = n/
              respondents_importance) %>%
  arrange(desc(score)) %>%
  mutate(pct = (score*100)) %>%
  mutate(essential = "X") %>%
  View()
      
# Fig 8. Mechanisms to find ----
mechanisms_to_find <- rawdataxl %>% 
  select(starts_with("how_do_you_go_about_finding")) %>% 
  filter(if_any(.cols = everything(), ~ !is.na(.))) 

total_responses <- nrow(mechanisms_to_find)

mechanisms_to_find %>% 
  pivot_longer(cols = starts_with("how_do_you_go_about_finding"),
               names_to = "question",
               values_to = "mechanisms",
               values_drop_na = TRUE) %>% 
  ## calculate the height of the bars
  count(mechanisms) %>%
  mutate(mechanisms = case_when(mechanisms == "Search \"Research Data Australia\" https://researchdata.edu.au/" ~ as.character(mechanisms),
                                n == 1 ~ "Other",
                                TRUE ~ as.character(mechanisms))) %>% 
  group_by(mechanisms) %>% 
  summarise(n = sum(n)) %>% 
  mutate(score = (n*100)/total_responses) %>% 
  arrange(desc(score)) %>% 
  ggplot(aes(x = forcats::fct_reorder(mechanisms, 
                                      score,
                                      max),
             y = score)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(score,2), "%")),
            position = position_dodge(width = .1),    # move to center of bars
            vjust = 0.5,    # nudge above top of bar
            hjust = 1.1,
            size = 2) + 
  coord_flip() +
  labs(title = "How do you go about finding",
       subtitle ="code / research software?",
       x = "Options",
       y = "Percentage") +
  theme(axis.text = element_text(size = 8))
ggsave(filename = paste0("plots/",Sys.Date(), "_Fig9_", 
                         "mechanisms_to_find", ".png"), 
       width = 15, height = 10, units = "in")

# Figure 12. The hindrances to finding code and research software ----
hindrances_to_find <- rawdataxl %>% 
  select(starts_with("what_factors_have_hindered")) %>% 
  filter(if_any(.cols = everything(), ~ !is.na(.))) 

total_responses <- nrow(hindrances_to_find)

hindrances_to_find %>% 
  pivot_longer(cols = starts_with("what_factors_have_hindered"),
               names_to = "question",
               values_to = "factors_hindered",
               values_drop_na = TRUE) %>% 
  ## calculate the height of the bars
  count(factors_hindered) %>%
  mutate(factors_hindered = case_when(n == 1 ~ "Other",
                                      TRUE ~ as.character(factors_hindered))) %>% 
  group_by(factors_hindered) %>% 
  summarise(n = sum(n)) %>% 
  mutate(score = (n*100)/total_responses) %>% 
  arrange(desc(score)) %>%
  #View()
  ggplot(aes(x = forcats::fct_reorder(factors_hindered, 
                                      score,
                                      max),
             y = score)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(score,2), "%")),
            position = position_dodge(width = .1),    # move to center of bars
            vjust = 0.5,    # nudge above top of bar
            hjust = 1.1,
            size = 4) + 
  coord_flip() +
  labs(title = "What factors have hindered your ability to find",
       subtitle ="code / research software?",
       x = "Options",
       y = "Percentage") +
  theme(axis.text = element_text(size = 14))
ggsave(filename = paste0("plots/",Sys.Date(), "_Fig13_", 
                         "hindrances_to_find", ".png"), 
       width = 15, height = 10, units = "in")

# Figure 18. The information in a catalogue ----
catalogue_info <- rawdataxl %>% 
  select(starts_with("what_information_would_you_find_most_useful_in")) %>% 
  filter(if_any(.cols = everything(), ~ !is.na(.))) 

total_responses <- nrow(catalogue_info)

catalogue_info %>% 
  pivot_longer(cols = starts_with("what_information_would_you_find_most_useful_in"),
               names_to = "question",
               values_to = "info_c",
               values_drop_na = TRUE) %>% 
  ## calculate the height of the bars
  count(info_c) %>%
  mutate(info_c = case_when(n == 1 ~ "Other",
                            TRUE ~ as.character(info_c))) %>% 
  group_by(info_c) %>% 
  summarise(n = sum(n)) %>% 
  mutate(score = (n*100)/total_responses) %>% 
  arrange(desc(score)) %>%
  #View()
  ggplot(aes(x = forcats::fct_reorder(info_c, 
                                      score,
                                      max),
             y = score)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(score,2), "%")),
            position = position_dodge(width = .1),    # move to center of bars
            vjust = 0.5,    # nudge above top of bar
            hjust = 1.1,
            size = 4) + 
  coord_flip() +
  labs(title = "What information would you find most useful in a catalogue of",
       subtitle ="code / research software to help you find it?",
       x = "Options",
       y = "Percentage") +
  theme(axis.text = element_text(size = 14))
ggsave(filename = paste0("plots/",Sys.Date(), "_Fig18_", 
                         "catalogue_info", ".png"), 
       width = 15, height = 10, units = "in")


#############################################
## 3 function count selection ----
count_selection <- function(data, selection_string){
  oneq_answers <- data %>% 
    select(starts_with(selection_string)) %>% 
    select(ends_with("yes")) %>% 
    drop_na() %>%
    count() %>% 
    as.integer()
  return(oneq_answers)
  
}

count_selection_no <- function(data, selection_string){
  oneq_answers <- data %>% 
    select(starts_with(selection_string)) %>% 
    select(ends_with("no")) %>% 
    drop_na() %>%
    count() %>% 
    as.integer()
  return(oneq_answers)
  
}
## 2 function Check data for comparison  ----

check_data_for_comparison <- function(data, selection_string, 
                                      comparison_group1,
                                      comparison_group2){
  
  total_selection_criteria <- count_selection(data, selection_string)
  total_group1 <- count_selection(data, comparison_group1)
  total_group2 <- count_selection(data, comparison_group2)
  print(paste(total_group1, total_group2))
  
  mygroup1 <- data %>%
           select(starts_with(comparison_group1))  %>%
           names() %>%
           as_vector()
  print(mygroup1)

  mygroup2 <- data %>%
    select(starts_with(comparison_group2))  %>%
    names() %>%
    as_vector()
  print(mygroup2)
  
  q_answers1 <- data %>% 
    select(respondent_id,
           starts_with(selection_string),
           starts_with(comparison_group1)) %>% 
    filter(if_any(.cols = everything(), ~ !is.na(.))) %>% 
    unite(col = "group1", mygroup1,
          sep = "", na.rm = TRUE) %>% 
    # merge long
    pivot_longer(cols = starts_with(selection_string),
                 names_to = "question",
                 values_to = "options",
                 values_drop_na = TRUE) %>% 
    count(options, group1) %>% 
    rename(!! quo_name(selection_string) := options,
           !! quo_name(comparison_group1) := group1) %>%
    mutate(pct = (n*100)/total_group1) %>%
    arrange(desc(pct))
  
  q_answers2 <- data %>% 
    select(respondent_id,
           starts_with(selection_string),
           starts_with(comparison_group2)) %>% 
    filter(if_any(.cols = everything(), ~ !is.na(.))) %>% 
    unite(col = "group2", mygroup2,
          sep = "", na.rm = TRUE) %>% 
    # merge long
    pivot_longer(cols = starts_with(selection_string),
                 names_to = "question",
                 values_to = "options",
                 values_drop_na = TRUE) %>% 
    count(options, group2) %>% 
    rename(!! quo_name(selection_string) := options,
           !! quo_name(comparison_group2) := group2) %>%
    mutate(pct = (n*100)/total_group2) %>%
    arrange(desc(pct))
  
  # combine tables
  q_answers_all <- q_answers1 %>% 
    inner_join(q_answers2, by = selection_string)
  
  l1 = list(q_answers1 = q_answers1,
            q_answers2 = q_answers2,
            q_answers_all = q_answers_all, 
            total_group1 = total_group1,
            total_group2 = total_group2 
            )
  
  # #write data 
  library(writexl)

  writexl::write_xlsx(
    x = list("question" = q_answers_all,
             "total_responses_group1" = as.data.frame(total_group1),
             "total_responses_group2" = as.data.frame(total_group2)),
    path = paste0("dataperquestion/",
                  Sys.Date(), "_", selection_string, "_",
                  comparison_group1, "_", comparison_group2,
                  ".xlsx"),
    format_headers = FALSE)
  
  return(l1)
}

test_compare <- check_data_for_comparison(
  data = rawdataxl, 
  selection_string = "as_a_person_who_codes", 
  comparison_group1 = "are_you_a_researcher",
  comparison_group2 = "do_you_consider_yourself")

q1 <- as_tibble(test_compare[[1]])
View(q1)
q2 <- as_tibble(test_compare[[2]])
View(q2)
fig27 <- as_tibble(test_compare[[3]])
View(fig27)

# compare answers with one group -----
compare_one_group <- function(data, selection_string, 
                                      comparison_group1){
  
  total_selection_criteria <- count_selection(data, selection_string)
  total_group1_yes <- count_selection(data, comparison_group1)
  total_group1_no <- count_selection_no(data, comparison_group1)
  print(paste(total_group1_yes, total_group1_no))
  
  mygroup1 <- data %>%
    select(starts_with(comparison_group1))  %>%
    names() %>%
    as_vector()
  print(mygroup1)

  q_answers1 <- data %>% 
    select(respondent_id,
           starts_with(selection_string),
           starts_with(comparison_group1)) %>% 
    filter(if_any(.cols = everything(), ~ !is.na(.))) %>% 
    unite(col = "group1", mygroup1,
          sep = "", na.rm = TRUE) %>% 
    # merge long
    pivot_longer(cols = starts_with(selection_string),
                 names_to = "question",
                 values_to = "options",
                 values_drop_na = TRUE) %>% 
    count(options, group1) %>% 
    group_by(group1) %>%
    # add names 
    rename(!! quo_name(selection_string) := options,
           !! quo_name(comparison_group1) := group1) %>%
    mutate(total = ifelse(
      (do_you_write_code == "Yes"), total_group1_yes, 
             ifelse(do_you_write_code == "No", total_group1_no, NA)))%>% 
    mutate(pct = ifelse(
      (do_you_write_code == "Yes"),round((n*100)/total_group1_yes, 2),
      ifelse(do_you_write_code == "No", round((n*100)/total_group1_no, 2), NA) )) %>% 
    arrange(desc(pct))
  
  l1 = list(q_answers1 = q_answers1,
            total_group1_yes = total_group1_yes,
            total_group1_no = total_group1_no
  )
  
  # #write data 
  library(writexl)
  
  writexl::write_xlsx(
    x = list("question" = q_answers1,
             "total_responses_group1_yes" = as.data.frame(total_group1_yes),
             "total_responses_group1_no" = as.data.frame(total_group1_no)),
    path = paste0("dataperquestion/",
                  Sys.Date(), "_", selection_string, "_and_",
                  comparison_group1, 
                  ".xlsx"),
    format_headers = FALSE)
  
  return(l1)
}


## fig 16 ----
catalogue_compare <- compare_one_group(
  data = rawdataxl, 
  selection_string = "what_information_would_you_find_most_useful_in",
  comparison_group1 = "do_you_write_code")

# q1 <- as_tibble(test_compare[[1]])
# View(q1)
# q2 <- as_tibble(test_compare[[2]])
# View(q2)
# fig27 <- as_tibble(test_compare[[3]])
# View(fig27)