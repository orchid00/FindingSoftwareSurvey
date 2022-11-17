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

# Figure 21. Programming languages long term access----
col_prog_longterm <- rawdataxl %>% 
  select(starts_with("list_up_to_3"))

library(writexl)

writexl::write_xlsx(
  x = list("question" = col_prog_longterm,
           "total_responses" = as.data.frame(nrow(col_prog_longterm))),
  path = paste0("dataperquestion/",
                Sys.Date(), "_", "list_up_to_3_unfiltered",
                ".xlsx"),
  format_headers = FALSE)

# filtering and cleaning
prog_longterm <- rawdataxl %>% 
  select(starts_with("list_up_to_3")) %>% 
  rename_at(1, ~"list_up_to_3_prog") %>% 
  filter(if_any(.cols = everything(), ~ !is.na(.))) %>% 
  # filter nulls 
  filter(!list_up_to_3_prog %in% 
           c("-", "..", "NA", "N/A","na",
             "Don't know","ddf", "dfg",
             "I don't know")) %>% 
  # filter comments without prog languages
  filter(!str_starts(list_up_to_3_prog, 
   "I'm not sure|Long term access|The base lan|advanced statistical analysis|All research software|Power line mapping")) %>% 
  # clean data
  # change commas for pipes
  mutate(list_up_to_3_prog = 
           str_replace_all(list_up_to_3_prog, ",", "|")) %>% 
  # change semicolons for pipes
  mutate(list_up_to_3_prog = 
           str_replace_all(list_up_to_3_prog, ";", "|")) %>% 
  # remove excess spaces
  mutate(list_up_to_3_prog = str_squish(list_up_to_3_prog)) %>%
  # change delete extra mentions of Github or URLs
  mutate(list_up_to_3_prog = 
           str_replace_all(list_up_to_3_prog, "\\(on GitHub\\)", "")) %>%
  mutate(list_up_to_3_prog = 
           str_replace(list_up_to_3_prog, " https://root.cern/", "")) %>%
  mutate(list_up_to_3_prog = 
           str_replace(list_up_to_3_prog, " https://github.com/ganga-devs/ganga", "")) %>%
  mutate(list_up_to_3_prog = 
           str_replace(list_up_to_3_prog, "https://github.com/csiro-coasts/", "")) %>%
  mutate(list_up_to_3_prog = 
           str_replace(list_up_to_3_prog, "https://github.com/geoserver", "")) %>%
  mutate(list_up_to_3_prog = 
           str_replace(list_up_to_3_prog, "https://github.com/OPENDAP/", "|")) %>%
  mutate(list_up_to_3_prog = 
           str_replace(list_up_to_3_prog, "mqAncientHistory/", "")) %>%
  # change / for pipes
  mutate(list_up_to_3_prog = 
           str_replace_all(list_up_to_3_prog, "/", "|")) %>%
  # separate items per row
  separate_rows(., list_up_to_3_prog, sep = "\\|")  %>% 
  # remove empty spaces
  mutate(list_up_to_3_prog = str_trim(list_up_to_3_prog)) %>% 
  # remove empty rows
  filter(list_up_to_3_prog != "") %>% 
  #remove comments not prog lang
  filter(!grepl("^Bioinformatics tools|^but am a|^data presentation tools|^etc|^Free Operating Systems|^maybe|^observatory|^social network|^variant calling|^various|^visualisation|^VM|^my own|^Survey", 
                list_up_to_3_prog)) %>% 
  # clean names
  mutate(list_up_to_3_prog = recode(list_up_to_3_prog, 
         "AI Zoo" = "AI_Model_Zoo",
         "AntConc SketchEngine"="AntConc|SketchEngine",
         "apptainer neurodesk" = "apptainer|neurodesk",
         "arcgis js api and loads of other open web based apps" =
           "ArcGIS|JavaScript",
         "Broadly expect needing examples on how to implement ecological models in R or Python"=
           "R|Python",
         "C++ & F90 high performance compilers with MP"=
           "C++|F90",
         "Code : Python"="Python",
         "CodeRespositories(GitHub"= "GitHub",
         "Cytoscape Rstudio"= "Cytoscape|Rstudio",
         "DB Browser for SQLite"="DB Browser|SQLite",
         "deep learning libraries (pytorch"="PyTorch",
         "differential expression) Galaxy (data analysis software) FigShare"=
           "Galaxy|FigShare",
         "Enterprise Grafana Plugin for MongoDB" =
           "Grafana|MongoDB",
         "Fieldworks Language Explorer" =
           "FieldWorks",
         "Finns spatial text and Trove analysis in 'historical fires near me' n github"=
           "Trove",
         "Fortran standard library project" ="Fortran",
         "Gitlab)"="GitLab",
         "HDF5 libraries"= "HDF5",
         "htsjdk cromwell rdflib"="HTSJDK|cromwell|RDFLib",
         "ImageJ and plugins" ="ImageJ",
         "Julia R Python"="Julia|R|Python",
         "Julia SQLite JuMP.jl"= "Julia|SQLite|JuMP",
         "Jupiter lab"="Jupyter",
         "linux docker"= "Linux|Docker",
         "Mathematica (Wolfram Research Inc)."="Mathematica",
         "matlab python"="Matlab|Python",
         "mne-python"="MNE-Python",
         "Neurodesk and all tools therein"="Neurodesk",
         "NVivo spss excel"="NVivo|SPSS|Excel",
         "Omeka Devonthink Tropy Tinderbox"="Omeka|Devonthink|Tropy|Tinderbox",
         "OMOP cdm DHIS2"="OMOP|cdm|DHIS2",
         "PowerBI NVIDIA Omniverse AWS SageMaker"=
         "PowerBI|NVIDIA|Omniverse|AWS|SageMaker",
         "PyData software ecosystem"="PyData",
         "Python - astropy"="Python|astropy",
         "Python and JavaScript"="Python|JavaScript",
         "Python ecosystem CASA Askapsoft"=
           "Python|CASA|ASKAPsoft",
         "Python pandas"="Python|Pandas",
         "Qualitative data analysis tools like NVivo."="NVivo",
         "qualitative"="",
         "R And many of its packages"="R",
         "R markdown"="RMarkdown",
         "R programming language"="R",
         "R SAS Graph Pad Prism"= "R|SAS|GraphPad_Prism",
         "R shiny"="shiny",
         "R."="R", "R Studio"="RStudio",
         "Redcap SPSS"="REDCap|SPSS",
         "SAS R Stata"="SAS|R|Stata",
         "SAS studio and office365"="SAS|office365",
         "SciPy stack Tidyverse packages for R"=
           "SciPy|tidyverse|R",
         "Scripting languages like R"="R",
         "spectra analysis"="spectra",
         "SPSS Google Scholar DSpace"="SPSS|Google Scholar|DSpace",
         "SPSS Stata Nvivo"="SPSS|Stata|NVivo",
         "standards and tools like open seadragon"="seadragon",
         "Stata R"="Stata|R",
         "FreeSurer"="FreeSurfer",
         "the scientific python ecosystem (scipy"="Python|SciPy",
         "tidyverse (R)"="tidyverse|R",
         "twarc sqlite Python"="twarc|SQLite|Python",
         "Underworld Badlands GPlates"="Underworld|Badlands|GPlates",
         "Wolfram Language & Mathematica R"="Wolfram|Mathematica|R",
         "wrf grace numpy" ="wrf|grace|numpy")) %>%
  # separate items per row
  separate_rows(., list_up_to_3_prog, sep = "\\|")  %>% 
  # remove empty rows
  filter(list_up_to_3_prog != "") %>% 
  # fix capitalisation
  mutate(list_up_to_3_prog = recode(list_up_to_3_prog, 
             "docker" = "Docker",
             "Elan" = "ELAN",
             "endnote"="Endnote",
             "fortran"="Fortran",
             "Git"="git",
             "Github"="GitHub",
             "MATLAB"="Matlab",
             "Mongodb"="MongoDB",
             "neurodesk"="Neurodesk",
             "nextflow"="NextFlow",
             "Nextflow"="NextFlow",
             "numpy"="NumPy",
             "Numpy"="NumPy",
             "nVivo"="NVivo",
             "Nvivo"="NVivo",
             "NVIVO"="NVivo",
             "pandas"="Pandas",
             "Phython"="Python",
             "python"="Python",
             "pytorch"="PyTorch",
             "pyGplates"="pyGPlates",
             "pygplates"="pyGPlates",
             "quarto"="Quarto",
             "Rstudio"="RStudio",
             "scipy"="SciPy",
             "STATA"="Stata",
             "tensorflow"="Tensorflow")) %>% 
  count(list_up_to_3_prog) %>% 
  arrange(desc(n)) 

writexl::write_xlsx(
  x = list("question" = prog_longterm,
           "total_responses" = as.data.frame(nrow(prog_longterm))),
  path = paste0("dataperquestion/",
                Sys.Date(), "_", "list_up_to_3_filtered_counts",
                ".xlsx"),
  format_headers = FALSE)

 
#157 everyone responded 
# but there are 
# 1 		-
# 1     ..
# 1   	Don't know 
# 1     ddf
# 1     dfg
# 1     I'm not sure I can meaningfully answer this, I use a huge variety of software for numerical modelling, data munging, and analysis.
# 1     I don't know
# 1     Long term access is not required.
# 3     N/A
# 1     na
# 1     NA
# 1     The base languages, I can code anything else. PS: I do have a software engineering BsC, but am a researcher.
# 1     advanced statistical analysis and modelling packages
# 1     All research software for my research in the past was available through github
# 1     Power line mapping gradient topography wind speedand directions
# 157 - 17 = 140 responses

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
