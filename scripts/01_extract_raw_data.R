
# extract the zip
unzip("rawdata/Data_All_220815.zip", exdir = "rawdata/")

#Show extracted files rawdata/Excel/Rese..
list.files("rawdata/Excel/")
#[1] "CollectorList.xlsx"                              "PageOrder.xlsx"                                 
#[3] "Research software findability in Australia.xlsx"

#Rename
file.rename(from = "rawdata/Excel/Research software findability in Australia.xlsx",
            to = "rawdata/Excel/2022-08-15__SurveyResults.xlsx")

#checks
file.size("rawdata/Excel/2022-08-15__SurveyResults.xlsx")

# call read xl
library(readxl)

rawdataxl <- readxl::read_xlsx("rawdata/Excel/2022-08-15__SurveyResults.xlsx")
df <- rawdataxl

#get header function
source("scripts/ext_headerfunction.R")
newfile <- sm_header_function(df, "...")
colnames(newfile)


# extract identifiable  information ----
identifiable <- newfile[,c(1:9,249:254)]
dim(identifiable)

#install.packages("janitor")
library(janitor)
library(tidyverse)

identifiable <- identifiable %>% 
                janitor::remove_empty("cols") %>% 
                janitor::clean_names() %>% 
                dplyr::rename(first_name = what_is_your_first_name_this_information_will_be_kept_confidential_open_ended_response,
                              last_name = what_is_your_last_name_this_information_is_optional_and_will_be_kept_confidential_open_ended_response,
                              email_address = what_is_your_email_address_this_information_is_optional_and_will_be_kept_confidential_and_will_only_to_be_used_if_further_clarification_is_needed_and_to_let_you_know_about_the_results_of_the_survey_open_ended_response,
                              sign_me_up_for_the_ardc_newsletter = next_steps_sign_me_up_for_the_ardc_newsletter_to_receive_fortnightly_emails_about_digital_research_news_events_and_jobs)
## Checkpoint ----                
colnames(identifiable)
dim(identifiable)
View(identifiable)

identifiable_email_responses <- identifiable %>% 
  filter(!is.na(email_address))

## 1. Newsletter emails ----
newsletter_emails <- identifiable %>% 
  filter(!is.na(email_address)) %>% 
  filter(!is.na(sign_me_up_for_the_ardc_newsletter)) %>% 
  select(first_name, last_name, email_address, sign_me_up_for_the_ardc_newsletter )
  
readr::write_excel_csv(
  x = newsletter_emails,
  file = "data/2022-08-15__SurveyResults_newsletter_emails.csv")


## 2. Contact me about the survey ----
survey_next_steps_emails <- identifiable %>% 
  filter(!is.na(email_address)) %>% 
  filter(!is.na(next_steps_let_me_know_what_the_results_of_the_survey_are)) %>% 
  select(first_name, last_name, email_address, 
         next_steps_let_me_know_what_the_results_of_the_survey_are) %>% 
  rename(survey_next_steps = next_steps_let_me_know_what_the_results_of_the_survey_are)

readr::write_excel_csv(
  x = survey_next_steps_emails,
  file = "data/2022-08-15__SurveyResults_survey_next_steps_emails.csv")

## 3. Contact me about the Research Software ----
contact_about_research_software_emails <- identifiable %>% 
  filter(!is.na(email_address)) %>% 
  filter(!is.na(next_steps_contact_me_to_talk_about_research_software)) %>% 
  select(first_name, last_name, email_address, 
         next_steps_contact_me_to_talk_about_research_software) %>% 
  rename(contact_about_research_software = next_steps_contact_me_to_talk_about_research_software)

readr::write_excel_csv(
  x = contact_about_research_software_emails,
  file = "data/2022-08-15__SurveyResults_contact_about_research_software_emails.csv")


# extract survey information ----
colnames(newfile)
survey_raw <- newfile[,c(1:4,10:248)]
dim(survey_raw)
colnames(survey_raw)
View(survey_raw)

install.packages("writexl")
library(writexl)

writexl::write_xlsx(
  x = survey_raw,
  path = "data/2022-08-15__SurveyResults_raw.xlsx",
  format_headers = FALSE)

