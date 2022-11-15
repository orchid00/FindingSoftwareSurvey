# read data ----
# call read xsl
library(readxl)
rawdata <- readxl::read_xlsx("data/2022-08-15__SurveyResults_raw.xlsx")

# clean data ----
# library(tidyverse)

df <- rawdata

#my_cells <- tidyxl::xlsx_cells("data/2022-08-15__SurveyResults_raw.xlsx")
# # recommended by: https://youtu.be/qUgVhpg-lP4
# library(tidyxl)
# library(unpivotr)
# library(tidyverse)
# my_cells %>% 
#   unpivotr::behead("up-left", "questions") %>% 
#   unpivotr::behead("left", `Are you a Researcher? Yes`) %>% 
#   # unpivotr::behead("left", "id_collector") %>%
#   # unpivotr::behead("left", "date_start") %>%
#   # unpivotr::behead("left", "date_end") %>%
#   #dplyr::select()
#   View()
