#helper functions
library(readxl)
library(tidyverse)
library(janitor)

rawdataxl <- readxl::read_xlsx("data/2022-08-15__SurveyResults_raw.xlsx") %>% 
  janitor::clean_names()

library(tidyverse)
#1 function Check data for one question  ----

check_data_for_one_question <- function(data, selection_string){
  oneq_answers <- data %>% 
  select(starts_with(selection_string)) %>% 
  filter(if_any(.cols = everything(), ~ !is.na(.))) 

  total <- nrow(oneq_answers)

  oneq_answers <- oneq_answers %>% 
  pivot_longer(cols = starts_with(selection_string),
               names_to = "question",
               values_to = "options",
               values_drop_na = TRUE) %>% 
  ## calculate the height of the bars
  count(options) %>%
  mutate(options = case_when(n == 1 ~ "Other",
                   TRUE ~ as.character(options))) %>% 
  group_by(options) %>% 
  # https://stackoverflow.com/questions/54575680/how-do-i-pass-a-dynamic-variable-name-created-using-enquo-to-dplyrs-mutate-fo
  rename(!! quo_name(selection_string) := options) %>% 
  summarise(n = sum(n)) %>% 
  mutate(pct = (n*100)/total) %>% 
  arrange(desc(pct))

  l1 = list(oneq_answers = oneq_answers, 
            total = total)
  
  #write data 
  library(writexl)
  
  writexl::write_xlsx(
    x = list("question" = oneq_answers,
             "total_responses" = as.data.frame(total)),
    path = paste0("dataperquestion/",
                 Sys.Date(), "_", selection_string,
                 ".xlsx"),
    format_headers = FALSE)
  
  return(l1)
}
# simple bar plot function ----
simple_barplot_report <- function(data,
                                  tit,
                                  subtit,
                                  fignum,
                                  fignam){
  library(forcats)
  options <- data %>% 
              select(1) %>% 
              as_vector()
  
  ggplot(data = data,
         aes(x = forcats::fct_reorder(options,
                                      pct,
                                      max),
             y = pct)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = .9),    # 1move to center of bars
            vjust = -0.5,    # positive nudge above top of bar
            hjust = 1.1,
            size = 5) +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  labs(title = tit,
       subtitle = subtit,
       x = "Options",
       y = "Percentage") +
  theme(axis.text = element_text(size = 14))
   
  ggsave(filename = paste0("plots/",Sys.Date(), "_", fignum,"_",
                         fignam, ".png"),
       width = 15, height = 10, units = "in")
}
