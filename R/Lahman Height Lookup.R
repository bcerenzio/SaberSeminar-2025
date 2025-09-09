library(Lahman)
library(tidyverse)

data(People)


People$mlbam_name <- str_c(People$nameLast, ', ', str_replace_all(People$nameFirst, ' ', ''))

write_csv(People %>% 
            mutate(across(everything(), ~as.character(.))) %>% 
            mutate(across(everything(), ~replace_na(., 'NULL'))), 'lahman_people.csv')
