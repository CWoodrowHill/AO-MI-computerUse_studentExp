#options("install.lock"=FALSE)

## Analysis of practice trials in Study 1 (students) ##

library(tidyverse)

#### Import data ####

#First combine all the student data into one df

allData <- list.files(path = "//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/studentRawData",
                      pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%          # Store all files in list   
  #lapply(\(x) mutate(x, across(participant, as.character)) %>%
  plyr::rbind.fill()  # Combine data sets into one data set                                                      
#allData <- as.data.frame(allData)    #convert from a tibble into a dataframe

#As a sanity check, you should have data from 51 Ps. 
#Run the below code to count how many unique IDs there are

dplyr::count(unique(allData[c("participant")]))

allData_prac <- allData %>%
  dplyr::select(c(participant, group, pracWords, pracKeys.keys, pracKeys.rt, 
                  practice_control.thisRepN, practice_control.thisTrialN,
                  practice_control.thisN, practice_control.thisIndex)) %>%
  dplyr::filter(!is.na(pracWords))

#Now filter on participants who completed the practice more than once

prac_repeats_dat <- allData_prac %>%
  dplyr::filter(practice_control.thisN >= 3)
