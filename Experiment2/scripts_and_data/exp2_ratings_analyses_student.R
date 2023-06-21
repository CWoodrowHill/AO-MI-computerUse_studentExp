#Before installing: options("install.lock"=FALSE)

library(tidyverse)
library(rcompanion)

## Script to analyse self-report ratings from Experiment 2 student typing study ##

#Import one set of data (we only need one set  for the rating scale)

wwData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/dataForAnalyses/study3data_ww.csv')

#### Tidy data #### 

rateData <- wwData %>%
  dplyr::select(c(participant, group, Condition, ratePerf, imageryQ, VI, KI)) %>%
  dplyr:: mutate(participant = factor(participant),
                 ratePerf = as.integer(ratePerf), 
                 imageryQ = as.integer(imageryQ),
                 VI = as.integer(VI), 
                 KI = as.integer(KI),
                 firstCond = ifelse(group == "A" | group == "C" | group == "E" | group == "G",
                                    "AOMIspec", "AOMIgen")) %>%
  dplyr::distinct()


#### Summary data ####

#Look at count of people who said they had imagined in the baseline condition
rateData %>%
  dplyr::filter(Condition == "Baseline" & imageryQ > 2) %>%
  dplyr::count(participant)
#n=12

#Look at count of people who said they had imagined in the control condition
rateData %>%
  dplyr::filter(Condition == "Control" & imageryQ > 2) %>%
  dplyr::count(participant)
#n=15

#Median imagery rating for baseline and control conditions
#Higher = more frequent imagery
rateData %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(median_ima = median(imageryQ))
#More people said they imagined in the baseline

#Median imagery rating in AO+MI conditions
#Higher = more vivid/intense imagery
rateData %>%
  dplyr::group_by(Condition) %>%
  dplyr::filter(!is.na(VI)) %>% #remove NAs from this column
  dplyr::summarise(median_vi = median(VI), median_KI = median(KI))
#VI better in AOMIgen and KI better in AOMIspec

#Do imagery ratings vary according to order group?
rateData %>%
  dplyr::group_by(Condition, firstCond) %>%
  dplyr::filter(!is.na(VI)) %>% #remove NAs from this column
  dplyr::summarise(median_vi = median(VI), median_KI = median(KI))
#VI better in AOMIgen and KI better in AOMIspec

#Median performance rating in all conditions
#Higher = better self-rated performance
rateData %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(median_perf = median(ratePerf))
#Lowest performance in control, then baseline. AO+MI equally highest


#### Non-parametric statistics ####


## Typing performance ratings - Friedman Test ##

perfFried <- friedman.test(ratePerf ~ Condition | participant, 
                           data = rateData)
perfFried
#No significant differences


### Run Wilcoxon signed rank test on imagery ratings ###

#Separate by condition

spec_rateDat <- rateData %>%
  filter(Condition == "AOMIspec")

gen_rateDat <- rateData %>%
  filter(Condition == "AOMIgen")

viTest <- wilcox.test(spec_rateDat$VI, gen_rateDat$VI, paired = TRUE,
                      exact = FALSE) 
viTest #no sig differences
#Cannot compute exact p values due to ties in the data, hence exact=FALSE

kiTest <- wilcox.test(spec_rateDat$KI, gen_rateDat$KI, paired = TRUE,
                      exact = FALSE) 
kiTest  #no sig differences


## Compare spontaneous imagery ratings in baseline and control conditions ##

#Separate by condition

base_rateDat <- rateData %>%
  filter(Condition == "Baseline")

cont_rateDat <- rateData %>%
  filter(Condition == "Control")

imageryQTest <- wilcox.test(base_rateDat$imageryQ, cont_rateDat$imageryQ, 
                            paired = TRUE,
                            exact = FALSE) 
imageryQTest
#Significant difference

#Get z statistic

Z = qnorm(imageryQTest$p.value/2)
#Calculation taken from https://stats.stackexchange.com/questions/330129/how-to-get-the-z-score-in-wilcox-test-in-r

#Get effect size

#Make joint dataset to do this

baseCont_rateDat <- rateData %>%
  dplyr::filter(Condition == "Baseline" | Condition == "Control") %>%
  droplevels 

baseCont_rateDat %>% 
  rstatix::wilcox_effsize(imageryQ ~ Condition, paired = TRUE)

