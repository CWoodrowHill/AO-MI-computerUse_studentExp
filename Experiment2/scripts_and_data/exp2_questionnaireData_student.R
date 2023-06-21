#options("install.lock"=FALSE)

library(tidyverse)

## Analysis of Experiment 2 student typing study questionnaire data ##

#Import reference data for matching Participant IDs

wwData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/dataForAnalyses/study3data_ww.csv')

#Create a column to show which condition was completed first out of AOMIspec and 
#AOMIgen according to group

wwData2 <- wwData %>%
  dplyr::mutate(firstCond = ifelse(group == "A" | group == "C" | group == "E" | group == "G", "AOMIspec", "AOMIgen"))

#### Imagery Questionnaire ####

##### Import and tidy data ####

# Import imagery questionnaire data
imaQ <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/Questionnaires/imageryQ_20230224.csv")

#Drop unnecessary columns

imaQ2 <- imaQ[,-c(1:11)]

#Rename columns for clarity and convert ratings to integers

imaQ3 <- imaQ2 %>%
  dplyr::rename(participant = Q1.1,
                rank_conds_baseline = Q1.2_1,
                rank_conds_control = Q1.2_2,
                rank_conds_spec = Q1.2_3,
                rank_conds_gen = Q1.2_4,
                diff_baseline = Q1.3_1,
                diff_control = Q1.4_1,
                diff_spec = Q1.5_1,
                diff_gen = Q1.6_1,
                ima_success = Q1.7,
                ima_quality = Q1.8,
                ima_perspective = Q1.9,
                spon_ima = Q1.11,
                hand_used = Q1.13,
                diff_general = Q1.14,
                rank_ima_fastVids = Q1.15_1,
                rank_ima_slowVids = Q1.15_2,
                rank_control_fs = Q1.15_3,
                rank_baseline_fs = Q1.15_4,
                improved = Q2.1,
                made_worse = Q2.2,
                actor_typing_slow = Q3.1,
                actor_typing_fast = Q3.2,
                actor_similar = Q3.3,
                speed_impact = Q3.4,
                speed_ima_impact = Q3.5,
                speed_similar = Q3.6
                ) %>%
  dplyr::mutate(rank_conds_baseline = as.integer(rank_conds_baseline),
                rank_conds_control = as.integer(rank_conds_control),
                rank_conds_spec = as.integer(rank_conds_spec),
                rank_conds_gen = as.integer(rank_conds_gen),
                diff_baseline = as.integer(diff_baseline),
                diff_control = as.integer(diff_control),
                diff_spec = as.integer(diff_spec),
                diff_gen = as.integer(diff_gen),
                diff_general = as.integer(diff_general),
                rank_ima_fastVids = as.integer(rank_ima_fastVids),
                rank_ima_slowVids = as.integer(rank_ima_slowVids),
                rank_control_fs = as.integer(rank_control_fs),
                rank_baseline_fs = as.integer(rank_baseline_fs)
                )

#Now just filter on the participants whose data we are including in the analysis
imaQ4 <- imaQ3[imaQ3$participant %in% wwData$participant,]
#And we now have 20 obs which matches our final sample of participants


##### Analyse answers ####


###### Condition difficulty ratings ####

#5 = Very difficult, 1 = Very easy

#Baseline

imaQ4 %>%
  dplyr::group_by(diff_baseline) %>%
  dplyr::summarise(n())
#Most Ps rated as very easy - none rated more difficult than a 2
imaQ4 %>%
  dplyr::summarise(median(diff_baseline))
#Median = 1

#Control

imaQ4 %>%
  dplyr::group_by(diff_control) %>%
  dplyr::summarise(n())
#Most Ps rated as very difficult, closely followed by 4/5 (difficult)
imaQ4 %>%
  dplyr::summarise(median(diff_control))
#Median = 4

#AO+MIspec

imaQ4 %>%
  dplyr::group_by(diff_spec) %>%
  dplyr::summarise(n())
#Most Ps rated as easy (2) closely followed by somewhat difficult/not sure (3)
imaQ4 %>%
  dplyr::summarise(median(diff_spec))
#Median = 2

#AO+MIgen

imaQ4 %>%
  dplyr::group_by(diff_gen) %>%
  dplyr::summarise(n())
#Most Ps rated as easy (2) closely followed by very easy (1)
imaQ4 %>%
  dplyr::summarise(median(diff_gen))
#Median = 2

## Do AO+MI ratings differ according to order group? ##

#Add on order information to imaQ data

orderDat <- wwData %>%
  dplyr::mutate(firstCond = ifelse(group == "A" | group == "C" | group == "E" | group == "G", "AOMIspec", "AOMIgen"),
                participant = as.character(participant)) %>%
  dplyr::select(c(participant, firstCond)) %>%
  dplyr::distinct()

imaQ4_order <- imaQ4 %>%
  dplyr::mutate(participant = as.character(participant)) %>%
  dplyr::left_join(orderDat) 

#AO+MIspec

imaQ4_order %>%
  dplyr::group_by(diff_spec, firstCond) %>%
  dplyr::summarise(n())

imaQ4_order %>%
  dplyr::group_by(firstCond) %>%
  dplyr::summarise(median(diff_spec))
#AO+MIspec first group found AO+MIspec easier

#AO+MIgen

imaQ4_order %>%
  dplyr::group_by(diff_gen, firstCond) %>%
  dplyr::summarise(n())

imaQ4_order %>%
  dplyr::group_by(firstCond) %>%
  dplyr::summarise(median(diff_gen))
#Same median ratings


###### Condition difficulty comparison/rankings ####

#4 = most difficult

 #Baseline ranking

imaQ4 %>%
  dplyr::group_by(rank_conds_baseline) %>%
  dplyr::summarise(n())
#Most Ps ranked baseline as the first or second easiest of the sections
imaQ4 %>%
  dplyr::summarise(median(rank_conds_baseline))
#Median = 2

#Control ranking

imaQ4 %>%
  dplyr::group_by(rank_conds_control) %>%
  dplyr::summarise(n())
#Most Ps ranked control as the most difficult section
imaQ4 %>%
  dplyr::summarise(median(rank_conds_control))
#Median = 4

#AO+MIspec ranking

imaQ4 %>%
  dplyr::group_by(rank_conds_spec) %>%
  dplyr::summarise(n())
#Most Ps ranked AO+MIspec as 2nd or 3rd most difficult section
imaQ4 %>%
  dplyr::summarise(median(rank_conds_spec))
#Median = 2

#AO+MIgen ranking

imaQ4 %>%
  dplyr::group_by(rank_conds_gen) %>%
  dplyr::summarise(n())
#Most Ps ranked AO+MIgen as 2nd most difficult section
imaQ4 %>%
  dplyr::summarise(median(rank_conds_gen))
#Median = 3

## Look at AO+MI rankings between order groups ##

#AO+MIspec ranking

imaQ4_order %>%
  dplyr::group_by(rank_conds_spec, firstCond) %>%
  dplyr::summarise(n())

imaQ4_order %>%
  dplyr::group_by(firstCond) %>%
  dplyr::summarise(median(rank_conds_spec))

#AO+MIgen ranking

imaQ4_order %>%
  dplyr::group_by(rank_conds_gen, firstCond) %>%
  dplyr::summarise(n())

imaQ4_order %>%
  dplyr::group_by(firstCond) %>%
  dplyr::summarise(median(rank_conds_gen))

###### Difficulty rankings compared to speed levels ####

#1 = most difficult

#Baseline

imaQ4 %>%
  dplyr::group_by(rank_baseline_fs) %>%
  dplyr::summarise(n())
#Most Ps ranked baseline as the first or second easiest of the sections
imaQ4 %>%
  dplyr::summarise(median(rank_baseline_fs, na.rm = TRUE))
#Median = 4

#Control

imaQ4 %>%
  dplyr::group_by(rank_control_fs) %>%
  dplyr::summarise(n())
#Most Ps ranked control as the most difficult section
imaQ4 %>%
  dplyr::summarise(median(rank_control_fs, na.rm = TRUE))
#Median = 1

#Slow videos

imaQ4 %>%
  dplyr::group_by(rank_ima_slowVids) %>%
  dplyr::summarise(n())
#Most Ps ranked the slow videos as the second easiest 
imaQ4 %>%
  dplyr::summarise(median(rank_ima_slowVids, na.rm = TRUE))
#Median = 3

#Fast videos

imaQ4 %>%
  dplyr::group_by(rank_ima_fastVids) %>%
  dplyr::summarise(n())
#Most Ps ranked the slow videos as the second most difficult 
imaQ4 %>%
  dplyr::summarise(median(rank_ima_fastVids, na.rm = TRUE))
#Median = 2


###### Actor Comparisons ####

#Slow typing

imaQ4 %>%
  dplyr::group_by(actor_typing_slow) %>%
  dplyr::summarise(n())
#Most Ps thought the slow typing was worse than their own ability

#Fast typing

imaQ4 %>%
  dplyr::group_by(actor_typing_fast) %>%
  dplyr::summarise(n())
#Most Ps thought the fast typing was better than their own ability

#Actor similarity

imaQ4 %>%
  dplyr::group_by(actor_similar) %>%
  dplyr::summarise(n())
#Most Ps thought the actor's typing was quite different to their own

#does this vary according to order group?

#Take group info from main dataset

groups <- wwData2 %>%
  dplyr::select(c(participant, firstCond)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(participant = as.character(participant))

imaQ5 <- imaQ4 %>%
  dplyr::left_join(groups)

imaQ5 %>%
  dplyr::group_by(firstCond, actor_similar) %>%
  dplyr::summarise(n())


###### Impact of speed differences ####

#Impact on typing

imaQ4 %>%
  dplyr::group_by(speed_impact) %>%
  dplyr::summarise(n())
#Most Ps thought faster videos made their typing faster and slower videos made their 
 #typing slower OR the speed made no difference

#Impact on imagery

imaQ4 %>%
  dplyr::group_by(speed_ima_impact) %>%
  dplyr::summarise(n())
#Most Ps thought it was harder to imagine during the fast videos

#Speed similarity

imaQ4 %>%
  dplyr::group_by(speed_similar) %>%
  dplyr::summarise(n())
#Most Ps thought the faster typing speed more closely resembled their own typing
 #closely followed by none of the speeds resembling their own


###### Imagery ####

imaQ4 %>%
  dplyr::group_by(ima_success) %>%
  dplyr::summarise(n())
#Most Ps said they were able to imagine, and all could at least some of the time

imaQ4 %>%
  dplyr::group_by(ima_quality) %>%
  dplyr::summarise(n())
#Most Ps could imagine fairly well. Only 2 said they imagined quite poorly

imaQ4 %>%
  dplyr::group_by(ima_perspective) %>%
  dplyr::summarise(n())
#Most Ps used 1st person perspective

imaQ4 %>%
  dplyr::group_by(spon_ima) %>%
  dplyr::summarise(n())
#Most Ps spontaneously imagined when not instructed to


###### General ####

#Did any sections improve typing?

imaQ4 %>%
  dplyr::group_by(improved) %>%
  dplyr::summarise(n())
#Most Ps said just seeing the word improved their typing, followed by AO+MIgen and AO+MIspec

#Did any sections worsen typing?

imaQ4 %>%
  dplyr::group_by(made_worse) %>%
  dplyr::summarise(n())
#Most Ps said no sections did, followed closely by AO+MIspec

#Overall experiment difficulty

imaQ4 %>%
  dplyr::group_by(diff_general) %>%
  dplyr::summarise(n())
#Most Ps rated 4/10 for difficulty
imaQ4 %>%
  dplyr::summarise(median(diff_general))
#median = 4

#Which hands did Ps use?

imaQ4 %>%
  dplyr::group_by(hand_used) %>%
  dplyr::summarise(n())
#All Ps said they used both hands simultaneously


#### Computer Use Questionnaire ####

##### Import and tidy ####

# Import imagery questionnaire data
compQ <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/Questionnaires/study3_compQ_20230207.csv")

#Drop unnecessary columns

compQ2 <- compQ[,-c(1:11)]

#Rename columns for clarity and convert ratings to integers

compQ3 <- compQ2 %>%
  dplyr::rename(participant = Q1.1,
                gender = Q1.2,
                ethnicity = Q1.3,
                ethnicity_other = Q1.8,
                cpq12 = SC2,
                phq9 = SC4,
  ) %>%
  dplyr::mutate(cpq12 = as.integer(cpq12)
  )

#Now just filter on the participants whose data we are including in the analysis
compQ4 <- compQ3[compQ3$participant %in% wwData$participant,]
#And we now have 20 obs which matches our final sample of participants

##### Analyse answers ####

#Gender

compQ4 %>%
  dplyr::group_by(gender) %>%
  dplyr::summarise(n())

#Ethnicity

compQ4 %>%
  dplyr::group_by(ethnicity) %>%
  dplyr::summarise(n())


###### CPQ-12 ####

#We first need to code Ps' answers as integers

compQ4[ , 17:28][ compQ4[ , 17:28 ] == "Very easily" ] <- "5"
compQ4[ , 17:28][ compQ4[ , 17:28 ] == "Somewhat easily" ] <- "4" 
compQ4[ , 17:28][ compQ4[ , 17:28 ] == "Not very easily" ] <- "3"
compQ4[ , 17:28][ compQ4[ , 17:28 ] == "Not at all" ] <- "2"
compQ4[ , 17:28][ compQ4[ , 17:28 ] == "Never tried" ] <- "1"

#Mutate so these columns are numbers not characters
compQ5 <- compQ4 %>%
  mutate(Q3.2 = as.integer(Q3.2),
         Q3.3 = as.integer(Q3.3),
         Q3.4 = as.integer(Q3.4),
         Q3.5 = as.integer(Q3.5),
         Q3.6 = as.integer(Q3.6),
         Q3.7 = as.integer(Q3.7),
         Q3.8 = as.integer(Q3.8),
         Q3.9 = as.integer(Q3.9),
         Q3.10 = as.integer(Q3.10),
         Q3.11 = as.integer(Q3.11),
         Q3.12 = as.integer(Q3.12),
         Q3.13 = as.integer(Q3.13))

#We now need to work out the average score for each subscale and then sum
#the averages

CPQ12dat <- compQ5 %>%
  group_by(participant) %>%
  summarise(compBasics = mean(Q3.2,Q3.3))

CPQ12dat1 <- compQ5 %>%
  group_by(participant) %>%
  summarise(Printer = mean(Q3.4,Q3.5))

CPQ12dat2 <- compQ5 %>%
  group_by(participant) %>%
  summarise(comms = mean(Q3.6,Q3.7))

CPQ12dat3 <- compQ5 %>%
  group_by(participant) %>%
  summarise(internet = mean(Q3.8,Q3.9))

CPQ12dat4 <- compQ5 %>%
  group_by(participant) %>%
  summarise(calendar = mean(Q3.10,Q3.11))

CPQ12dat5 <- compQ5 %>%
  group_by(participant) %>%
  summarise(entertain = mean(Q3.12,Q3.13))

#Combine subscale averages

cpq12dat_tot <- cbind(CPQ12dat, printer = CPQ12dat1$Printer,
                      comms = CPQ12dat2$comms,
                      internet = CPQ12dat3$internet,
                      calendar = CPQ12dat4$calendar,
                      entertain = CPQ12dat5$entertain)

#Work out the total

cpq12dat_tot <- cpq12dat_tot %>%
  rowwise() %>%
  summarise(CPQ12score = sum(compBasics + printer + comms + internet + calendar + entertain))

cpq12dat_tot %>%
  summarise(mean = mean(CPQ12score), sd = sd(CPQ12score))


#### EHI and KVIQ-10 ####

#Handedness and motor imagery ability

#Import data
kviqdat <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/rawData/KVIQ/lab_session_spreadsheet.csv")

#Filter data so we only have our included final sample of Ps. Use wwData as reference

kviqdat_tidy <- kviqdat[kviqdat$...1 %in% wwData$participant,]

##### Handedness ####

ehi_dat <- kviqdat_tidy %>%
  dplyr::select(...1, ...10) %>%
  dplyr::distinct() #one row for each participant. Should be 20 obs

#Count how many of each handedness

ehi_dat %>%
  dplyr::group_by(...10) %>%
  dplyr::summarise(n())



##### KVIQ-10 ####

kviq_dat <- kviqdat_tidy %>%
  dplyr::select(...1, ...17, ...20) %>%
  dplyr::filter(!is.na(...17) & !is.na(...20)) %>% #remove rows with NAs
  dplyr::mutate(VIscore = as.integer(...17),
                KIscore = as.integer(...20)) 
#Should be 20 obs

#Get summaries
kviq_dat %>%
  dplyr::summarise(VImean = mean(VIscore),
                   KImean = mean(KIscore),
                   VIsd = sd(VIscore),
                   KIsd = sd(KIscore))

#See whether participants in different order groups had imagery differences

#Extract order and participant info

orderDat <- wwData %>%
  dplyr::mutate(firstCond = ifelse(group == "A" | group == "C" | group == "E" | group == "G", "AOMIspec", "AOMIgen"),
                participant = as.character(participant)) %>%
  dplyr::select(c(participant, firstCond)) %>%
  dplyr::distinct()

kviq_dat_order <- kviq_dat %>%
  dplyr::mutate(participant = as.character(`...1`)) %>%
  dplyr::left_join(orderDat) %>%
  dplyr::select(-c(`...1`, `...17`, `...20`))

#Get summaries
kviq_dat_order %>%
  dplyr::group_by(firstCond) %>%
  dplyr::summarise(VImean = mean(VIscore),
                   KImean = mean(KIscore),
                   VIsd = sd(VIscore),
                   KIsd = sd(KIscore))

#### Eligibility Questionnaire ####

elQ <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/Questionnaires/study3_elQ_20230315.csv")

#I have manually marked which participants are in the final sample, so filter on 
 #this column

elQ2 <- elQ %>%
  dplyr::filter(`Include?` == "Y") %>%
  dplyr::mutate(Q3.4 = as.integer(Q3.4)) #Change age column to a number
#Should be 20 obs

#Get age

elQ2 %>%
  dplyr::summarise(mean_age = mean(Q3.4),
                   sd_age = sd(Q3.4),
                   max_age = max(Q3.4),
                   min_age = min(Q3.4))
