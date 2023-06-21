#Before installing: options("install.lock"=FALSE)

library(tidyverse)
library(data.table) #needed for shift
library(stats) #needed for reshape
library(sjmisc) #needed for str_contains

#### Script for data-wrangling PsychoPy data from Experiment 2 student typing study ####

#import a participant's raw data file (PsychoPy .csv output):
rawData <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/rawData/12_student_TT_local_2023_Feb_22_1643.csv")

#get a list of column names in the dataset
colnames(rawData)

#make separate new datasets for each condition with the columns we want

baseData <- dplyr::select(rawData, participant, group, baseline_words, baseBkgdKeys.keys,
                          base_typedWord, baseKeys.keys,baseKeys.rt)

contData <- dplyr::select(rawData, participant, group, control_words, cont_time, contBkgdKeys.keys, 
                          cont_typedWord, contKeys.keys, contKeys.rt)

AOMIfirstData <- dplyr::select(rawData, participant, group, AOMIfirst_words, cont_time, AOMI_promptBkgdKeys.keys,
                               AOMI1bkdgKeys.keys, AOMI1_typedWord, AOMI1keys.keys, AOMI1keys.rt)

AOMIsecData <- dplyr::select(rawData, participant, group, AOMIsecond_words, cont_time, bkgdKeys4.keys,
                             AOMI_promptBkgdKeys.keys,AOMI2_typedWord, keyboardCom4.keys, 
                             keyboardCom4.rt)

#Get rid of empty first rows in each above df. The below removes all rows containing only NAs before
#a value appears in the specified column
baseData2 <- baseData %>%
  do(.[first(which(!is.na(.$baseBkgdKeys.keys))):nrow(.),])

#Then remove empty rows/repetitions at bottom of dataset
baseData2 <- baseData2[-c(31:nrow(baseData2)),]

#Then add a column to show what condition this is
#Add a column to show what condition this is
baseCond <- rep("Baseline", times = nrow(baseData2))
baseData2 <- cbind(baseData2, baseCondition = baseCond)

#Do the same for other dfs
contData2 <- contData %>%
  do(.[first(which(!is.na(.$contBkgdKeys.keys))):nrow(.),])
contData2 <- contData2[-c(31:nrow(contData2)),]
contCond <- rep("Control", times = nrow(contData2))
contData2 <- cbind(contData2, contCondition = contCond)
AOMIfirstData2 <- AOMIfirstData %>%
  do(.[first(which(!is.na(.$AOMI1bkdgKeys.keys))):nrow(.),])
AOMIfirstData2 <- AOMIfirstData2[-c(31:nrow(AOMIfirstData2)),]
AOMIfirstCond <- rep("AOMI_1", times = nrow(AOMIfirstData2))
AOMIfirstData2 <- cbind(AOMIfirstData2, AOMI1Condition = AOMIfirstCond)
AOMIsecData2 <- AOMIsecData %>%
  do(.[first(which(!is.na(.$bkgdKeys4.keys))):nrow(.),])
AOMIsecData2 <- AOMIsecData2[-c(31:nrow(AOMIsecData2)),]
AOMIsecCond <- rep("AOMI_2", times = nrow(AOMIsecData2))
AOMIsecData2 <- cbind(AOMIsecData2, AOMI2Condition = AOMIsecCond)

#Combine these datasets

tidyData <- cbind(baseData2,
                  contWords = contData2$control_words, 
                  cont_speed = contData2$cont_time,
                  contBkgd = contData2$contBkgdKeys.keys, 
                  contTyped = contData2$cont_typedWord, 
                  contKeys = contData2$contKeys.keys,
                  contRTs = contData2$contKeys.rt, 
                  AOMI1Words = AOMIfirstData2$AOMIfirst_words,
                  AOMI1speed = AOMIfirstData2$cont_time,
                  AOMI1Bkgd = AOMIfirstData2$AOMI1bkdgKeys.keys,
                  AOMI1PromptBkgd = AOMIfirstData2$AOMI_promptBkgdKeys.keys,
                  AOMI1Typed = AOMIfirstData2$AOMI1_typedWord,
                  AOMI1Keys = AOMIfirstData2$AOMI1keys.keys,
                  AOMI1RTs = AOMIfirstData2$AOMI1keys.rt,
                  AOMI2Words = AOMIsecData2$AOMIsecond_words,
                  AOMI2speed = AOMIsecData2$cont_time,
                  AOMI2Bkgd = AOMIsecData2$bkgdKeys4.keys,
                  AOMI2PromptBkgd = AOMIsecData2$AOMI_promptBkgdKeys.keys,
                  AOMI2Typed = AOMIsecData2$AOMI2_typedWord,
                  AOMI2Keys = AOMIsecData2$keyboardCom4.keys,
                  AOMI2RTs = AOMIsecData2$keyboardCom4.rt,
                  contCondition = contData2$contCondition,
                  A1cond = AOMIfirstData2$AOMI1Condition,
                  A2cond = AOMIsecData2$AOMI2Condition)

#Create a column of NAs for baseline speed so we can put all this into long format

base_speed_vec <- rep(NA, times = nrow(baseData2))

tidyData2 <- cbind(tidyData, base_speed = base_speed_vec)

#If participants are in group F this will come up as 'FALSE' so replace this

tidyData2$group[tidyData2$group=="FALSE"]<-"F" 

#Turn columns from wide to long format
longData <- reshape(tidyData2, direction="long", 
                    varying=list(c("baseCondition", "contCondition", "A1cond", "A2cond"),
                                 c("baseline_words","contWords", "AOMI1Words", "AOMI2Words"),
                                 c("base_speed", "cont_speed", "AOMI1speed", "AOMI2speed"),
                                 c("baseBkgdKeys.keys","contBkgd", "AOMI1Bkgd", "AOMI2Bkgd"),
                                 c("baseBkgdKeys.keys","contBkgd", "AOMI1PromptBkgd", "AOMI2PromptBkgd"),
                                 c("base_typedWord","contTyped", "AOMI1Typed", "AOMI2Typed"),
                                 c("baseKeys.keys", "contKeys", "AOMI1Keys", "AOMI2Keys"),
                                 c("baseKeys.rt", "contRTs", "AOMI1RTs", "AOMI2RTs")), 
                    v.names=c("Condition", "Word","stim_speed","BkgdKeys","promptBkgdKeys","typedWord","Keys","RTs")) 

#Remove duplicates/blank trials
#Do this based on NA value in the Keys column - participants always have to press
#something to move onto the next trial even if it's just 'return', so if this row
#is empty, it indicates it's not a true trial and can be removed
longData2 <- longData[!is.na(longData$Keys), ]

#Check number of rows (should be 120)
nrow(longData2)

#Get the first RT and put into a different column (so we can extract just the first
#RT from each word for separate analysis)
fPressCols <- separate(longData2, RTs, into = c("firstPress"),
                       sep = ",", remove = TRUE)

#Remove brackets and spaces from RTs in this new column
fPressCols$firstPress <- gsub("[","", fPressCols$firstPress, fixed=TRUE)
fPressCols$firstPress <- gsub("]","", fPressCols$firstPress, fixed=TRUE)
fPressCols$firstPress <- gsub("[:blank:]","", fPressCols$firstPress, fixed=TRUE)

#Add firstPress column to main df
longData3 <- cbind(longData2, firstPress = fPressCols$firstPress)

#Separate RTs column so on separate rows
longData4 <- longData3 %>% 
  separate_rows(Keys, RTs, sep = ",")

#Remove punctuation and spaces from values in RTs column
longData4$RTs <- gsub("[","", longData4$RTs, fixed=TRUE)
longData4$RTs <- gsub("]","", longData4$RTs, fixed=TRUE)
longData4$RTs <- gsub("[:blank:]","", longData4$RTs, fixed=TRUE)

#Remove punctuation from Keys column as above
longData4$Keys <- gsub("[[:punct:][:blank:]]","", longData4$Keys)

#Filter to just get the rows when the participant pressed 'enter/return' to end 
#a trial. The RT for this key press indicates the time to type the whole word
wholeWord <- longData4 %>%
  filter(longData4$Keys == "return") 

#Check for duplicates and missing 'return' key presses

#If due to a duplicate:

# First check for duplicates:
 duplicated(wholeWord$Word)
#' #Show duplicate rows
 wholeWord[duplicated(wholeWord$Word),]
#' wholeWord %>%
#'   filter(Word == "walrus")
#' # Remove duplicate and save dataset:
#' wholeWord <- wholeWord[!duplicated(wholeWord$Word),]
#' #This will remove the second duplicate which is (likely) what we want - check
#' #Remove the duplicate from longData3 as well
#' longData3 <- longData3[!duplicated(longData3$Word),]
#' 
#' #If due to a missing 'return' press:
#' 
#' #Find non-matching trials in both datasets if this is not due to an additional trial
 compare_df <- longData3 %>%
   dplyr::select(Word)
 compare_df1 <- wholeWord %>%
   dplyr::select(Word)
 mismatch_df <- anti_join(compare_df, compare_df1)
 mismatch_df
#' #Inspect the missing trial/s in the main dataset
# view_mismatch <- longData4 %>%
#    dplyr::filter(Word %in% mismatch_df$Word)
# view_mismatch
#in the case of P4, no 'return' press for
#'fusion' trial - probably just pressed too lightly so not registered
#' 
#' #Add an additional row to the shorter dataset so we can combine them - amend 
#'  #manually each time
# wholeWord <- wholeWord %>%
#   dplyr::add_row(participant = rep(wholeWord$participant[1], times = nrow(mismatch_df)),
#                  group = rep(wholeWord$group[1], times = nrow(mismatch_df)),
#                  time = c(4),
#                  Condition = c("AOMI_2"),
#                  Word = c("virile"),
#                  stim_speed = c(6.5),
#                  BkgdKeys = rep(NA, times = nrow(mismatch_df)),
#                  promptBkgdKeys = rep(NA, times = nrow(mismatch_df)),
#                  typedWord = rep(NA, times = nrow(mismatch_df)),
#                  Keys = rep(NA, times = nrow(mismatch_df)),
#                  RTs = rep(NA, times = nrow(mismatch_df)),
#                  id = c(1),
#                  firstPress = rep(NA, times = nrow(mismatch_df)))

#Rename RT column in wholeWord dataset
wholeWord2 <- wholeWord %>%
  dplyr::rename(wholeWordTime = RTs)

#Combine the whole word column to the older dataset which has the same number of rows
longData5 <- longData3 %>%
  dplyr::left_join(wholeWord2, by = c("participant", "group", "time", "Condition", "Word", "stim_speed", "typedWord", "firstPress", "id")) %>%
  dplyr::select(-c(Keys.y, BkgdKeys.y, promptBkgdKeys.y)) %>%
  dplyr::rename(Keys = Keys.x)
  
#Do the above again, i.e. separate the dataset so RTs and keys on their own rows
longData6 <- longData5 %>% 
  separate_rows(Keys, RTs, sep = ",")

#Remove punctuation again
longData6$RTs <- gsub("[","", longData6$RTs, fixed=TRUE)
longData6$RTs <- gsub("]","", longData6$RTs, fixed=TRUE)
longData6$RTs <- gsub("[:blank:]","", longData6$RTs, fixed=TRUE)
longData6$Keys <- gsub("[[:punct:][:blank:]]","", longData6$Keys)

#change RTs column to numeric instead of character
longData6$RTs <- as.numeric(longData6$RTs)

#find inter-key interval times (IKIs) of key presses from RTs and add in new column
#i.e. this is the difference between RTs for each word, where the first key
#press = 0
completeData <- longData6 %>%
  group_by(Word) %>%
  mutate(IKIs = RTs - lag(RTs, default = RTs[1]))

#Turn any negative numbers in IKIs column into 0 (this indicates the first key press
#of a word)
completeData[,15][completeData[, 15] < 0] <- 0

#Compare column 'typedWord' with 'Word' and if they match, code as '1' in new 
#column to show the word was typed correctly. If they don't match, code as '0'
completeData$typedCorrect <- ifelse(completeData$typedWord == completeData$Word, 1, 0)

# REMOVE TRIALS THAT YOU HAVE MENTIONED IN BENCHLING ETC #
# This is manually changed for each participant

#completeData <- completeData[!(completeData$Word == "fodder"),]# & !(completeData$Word == "hippie") & !(completeData$Word == "botany") & !(completeData$Word == "forage"),]

#Use the below to check the trials remaining don't include the ones you just removed ^^
checkTrials <- completeData%>%
  filter(Word == "fodder")


#### Add in condition ratings to dataset ####

rateData <-  dplyr::select(rawData, participant, group, baseRateTypeKey.keys, 
                           baseRateLoop.thisTrialN, AOMI1rateTypeKey.keys, 
                           AOMI1rateLoop.thisTrialN, contTypeKey.keys, 
                           contRateLoop.thisTrialN, AOMI2typeKey.keys,
                           AOMI2rateLoop.thisTrialN) 

#Get rid of NAs
rateData2 <- rateData %>% 
  filter_at(vars(baseRateTypeKey.keys, 
                 baseRateLoop.thisTrialN, AOMI1rateTypeKey.keys, 
                 AOMI1rateLoop.thisTrialN, contTypeKey.keys, 
                 contRateLoop.thisTrialN, AOMI2typeKey.keys,
                 AOMI2rateLoop.thisTrialN), any_vars(!is.na(.)))

#Split the columns which need shifting up into different dataframes so we can
#shift up the columns and then reattach into the same df

contCols <- as.data.frame(rateData2$contTypeKey.keys)
contCols <- cbind(contCols, contRateLoop.thisTrialN = rateData2$contRateLoop.thisTrialN)
#Get rid of all NAs above the data to shift it up
contCols <- contCols %>%
  do(.[first(which(!is.na(.$contRateLoop.thisTrialN))):nrow(.),])
#Now get rid of all bottom rows (containing NAs) except for one. This is because
#we need 3 rows so this can be combined with the AOMI and MI dfs 
contCols <- contCols[-c(4:nrow(contCols)),]

AOMI1Cols <- as.data.frame(rateData2$AOMI1rateTypeKey.keys)
AOMI1Cols <- cbind(AOMI1Cols, AOMI1rateLoop.thisTrialN = rateData2$AOMI1rateLoop.thisTrialN)
#Get rid of all NAs to shift the data up
AOMI1Cols <- AOMI1Cols %>%
  filter_at(vars(AOMI1rateLoop.thisTrialN,
                 `rateData2$AOMI1rateTypeKey.keys`), any_vars(!is.na(.)))

AOMI2Cols <- as.data.frame(rateData2$AOMI2typeKey.keys)
AOMI2Cols <- cbind(AOMI2Cols, AOMI2rateLoop.thisTrialN = rateData2$AOMI2rateLoop.thisTrialN)
#Get rid of all NAs to shift the data up
AOMI2Cols <- AOMI2Cols %>%
  filter_at(vars(AOMI2rateLoop.thisTrialN,
                 `rateData2$AOMI2typeKey.keys`), any_vars(!is.na(.)))

#Remove extra rows from the original df
rateData3 <- rateData2[-c(4:nrow(rateData2)),]

#Combine these into one df

rateData4 <- cbind(rateData3, 
                   contRateTypeKey.keys = contCols$`rateData2$contTypeKey.keys`,
                   contRateLoop.thisTrialN = contCols$contRateLoop.thisTrialN,
                   AOMI1rateTypeKey.keys = AOMI1Cols$`rateData2$AOMI1rateTypeKey.keys`,
                   AOMI1rateLoop.thisTrialN = AOMI1Cols$AOMI1rateLoop.thisTrialN,
                   AOMI2rateTypeKey.keys = AOMI2Cols$`rateData2$AOMI2typeKey.keys`,
                   AOMI2rateLoop.thisTrialN = AOMI2Cols$AOMI2rateLoop.thisTrialN)

#Remove old columns
rateData4 <- rateData4[,-c(5:10)]

#Get rid of punctuation from ratings

rateData4$baseRateTypeKey.keys <- gsub("[[:punct:][:blank:]]","", rateData4$baseRateTypeKey.keys)
rateData4$AOMI1rateTypeKey.keys <- gsub("[[:punct:][:blank:]]","", rateData4$AOMI1rateTypeKey.keys)
rateData4$contRateTypeKey.keys <- gsub("[[:punct:][:blank:]]","", rateData4$contRateTypeKey.keys)
rateData4$AOMI2rateTypeKey.keys <- gsub("[[:punct:][:blank:]]","", rateData4$AOMI2rateTypeKey.keys)

#Rearrange the data

rateData5 <- reshape(rateData4, direction="long", 
                     varying=list(c("baseRateTypeKey.keys", 
                                    "contRateTypeKey.keys",
                                    "AOMI1rateTypeKey.keys",
                                    "AOMI2rateTypeKey.keys"),
                                  c("baseRateLoop.thisTrialN",
                                    "contRateLoop.thisTrialN",
                                    "AOMI1rateLoop.thisTrialN",
                                    "AOMI2rateLoop.thisTrialN")),
                     v.names=c("Rating", "LoopNo."))

#Rename conditions as a new column/df
conditionCol <- as.data.frame(recode(rateData5$time, "1" = "Baseline", "2" = "Control", "3" = "AOMI_1",
                                     "4" = "AOMI_2"))
#Rename column
colnames(conditionCol)[1] <- "Condition"

#Add onto previous df
rateData6 <- cbind(rateData5, Condition = conditionCol$Condition)

#Get rid of 'time' column
rateData6 <- rateData6 %>%
  dplyr::select(-time)

#Now we need to rename the loop no. 1 to either be the imageryPres rating or the 
#visual imagery Q depending on condition

#Change the LoopNo. column from numeric to character for the below to work
rateData7 <- rateData6 %>%
  mutate(LoopNo. = as.character(LoopNo.)) %>%
  mutate(LoopNo. = case_when(
    LoopNo. == "1" & Condition %in% c("Baseline","Control") ~ "imageryQ",
    LoopNo. == "1" & Condition %in% c("AOMI_1","AOMI_2") ~ "VI",
    TRUE ~ LoopNo.))

#Now rename other numbers in LoopNo. column
rateData7$LoopNo.[rateData7$LoopNo.=="0"]<-"ratePerf" 
rateData7$LoopNo.[rateData7$LoopNo.=="2"]<-"KI"

#Now put this column into wide format so we have the different rating types as
#diff columns

rateData8 <- pivot_wider(rateData7, names_from = LoopNo., values_from = Rating) %>%
  dplyr::select(-`NA`) #remove 'NA' column

#Shift data so it lines up better
imaQcol <- as.data.frame(shift(rateData8$imageryQ, n=1L, fill=NA, type="lead", 
                               give.names=FALSE))
VIcol <- as.data.frame(shift(rateData8$VI, n=1L, fill=NA, type="lead", 
                             give.names=FALSE))
KIcol <- as.data.frame(shift(rateData8$KI, n=2L, fill=NA, type="lead", 
                             give.names=FALSE))
#Rename these columns
colnames(imaQcol)[1] <- "imageryQ"
colnames(VIcol)[1] <- "VI"
colnames(KIcol)[1] <- "KI"

#Combine into dataset
rateData9 <- cbind(rateData8, imageryQ = imaQcol$imageryQ, VI = VIcol$VI, KI = KIcol$KI)

#Get rid of old columns
rateData9 <- rateData9[,-c(6:8)]

#Get rid of duplicate rows
rateData9 <- rateData9[!duplicated(rateData9$Condition),]
#This will remove the second and third duplicates which is (likely) what we want - check

#If group = F, this will show as 'FALSE' - correct this

rateData9$group[rateData9$group=="FALSE"]<-"F" 

#Merge this dataset with the main one
completeData2 <- full_join(completeData, rateData9, by = c("participant", "group", "Condition"))

#Change AO+MI condition names depending on assigned group
if (completeData2$group == "A" || completeData2$group == "C" || completeData2$group == "E" || completeData2$group == "G") {
  completeData2$Condition[completeData2$Condition=="AOMI_1"]<-"AOMIspec" 
  completeData2$Condition[completeData2$Condition=="AOMI_2"]<-"AOMIgen" 
} else {
  completeData2$Condition[completeData2$Condition=="AOMI_1"]<-"AOMIgen" 
  completeData2$Condition[completeData2$Condition=="AOMI_2"]<-"AOMIspec" 
}


#### Add PHQ-9 scores and typing style to dataset from computer questionnaire responses ####

#Import comp questionnaire spreadsheet (Qualtrics output)
compQ <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/Questionnaires/study3_compQ_20230207.csv")

#Select just the participant and PHQ-9 total column
filterCompQ <- compQ %>%
  dplyr::select(Q1.1, SC4)

#Change participant ID to a string so we can use the below comparison code
filterCompQ <- filterCompQ %>%
  mutate(pID = as.character(Q1.1))

#Remove any punctuation from the PID column
filterCompQ$pID <- gsub("[[:punct:][:blank:]]","", filterCompQ$pID)

#Filter so we just get this participant's scores by comparing pIDs in both datasets
PcompQ <- filterCompQ[filterCompQ$pID %in% completeData2$participant,]

#Repeat the PHQ-9 column the amount of times as rows there are in completeData2 so
#we can combine them
repPcompQ <- as.data.frame(rep(PcompQ$SC4[1], times = nrow(completeData2)))

#Rename so comprehensible
colnames(repPcompQ)[1] <- "PHQ9"

#Add onto whole dataset
completeData3 <- cbind(completeData2, PHQ9 = repPcompQ$PHQ9)

#### Export to .csv ####

#Extract the participant ID from df to use for naming the csv file
participantNo. <- toString(completeData3[1,1])

#Write the wrangled data to a new csv file to manually code accuracy and correct typing
write.csv(completeData3, 
          sprintf("\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\study3\\WrangledData\\P%s_wrangled.csv", participantNo.),
          row.names = FALSE)
