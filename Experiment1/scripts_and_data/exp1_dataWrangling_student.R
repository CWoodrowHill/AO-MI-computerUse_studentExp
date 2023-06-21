#Before installing: options("install.lock"=FALSE)

library(tidyverse)
library(data.table) #needed for shift
library(stats) #needed for reshape
library(plyr) #needed for count
library(binaryLogic) #needed for as.binary
library(sjmisc) #needed for str_contains

#### Script for data-wrangling Pavlovia output from Student Computer Experiment ####

#import a participant's raw data file (Pavlovia .csv output):
rawData <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/studentRawData/70_PavloviaData.csv")

#get dataset summary
summary(rawData)
#get a list of column names in the dataset
colnames(rawData)

#make a new dataset based on just the columns we want
keyData <- dplyr::select(rawData, participant, Order, baseline_words, baseBkgdKeys.keys,
                  base_typedWord, baseKeys.keys,baseKeys.rt, control_words, 
                  contBkgdKeys.keys, cont_typedWord, contKeys.keys, contKeys.rt,
                  AOMIfirst_words, AOMI1_BkgdKeys.keys, AOMI_promptBkgdKeys.keys,
                  AOMI1_typedWord, AOMI1keys.keys, AOMI1keys.rt, AOMIsecond_words,
                  bkgdKeys4.keys, AOMI2_typedWord, keyboardCom4.keys, keyboardCom4.rt)
head(keyData)

#Get rid of empty first rows. The below removes all rows containing only NAs before
 #a value appears in the column 'baseline_words'
keyDataTidy <- keyData %>%
  do(.[first(which(!is.na(.$baseline_words))):nrow(.),])

#Shift values in control condition so they align with words. This creates a new df for each
 #AMOUNT TO SHIFT DIFFERS PER PARTICIPANT
contWordsColumn <- as.data.frame(shift(keyDataTidy$control_words, n=66L, fill=NA, type="lead", 
                      give.names=FALSE))
contBkdCol <- as.data.frame(shift(keyDataTidy$contBkgdKeys.keys, n=66L, fill=NA, type="lead", 
                                  give.names=FALSE))
contTypedcol <- as.data.frame(shift(keyDataTidy$cont_typedWord, n=66L, fill=NA, type="lead", 
                                  give.names=FALSE))
contKeyscol <- as.data.frame(shift(keyDataTidy$contKeys.keys, n=66L, fill=NA, type="lead", 
                                    give.names=FALSE))
contRTcol <- as.data.frame(shift(keyDataTidy$contKeys.rt, n=66L, fill=NA, type="lead", 
                                   give.names=FALSE))

#Shift values for AOMIfirst
AOMI1wordscol <- as.data.frame(shift(keyDataTidy$AOMIfirst_words, n=33L, fill=NA, type="lead", 
                                     give.names=FALSE))
AOMI1Bkgdcol <- as.data.frame(shift(keyDataTidy$AOMI1_BkgdKeys.keys, n=33L, fill=NA, type="lead", 
                                     give.names=FALSE))
AOMIpromptcol <- as.data.frame(shift(keyDataTidy$AOMI_promptBkgdKeys.keys, n=33L, fill=NA, type="lead", 
                                    give.names=FALSE))
AOMI1typedcol <- as.data.frame(shift(keyDataTidy$AOMI1_typedWord, n=33L, fill=NA, type="lead", 
                                     give.names=FALSE))
AOMI1keyscol <- as.data.frame(shift(keyDataTidy$AOMI1keys.keys, n=33L, fill=NA, type="lead", 
                                    give.names=FALSE))
AOMI1RTcol <- as.data.frame(shift(keyDataTidy$AOMI1keys.rt, n=33L, fill=NA, type="lead", 
                                  give.names=FALSE))
#As the prompt column includes keys pressed during both AOMI conditions we need to separate these.
#The below drops all columns from 31 onwards so we just get rows relevant to the 1st AOMI
 #condition
AOMIpromptcol <- AOMIpromptcol[-c(31:nrow(AOMIpromptcol)), ,drop = FALSE]

#Shift values for AOMIsecond
AOMI2wordscol <- as.data.frame(shift(keyDataTidy$AOMIsecond_words, n=99L, fill=NA, type="lead", 
                                  give.names=FALSE))
AOMI2BKgdcol <- as.data.frame(shift(keyDataTidy$bkgdKeys4.keys, n=99L, fill=NA, type="lead", 
                                     give.names=FALSE))
AOMI2promptcol <- as.data.frame(shift(keyDataTidy$AOMI_promptBkgdKeys.keys, n=99L, fill=NA, type="lead", 
                                    give.names=FALSE))
AOMI2typedcol <- as.data.frame(shift(keyDataTidy$AOMI2_typedWord, n=99L, fill=NA, type="lead", 
                                      give.names=FALSE))
AOMI2keyscol <- as.data.frame(shift(keyDataTidy$keyboardCom4.keys, n=99L, fill=NA, type="lead", 
                                     give.names=FALSE))
AOMI2RTcol <- as.data.frame(shift(keyDataTidy$keyboardCom4.rt, n=99L, fill=NA, type="lead", 
                                    give.names=FALSE))

#As the prompt column includes keys pressed during both AOMI conditions we need to separate these.
#The below drops all columns from 31 onwards so we just get rows relevant to the 1st AOMI
#condition
AOMI2promptcol <- AOMI2promptcol[-c(31:nrow(AOMI2promptcol)), ,drop = FALSE]


#Rename columns in dfs with sensible title
colnames(contWordsColumn) <- "control_words" #this code changes all column names in the df
colnames(contBkdCol) <- "control_bkgdKeys"
colnames(contTypedcol) <- "control_typedWord"
colnames(contKeyscol) <- "control_keys"
colnames(contRTcol) <- "control_RTs"
colnames(AOMI1wordscol) <- "AOMI1_words"
colnames(AOMI1Bkgdcol) <- "AOMI1_bkgdKeys"
colnames(AOMIpromptcol) <- "AOMI1_promptBkgdKeys"
colnames(AOMI1typedcol) <- "AOMI1_typedWord"
colnames(AOMI1keyscol) <- "AOMI1_keys"
colnames(AOMI1RTcol) <- "AOMI1_RTs"
colnames(AOMI2wordscol) <- "AOMI2_words"
colnames(AOMI2BKgdcol) <- "AOMI2_bkgdKeys"
colnames(AOMI2promptcol) <- "AOMI2_promptBkgdKeys"
colnames(AOMI2typedcol) <- "AOMI2_typedWord"
colnames(AOMI2keyscol) <- "AOMI2_keys"
colnames(AOMI2RTcol) <- "AOMI2_RTs"

#Make a new df just with the bits we want from the original df (keyDataTidy)
new_df <-data.frame(ParticipantID=keyDataTidy$participant,
                    Order=keyDataTidy$Order, 
                    base_words=keyDataTidy$baseline_words, 
                    baseBkgdKeys = keyDataTidy$baseBkgdKeys.keys,
                    base_typedWord = keyDataTidy$base_typedWord,
                    baseKeys = keyDataTidy$baseKeys.keys,
                    baseRTs = keyDataTidy$baseKeys.rt)


#Add all the columns from the new individual dfs to join new_df and rename
#We'll leave out the AOMI prompt columns for now as they have a different number
 #of rows
keyDataTidy2 <- cbind(new_df, control_words = contWordsColumn$control_words,
                      controlBkgdKeys = contBkdCol$control_bkgdKeys,
                      cont_typedWord = contTypedcol$control_typedWord,
                      contKeys = contKeyscol$control_keys,
                      contRTs = contRTcol$control_RTs,
                      AOMI1_words = AOMI1wordscol$AOMI1_words,
                      AOMI1BkgdKeys = AOMI1Bkgdcol$AOMI1_bkgdKeys,
                      #AOMI1promptBkgdKeys = AOMIpromptcol$AOMI1_promptBkgdKeys,
                      AOMI1_typedWord = AOMI1typedcol$AOMI1_typedWord,
                      AOMI1keys = AOMI1keyscol$AOMI1_keys,
                      AOMI1RTs = AOMI1RTcol$AOMI1_RTs,
                      AOMI2_words = AOMI2wordscol$AOMI2_words,
                      AOMI2bkgdKeys = AOMI2BKgdcol$AOMI2_bkgdKeys,
                      #AOMI2promptBkgdKeys = AOMI2promptcol$AOMI2_promptBkgdKeys,
                      AOMI2_typedWord = AOMI2typedcol$AOMI2_typedWord,
                      AOMI2keys = AOMI2keyscol$AOMI2_keys,
                      AOMI2RTs = AOMI2RTcol$AOMI2_RTs)

#Sort the Order column so that same values are grouped together with "1=" first
OrderSort <- as.data.frame(sort(keyDataTidy2$Order))

#replicate the first row ("1=" entry) the number of times as rows there are in main df
OrderSort2 <- as.data.frame(rep(OrderSort$`sort(keyDataTidy2$Order)`[c(1, 0)], 
                                times = nrow(keyDataTidy2)))

#Rename this column something comprehensible
colnames(OrderSort2)[1] <- "Order_New"
                       
#Attach OrderSort3 to main df
keyDataTidy3 <- cbind(keyDataTidy2, Order = OrderSort2$Order_New)

#Get rid of old Order column
keyDataTidy3 <- keyDataTidy3[-(2)]

#Move Order column up 
keyDataTidy3 <- keyDataTidy3[ , c(1, 22, (2:21))]  

#get rid of the rows from 31 onwards which are just repetitions
keyDataTidy3 <- keyDataTidy3[-c(31:nrow(keyDataTidy3)),]

#Now add on the AOMI prompt columns now they have the same no. rows as main df
keyDataTidy3.5 <- cbind(keyDataTidy3, AOMI1promptKeys = AOMIpromptcol$AOMI1_promptBkgdKeys,
                        AOMI2promptKeys = AOMI2promptcol$AOMI2_promptBkgdKeys)

#Move prompt columns into a nicer position
keyDataTidy3.5 <- keyDataTidy3.5[ , c((1:13), 23, (14:18), 24, (19:22))]

#create a new df just with the words in each condition
conds <- dplyr::select(keyDataTidy3.5, base_words, control_words, AOMI1_words, AOMI2_words)

#Turn conds columns from wide to long format
conds2 <- conds %>%
  pivot_longer(cols = c(base_words, control_words, AOMI1_words, AOMI2_words), 
               names_to = c("Condition","extra"),
               names_sep = "_",
               values_to = "Word")
#values_drop_na = TRUE)

#Turn columns from wide to long format
keyDataTidyLong <- reshape(keyDataTidy3.5, direction="long", 
                           varying=list(c("base_words","control_words", "AOMI1_words", "AOMI2_words"),
                                        c("baseBkgdKeys","controlBkgdKeys", "AOMI1BkgdKeys", "AOMI2bkgdKeys"), 
                                        c("base_typedWord","cont_typedWord", "AOMI1_typedWord", "AOMI2_typedWord"),
                                        c("baseKeys", "contKeys", "AOMI1keys", "AOMI2keys"),
                                        c("baseRTs", "contRTs", "AOMI1RTs", "AOMI2RTs"),
                                        c("baseBkgdKeys","controlBkgdKeys","AOMI1promptKeys", "AOMI2promptKeys")), 
                           v.names=c("Word","BkgdKeys","typedWord","Keys", "RTs", "promptBkgdKeys")) 
#timevar="id1", idvar="id2")

#Merge conds2 df with keyDataTidyLong according to the common column, 'Word'
keyDataTidyLong2 <- merge(conds2, keyDataTidyLong, by = "Word")  

#Arrange by condition
keyDataTidyLong2 <- keyDataTidyLong2 %>%
  arrange(Condition)

#Remove unneeded 'extra' column
keyDataTidyLong2 <- keyDataTidyLong2[,-c(3)]

#Remove duplicates/blank trials
#Do this based on NA value in the Keys column - participants always have to press
 #something to move onto the next trial even if it's just 'return', so if this row
 #is empty, it indicates it's not a true trial and can be removed
keyDataTidyLong3 <- keyDataTidyLong2[!is.na(keyDataTidyLong2$Keys), ]

#Check the number of rows is correct (should be 120 for student exp)
nrow(keyDataTidyLong3)

#Reorder columns for clarity
keyDataTidyLong3 <- keyDataTidyLong3[ ,c(3, 4, 5, 2, 1, 6, 10, (7:9), 11)]  

#Rename AOMI1 and AOMI2 with actual condition that was completed first/second 
 #depending on Order
if (keyDataTidyLong3$Order[1] == "1 = AOMIspec") {
  keyDataTidyLong3$Condition[keyDataTidyLong3$Condition=="AOMI1"]<-"AOMIspec" 
  keyDataTidyLong3$Condition[keyDataTidyLong3$Condition=="AOMI2"]<-"AOMIgen"

} else if (keyDataTidyLong3$Order[1] == "1 = AOMIgen") {
  keyDataTidyLong3$Condition[keyDataTidyLong3$Condition=="AOMI1"]<-"AOMIgen" 
  keyDataTidyLong3$Condition[keyDataTidyLong3$Condition=="AOMI2"]<-"AOMIspec"
  
}

#Get the first RT and put into a different column (so we can extract just the first
 #RT from each word for separate analysis)
RTsCols <- separate(keyDataTidyLong3, RTs, into = c("firstPress"),
                    sep = ",", remove = TRUE)

#Remove brackets from RTs in this new column
RTsCols$firstPress <- gsub("[","", RTsCols$firstPress, fixed=TRUE)
RTsCols$firstPress <- gsub("]","", RTsCols$firstPress, fixed=TRUE)
RTsCols$firstPress <- gsub("[:blank:]","", RTsCols$firstPress, fixed=TRUE)

#Add firstPress column to main df
keyDataTidyLong4 <- cbind(keyDataTidyLong3, firstPress = RTsCols$firstPress)

#Separate RTs column so on separate rows
keyDataTidyLong5 <- keyDataTidyLong4 %>% 
  separate_rows(Keys, RTs, sep = ",")

#Remove punctuation and spaces from values in RTs column
keyDataTidyLong5$RTs <- gsub("[","", keyDataTidyLong5$RTs, fixed=TRUE)
keyDataTidyLong5$RTs <- gsub("]","", keyDataTidyLong5$RTs, fixed=TRUE)
keyDataTidyLong5$RTs <- gsub("[:blank:]","", keyDataTidyLong5$RTs, fixed=TRUE)

#Remove punctuation from Keys column as above
keyDataTidyLong5$Keys <- gsub("[[:punct:][:blank:]]","", keyDataTidyLong5$Keys)

#Filter to just get the rows when the participant pressed 'enter/return' to end 
 #a trial. The RT for this key press indicates the time to type the whole word
wholeWord <- keyDataTidyLong5 %>%
  filter(keyDataTidyLong5$Keys == "return") 

#In participants 21, 23, they pressed return twice on a trial and so there was a duplicate
#This needs to be removed before combining these datasets as the number of rows
#were not equal
# First check for duplicates:
#duplicated(wholeWord$Word)
# Remove duplicate and save dataset:
#wholeWord <- wholeWord[!duplicated(wholeWord$Word),]
#This will remove the second duplicate which is what we want in this case

#Combine the above whole word column to the older dataset which has the same number of rows
keyDataTidyLong6 <- cbind(keyDataTidyLong4, wholeWordTime = wholeWord$RTs)

#Do the above again, i.e. separate the dataset so RTs and keys on their own rows
keyDataTidyLong7 <- keyDataTidyLong6 %>% 
  separate_rows(Keys, RTs, sep = ",")
#Remove punctuation again
keyDataTidyLong7$RTs <- gsub("[","", keyDataTidyLong7$RTs, fixed=TRUE)
keyDataTidyLong7$RTs <- gsub("]","", keyDataTidyLong7$RTs, fixed=TRUE)
keyDataTidyLong7$RTs <- gsub("[:blank:]","", keyDataTidyLong7$RTs, fixed=TRUE)
keyDataTidyLong7$Keys <- gsub("[[:punct:][:blank:]]","", keyDataTidyLong7$Keys)

#change RTs column to numeric instead of character
keyDataTidyLong7$RTs <- as.numeric(keyDataTidyLong7$RTs)

#find inter-key interval times (IKIs) of key presses from RTs and add in new column
 #i.e. this is the difference between RTs for each word, where the first key
 #press = 0
IKIs <- keyDataTidyLong7 %>%
  group_by(Word) %>%
  mutate(IKIs = RTs - lag(RTs, default = RTs[1]))

#Turn any negative numbers in IKIs column into 0 (this indicates the first key press
 #of a word)
IKIs[,14][IKIs[, 14] < 0] <- 0

#Compare column 'typedWord' with 'Word' and if they match, code as '1' in new 
 #column to show the word was typed correctly. If they don't match, code as '0'
IKIs$typedCorrect <- ifelse(IKIs$typedWord == IKIs$Word, 1, 0)

#Change columns to relevant data type
completeData <- IKIs %>%
  mutate(Order = factor(Order), pID = as.character(ParticipantID),
         Condition = factor(Condition), Word = factor(Word), 
         typedCorrect = as.binary(typedCorrect, logic = TRUE), IKIs = as.numeric(IKIs),
         firstPress = as.numeric(firstPress), wholeWordTime = as.numeric(wholeWordTime))

#### Add in condition ratings to dataset ####
rateData <-  dplyr::select(rawData, participant, Order, baseRateTypeKey.keys, 
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

#move certain columns up so data is in line
rateData2 <- rateData2 %>% 
  mutate_at(c("AOMI1rateTypeKey.keys","AOMI1rateLoop.thisTrialN"), funs(lead), n = 2) %>%
  mutate_at(c("contTypeKey.keys", "contRateLoop.thisTrialN"), funs(lead), n = 5) %>%
  mutate_at(c("AOMI2typeKey.keys","AOMI2rateLoop.thisTrialN"), funs(lead), n = 7)

#Remove NAs at the bottom of df
rateData2 <- rateData2[-c(4:nrow(rateData2)),]

#Rearrange data so we have the format we want

#First make sure data is a dataframe
as.data.frame(rateData2)

rateData3 <- reshape(as.data.frame(rateData2), direction="long", 
                           varying=list(c("baseRateTypeKey.keys", 
                                         "AOMI1rateTypeKey.keys",
                                         "contTypeKey.keys",
                                         "AOMI2typeKey.keys"),
                                        c("baseRateLoop.thisTrialN",
                                          "AOMI1rateLoop.thisTrialN",
                                          "contRateLoop.thisTrialN",
                                          "AOMI2rateLoop.thisTrialN")),
                     v.names=c("Rating", "LoopNo."))

#Rename conditions as a new column/df
condCol <- as.data.frame(recode(rateData3$time, "1" = "base", "2" = "AOMI1", "3" = "control",
                    "4" = "AOMI2"))
#Rename column
colnames(condCol)[1] <- "Condition"

#Add onto previous df
rateData4 <- cbind(rateData3, Condition = condCol$Condition)

#Get rid of 'time' column
rateData4 <- rateData4[,-3]

#Now we need to rename the loop no. 1 to either be the imageryPres rating or the 
 #visual imagery Q depending on condition

#Change the LoopNo. column from numeric to character for the below to work
rateData4 <- rateData4 %>%
  mutate(LoopNo. = as.character(LoopNo.))

rateData5 <- rateData4 %>%
  mutate(LoopNo. = case_when(
    LoopNo. == "1" & Condition %in% c("base","control") ~ "imageryQ",
    LoopNo. == "1" & Condition %in% c("AOMI1","AOMI2") ~ "VI",
    TRUE ~ LoopNo.))

#Now rename other numbers in LoopNo. column
rateData5$LoopNo.[rateData5$LoopNo.=="0"]<-"ratePerf" 
rateData5$LoopNo.[rateData5$LoopNo.=="2"]<-"KI"

#Now put this column into wide format so we have the different rating types as
 #diff columns

rateData6 <- pivot_wider(rateData5, names_from = LoopNo., values_from = Rating)

#Get rid of NAs
rateData6 <- rateData6 %>% 
  filter_at(vars(ratePerf, imageryQ, VI, KI), any_vars(!is.na(.)))
rateData6 <- rateData6[,-7]

#Shift data so it lines up better
imaQcol <- as.data.frame(shift(rateData6$imageryQ, n=1L, fill=NA, type="lead", 
                                       give.names=FALSE))
VIcol <- as.data.frame(shift(rateData6$VI, n=1L, fill=NA, type="lead", 
                               give.names=FALSE))
KIcol <- as.data.frame(shift(rateData6$KI, n=2L, fill=NA, type="lead", 
                             give.names=FALSE))
#Rename these columns
colnames(imaQcol)[1] <- "imageryQ"
colnames(VIcol)[1] <- "VI"
colnames(KIcol)[1] <- "KI"

#Combine into dataset
rateData7 <- cbind(rateData6, imageryQ = imaQcol$imageryQ, VI = VIcol$VI, KI = KIcol$KI)

#Get rid of old columns
rateData7 <- rateData7[,-c(6:8)]

#Get rid of blank rows
rateData8 <- rateData7 %>% 
  filter_at(vars(ratePerf, imageryQ, VI, KI), any_vars(!is.na(.)))

#Rename AOMI1 and AOMI2 depending on order in completeData
if (completeData$Order[1] == "1 = AOMIspec") {
  rateData8$Condition[rateData8$Condition=="AOMI1"]<-"AOMIspec" 
  rateData8$Condition[rateData8$Condition=="AOMI2"]<-"AOMIgen"
  
} else if (completeData$Order[1] == "1 = AOMIgen") {
  rateData8$Condition[rateData8$Condition=="AOMI1"]<-"AOMIgen" 
  rateData8$Condition[rateData8$Condition=="AOMI2"]<-"AOMIspec"
  
}

#Merge this dataset with the main one
completeData2 <- merge(completeData, rateData8, by = "Condition", all = TRUE)  

#Remove punctuation from rating columns
completeData2$ratePerf <- gsub("[[:punct:][:blank:]]","", completeData2$ratePerf)
completeData2$imageryQ <- gsub("[[:punct:][:blank:]]","", completeData2$imageryQ)
completeData2$VI <- gsub("[[:punct:][:blank:]]","", completeData2$VI)
completeData2$KI <- gsub("[[:punct:][:blank:]]","", completeData2$KI)

#### Add PHQ-9 scores and typing style to dataset from computer questionnaire responses ####

#Import comp questionnaire spreadsheet (Qualtrics output)
compQ <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/studentRawData/PHQ-9/compQ_20220426.csv")

#Select just the participant and PHQ-9 total column
filterCompQ <- compQ %>%
  dplyr::select(Q1, Q19, SC4)

#Change participant ID to a string so we can use the below comparison code
filterCompQ <- filterCompQ %>%
  mutate(pID = as.character(Q1))

#Remove any preceding 0s from the participant ID so they will match the completeData
filterCompQ$pID = str_remove(filterCompQ$pID, "^0+")

#Or do it for the completeData if necessary
completeData2$pID = str_remove(completeData2$pID, "^0+")

#Filter so we just get this participant's scores by comparing pIDs in both datasets
PcompQ <- filterCompQ[filterCompQ$pID %in% completeData2$pID,]

#Convert typing style column so Ps are classified as either Touch or H+P typists
#The below renames the items in the typing style column depending on whether the 
 #answer contains "multiple" because this answer indicates touch typing
#Only run the below ONCE or it will always end up as HP
if (str_contains(PcompQ$Q19, "multiple", ignore.case = TRUE) == TRUE) {
  PcompQ$Q19[1]<-"touch" 
} else {
  PcompQ$Q19[1]<-"HP" 
}
#If a participant has put 'other', this will need to be manually reviewed (e.g., P11)

#Repeat the PHQ-9 column the amount of times as rows there are in completeData so
 #we can combine them
repPcompQ <- as.data.frame(rep(PcompQ$SC4[1], times = nrow(completeData2)))

#Rename so comprehensible
colnames(repPcompQ)[1] <- "PHQ9"

#Repeat the typing style column the amount of times as rows there are in completeData so
#we can combine them
repPcompQ2 <- as.data.frame(rep(PcompQ$Q19[1], times = nrow(completeData2)))

#Rename so comprehensible
colnames(repPcompQ2)[1] <- "tStyle"

#Combine datasets
addData <- cbind(completeData2, PHQ9 = repPcompQ$PHQ9)
completeData3 <- cbind(addData, tStyle = repPcompQ2$tStyle)

#Remove extra unneeded columns
completeData3 <- completeData3[,-c(16:19)]

#### Export as .csv to manually code accuracy ####

#Rename Participant ID if need be - NOT AUTOMATED, change individually
#completeData3$ParticipantID <- completeData3$ParticipantID[completeData3$ParticipantID=="02=A"]<-"2" 

#Extract the participant ID from df to use for naming the csv file
ParticipantNo. <- toString(completeData3[1,2])

#Write the wrangled data to a new csv file to manually code accuracy and correct typing

write.csv(completeData3, 
          sprintf("\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\CSVsforCodingAccuracy\\wrangledData\\P%s.csv", ParticipantNo.),
          row.names = FALSE)
#If there is an error message for the above, check you don't have a file already
 #open with the same name.
