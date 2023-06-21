#Before installing: options("install.lock"=FALSE)

library(tidyverse)
library(binaryLogic)
library(fitdistrplus) #for checking data distribution
library(trimr) #needed for outlier removal
library(data.table) #needed for rbindlist

## Script to combine datasets and apply exclusions to Experiment 2 student typing study ##

#### Import data ####

#First combine all the student data into one df

allData <- list.files(path = "//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/wrangledData/CodedData",
                      pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  data.table::rbindlist(fill = FALSE) %>%
  as.data.frame()

#As a sanity check, you should have data from 20 participants, so there should be 
#20 unique IDs.
#Run the below code to count how many unique IDs there are

dplyr::count(unique(allData[c("participant")]))

#### Data exclusions ####

### Remove poor imagers ###

#Find out if any participants reported no imagery in all imagery conditions
noImagery <- allData %>%
  dplyr::filter(VI == 1 & KI == 1)
#If this results in 0 observations then this means that no participants reported
#no imagery in all imagery conditions

### Typing too early ###

#Remove trials which have keys presses in the BkgdKeys and prompt columns
filteredData <- allData %>%
  dplyr::filter(BkgdKeys.x == "None" & promptBkgdKeys.x == "None")

### Check for manual accuracy coding errors ###

filteredData %>%
  dplyr::filter(is.na(accuracyScore))

filteredData %>%
  dplyr::filter(is.na(No.Errors))
#Hopefully the above return 0 rows

#Convert accuracyScore to integer
filteredData$accuracyScore <- as.integer(filteredData$accuracyScore)
#It is fine that NAs are introduced by coercion

#Now get the max score in the accuracyScore column (should not be > 1)

max(filteredData$accuracyScore, na.rm = TRUE)

### Cheating ###

#Find out whether any participants have more than 50% of their data < 100ms
#(this would indicate cheating and their whole dataset is likely unreliable
#so needs to be removed). 
#The below filters all RTs < 100ms. Hopefully the below returns 0 rows
#If it doesn't and seems like a lot of data for certain participants, find out
#what proportion of their data this is to decide whether to exclude whole dataset

filteredData %>%
  dplyr::filter(firstPress < 0.1)  
#There is only 1 trial for a few Ps so  we don't need to exclude any Ps

### Tidying ###

#Change group FALSE to F

filteredData$group[filteredData$group=="FALSE"]<-"F" 


#### Separate and tidy different measures ####

#Start with first press data

fpData <- filteredData %>%
  dplyr::filter(IKIs == 0 & firstPress < 10 & firstPress > 0.1
         & accuracyScore == 1)  
#Ensure only first press is included (when IKI = 0), remove 
#responses > 10 secs and < 0.1 secs as these are unlikely to be true responses,
#and remove incorrect key presses

fpData$firstPress <- fpData$firstPress * 1000 #Change data into milliseconds

#Now whole word time

wwData <- filteredData %>%
  dplyr::filter(Keys == "return") #filter so we just get one value per trial on press of 
#return key

#IKIs data

ikiData <- filteredData %>%
  dplyr::filter(IKIs != 0 & Keys != "return" & accuracyScore == 1) 
#Being filtered out above are: 
# - incorrect key presses
# - key presses that follow an error correction (keys pressed after the 'backspace'
#   key). These are coded as '0' in the AccuracyScore column
# - when participants press 'return' to end the trial, as this could include extra
#   time taken to check their answer before going onto the next trial
# - multiple presses of the same key (for example if a key is pressed twice, the
#   second key would be treated as the error and excluded, and the time of the first
#   press would be analysed). Multiple presses are also coded as '0' in AccuracyScore
# - the first key press on each trial which is always 0

#Convert to milliseconds for clarity

ikiData$IKIs <- ikiData$IKIs * 1000 

#Accuracy data

accData <- filteredData %>%
  dplyr::filter(Keys == "return") #filter so we just get one count of errors per trial (not per
#key press)


### Remove outliers ####

#Do first press data first

#Remove outliers from the raw data using a non-recurisve method that removes
#outliers per participant per condition according to the sample size and omits 
#incorrect responses:
#https://cran.r-project.org/web/packages/trimr/vignettes/trimr-vignette.html

fpDatatrim <- trimr::nonRecursive(data = fpData, minRT = 100, pptVar = "participant",
                           condVar = "Condition", rtVar = "firstPress", 
                           accVar = "accuracyScore", digits = 5, 
                           returnType = "raw", omitErrors = TRUE)
#Check histograms before and after outlier removal

hist(fpData$firstPress)
hist(fpDatatrim$firstPress)

#Remove outliers from whole word time

#Remove NAs first

wwData2 <- wwData %>%
  dplyr::filter(!is.na(wholeWordTime))

wwDatatrim <- trimr::nonRecursive(data = wwData2, minRT = 0.1, pptVar = "participant",
                           condVar = "Condition", rtVar = "wholeWordTime", 
                           accVar = "accuracyScore", digits = 5, 
                           returnType = "raw", omitErrors = FALSE)


#Keep incorrect answers for this measure

hist(wwData2$wholeWordTime)
hist(wwDatatrim$wholeWordTime)


#Remove outliers from IKIs data

ikiDatatrim <- nonRecursive(data = ikiData, minRT = 0, pptVar = "participant",
                            condVar = "Condition", rtVar = "IKIs", 
                            accVar = "accuracyScore", digits = 5, 
                            returnType = "raw", omitErrors = TRUE)

#Check this data before and after outlier removal:

hist(ikiData$IKIs)
hist(ikiDatatrim$IKIs)

#We won't remove outliers from accuracy data

#Check this data:

hist(accData$No.Errors)


#### Check data distributions ####

# First Press
fpDist <- fitdistrplus::descdist(fpDatatrim$firstPress, discrete = FALSE)
#Gamma, normal or lognormal

# Whole word time
wwDist <- fitdistrplus::descdist(wwDatatrim$wholeWordTime, discrete = FALSE)
#Gamma

# IKIs
ikiDist <- fitdistrplus::descdist(ikiDatatrim$IKIs, discrete = FALSE)
#Lognormal/gamma

# Accuracy
accDist <- fitdistrplus::descdist(accData$No.Errors, discrete = TRUE)
#Negative binomial/poisson


#### Save as new .csv files ####

#One .csv file for each tidied and filtered measure:

# First press
write.csv(fpDatatrim, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\study3\\dataForAnalyses\\study3data_fp.csv",
          row.names = FALSE)

# Whole word time
write.csv(wwDatatrim, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\study3\\dataForAnalyses\\study3data_ww.csv",
          row.names = FALSE)

# IKIs
write.csv(ikiDatatrim, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\study3\\dataForAnalyses\\study3data_IKI.csv",
          row.names = FALSE)

#Accuracy
write.csv(accData, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\study3\\dataForAnalyses\\study3data_acc.csv",
          row.names = FALSE)



