#This script is for any additional data tidying of the student data before passing
 #to a different researcher to run the interim analysis
# E.g.:
#Data exclusions
#Remove outliers from each measure
#Check data distributions so can add correct models into interim analysis scripts

#Before installing: options("install.lock"=FALSE)

library(tidyverse)
library(binaryLogic)
library(fitdistrplus) #for checking data distribution
library(trimr) #needed for outlier removal

#### Import data ####

#First combine all the student data into one df

allData <- list.files(path = "//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/CSVsforCodingAccuracy/CodedFiles",
                      pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%          # Store all files in list                                  
  bind_rows  # Combine data sets into one data set                                                      
allData <- as.data.frame(allData)    #convert from a tibble into a dataframe

#As a sanity check, you should have data from 25 participants, so there should be 
#25 unique IDs (for the interim analysis, 50-51 for final). 
 #Run the below code to count how many unique IDs there are

count(unique(allData[c("ParticipantID")]))

#### Data exclusions ####

#Find out if any participants reported no imagery in both AO+MI conditions
noImagery <- allData %>%
  filter(VI == 1 & KI == 1)
#If this results in 0 observations then this means that no participants reported
 #no imagery in both AO+MI conditions

#If a participant HAS reported no imagery in any of the conditions, flagged by the above, 
#find out which participants and conditions this was in. If there is only one person
#the below will be sufficient to figure out whether their data needs removing
unique(noImagery$ParticipantID)
unique(noImagery$Condition)

#If a participant HAS reported no imagery in both conditions, find the ParticipantID
 #of the participant and run the below line of code, replacing the ? with their
 #ParticipantID to remove their data:

#subset(allData, ParticipantID!="?")

#Interim & final analysis = only P4 reported no imagery in AOMIgen only so not removed

#Now, filter out trials from further analysis on which participants started typing too
#early, i.e. if there are values in the BkgdKeys and promptBkgdKeys columns

filteredData <- allData %>%
  filter(is.na(BkgdKeys))

filteredData2 <- filteredData %>%
  filter(is.na(promptBkgdKeys)) 

#Check for any manual accuracy coding errors - hopefully the below returns 0 rows
accCheck <- filteredData2 %>%
  filter(is.na(accuracyScore))
#For interim & final analysis the above only returned NAs for the 'return' key presses, which 
 #is fine 

#See if there are NAs in No.Errors column
filteredData2 %>%
  filter(is.na(No.Errors))

#Convert accuracyScore to integer
filteredData2$accuracyScore <- as.integer(filteredData2$accuracyScore)

#Now get the max score in the accuracyScore column (should not be > 1)

max(filteredData2$accuracyScore, na.rm = TRUE)

#Find out whether any participants have more than 50% of their FP data < 100ms
#(this would indicate cheating and their whole dataset is likely unreliable
#so needs to be removed). 
#The below filters all RTs < 100ms. Hopefully the below returns 0 rows
#If it doesn't and seems like a lot of data for certain participants, find out
#what proportion of their data this is to decide whether to exclude whole dataset

filteredData2 %>%
  filter(firstPress < 0.1)  
#In interim & final analysis, a few participants had one word < 100ms - this is fine

#See whether any 'return' key presses have accidentally been coded as a 1 or 0
 #(should be NA). Hopefully the below returns 0 rows

filteredData2 %>%
  filter(Keys == "return" && accuracyScore != NA)

#Check Benchling & experiment notes to see whether any specific trials need removing
#For P24, remove 'fungal' and 'hearth' trials as P looked like they were typing
 #while supposed to be observing
# P25, 'girdle' - looked away from screen
# P47 - remove 'shrill' and 'portal' as P was looking down, not at videos

filteredData3 <- filteredData2[!(filteredData2$Word == "fungal" & filteredData2$ParticipantID == "24") & !(filteredData2$Word == "hearth" & filteredData2$ParticipantID == "24") & !(filteredData2$Word == "girdle" & filteredData2$ParticipantID == "25") & !(filteredData2$Word == "shrill" & filteredData2$ParticipantID == "47")& !(filteredData2$Word == "portal" & filteredData2$ParticipantID == "47"),]

#Use the below to check the trials remaining don't include the ones you just removed ^^
checkTrials <- filteredData3 %>%
  filter(Word == "portal" & ParticipantID == "47")

#Now compare the total word time of the first three trials of the control condition
 #to the remaining trials to see whether there is a practice effect

#Filter the data so we just get the first three trials in the control cond for each
 #participant
contTrials1st <- filteredData3 %>%
  filter(Condition == "control" & Keys == "return") %>%
  group_by(ParticipantID) %>%
  slice(1:3)

#Then filter so we get all control trials except the first three for each ppt
contTrialsLast <- filteredData3 %>%
  filter(Condition == "control" & Keys == "return") %>%
  group_by(ParticipantID) %>%
  slice(4:n())
  
#Check the data distribution of the above data to see whether we can run a t-test

# Generate Cullen & Frey graph to see which distribution fits closest
cont1stDist <- descdist(contTrials1st$wholeWordTime, discrete = FALSE)
hist(contTrials1st$wholeWordTime)

contLastDist <- descdist(contTrialsLast$wholeWordTime, discrete = FALSE)
hist(contTrialsLast$wholeWordTime)
#Interim analysis, the data looks close to a beta/gamma distribution
#Final analysis, both sets of data look close to a gamma distribution

#Check assumptions to run an ANOVA - both should look fairly normal
qqnorm(contTrials1st$wholeWordTime)
qqnorm(contTrialsLast$wholeWordTime)
#This gives an exponential distribution of the residuals in interim analysis

#Combine these datasets
contTrials <- merge(contTrials1st, contTrialsLast, by=c("ParticipantID", "Word"), 
                    all=TRUE, sort=FALSE)

#Convert data from wide to long format:
contTrialsLong <- contTrials %>%
  pivot_longer(cols = c(wholeWordTime.x, wholeWordTime.y), 
               names_to = c("extra", "trialsPos"),
               names_sep = 14,
               values_to = "wholeWordTime")
#Where x = first trials and y = last trials

#The below code is to run an ANOVA, but as the residuals are not normally distributed
 #in the interim or final analysis, we can't run this

# Bartlett Test of Homogeneity of Variances
#bartlett.test(wholeWordTime ~ trialsPos, data=contTrialsLong)

#Run ANOVA
#contTrialsAOV <- aov(wholeWordTime ~ trialsPos, data = contTrialsLong)
#summary(contTrialsAOV)
#If significant, remove the first three trials from the main dataset

#Run GLMM instead with a gamma distribution!

contGLMM <- glmer(wholeWordTime ~ trialsPos + (1|ParticipantID) + 
                    (1|Word), family = Gamma, nAGQ = 1, data = contTrialsLong)
summary(contGLMM)
#Woohoo, no sig difference, so can include all control trials in data

#### Separate measures ####

#Start with first press data

fpData <- filteredData3 %>%
  filter(!is.na(accuracyScore) & IKIs == 0 & firstPress < 10 & firstPress > 0.1
         & accuracyScore == 1)  
#filter out any NAs, ensure only first press is included (when IKI = 0), remove 
 #responses > 10 secs and < 0.1 secs as these are unlikely to be true responses,
 #and remove incorrect key presses

fpData$firstPress <- fpData$firstPress * 1000 #Change data into milliseconds

#Now whole word time

wwData <- filteredData3 %>%
  filter(IKIs == 0) #filter so we just get one value per trial on press of 
#return key

#IKIs data

ikiData <- filteredData3 %>%
  filter(IKIs != 0 & Keys != "return" & accuracyScore == 1) 
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

accData <- filteredData3 %>%
  filter(IKIs == 0) #filter so we just get one accuracy score per trial (not per
#key press)

#### Visualise Data #### 
# IMPORTANT - DO NOT RUN THIS IF HANDING OVER TO ANOTHER RESEARCHER TO AVOID BIAS

fpPlot <- as.data.frame(fpData) %>%
  pirateplot(formula = firstPress ~ Condition, #data = mutData,
             pal = "info", jitter.val = 0.08, theme = 1, 
             point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
             sortx = "mean", decreasing = TRUE, cex.names = 0.7,
             ylab = "Reaction Time (ms)", main = "Time of First Key Press by Condition",
             inf.method = "ci",inf.within = "ParticipantID", 
             inf.disp = "rect", gl.col="white")

wwPlot <- as.data.frame(wwData) %>%
  pirateplot(formula = wholeWordTime ~ Condition, #data = mutData,
             pal = "info", jitter.val = 0.08, theme = 1, 
             point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
             sortx = "mean", decreasing = TRUE, cex.names = 0.7,
             ylab = "Time to Type Whole Word (secs)", main = "Time to Type Whole Word by Condition",
             inf.method = "ci",inf.within = "ParticipantID", 
             inf.disp = "rect", gl.col="white")

ikiPlot <- as.data.frame(ikiData) %>%
  pirateplot(formula = IKIs ~ Condition, #data = mutData,
             pal = "info", jitter.val = 0.08, theme = 1, 
             point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
             sortx = "mean", decreasing = TRUE, cex.names = 0.7,
             ylab = "Inter-Key Interval Times (ms)", main = "Inter-Key Interval Times by Condition",
             inf.method = "ci",inf.within = "ParticipantID", 
             inf.disp = "rect", gl.col="white")

accPlot <- as.data.frame(accData) %>%
  pirateplot(formula = No.Errors ~ Condition, #data = mutData,
             pal = "info", jitter.val = 0.08, theme = 1, 
             point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
             sortx = "mean", decreasing = TRUE, cex.names = 0.7,
             ylab = "No. Errors", main = "No. Errors by Condition",
             inf.method = "ci",inf.within = "ParticipantID", 
             inf.disp = "rect", gl.col="white")

#### Remove outliers ####

#Do first press data first

#Remove outliers from the raw data using a non-recurisve method that removes
#outliers per participant per condition according to the sample size and omits 
#incorrect responses:
#https://cran.r-project.org/web/packages/trimr/vignettes/trimr-vignette.html

fpDatatrim <- nonRecursive(data = fpData, minRT = 100, pptVar = "ParticipantID",
                                condVar = "Condition", rtVar = "firstPress", 
                                accVar = "accuracyScore", digits = 5, 
                                returnType = "raw", omitErrors = TRUE)

#Check this data before and after outlier removal (to check code seems to have worked
#correctly) by visualising as histograms:

hist(fpData$firstPress)
hist(fpDatatrim$firstPress)

#Remove outliers from whole word time

wwDatatrim <- nonRecursive(data = wwData, minRT = 0.1, pptVar = "ParticipantID",
                                condVar = "Condition", rtVar = "wholeWordTime", 
                                accVar = "accuracyScore", digits = 5, 
                                returnType = "raw", omitErrors = FALSE)
#Keep incorrect answers for this measure

#Compare before and after:
hist(wwData$wholeWordTime)
hist(wwDatatrim$wholeWordTime)

#Remove outliers from IKIs data

ikiDatatrim <- nonRecursive(data = ikiData, minRT = 0, pptVar = "ParticipantID",
                                 condVar = "Condition", rtVar = "IKIs", 
                                 accVar = "accuracyScore", digits = 5, 
                                 returnType = "raw", omitErrors = TRUE)

 #Check this data before and after outlier removal:

hist(ikiData$IKIs)
hist(ikiDatatrim$IKIs)

#We won't remove outliers from accuracy data

# accDatatrim <- nonRecursive(data = accData, minRT = -0.01, pptVar = "ParticipantID",
#                                  condVar = "Condition", rtVar = "No.Errors", 
#                                  accVar = "accuracyScore", digits = 1, 
#                                  returnType = "raw", omitErrors = FALSE)
#Here minRT is sort of irrelevant because a participant will feasibly get 0 errors
#on a trial. Hence this has been set to -0.01 just so scores of 0 are included
#in the data

#Check this data before and after outlier removal:

hist(accData$No.Errors)
#hist(accDatatrim$No.Errors)

#### Visualise data without outliers ####
# IMPORTANT - DO NOT RUN THIS IF HANDING OVER TO ANOTHER RESEARCHER TO AVOID BIAS

fpPlot2 <- as.data.frame(fpDatatrim) %>%
  pirateplot(formula = firstPress ~ Condition, #data = mutData,
             pal = "info", jitter.val = 0.08, theme = 1, 
             point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
             sortx = "mean", decreasing = TRUE, cex.names = 0.7,
             ylab = "Reaction Time (ms)", main = "Time of First Key Press by Condition",
             inf.method = "ci",inf.within = "ParticipantID", 
             inf.disp = "rect", gl.col="white")

wwPlot2 <- as.data.frame(wwDatatrim) %>%
  pirateplot(formula = wholeWordTime ~ Condition, #data = mutData,
             pal = "info", jitter.val = 0.08, theme = 1, 
             point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
             sortx = "mean", decreasing = TRUE, cex.names = 0.7,
             ylab = "Time to Type Whole Word (secs)", main = "Time to Type Whole Word by Condition",
             inf.method = "ci",inf.within = "ParticipantID", 
             inf.disp = "rect", gl.col="white")

ikiPlot2 <- as.data.frame(ikiDatatrim) %>%
  pirateplot(formula = IKIs ~ Condition, #data = mutData,
             pal = "info", jitter.val = 0.08, theme = 1, 
             point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
             sortx = "mean", decreasing = TRUE, cex.names = 0.7,
             ylab = "Inter-Key Interval Times (ms)", main = "Inter-Key Interval Times by Condition",
             inf.method = "ci",inf.within = "ParticipantID", 
             inf.disp = "rect", gl.col="white")

accPlot2 <- as.data.frame(accDatatrim) %>%
  pirateplot(formula = No.Errors ~ Condition, #data = mutData,
             pal = "info", jitter.val = 0.08, theme = 1, 
             point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
             sortx = "mean", decreasing = TRUE, cex.names = 0.7,
             ylab = "No. Errors", main = "No. Errors by Condition",
             inf.method = "ci",inf.within = "ParticipantID", 
             inf.disp = "rect", gl.col="white")
  
#### Check data distributions ####

# First Press
fpDist <- descdist(fpDatatrim$firstPress, discrete = FALSE)
#Gamma/lognormal

# Whole word time
wwDist <- descdist(wwDatatrim$wholeWordTime, discrete = FALSE)
#Gamma

# IKIs
ikiDist <- descdist(ikiDatatrim$IKIs, discrete = FALSE)
#Gamma/lognormal

# Accuracy
accDist <- descdist(accData$No.Errors, discrete = TRUE)
#Poisson/negative binomial

#### Save as new .csv files ####

#One .csv file for each tidied and filtered measure:

# First press
write.csv(fpDatatrim, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\dataForAnalyses\\finalStudentData_fp_v2.csv",
          row.names = FALSE)

# Whole word time
write.csv(wwDatatrim, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\dataForAnalyses\\finalStudentData_v2_ww.csv",
          row.names = FALSE)

# IKIs
write.csv(ikiDatatrim, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\dataForAnalyses\\finalStudentData_IKI_v2.csv",
          row.names = FALSE)

#Accuracy
write.csv(accData, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\dataForAnalyses\\finalStudentData_acc_v2.csv",
          row.names = FALSE)

#Count number of H&P typists to see whether there's a sufficient amount to be worth
 #including in LMMs

unique(fpDatatrim[c("tStyle")])
unique(wwDatatrim[c("tStyle")])
unique(ikiDatatrim[c("tStyle")])
unique(accDatatrim[c("tStyle")])

HPunique <- fpDatatrim %>%
  filter(tStyle == "HP")

count(unique(HPunique[c("ParticipantID")]))
#In inteirm analysis, just P13 was a HP typist. We won't include typing style as a covariate, but we'll
 #run the analysis with and without their data to see if it affects the findings

#In final analysis, 4 Ps were HP typists - include as covariate and see if model
 #runs