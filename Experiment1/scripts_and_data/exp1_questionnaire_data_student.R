#options("install.lock"=FALSE)

library(tidyverse)
library(lmerTest)
library(emmeans)

#### Import and tidy data ####

#Import comp questionnaire spreadsheet (Qualtrics output)
compQ <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/studentRawData/PHQ-9/compQ_20220426_edit.csv")

#Import full sample for analysis (only need one measure)
wwData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_v2_ww.csv')

# Import imagery questionnaire data
imaQ <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/studentRawData/imageryQ/imageQ_20220812_edit.csv")

#Drop unnecessary columns

compQ2 <- compQ[,-c(1:11)]
imaQ2 <- imaQ[,-c(1:11)]

#Check what the participant IDs look like
unique(compQ2$Q1)
unique(imaQ2$Q13)
#We have some errors but we may not need these Ps anyway

#Now just filter on the participants whose data we are including in the analysis
compQ3 <- compQ2[compQ2$Q1 %in% wwData2$ParticipantID,]
#Remove 0s
imaQ2$Q13 <- str_remove(imaQ2$Q13, "^0+")
imaQ3 <- imaQ2[imaQ2$Q13 %in% wwData2$ParticipantID,]
#And we now have 51 obs in each df which matches our final sample of participants

#### Gender ####

genDat <- compQ3 %>%
  group_by(Q2) %>%
  summarise(n())

#### Ethnicity ####

ethDat <- compQ3 %>%
  group_by(Q3) %>%
  summarise(n()) %>%
  rowwise() %>%
  mutate(`%` = (`n()`/51)*100)

#See which groups our 'mixed' Ps belong to

mixDat <- compQ3 %>% 
  filter(Q3 == "Mixed or multiple ethnic groups") %>%
  select(Q5)
#They are both White and Asian

#See how the person who put 'other' identifies

othDat <- compQ3 %>% 
  filter(Q3 == "Other ethnic group") %>%
  select(Q8)
#Arab

#### Computer details ####

#How many Ps used Mac vs Windows?

OSdat <- compQ3 %>%
  group_by(Q12) %>%
  summarise(n())
#Very equal! 


#Calculate CPQ-12 scores

#We first need to code Ps' answers as integers
  
compQ3[ , 24:35][ compQ3[ , 24:35 ] == "Very easily" ] <- "5"
compQ3[ , 24:35][ compQ3[ , 24:35 ] == "Somewhat easily" ] <- "4" 
compQ3[ , 24:35][ compQ3[ , 24:35 ] == "Not very easily" ] <- "3"
compQ3[ , 24:35][ compQ3[ , 24:35 ] == "Not at all" ] <- "2"
compQ3[ , 24:35][ compQ3[ , 24:35 ] == "Never tried" ] <- "1"

#Mutate so these columns are numbers not characters
compQ4 <- compQ3 %>%
  mutate(Q25 = as.integer(Q25),
         Q26 = as.integer(Q26),
         Q27 = as.integer(Q27),
         Q28 = as.integer(Q28),
         Q29 = as.integer(Q29),
         Q30 = as.integer(Q30),
         Q31 = as.integer(Q31),
         Q32 = as.integer(Q32),
         Q33 = as.integer(Q33),
         Q34 = as.integer(Q34),
         Q35 = as.integer(Q35),
         Q36 = as.integer(Q36))

#We now need to work out the average score for each subscale and then sum
 #the averages

CPQ12dat <- compQ4 %>%
  group_by(Q1) %>%
  summarise(compBasics = mean(Q25,Q26))

CPQ12dat1 <- compQ4 %>%
  group_by(Q1) %>%
  summarise(Printer = mean(Q27,Q28))

CPQ12dat2 <- compQ4 %>%
  group_by(Q1) %>%
  summarise(comms = mean(Q29,Q30))

CPQ12dat3 <- compQ4 %>%
  group_by(Q1) %>%
  summarise(internet = mean(Q31,Q32))

CPQ12dat4 <- compQ4 %>%
  group_by(Q1) %>%
  summarise(calendar = mean(Q33,Q34))

CPQ12dat5 <- compQ4 %>%
  group_by(Q1) %>%
  summarise(entertain = mean(Q35,Q36))

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

#### Imagery Q answers ####

#Could you imagine?
imaDat<- imaQ3 %>%
  group_by(Q1) %>%
  summarise(n())

#How well could you imagine?
imaDat2 <- imaQ3 %>%
  group_by(Q2) %>%
  summarise(n())

#Find which Ps reported poor imagery

poorIma <- imaQ3 %>%
  filter(Q2 == "Very poorly" | Q2 == "Quite poorly") %>%
  select(Q13, Q2)

#Perspective
persDat<- imaQ3 %>%
  group_by(Q4) %>%
  summarise(n())

#Imagine when not supposed to?

badImaDat<- imaQ3 %>%
  group_by(Q6) %>%
  summarise(n())

#Hands?
handsDat<- imaQ3 %>%
  group_by(Q8) %>%
  summarise(n())

#Difficult?
diffDat<- imaQ3 %>%
  group_by(Q9) %>%
  summarise(n())

diffSum <- imaQ3 %>%
  mutate(Q9 = as.integer(Q9)) %>%
  summarise(median(Q9), mad(Q9))

#Improved?
impDat<- imaQ3 %>%
  group_by(Q10) %>%
  summarise(n())

#Worse?
worseDat<- imaQ3 %>%
  group_by(Q11) %>%
  summarise(n())

#Actor?
actDat<- imaQ3 %>%
  group_by(Q14) %>%
  summarise(n())

#Replace image identifiers with numbers for IOS scale

imaQ3$Q12[imaQ3$Q12 =="IM_aXi7uh3v93x47Yi"]<-"1"
imaQ3$Q12[imaQ3$Q12 =="IM_0VQuAuaY4RS2ys6"]<-"2"
imaQ3$Q12[imaQ3$Q12 =="IM_eCMCHSYAm9jqNiC"]<-"3"
imaQ3$Q12[imaQ3$Q12 =="IM_6KFSUggbHRvBu0C"]<-"4"
imaQ3$Q12[imaQ3$Q12 =="IM_eboiK4scJPxasF8"]<-"5"
imaQ3$Q12[imaQ3$Q12 =="IM_0B43ID1P2z6mAlg"]<-"6"
imaQ3$Q12[imaQ3$Q12 =="IM_a5eiCsn1qRKri5w"]<-"7"

iosDat<- imaQ3 %>%
  group_by(Q12) %>%
  mutate(Q12 = as.integer(Q12)) %>%
  summarise(n())

iosSum <- imaQ3 %>%
  mutate(Q12 = as.integer(Q12)) %>%
  summarise(median(Q12), mad(Q12))

#### Repeat student analyses without poor imagers ####

wwDatamut2 <- wwData2 %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         wwTime = as.numeric(wholeWordTime), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle)) %>%
  filter(!ParticipantID %in% poorIma$Q13) %>% #Filter out the participants who
 #reported poor imagery

#Check no. IDs
plyr::count(unique(wwDatamut2$ParticipantID))
#Now 46 Ps which is correct


gammaWWtime5_2 <- glmer(wwTime ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                        + Condition:PHQ9 + Order + Condition:Order  + 
                          (1|Word) + (1 + Condition | ParticipantID),
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                        family = Gamma(link = "log"), nAGQ = 0, data = wwDatamut2)
#Did not converge with naGQ = 1

summary(gammaWWtime5_2)

wwPariwise5_2 <- emmeans(gammaWWtime5_2, pairwise ~ Condition, adjust="tukey",
                         lmer.df = 'satterthwaite', type = "response")

wwPariwise5_3 <- emmeans(gammaWWtime5_2, pairwise ~ Condition:Order, adjust="tukey",
                         lmer.df = 'satterthwaite', type = "response")


#### Repeat student analysis comparing Ps' actor-similarity ####

#We want to see whether Ps who reported similar typing to the actor have different 
# results than those who reported different typing

actDat_2 <- imaQ3 %>%
  dplyr::select(Q13, Q14) %>%
  dplyr::distinct()

#Rename actDat options so we can easily code as a factor
actDat_2$Q14[actDat_2$Q14 == "Better than my own ability"] <- "better"
actDat_2$Q14[actDat_2$Q14 == "The same as my own ability"] <- "same"
actDat_2$Q14[actDat_2$Q14 == "Worse than my own ability"] <- "worse"

##### Whole word times ####

#Tidy whole word data

wwDatamut2 <- wwData2 %>%
  dplyr::mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         wwTime = as.numeric(wholeWordTime), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle))

#Combine datasets
wwDat_act <- wwDatamut2 %>%
  dplyr::left_join(actDat_2, by = c("ParticipantID" = "Q13"))

#Run model
#Removed PHQ9 and order as these factors weren't significant in original model
wwAct_mod <- lme4::glmer(wwTime ~ Condition + tStyle  + Condition:tStyle + 
                                Q14 + Condition:Q14 +
                          (1|Word) + (1 + Condition | ParticipantID),
                        control = glmerControl(optimizer = "bobyqa", 
                                               optCtrl = list(maxfun = 500000)),
                        family = Gamma(link = "log"), nAGQ = 0, data = wwDat_act)
summary(wwAct_mod) #Main effect of Q14

#Look at effects of typing similarity

emmeans::emmeans(wwAct_mod, pairwise ~ Q14, adjust="tukey",
                 lmer.df = 'satterthwaite', type = "response")

emmeans::emmeans(wwAct_mod, pairwise ~ Condition:Q14, adjust="tukey",
                 lmer.df = 'satterthwaite', type = "response") 

##### IKIs ####

#Import data
ikiData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_iki_v2.csv')

#Tidy

ikiDatamut2 <- ikiData2 %>%
  dplyr::mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
                Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
                Word = as.character(Word), 
                typedWord = as.character(typedWord), Keys = as.character(Keys),
                IKIs = as.numeric(IKIs), PHQ9 = as.integer(PHQ9), 
                tStyle = factor(tStyle))

#Combine datasets
ikiDat_act <- ikiDatamut2 %>%
  dplyr::left_join(actDat_2, by = c("ParticipantID" = "Q13"))

#Run model
ikiAct_mod  <- lme4::glmer(IKIs ~ Condition + tStyle + Condition:tStyle + 
                             Q14 + Condition:Q14 + 
                     (1 | Word) + (1 + Condition | ParticipantID), 
                   family = Gamma(link = "log"),
                   nAGQ = 0,
                   glmerControl(optimizer="bobyqa", 
                                optCtrl = list(maxfun = 500000)),
                   data = ikiDat_act)
summary(ikiAct_mod) #Main effect of Q14


#Look at effects of typing similarity

emmeans::emmeans(ikiAct_mod, pairwise ~ Q14, adjust="tukey",
                 lmer.df = 'satterthwaite', type = "response")

emmeans::emmeans(ikiAct_mod, pairwise ~ Condition:Q14, adjust="tukey",
                 lmer.df = 'satterthwaite', type = "response") 
#People that were 'better' and 'worse' were more slowed by AO+MIspec compared
 #to control. This difference was not significant for people that reported 
 #typing 'the same' or 'not sure'.

##### First press time ####

#Import data
fpData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_fp_v2.csv')

#Tidy
fpDatamut2 <- fpData2 %>%
  dplyr::mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
                Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
                #level the factor of condition according to treatment levels
                Word = as.character(Word), 
                typedWord = as.character(typedWord), Keys = as.character(Keys),
                firstPress = as.numeric(firstPress), PHQ9 = as.integer(PHQ9), 
                tStyle = factor(tStyle))

#Combine datasets
fpDat_act <- fpDatamut2 %>%
  dplyr::left_join(actDat_2, by = c("ParticipantID" = "Q13"))

#Run model
#Model output looked overly significant when using identity link and relaxed
 #intercept slope so amended
fpAct_mod <- lme4::glmer(firstPress ~ Condition + tStyle  + Condition:tStyle + 
                           Q14 + Condition:Q14 + (1 | Word) + 
                       (1 + Condition | ParticipantID), 
                     family = Gamma(link = "log"),
                     nAGQ = 1,
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                     data = fpDat_act)
summary(fpAct_mod)

#Look at effects of typing similarity

emmeans::emmeans(fpAct_mod, pairwise ~ Q14, adjust="tukey",
                 lmer.df = 'satterthwaite', type = "response")

emmeans::emmeans(fpAct_mod, pairwise ~ Condition:Q14, adjust="tukey",
                 lmer.df = 'satterthwaite', type = "response") 
#No effects
