#Before installing: options("install.lock"=FALSE)

# MAKE BINDER #

library(tidyverse)
#library(binaryLogic) #needed for as.binary
library(lmerTest) #needed for linear mixed models with p values in output
library(emmeans) #needed for pairwise comparisons
#library(glmmTMB) #alternative package for running glmms
#library(bbmle) #for AIC tab with glmmTMB
library(fitdistrplus) #for checking data distribution
#library(optimx) #needed for allFit()
#library(yarrr) #needed for pirate plots
#library(sjPlot) #needed for html table outputs for GLMMs
library(performance) #needed for check_overdispersion
#library(EMAtools) #needed for lme.dscore to obtain Cohen's d
#library(buildmer) #needed to find maximal fixed and random effects structure for models
#library(afex) #needed for all_fit()
library(rstatix) #needed for friedman_test()
library(PMCMRplus) #needed for Nemenyi-test
library(coin) #needed for wilcox_effsize()


#Good resource: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html


### INTERIM/FINAL ANALYSIS FOR STUDENT TYPING EXPERIMENT ###

#### Import the data into R ####

#You will need to change the file path below to the folder where you saved the data
 #files on your computer. Make sure you use '/' instead of '\'.

#Tidied and filtered data of 50 Ps accuracy data
#Accuracy data
#accData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_acc.csv')
#First press data
#fpData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_fp.csv')
#Inter-key-interval times (IKIs)
#ikiData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_iki.csv')
#Whole word time
#wwData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_ww.csv')

#Tidied and filtered data including P70 (51 Ps)
#Accuracy data
accData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_acc_v2.csv')
#First press data
fpData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_fp_v2.csv')
#Inter-key-interval times (IKIs)
ikiData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_iki_v2.csv')
#Whole word time
wwData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_v2_ww.csv')

#As a sanity check, you should have data from 25/50 participants, so there should be 
 #25/50 unique IDs. Run the below code to count how many unique IDs there are in 
 #each dataset

#count(unique(accData[c("ParticipantID")]))
#count(unique(fpData[c("ParticipantID")]))
#count(unique(ikiData[c("ParticipantID")]))
#count(unique(wwData[c("ParticipantID")]))

#Should be 51 IDs for the below
count(unique(accData2[c("ParticipantID")]))
count(unique(fpData2[c("ParticipantID")]))
count(unique(ikiData2[c("ParticipantID")]))
count(unique(wwData2[c("ParticipantID")]))

#Filter participants to just get interim sample (if using)

#Create vector of Participant IDs in interim sample

int_vec <- c("1", "2", "3", "4", "5", "6", "8", "9", "10", "11", "12", 
             "13", "14", "18", "20", "21", "23", "24", "25", "26", "30", 
             "38", "41", "42", "46")

accData2_in <- accData2 %>%
  dplyr::filter(ParticipantID %in% int_vec)

fpData2_in <- fpData2 %>%
  dplyr::filter(ParticipantID %in% int_vec)

ikiData2_in <- ikiData2 %>%
  dplyr::filter(ParticipantID %in% int_vec)

wwData2_in <- wwData2 %>%
  dplyr::filter(ParticipantID %in% int_vec)

# Change column types for analysis in each dataset

# accDatamut <- accData %>%
#   mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
#          Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")), 
#          #level the factor of condition according to treatment levels
#          Word = as.character(Word), 
#          typedWord = as.character(typedWord), Keys = as.character(Keys),
#          AccuracyScore = as.numeric(accuracyScore),
#          No.Errors = as.numeric(No.Errors), PHQ9 = as.integer(PHQ9), 
#          tStyle = factor(tStyle))

# fpDatamut <- fpData %>%
#   mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
#          Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
#          Word = as.character(Word), 
#          typedWord = as.character(typedWord), Keys = as.character(Keys),
#          firstPress = as.numeric(firstPress), PHQ9 = as.integer(PHQ9), 
#          tStyle = factor(tStyle))
# 
# ikiDatamut <- ikiData %>%
#   mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
#          Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
#          Word = as.character(Word), 
#          typedWord = as.character(typedWord), Keys = as.character(Keys),
#          IKIs = as.numeric(IKIs), PHQ9 = as.integer(PHQ9), 
#          tStyle = factor(tStyle))
# 
# wwDatamut <- wwData %>%
#   mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
#          Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
#          Word = as.character(Word), 
#          typedWord = as.character(typedWord), Keys = as.character(Keys),
#          wwTime = as.numeric(wholeWordTime), PHQ9 = as.integer(PHQ9), 
#          tStyle = factor(tStyle))


#With extra P
fpDatamut2 <- fpData2#_in %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         firstPress = as.numeric(firstPress), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle))

ikiDatamut2 <- ikiData2#_in %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         IKIs = as.numeric(IKIs), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle))

wwDatamut2 <- wwData2#_in %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         wwTime = as.numeric(wholeWordTime), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle))

accDatamut2 <- accData2#_in %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")), 
         #level the factor of condition according to treatment levels
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         AccuracyScore = as.numeric(accuracyScore),
         No.Errors = as.numeric(No.Errors), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle))

# checking structure of dataframe
str(fpDatamut2)
str(wwDatamut2)
str(ikiDatamut2)
str(accDatamut2)

# checking contrasts look as expected
contrasts(fpDatamut2$Condition)
contrasts(wwDatamut2$Condition)
contrasts(ikiDatamut2$Condition)
contrasts(accDatamut2$Condition)

#### Generate summary data ####

#This will give a general directional pattern for each of the measures

#First we'll check the means in each condition for the Accuracy data

accMeans <- accDatamut2 %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_acc = mean(No.Errors), sd_rt = sd(No.Errors))

#Now time to type whole word

WWmeans <- wwDatamut2 %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_WholeWord = mean(wwTime), sd_WholeWord = sd(wwTime))

#Next is first press time

fpMeans <- fpDatamut2 %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_firstPress = mean(firstPress), sd_firstPress = sd(firstPress))

#Now, IKIs

IKImeans <- ikiDatamut2 %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_IKIs = mean(IKIs), sd_IKIs = sd(IKIs))

#Count number of HP typists in each dataset
ww_Count <- wwDatamut2 %>%
  group_by(ParticipantID) %>%
  count(tStyle)
plyr::count(ww_Count$tStyle)

fp_Count <- fpDatamut2 %>%
  group_by(ParticipantID) %>%
  count(tStyle)
plyr::count(fp_Count$tStyle)

iki_Count <- ikiDatamut2 %>%
  group_by(ParticipantID) %>%
  count(tStyle)
plyr::count(iki_Count$tStyle)

acc_Count <- accDatamut2 %>%
  group_by(ParticipantID) %>%
  count(tStyle)
plyr::count(acc_Count$tStyle)


#### Plot data ####

#fpPlot <- as.data.frame(fpDatamut) %>%
 # pirateplot(formula = firstPress ~ Condition, #data = mutData,
  #           pal = "info", jitter.val = 0.08, theme = 1, 
   #          point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
    #         sortx = "mean", decreasing = TRUE, cex.names = 0.7,
     #        ylab = "Reaction Time (ms)", main = "Time of First Key Press by Condition",
      #       inf.method = "ci",inf.within = "ParticipantID", 
       #      inf.disp = "rect", gl.col="white")

fpPlot2 <-fpDatamut2 %>%
  ggplot(aes(x = Condition, y = firstPress, colour = Condition)) +
  ggtitle("Mean time of first key press on each trial by condition") +
  xlab("Condition") +
  ylab("Time of first key press (ms)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))

#wwPlot <- as.data.frame(wwDatamut) %>%
 # pirateplot(formula = wholeWordTime ~ Condition, #data = mutData,
  #           pal = "info", jitter.val = 0.08, theme = 1, 
   #          point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
    #         sortx = "mean", decreasing = TRUE, cex.names = 0.7,
     #        ylab = "Time to Type Whole Word (secs)", main = "Time to Type Whole Word by Condition",
      #       inf.method = "ci",inf.within = "ParticipantID", 
       #      inf.disp = "rect", gl.col="white")

wwPlot2 <-wwDatamut2 %>%
  ggplot(aes(x = Condition, y = wwTime, colour = Condition)) +
  ggtitle("Mean time to type whole word by condition") +
  xlab("Condition") +
  ylab("Time to type whole word (secs)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))


#ikiPlot <- as.data.frame(ikiDatamut) %>%
 # pirateplot(formula = IKIs ~ Condition, #data = mutData,
  #           pal = "info", jitter.val = 0.08, theme = 1, 
   #          point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
    #         sortx = "mean", decreasing = TRUE, cex.names = 0.7,
     #        ylab = "Inter-Key Interval Times (ms)", main = "Inter-Key Interval Times by Condition",
      #       inf.method = "ci",inf.within = "ParticipantID", 
       #      inf.disp = "rect", gl.col="white")

ikiPlot2 <-ikiDatamut2 %>%
  ggplot(aes(x = Condition, y = IKIs, colour = Condition)) +
  ggtitle("Mean inter-key-interval times (IKIs) by condition") +
  xlab("Condition") +
  ylab("IKIs (ms)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.05) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none",
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))

#accPlot <- as.data.frame(accDatamut) %>%
 # pirateplot(formula = No.Errors ~ Condition, #data = mutData,
  #           pal = "info", jitter.val = 0.08, theme = 1, 
   #          point.o = 0.3, inf.f.o = 1, inf.b.o = 0.6, bean.b.o = 0.5, 
    #         sortx = "mean", decreasing = TRUE, cex.names = 0.7,
     #        ylab = "No. Errors", main = "No. Errors by Condition",
      #       inf.method = "ci",inf.within = "ParticipantID", 
       #      inf.disp = "rect", gl.col="white")

accPlot2 <-accDatamut2 %>%
  ggplot(aes(x = Condition, y = No.Errors, colour = Condition)) +
  ggtitle("Mean number of errors by condition") +
  xlab("Condition") +
  ylab("No. Errors")  +
  geom_violin() +
  ylim(0, max(accDatamut2$No.Errors)) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))

#Check for missing data, as being flagged by ggplot
accMissing <- accDatamut2 %>%
  filter(is.na(No.Errors))

#Look at sum of total errors made in each condition

accSum <- accDatamut2 %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(totalErrors = sum(No.Errors))

accPlot3 <- accSum %>%
  ggplot(aes(x = Condition, y = totalErrors, fill = Condition)) +
  scale_fill_manual(values=c("tomato1", "yellowgreen", "mediumturquoise", "mediumpurple1")) +
  ggtitle("Total errors made in each condition") +
  xlab("Condition") +
  ylab("Sum of errors made by all participants")  +
  geom_col() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none")

#### Distribution checks ####

wwDist <- descdist(wwDatamut2$wholeWordTime, discrete = FALSE)
#Gamma

fpDist <- descdist(fpDatamut2$firstPress, discrete = FALSE)
#Gamma/lognormal

# IKIs
ikiDist <- descdist(ikiDatamut2$IKIs, discrete = FALSE)
#Gamma/lognormal

# Accuracy
accDist <- descdist(accDatamut2$No.Errors, discrete = TRUE)
#Poisson/negative binomial

#### Previous model attempts #### 

#As there is only one P with HP typing style (P13) the models will be run with
 #and without their data to see if it impacts the effect. Typing style will not
 #be included as a covariate

#WHOLE WORD TIME

#Gamma models:

gammaWWtime <- glmer(wwTime ~ Condition * PHQ9 * Order + (1|ParticipantID) + 
                       (1|Word) + (1 + Condition * PHQ9|ParticipantID), 
                     family = Gamma, nAGQ = 1, data = wwDatamut)
#This didn't converge
#nAGQ = 0 changes the parameter estimates to be a little less exact. Default = 1

gammaWWtime <- glmer(wwTime ~ Condition * PHQ9 * Order + (1|ParticipantID) + 
                       (1|Word) + (1 + Condition * PHQ9|ParticipantID), 
                     family = Gamma, nAGQ = 0, data = wwDatamut)
#Model still won't converge

#Changed optimizer and maximum number of iterations
gammaWWtime <- glmer(wwTime ~ Condition * PHQ9 * Order + (1|ParticipantID) + 
                       (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                     family = Gamma, nAGQ = 0, data = wwDatamut)
summary(gammaWWtime)
#The above converged and showed significant differences between AOMIgen and the 
#control and baseline conditions, but not AOMIspec

#In the above model, the effect of Order doesn't seem to be significant, 
 #remove this and see if this changes the model at all:
gammaWWtime2 <- glmer(wwTime ~ Condition * PHQ9 + (1|ParticipantID) + 
                       (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                     family = Gamma, nAGQ = 0, data = wwDatamut)
summary(gammaWWtime2)
#Findings are generally the same, but slightly more significant

#Build a null model for comparison

#This is a gamma null - wouldn't run with the naGQ argument included
nullWWtime <- glmer(wwTime ~ 1 + (1|ParticipantID) + (1|Word) + 
                      (1 + Condition * PHQ9|ParticipantID),
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                    family = "Gamma", data = wwDatamut)
summary(nullWWtime)
#This won't converge??

#Run pairwise comparisons

emmeans(gammaWWtime2, pairwise ~ Condition * PHQ9, adjust="tukey", lmer.df = 'satterthwaite')

#Run correlation/regression to explore effect of PHQ9

#FIRST PRESS TIME

#Gamma model

gammaFPtime <- glmer(firstPress ~ Condition * PHQ9 * Order + (1|ParticipantID) + 
                       (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                     family = Gamma, nAGQ = 0, data = fpDatamut)
summary(gammaFPtime)
#Everything is incredibly UNsignificant?

emmeans(gammaFPtime, pairwise ~ Condition * PHQ9, adjust="tukey", lmer.df = 'satterthwaite')
#All values = 1

#Try model again without order

gammaFPtime2 <- glmer(firstPress ~ Condition * PHQ9 + (1|ParticipantID) + 
                       (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                     family = Gamma, nAGQ = 0, data = fpDatamut)
summary(gammaFPtime2)
#Makes very little difference!


#IKIS

gammaIKIs <- glmer(IKIs ~ Condition * PHQ9 * Order + (1|ParticipantID) + 
                     (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                   family = Gamma, nAGQ = 0, data = ikiDatamut)
summary(gammaIKIs)
#Very non-significant

#ACCURACY
#This one used for interim analysis
poisAcc <- glmer(No.Errors ~ Condition * PHQ9 * Order + 
                     (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 5e+05)),
                   family = poisson, nAGQ = 0, data = accDatamut2)
summary(poisAcc)

#Relevel so AOMIgen is intercept
accDatamut2$Condition <- stats::relevel(accDatamut2$Condition, "AOMIgen")

#Update model

poisAcc_rl <- stats::update(poisAcc, data = accDatamut2)

#Pairwise comps
acc_in_pw <- emmeans(poisAcc, pairwise ~ Condition + Order + Condition:Order, adjust="tukey",
                     lmer.df = 'satterthwaite', type = "response")

#Try negative binomial model instead
#Details here: https://rdrr.io/cran/lme4/man/glmer.nb.html
nbAcc <- glmer.nb(No.Errors ~ Condition * PHQ9 * Order + (1|ParticipantID) + 
                 (1|Word) + (1 + Condition * PHQ9|ParticipantID),
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
               data = accDatamut)
#Warning messages:
#1: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
 #               convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
  #            2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
   #                           convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
    #                        3: In theta.ml(Y, mu, weights = object@resp$weights, limit = limit,  :
     #                                        iteration limit reached
#Cannot change to naGQ = 0, gives error: unused argument

#Try model without Order factor
nbAcc <- glmer.nb(No.Errors ~ Condition * PHQ9 + (1|ParticipantID) + 
                    (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                  data = accDatamut)
#Warning messages:
 # 1: In theta.ml(Y, mu, weights = object@resp$weights, limit = limit,  :
  #                 iteration limit reached
   #              2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
    #                             convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded

#Try by simplifying the random effects
nbAcc <- glmer.nb(No.Errors ~ Condition * PHQ9 + (1|ParticipantID) + 
                    (1|Word) + (1 + Condition|ParticipantID),
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                  data = accDatamut)
#Warning message:
#In theta.ml(Y, mu, weights = object@resp$weights, limit = limit,  :
 #             iteration limit reached
#Below suggests this error could mean Poisson is a better fit: 
#https://stackoverflow.com/questions/70120393/iteration-limit-reached-in-lme4-glmm-what-does-it-mean

#Try and fit various optimizwers to the Poisson model to see what we get
#poisFit <- allFit(poisAcc, 
 #                 meth.tab = cbind(optimizer = rep(c("bobyqa", "Nelder_Mead", "optimx", "nloptwrap"),
  #                                                 c(1, 1, 2, 2)), 
   #                                method = c("", "", "nlminb", "L-BFGS-B", "NLOPT_LN_NELDERMEAD", "NLOPT_LN_BOBYQA")),
    #              verbose = TRUE, maxfun = 2e5, data = accDatamut)


#SO this link here suggests running the PHQ9 as a covariate/fixed effect at all 
 #will be an issue because there is no variation at the level of participant
#Instead we're going to remove this from the models and run a general linear model
 #or regression to explore the impact of the PHQ9



#WHOLE WORD TIME

gammaWWtime3 <- glmer(wwTime ~ Condition + (1|ParticipantID) + 
                       (1|Word) + (1 + Condition|ParticipantID),
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                     family = Gamma, nAGQ = 1, data = wwDatamut)
#Singular fit warning but did converge
summary(gammaWWtime3)
#This has similar results to the model including the PHQ9

emmeans(gammaWWtime3, pairwise ~ Condition, adjust="tukey", lmer.df = 'satterthwaite')

#FIRST PRESS TIME

gammaFPtime3 <- glmer(firstPress ~ Condition + (1|ParticipantID) + 
                       (1|Word) + (1 + Condition|ParticipantID),
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                     family = Gamma, verbose = 2, nAGQ = 0, data = fpDatamut)
#Failed to converge with naGQ=1
summary(gammaFPtime3)
#Very similar to model with PHQ9

emmeans(gammaFPtime3, pairwise ~ Condition, adjust="tukey", lmer.df = 'satterthwaite')

#IKIs

gammaIKIs2 <- glmer(IKIs ~ Condition + (1|ParticipantID) + 
                     (1|Word) + (1 + Condition|ParticipantID),
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                   family = Gamma, nAGQ = 0, data = ikiDatamut)
#Failed to converge with naGQ=1
summary(gammaIKIs2)
#Similar to model with PHQ9

emmeans(gammaIKIs2, pairwise ~ Condition, adjust="tukey", lmer.df = 'satterthwaite')

#ACCURACY
poisAcc2 <- glmer(No.Errors ~ Condition + (1|ParticipantID) + 
                   (1|Word) + (1 + Condition|ParticipantID),
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                 family = poisson, nAGQ = 1, data = accDatamut)
#Singular fit warning but converged
summary(poisAcc2)

emmeans(poisAcc2, pairwise ~ Condition, adjust="tukey", lmer.df = 'satterthwaite')


#### Find best model structure ####

#Whole word time

wwModTest4 <- buildmer(wwTime ~ Condition + tStyle + Condition:tStyle + PHQ9 
      + Condition:PHQ9 + Order + Condition:Order  + 
        (1|Word) + (1 + Condition * PHQ9|ParticipantID),
      buildmerControl=buildmerControl(direction='order', 
                                      args=list(control = glmerControl(optimizer = "bobyqa"))),
                                                #optCtrl = list(maxfun = 500000))),
      family = Gamma(link = "log"), data = wwDatamut, crit="LRT")
#Results: Ending the ordering procedure due to having reached the maximal feasible model - all
#higher models failed to converge. The types of convergence failure are: lme4 reports
#not having converged (2)

wwForm4 <- formula(wwModTest4@model) #Only fixed effects

#Do backward step-wise method to see if random effects can be added. See:
#https://cran.r-project.org/web/packages/buildmer/vignettes/buildmer.html

wwModTest4.5 <- buildmer(wwTime ~ Condition + tStyle + Condition:tStyle + PHQ9 
                       + Condition:PHQ9 + Order + Condition:Order  + 
                         (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                       buildmerControl=buildmerControl(direction='backward', 
                                                       args=list(control = glmerControl(optimizer = "bobyqa"))),
                       #optCtrl = list(maxfun = 500000))),
                       family = Gamma(link = "log"), data = wwDatamut, crit="LRT")

wwForm4.5 <- formula(wwModTest4.5@model) #Still no random effects???
#Says all terms are significant: wwTime ~ 1 + Condition + tStyle + PHQ9 + Condition:PHQ9 + Order + 
 #Condition:Order

#This doesn't seem to work as expected - having run the models, they do converge
 #with included random effects whereas buildmer is suggesting they won't


#### WORKING MODELS ####

#Default log link means values are close to 0. We can use identity instead when
 #we know the values are far from 0. For the firstPress we removed anything < 100ms
 #so we know there aren't values close to 0. For IKIs we removed all instances of 0
 #to remove the first key presses on that trial, so all other presses will be > 0

#Change dataset for below models to either include or exclude the extra P (P70)

#Find the minimum wwTime to check we don't have any 0s
min(wwDatamut2$wwTime)
#Not close to 0

#FIRST PRESS
#gammaFPtime3_id <- glmer(firstPress ~ (Condition * tStyle) + (Condition * PHQ9) + (Condition * Order) + 
 #                          (1 | Word) + 
  #                         (1 + Condition * PHQ9 | ParticipantID), 
   #                      # additional (1 | Participant) term is not required:
    #                     # random intercepts for participants are captured as part of
     #                    # (1 + Condition | ParticipantID)
      #                   family = Gamma(link = "identity"),
       #                  nAGQ = 0,
        #                 glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
         #                data = fpDatamut)

#summary(gammaFPtime3_id)
#summary(gammaFPtime3_id)$coefficients

#Try model Andrew suggested:
#gammaFPtime4_id <- glmer(firstPress ~ Condition + tStyle  + Condition:tStyle + PHQ9 
 #                      + Condition:PHQ9 + Order + Condition:Order + (1 | Word) + 
  #                        (1 + Condition * PHQ9 | ParticipantID), 
                         # additional (1 | Participant) term is not required:
                         # random intercepts for participants are captured as part of
   #                      # (1 + Condition | ParticipantID)
    #                     family = Gamma(link = "identity"),
     #                    nAGQ = 0,
      #                   glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
       #                  data = fpDatamut)
#summary(gammaFPtime4_id)
#This is exactly the same results as the first model

#As per Duncan's suggestion, remove PHQ9 from random slope
#gammaFPtime5_id <- glmer(firstPress ~ Condition + tStyle  + Condition:tStyle + PHQ9 
 #                        + Condition:PHQ9 + Order + Condition:Order + (1 | Word) + 
  #                         (1 + Condition | ParticipantID), 
                         # additional (1 | Participant) term is not required:
                         # random intercepts for participants are captured as part of
   #                     # (1 + Condition | ParticipantID)
    #                     family = Gamma(link = "identity"),
    #                     nAGQ = 1,
     #                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
      #                   data = fpDatamut)
#When NAGQ=1, singular fit warning but does converge. This is because the correlation 
 #between the by-participant slope in the control condition is exactly correlated
 #with the by-participant intercept. This could indicate this couldn't be estimated 
 #by the model (see Bodo Winter chapter 13)

#When nAGQ=1, FAR more significant findings! Lower AIC than same model with nAGQ=0

#summary(gammaFPtime5_id)
#Look at the coefficients
#summary(gammaFPtime5_id)$coefficients
#isSingular(gammaFPtime5_id) #=TRUE


#Look at the coefficients for the random effects
#fpRand <- coef(gammaFPtime5_id)

#Plot the correlation between the participant intercept and slope in the control condition

#randfpPlot <-ggplot(data = fpRand$ParticipantID, 
 #                   aes(x = `(Intercept)`, y = Conditioncontrol, colour = Conditioncontrol)) +
  #ggtitle("Correlation between participant intercept, and slope in control condition") +
  #xlab("Participant intercept (ms)") +
  #ylab("Slope effect in control condition") +
  #geom_point() +
  #geom_smooth(method='lm', formula= y~x) +
  #geom_jitter(alpha = 0.1) +
  #theme(plot.title = element_text(size=9, face="bold"), axis.title.x = element_text(size=9),
   #     axis.title.y = element_text(size=9))
#This shows an exact negative correlation which explains our singular fit warning earlier

#Compare to null model

#gammaFPtime5_null <- glmer(firstPress ~ 1 + (1 | Word) + 
 #                          (1 + Condition | ParticipantID), 
                         # additional (1 | Participant) term is not required:
                         # random intercepts for participants are captured as part of
                         # (1 + Condition | ParticipantID)
  #                       family = Gamma(link = "identity"),
   #                      nAGQ = 1,
    #                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
     #                    data = fpDatamut)

#summary(gammaFPtime5_null)

#anova(gammaFPtime5_id, gammaFPtime5_null)
#No sig differences between the above and null has lower AIC

#Try models with and without effect of condition

#gammaFPtime7 <- glmer(firstPress ~ tStyle  + PHQ9 + Order + (1 | Word) + 
 #                       (1 + Condition | ParticipantID), 
                      # additional (1 | Participant) term is not required:
                      # random intercepts for participants are captured as part of
                      # (1 + Condition | ParticipantID)
  #                    family = Gamma(link = "identity"),
   #                   nAGQ = 0,
    #                  glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
     #                 data = fpDatamut)
#Model failed to converge - changed nAGQ to 0

#summary(gammaFPtime7)
#No sig effects

#Compare model just with covariates with null model
#anova(gammaFPtime7, gammaFPtime5_null)
#Models are very similar, null has lower AIC

#Compare with model including condition
#anova(gammaFPtime7, gammaFPtime5_id)
#Models are not sig different and model without effect of condition has lower AIC

#See what happens when we remove the intercept~slope correlations

# gammaFPtime6_id <- glmer(firstPress ~ Condition + tStyle  + Condition:tStyle + PHQ9 
#                          + Condition:PHQ9 + Order + Condition:Order + (1 | Word) + 
#                            (0 + Condition | ParticipantID), 
#                          # additional (1 | Participant) term is not required:
#                          # random intercepts for participants are captured as part of
#                          # (1 + Condition | ParticipantID)
#                          family = Gamma(link = "identity"),
#                          nAGQ = 1,
#                          glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
#                          data = fpDatamut)
# 
# summary(gammaFPtime6_id)
#This model does not have a singular fit warning and has a lower AIC than model
#with (1 + Condition|ParticipantID) slope

#Compare to sample with 51 Ps

gammaFPtime6_id_2 <- glmer(firstPress ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                         + Condition:PHQ9 + Order + Condition:Order + (1 | Word) + 
                           (0 + Condition | ParticipantID) + (1 | ParticipantID), 
                         # additional (1 | Participant) term is not required:
                         # random intercepts for participants are captured as part of
                         # (1 + Condition | ParticipantID) BUT #Added intercept 
                         # back in to try and get variance for this in output to
                         # calculate effect sizes
                         family = Gamma(link = "identity"),
                         nAGQ = 1,
                         glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                         data = fpDatamut2)

summary(gammaFPtime6_id_2)

#Compare to null model

gammaFPtime6_null <- glmer(firstPress ~ 1 + (1 | Word) + 
                             (0 + Condition | ParticipantID), 
                           # additional (1 | Participant) term is not required:
                           # random intercepts for participants are captured as part of
                           # (1 + Condition | ParticipantID)
                           family = Gamma(link = "identity"),
                           nAGQ = 1,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                           data = fpDatamut2)

#Compare models
anova(gammaFPtime6_id_2, gammaFPtime6_null)
#Yay! experimental model is a significantly better fit for data compared to null
 #(lower AIC)

#Write model into table format
tab_model(gammaFPtime5_id, show.ci = FALSE)

#Work out model effect size according to Judd et al. (2014)
 #(Counterbalanced design) - note that participant X target intercept variance
 #and participant X target intercept-slope covariance cannot be estimated because
 #they are confounded with residual error variance (see email from C.Judd)

effSize_fp <- (((106.3996 + 145.1392) - 37.3852)/sqrt(2880 + (586.4 + 458.2 + 2833 + 2001) + 291.2 + 0.06794))
#This suggests extremely large effect size (2.251)

#As effect sizes usually worked out for comparison of two means, work out
 #comparisons with baseline separately (as baseline is intercept)

#Baseline X AOMIspec

effSize_fp_baseSpec <- (145.0227/sqrt(2880 + (586.4 + 2001) + 291.2 + 0.06794))
#This is still very large

#Try with base/control comparison
effSize_fp_baseCont <- (37.4774/sqrt(2880 + (586.4 + 458.2) + 291.2 + 0.06794))

#Base X AOMIgen

effSize_fp_baseGen <- (106.3996/sqrt(2880 + (586.4 + 2833) + 291.2 + 0.06794))

#All large effects


#Pairwise comparisons

#g1_pairwise2 <- emmeans(gammaFPtime3_id, pairwise ~ (Condition * tStyle) + 
 #         (Condition * PHQ9) + (Condition * Order), adjust="tukey",
  #      lmer.df = 'satterthwaite')

#g1_pairwise3 <- emmeans(gammaFPtime4_id, pairwise ~ Condition + tStyle  + Condition:tStyle + PHQ9 
 #                       + Condition:PHQ9 + Order + Condition:Order, adjust="tukey",
  #                      lmer.df = 'satterthwaite')
#Pairwise comparisons are also identical for model Andrew recommended 

#Try pairwise comps with separate interactions to see if improves clarity
#g1_pairwise4 <- emmeans(gammaFPtime4_id, pairwise ~ Condition * tStyle, adjust="tukey",
 #                       lmer.df = 'satterthwaite')

#g1_pairwise5 <- emmeans(gammaFPtime4_id, pairwise ~ Condition, adjust="tukey",
 #                       lmer.df = 'satterthwaite')
#Above gives warning: NOTE: Results may be misleading due to involvement in interactions

# g1_pairwise6 <- emmeans(gammaFPtime4_id, pairwise ~ Order, adjust="tukey",
#                         lmer.df = 'satterthwaite')

g_pairwise7 <- emmeans(gammaFPtime6_id_2, pairwise ~ Condition + tStyle + Condition:tStyle +
                         Order + Condition:Order + PHQ9 + Condition:PHQ9,
                       adjust="tukey", lmer.df="satterthwaite")

#g1_pairwise2_df <- as.data.frame(g1_pairwise2, which = 2)
#g1_pairwise4_df <- as.data.frame(g1_pairwise4, which = 2)
#g1_pairwise6_df <- as.data.frame(g1_pairwise6, which = 2)
g_pairwise7_df <- as.data.frame(g_pairwise7, which = 2)

#Effect sizes of pairwise comparisons


#Save pairwise comparisons as an Excel file for easier examination
write.csv(g_pairwise7_df, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\Tables\\gamma_fpFinal_51.csv",
          row.names = FALSE)

#Also tried a lognormal model (lmer) and received singular fit warning

g_pairwise7.15 <- emmeans(gammaFPtime6_id_2, pairwise ~ Condition:Order,
                         adjust="tukey", lmer.df="satterthwaite")

#Now let's make some more sense of these comparisons by separating them
#Cond ~ tStyle
g_pairwise7.1 <- emmeans(gammaFPtime6_id_2, pairwise ~ Condition,
                       adjust="tukey", lmer.df="satterthwaite")

#FP data not so different between samples - use 51 Ps

#Get effect sizes - TEST
#Very helpful:
#http://jakewestfall.org/blog/index.php/2016/03/25/five-different-cohens-d-statistics-for-within-subject-designs/
#sigma = combination of random effect variances
#https://rdrr.io/cran/emmeans/man/eff_size.html

#g_pairwise7.1_eff <- eff_size(g_pairwise7.1[2], sigma = (291.2 + 2880 + 0.06794),
 #edf = 25)

#Get SDs for model
VarCorr(gammaFPtime6_id_2)

#Combine SDs to get overall variance
totSD <- sqrt(17.06470 + 24.21510 + 21.40660 + 53.22280 + 44.73254 + 53.66697 + 0.26066)

#g_pairwise7.1.2 <- g_pairwise7.1[2]

#g_pairwise7.1_eff1 <- eff_size(g_pairwise7.1$emmeans, sigma = totSD, edf = 5000)
g_pairwise7.1_eff2 <- eff_size(g_pairwise7.1$emmeans, sigma = totSD, edf = 50)
#Larger edf = smaller SEs and confidence boundaries, go with smaller. Chose 50
 #as this is the participant degrees of freedom
#CLs indicate the range the effect size could fall within in the population
 #(Durlak, 2009)

#Check whether sample size is the same for each condition
plyr::count(fpDatamut$Condition)
#It isn't, which complicates Cohen's d

g_pairwise7.1_df <- as.data.frame(g_pairwise7.1, which = 2)

write.csv(g_pairwise7.1_df, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\Tables\\gamma_fpFinal_cond~tStyle_51.csv",
          row.names = FALSE)

#Cond ~ Order
g_pairwise7.2 <- emmeans(gammaFPtime6_id_2, pairwise ~ Condition:Order,
                         adjust="tukey", lmer.df="satterthwaite")

g_pairwise7.2_df <- as.data.frame(g_pairwise7.2, which = 2)

write.csv(g_pairwise7.2_df, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\Tables\\gamma_fpFinal_cond~order_51.csv",
          row.names = FALSE)

#Condition main effect
g_pairwise7.3 <- emmeans(gammaFPtime6_id_2, pairwise ~ Condition,
                         adjust="tukey", lmer.df="satterthwaite")


# IKIs

#Tried log link as identity was generating an error: 
#https://stats.stackexchange.com/questions/356053/the-identity-link-function-does-not-respect-the-domain-of-the-gamma-family
#https://stats.stackexchange.com/questions/67547/when-to-use-gamma-glms
#https://civil.colorado.edu/~balajir/CVEN6833/lectures/GammaGLM-01.pdf
#gammaIKIs_id <- glmer(IKIs ~ (Condition * tStyle) + (Condition * PHQ9) + (Condition * Order) + 
 #                          (1 | Word) + 
  #                         (1 + Condition * PHQ9 | ParticipantID), 
                         # additional (1 | Participant) term is not required:
                         # random intercepts for participants are captured as part of
   #                      # (1 + Condition | ParticipantID)
    #                     family = Gamma(link = "log"),
     #                    nAGQ = 0,
      #                   glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
       #                  data = ikiDatamut)

#Try model Andrew recommended
#gammaIKIs_id2 <- glmer(IKIs ~ Condition + tStyle + Condition:tStyle + PHQ9 
 #                      + Condition:PHQ9 + Order + Condition:Order + 
  #                      (1 | Word) + 
   #                     (1 + Condition * PHQ9 | ParticipantID), 
                      # additional (1 | Participant) term is not required:
                      # random intercepts for participants are captured as part of
                      # (1 + Condition | ParticipantID)
    #                  family = Gamma(link = "log"),
     #                 nAGQ = 0,
      #                glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
       #               data = ikiDatamut)

#Try Duncan's recommended model:
# 
# gammaIKIs_id3 <- glmer(IKIs ~ Condition + tStyle + Condition:tStyle + PHQ9 
#                        + Condition:PHQ9 + Order + Condition:Order + 
#                          (1 | Word) + 
#                          (1 + Condition | ParticipantID), 
#                        # additional (1 | Participant) term is not required:
#                        # random intercepts for participants are captured as part of
#                        # (1 + Condition | ParticipantID)
#                        family = Gamma(link = "log"),
#                        nAGQ = 0,
#                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
#                        data = ikiDatamut)
#Did not converge with nAGQ=1

#Try all_Fit to find other optimizers

#all_fit(gammaIKIs_id3) #This says bobyqa should be fine?
#Changed optimizer from bobyqa to nloptwrap.NLOPT_LN_BOBYQA, then tried Nelder_Mead
 #All these failed to converge. Change random slope to remove correlation - still not converging
#Test allFit with correlation removed - still suggesting bobyqa should converge
#Change nAGQ=0

summary(gammaIKIs_id3)

#This has lower AIC than model with PHQ9 in random slope

#Compare with sample of 51 Ps
gammaIKIs_id3_2 <- glmer(IKIs ~ Condition + tStyle + Condition:tStyle + PHQ9 
                       + Condition:PHQ9 + Order + Condition:Order + 
                         (1 | Word) + 
                         (1 + Condition | ParticipantID), 
                       # additional (1 | Participant) term is not required:
                       # random intercepts for participants are captured as part of
                       # (1 + Condition | ParticipantID)
                       family = Gamma(link = "log"),
                       nAGQ = 0,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                       data = ikiDatamut2)

summary(gammaIKIs_id3_2)

#summary(gammaIKIs_id)
#summary(gammaIKIs_id)$coefficients

#summary(gammaIKIs_id2)

#Make null model
gammaIKIs_id3_null <- glmer(IKIs ~ 1 +
                         (1 | Word) + 
                         (1 + Condition | ParticipantID), 
                       # additional (1 | Participant) term is not required:
                       # random intercepts for participants are captured as part of
                       # (1 + Condition | ParticipantID)
                       family = Gamma(link = "log"),
                       nAGQ = 0,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                       data = ikiDatamut2)

summary(gammaIKIs_id3_null)

anova(gammaIKIs_id3_null, gammaIKIs_id3_2)
#Models are sig different and experimental model has lower AIC but higher BIC
 #As BIC penalises differences in no. parameters more heavily (I think), go with AIC


#Write model into table format
tab_model(gammaIKIs_id, show.ci = FALSE)

#ikiPairwise <- emmeans(gammaIKIs_id, pairwise ~ (Condition * tStyle) + (Condition * PHQ9) + 
 #         (Condition * Order), adjust="tukey", lmer.df = 'satterthwaite', type = "response")

#Try modified pairwise comparisons
#ikiPairwise2 <- emmeans(gammaIKIs_id2, pairwise ~ Condition, adjust="tukey", 
 #                       lmer.df = 'satterthwaite', type = "response")
#Warning for the above: NOTE: Results may be misleading due to involvement in interactions

#ikiPairwise3 <- emmeans(gammaIKIs_id2, pairwise ~ Condition * tStyle, adjust="tukey", 
 #                       lmer.df = 'satterthwaite', type = "response")
#No warning for the above

#Compare typing styles
#ikiPairwise4 <- emmeans(gammaIKIs_id2, pairwise ~ tStyle, adjust="tukey", 
 #                       lmer.df = 'satterthwaite', type = "response")

#This finds the same main effect of typing style seen in the model summary

#Check for any effect of order
#ikiPairwise5 <- emmeans(gammaIKIs_id2, pairwise ~ Condition * Order, adjust="tukey", 
 #                       lmer.df = 'satterthwaite', type = "response")

ikiPairwise6 <- emmeans(gammaIKIs_id3_2, pairwise ~ Condition + tStyle + Condition:tStyle
                        + PHQ9 + Condition:PHQ9 + Order + Condition:Order, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")

#ikiPairwise_df <- as.data.frame(ikiPairwise, which = 2)
#ikiPairwise_df2 <- as.data.frame(ikiPairwise3 , which = 2)
ikiPairwise_df3 <- as.data.frame(ikiPairwise6 , which = 2)

#Save pairwise comparisons as an Excel file for easier examination
write.csv(ikiPairwise_df3, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\Tables\\gamma_ikiFinal_51.csv",
          row.names = FALSE)

#Separate effects for a closer look

#Condition main effects

ikiPairwise6.1 <- emmeans(gammaIKIs_id3_2, pairwise ~ Condition, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")
#Interaction warning for above despite no sig interactions

#Effect of typing style
ikiPairwise6.2 <- emmeans(gammaIKIs_id3_2, pairwise ~ tStyle, adjust="tukey",
                          lmer.df = 'satterthwaite', type = "response")
#Interaction warning

#Cond : tStyle interaction
ikiPairwise6.3 <- emmeans(gammaIKIs_id3_2, pairwise ~ Condition:tStyle, adjust="tukey",
                          lmer.df = 'satterthwaite', type = "response")

ikiPairwise_df6.3 <- as.data.frame(ikiPairwise6.3 , which = 2)

#Save pairwise comparisons as an Excel file for easier examination
write.csv(ikiPairwise_df6.3, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\Tables\\gamma_ikiFinal_cond~tStyle_51.csv",
          row.names = FALSE)

#Run a lognormal model of the IKIs and compare AIC values of the models

#lnIKIs <- lmer(IKIs ~ Condition + tStyle + Condition:tStyle
 #                      + PHQ9 + Condition:PHQ9 + Order + Condition:Order + 
  #                     (1 | Word) + 
   #                    (1 + Condition | ParticipantID), 
                      # additional (1 | Participant) term is not required:
                      # random intercepts for participants are captured as part of
                      # (1 + Condition | ParticipantID),
             #         data = ikiDatamut)
#Converged, no warnings
#summary(lnIKIs)
#summary(lnIKIs)$coefficients
#Get AIC value
#lnIKIs_aic <- AIC(logLik(lnIKIs)) # 307914.8. Lower AIC = better. Gamma model had lower AIC,
 #use that one


#This is the same as a model above - previously ran with order which was not significant
 #neither as a main effect nor in any interactions in interim analyses, so removed
#gammaWWtime2 <- glmer(wwTime ~ Condition * PHQ9 + 
 #                       (1|Word) + (1 + Condition * PHQ9|ParticipantID),
  #                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
   #                   family = Gamma, nAGQ = 0, data = wwDatamut)
#summary(gammaWWtime2)


#ACCURACY

#Helpful:
#https://www.middleprofessor.com/files/applied-biostatistics_bookdown/_book/generalized-linear-models-i-count-data.html

#poisAcc <- glmer(No.Errors ~ (Condition * tStyle) + (Condition * PHQ9) + (Condition * Order) + 
 #                  (1|Word) + (1 + Condition * PHQ9|ParticipantID),
  #               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
   #              family = poisson, nAGQ = 0, data = accDatamut)

#All P values were = 1, doesn't seem correct - try below to see if different

#poisAcc2 <- glmer(No.Errors ~ Condition * tStyle * PHQ9 * Order + 
 #                  (1|Word) + (1 + Condition * PHQ9|ParticipantID),
  #               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
   #              family = poisson, nAGQ = 0, data = accDatamut)
#Higher AIC than poisAcc and poisAcc3, and no sig effects

#Try Andrew's suggested model
#poisAcc3 <- glmer(No.Errors ~ Condition + tStyle  + Condition:tStyle + PHQ9 
 #                 + Condition:PHQ9 + Order + Condition:Order + 
  #                  (1|Word) + (1 + Condition * PHQ9|ParticipantID),
   #               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
    #              family = poisson, nAGQ = 0, data = accDatamut2)
#Model looks the same

#Try a negative binomial model
#nbAcc <- glmer.nb(No.Errors ~ Condition + tStyle  + Condition:tStyle + PHQ9 
 #                 + Condition:PHQ9 + Order + Condition:Order + 
  #                  (1|Word) + (1 + Condition * PHQ9|ParticipantID),
   #               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
    #              nAGQ = 0, data = accDatamut)
#This model gives warning: In theta.ml(Y, mu, weights = object@resp$weights, limit = limit,  :
 #iteration limit reached - this seems to mean the Poisson model is likely a better
 #fit: https://stackoverflow.com/questions/70120393/iteration-limit-reached-in-lme4-glmm-what-does-it-mean
#Also higher AIC than original poisAcc model

#Try Duncan's suggested model:
# 
# poisAcc4 <- glmer(No.Errors ~ Condition + tStyle  + Condition:tStyle + PHQ9 
#                   + Condition:PHQ9 + Order + Condition:Order + 
#                     (1|Word) + (1 + Condition | ParticipantID),
#                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
#                   family = poisson, nAGQ = 1, data = accDatamut)
#Lower AIC than prior model with nAGQ=0
#Even lower AIC with nAGQ=1, though not by much compared to nAGQ=0

#summary(poisAcc)
#summary(poisAcc)$coefficients

#summary(poisAcc2)
#summary(poisAcc3)
#The above model has more significant effects including P70
summary(poisAcc4)

#Compare with larger sample (51)
poisAcc4_2 <- glmer(No.Errors ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                  + Condition:PHQ9 + Order + Condition:Order + 
                    (1|Word) + (1 + Condition | ParticipantID),
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                  family = poisson, nAGQ = 1, data = accDatamut2)

summary(poisAcc4_2) #main effect of PHQ9 is now significant (before was marginal)

#summary(nbAcc)

#Build a null model

poisAcc4_null <- glmer(No.Errors ~ 1 + 
                    (1|Word) + (1 + Condition | ParticipantID),
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                  family = poisson, nAGQ = 1, data = accDatamut2)

anova(poisAcc4_null, poisAcc4_2)
#When models have nAGQ=0 not sig different and null has lower AIC
#When nAGQ=1, still not sig difference but null has lower AIC and BIC

#Obtain Cohen's d effect sizes - but this is supposed to be used for independent samples?
#lme.dscore(poisAcc3,data= accDatamut, type = "lme4")

#Check for overdispersion in the Poisson models - none found
check_overdispersion(poisAcc4)

#Look at the relationship between condition and PHQ9

accTrend <- emtrends(poisAcc3, pairwise ~ Condition, var = "PHQ9")
#Not sure what emtrends does compared to emmeans?

#This is helpful: https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html#covariates

#Write model into table format - this took ages to load? Didn't finish
#tab_model(poisAcc, show.ci = FALSE)

#Pairwise comparisons
#accPairwise <- emmeans(poisAcc, pairwise ~ (Condition * tStyle) + (Condition * PHQ9) + 
 #                        (Condition * Order), adjust="tukey", lmer.df = 'satterthwaite',
  #                     type = "response")

#accPairwise_df <- as.data.frame(accPairwise, which = 2)

#accPairwise2 <- emmeans(poisAcc2, pairwise ~ (Condition * tStyle) + (Condition * PHQ9) + 
 #                        (Condition * Order), adjust="tukey", lmer.df = 'satterthwaite',
  #                     type = "response")

accPairwise3 <- emmeans(poisAcc4_2, pairwise ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                        + Condition:PHQ9 + Order + Condition:Order, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")

accPairwise4 <- emmeans(poisAcc4_2, pairwise ~ Condition:PHQ9, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")

#As no sig effects, don't bother with effect sizes

#these effects are still not significant with 51 Ps, go with larger sample

#Convert the emm_list into emmGrid format
#accP_grid <- as.emmGrid(accPairwise3[[2]])

#joint_tests(accP_grid)

#accPairwise4 <- emmeans(poisAcc3, pairwise ~ Condition, adjust="tukey",
 #                       lmer.df = 'satterthwaite', type = "response")
#NOTE: Results may be misleading due to involvement in interactions

#accPairwise5 <- emmeans(poisAcc3, pairwise ~ Condition * PHQ9, adjust="tukey",
 #                       lmer.df = 'satterthwaite', type = "response")

accPairwise_df2 <- as.data.frame(accPairwise4, which = 2)

#Save pairwise comparisons as an Excel file for easier examination
write.csv(accPairwise_df2, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\Tables\\accFinal_v3_cond:PHQ9.csv",
          row.names = FALSE)

# WHOLE WORD TIME 

#gammaWWtime <- glmer(wwTime ~ (Condition * tStyle) + (Condition * PHQ9) + 
 #                       (Condition * Order) + (1|ParticipantID) + 
  #                      (1|Word) + (1 + Condition * PHQ9|ParticipantID),
   #                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
    #                  family = Gamma, nAGQ = 0, data = wwDatamut)
#summary(gammaWWtime)

#Try model with log or identity link and compare AIC

#gammaWWtime2 <- glmer(wwTime ~ (Condition * tStyle) + (Condition * PHQ9) + 
 #                      (Condition * Order) + 
  #                     (1|Word) + (1 + Condition * PHQ9|ParticipantID),
   #                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
    #                 family = Gamma(link = "log"), nAGQ = 0, data = wwDatamut)
#summary(gammaWWtime2)
#This has lower AIC - go with this model

#Try with Andrew's suggested model:
#gammaWWtime3 <- glmer(wwTime ~ Condition + tStyle  + Condition:tStyle + PHQ9 
 #                     + Condition:PHQ9 + Order + Condition:Order  + 
  #                      (1|Word) + (1 + Condition * PHQ9|ParticipantID),
   #                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
    #                  family = Gamma(link = "log"), nAGQ = 0, data = wwDatamut2)

#summary(gammaWWtime3)

#Try removing PHQ9 random slope term as Duncan suggested:

#Try with nAGQ=1
# gammaWWtime5 <- glmer(wwTime ~ Condition + tStyle  + Condition:tStyle + PHQ9 
#                       + Condition:PHQ9 + Order + Condition:Order  + 
#                         (1|Word) + (1 + Condition | ParticipantID),
#                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
#                       family = Gamma(link = "log"), nAGQ = 1, data = wwDatamut)
# 
# summary(gammaWWtime5)
#Model fails to converge with identity link
#AIC is lower than model inc. PHQ9 in slope. But now no sig effects? No warnings

#Compare with larger sample (51 Ps)
gammaWWtime5_2 <- glmer(wwTime ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                      + Condition:PHQ9 + Order + Condition:Order  + 
                        (1|Word) + (1 + Condition | ParticipantID),
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                      family = Gamma(link = "log"), nAGQ = 1, data = wwDatamut2)

summary(gammaWWtime5_2)
#Same effects

#Build a null model
gammaWWtime5_null <- glmer(wwTime ~ 1 + (1|Word) + (1 + Condition | ParticipantID),
                                           control = glmerControl(optimizer = "bobyqa",
                                                                  optCtrl = list(maxfun = 500000)),
                                           family = Gamma(link = "log"),
                           nAGQ = 1, data = wwDatamut2)

#Compare models
anova(gammaWWtime5_2, gammaWWtime5_null) #sig different and null has higher AIC (good)


#Try model with all interactions combined
#gammaWWtime3 <- glmer(wwTime ~ Condition * tStyle  * PHQ9 * Order + 
 #                       (1|Word) + (1 + Condition * PHQ9|ParticipantID),
  #                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
   #                   family = Gamma(link = "log"), nAGQ = 0, data = wwDatamut)
#summary(gammaWWtime3)
#Lower AIC than model which has separated interaction terms (gammaWWtime) but rank
 #deficient. Go with gammaWWtime2 model

#Make table
tab_model(gammaWWtime2, show.ci = FALSE)

#Pairwise comparisons
#wwPairwise <- emmeans(gammaWWtime2, pairwise ~ (Condition * tStyle) + (Condition * PHQ9) + 
 #                        (Condition * Order), adjust="tukey", lmer.df = 'satterthwaite',
  #                    type = "response")

#Just look at condition in pairwise comps
#wwPairwise2 <- emmeans(gammaWWtime2, pairwise ~ Condition, adjust="tukey", 
 #                      lmer.df = 'satterthwaite', type = "response")
#Warning given: NOTE: Results may be misleading due to involvement in interactions

#wwPairwise3 <- emmeans(gammaWWtime2, pairwise ~ Condition * PHQ9, adjust="tukey", 
 #                      lmer.df = 'satterthwaite', type = "response")
#No warning given

#wwPairwise4 <- emmeans(gammaWWtime3, pairwise ~ Condition * PHQ9, adjust="tukey", 
 #                      lmer.df = 'satterthwaite', type = "response")

# #Main effect of condition?
# wwPariwise5 <- emmeans(gammaWWtime5, pairwise ~ Condition, adjust="tukey", 
#                        lmer.df = 'satterthwaite', type = "response")
# #Interaction warning
# 
# #try tStyle interaction
# wwPairwise6 <- emmeans(gammaWWtime5, pairwise ~ Condition:tStyle, adjust="tukey", 
#                        lmer.df = 'satterthwaite', type = "response")

#Compare with larger sample
wwPariwise5_2 <- emmeans(gammaWWtime5_2, pairwise ~ tStyle, adjust="tukey",
                       lmer.df = 'satterthwaite', type = "response")

wwPairwise6_2 <- emmeans(gammaWWtime5_2, pairwise ~ Condition:tStyle, adjust="tukey", 
                       lmer.df = 'satterthwaite', type = "response")
#Same effects are significant/non-sig - go with larger sample

wwPairwise_df6 <- as.data.frame(wwPairwise6_2, which = 2)

#Try order interaction
# wwPairwise7 <- emmeans(gammaWWtime5, pairwise ~ Condition:Order, adjust="tukey", 
#                        lmer.df = 'satterthwaite', type = "response")

wwPairwise_df <- as.data.frame(wwPairwise6_2, which = 2)

#Save pairwise comparisons as an Excel file for easier examination
write.csv(wwPairwise_df, 
          "\\\\nask.man.ac.uk\\home$\\Documents\\R_PhD\\dataAnalysis\\StudentCompExp\\Tables\\gamma_wwFinal_cond~tStyle_51.csv",
          row.names = FALSE)

#### Visualise interactions ####

#WHOLE WORD

#Condition ~ PHQ9 (final)
condPHQ9_ww <- wwDatamut %>%
  ggplot(aes(x = PHQ9, y = wholeWordTime, colour = Condition)) +
  ggtitle("Correlation between depression and whole word time across conditions") +
  xlab("Depression (PHQ9) Score") +
  ylab("Time to type whole word (secs)") +
  #geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  #geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_colour_discrete(labels = c("base" = "Baseline", "control" = "Control",
                                   "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))

#FIRST PRESS

#Plot the PHQ9 scores with first presses in AOMIspec condition (as this was a sig
#interaction) - interim
PHQ9fpPlot <-ggplot(data = fpDatamut[fpDatamut$Condition == "AOMIspec" | fpDatamut$Condition == "base",], 
                    aes(x = PHQ9, y = firstPress, colour = Condition)) +
  ggtitle("Correlation between depression scores and reaction times in AOMIspec") +
  xlab("Depression (PHQ9) Score") +
  ylab("Time of first key press on each trial (ms)") +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=9, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#This suggests that the higher the PHQ9 score, the more likely Ps were to have slower
#RTs in AOMIspec relative to the baseline

#Let's compare this to AOMIgen which was very not-significant, so there should be less
#of a relationship - interim
PHQ9fpPlot2 <-ggplot(data = fpDatamut[fpDatamut$Condition == "AOMIgen" | fpDatamut$Condition == "base",], 
                     aes(x = PHQ9, y = firstPress, colour = Condition)) +
  ggtitle("Correlation between depression scores and reaction times in AOMIgen") +
  xlab("Depression (PHQ9) Score") +
  ylab("Time of first key press on each trial (ms)") +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=9, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#Relationship is there but smaller

#Let's look at the significant main effect of Order - interim

OrderPlot <- fpDatamut %>%
  group_by(Order) %>%
  dplyr::summarise(meanFP = mean(firstPress)) %>%
  ggplot(aes(x = Order, y = meanFP)) +
  ggtitle("Reaction times across all conditions in each condition order") +
  xlab("Order") +
  ylab("Time of first key press on each trial (ms)") +
  geom_bar(stat="identity") +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#So people were slower in general when they completed the AOMIspec condition first

#Condition ~ typing style interaction

condTStyle_fp <-fpDatamut %>%
  ggplot(aes(x = Condition, y = firstPress, colour = tStyle)) +
  ggtitle("Mean time of first key press on each trial by condition and typing style") +
  xlab("Condition") +
  ylab("Time of first key press (ms)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               size = 0.8,
               geom = "crossbar",
               color = "black",
               position = position_dodge(width=0.75),
               aes(fill = tStyle),
               width = 0.6) +
  #scale_colour_manual(values = c("tStyle"="Typing Style")) +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), #legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_colour_discrete("Typing Style")

#Condition ~ order interaction

condOrder_fp <-fpDatamut %>%
  ggplot(aes(x = Condition, y = firstPress, colour = Order)) +
  ggtitle("Mean time of first key press on each trial by condition and order") +
  xlab("Condition") +
  ylab("Time of first key press (ms)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               size = 0.8,
               geom = "crossbar",
               color = "black",
               position = position_dodge(width=0.75),
               aes(fill = Order),
               width = 0.6) +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), #legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))  

#ACCURACY

#Explore the interaction between control condition and the PHQ9 (interim)

PHQ9ContAccPlot <-ggplot(data = accDatamut[accDatamut$Condition == "control" | accDatamut$Condition == "base",], 
                         aes(x = No.Errors, y = PHQ9, colour = Condition)) +
  ggtitle("Correlation between depression scores and No.Errors in Control condition") +
  xlab("No.Errors") +
  ylab("Depression (PHQ9) Score") +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))

#Look at PHQ9 ~ Order interaction (interim)

PHQ9orderAccPlot <-accDatamut %>%
  ggplot(aes(x = No.Errors, y = PHQ9, colour = Order)) +
  ggtitle("Correlation between depression scores and order of conditions") +
  xlab("No.Errors") +
  ylab("Depression (PHQ9) Score") +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))

# Look at sig effect of PHQ (interim and final)

PHQ9AccPlot <- accDatamut %>%
  ggplot(aes(x = PHQ9, y = No.Errors, colour = Condition)) +
  ggtitle("Correlation between depression and number of errors across conditions") +
  xlab("Depression (PHQ9) Score") +
  ylab("No. Errors") +
  #geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  #geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_colour_discrete(labels = c("base" = "Baseline", "control" = "Control",
                                   "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))

#Look at condition ~ typing style (final) - curiosity, not due to sig effect

condTStyle_acc <-accDatamut %>%
  ggplot(aes(x = Condition, y = No.Errors, colour = tStyle)) +
  ggtitle("Mean number of errors on each trial by condition and typing style") +
  xlab("Condition") +
  ylab("No. Errors")  +
  geom_violin() +
  stat_summary(fun = "mean",
               size = 0.8,
               geom = "crossbar",
               color = "black",
               position = position_dodge(width=0.75),
               aes(fill = tStyle),
               width = 0.6) +
  #scale_colour_manual(values = c("tStyle"="Typing Style")) +
  geom_jitter(alpha = 0.03) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), #legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_colour_discrete("Typing Style")

#IKIs

#Condition ~ typing style interaction

condTStyle_iki <-ikiDatamut2 %>%
  ggplot(aes(x = Condition, y = IKIs, colour = tStyle)) +
  ggtitle("Mean inter-key-interval times (IKIs) by condition and typing style") +
  xlab("Condition") +
  ylab("IKIs (ms)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               size = 0.8,
               geom = "crossbar",
               color = "black",
               position = position_dodge(width=0.75),
               aes(fill = tStyle),
               width = 0.6) +
  #scale_colour_manual(values = c("tStyle"="Typing Style")) +
  geom_jitter(alpha = 0.03) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), #legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_colour_discrete("Typing Style")

#Look at condition ~ order

condOrder_iki <-ikiDatamut %>%
  ggplot(aes(x = Condition, y = IKIs, colour = Order)) +
  ggtitle("Mean inter-key-interval times (IKIs) by condition and order of completion") +
  xlab("Condition") +
  ylab("IKIs (ms)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               size = 0.8,
               geom = "crossbar",
               color = "black",
               position = position_dodge(width=0.75),
               aes(fill = Order),
               width = 0.6) +
  #scale_colour_manual(values = c("tStyle"="Typing Style")) +
  geom_jitter(alpha = 0.03) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), #legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_colour_discrete("Order")

#### Rating scales ####

rateDatamut <- wwDatamut2 %>%
  mutate(ratePerf = as.integer(ratePerf), imageryQ = as.integer(imageryQ),
         VI = as.integer(VI), KI = as.integer(KI))

rateMeans <- rateDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_bcImagery = mean(imageryQ), sd_bcImagery = sd(imageryQ),
                   mean_ratePerf = mean(ratePerf), sd_ratePerf = sd(ratePerf),
                   mean_VI = mean(VI), sd_VI = mean(VI), mean_KI = mean(KI),
                   sd_KI = sd(KI))

#Look at summaries 

#mad = Median Absolute Deviation

rateDatamut %>%
  group_by(Condition) %>%
  filter(Condition == "base" & imageryQ > 2) %>%
  dplyr::count(ParticipantID)

rateDatamut %>%
  group_by(Condition) %>%
  filter(Condition == "control" & imageryQ > 2) %>%
  dplyr::count(ParticipantID)

rateDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(median_ima = median(imageryQ), mad_ima = mad(imageryQ))

rateDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(median_vi = median(VI), mad_VI = mad(VI), median_KI = median(KI),
                   mad_KI = mad(KI))

rateDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(median_perf = median(ratePerf), mad_perf = mad(ratePerf))

#The MAD value for the control condition = 0 (likely because > 50% data poitns 
 #are identical)

#Run Friedman test on typing performance ratings

#https://www.datanovia.com/en/lessons/friedman-test-in-r/

#Make ParticipantID a factor and filter so we just have one rating per condition 
 #per participant

rateDatamut2 <- rateDatamut %>%
  mutate(ParticipantID = factor(ParticipantID)) %>%
  group_by(ParticipantID, Condition) %>%
  slice(1) %>%
  as.data.frame()

perfFried <- friedman.test(ratePerf ~ Condition | ParticipantID, 
                           data = rateDatamut2)
#This runs a Friedman test on unreplicated blocked data (i.e. just one observation
 #for each participant and condition)
perfFried #Sig difference

#Do post-hoc Nemenyi test to find which conditions have differences between them

frdAllPairsNemenyiTest(ratePerf ~ Condition | ParticipantID, 
                              data = rateDatamut2)
# Shows sig difference between control and AOMIgen but no others

#Find effect size

rateDatamut2 <- as.data.frame(rateDatamut2)

friedman_effsize(ratePerf ~ Condition | ParticipantID, data = rateDatamut2)


#Now run Wilcoxon signed rank test on imagery ratings

#Separate by condition

spec_rateDat <- rateDatamut %>%
  filter(Condition == "AOMIspec")

gen_rateDat <- rateDatamut %>%
  filter(Condition == "AOMIgen")

#Filter by unique PIDs because we only need one rating for each person and 
 #condition
spec_rateDat2 <- spec_rateDat %>%
  group_by(ParticipantID) %>%
  slice(1)

gen_rateDat2 <- gen_rateDat %>%
  group_by(ParticipantID) %>%
  slice(1)

#https://data.library.virginia.edu/the-wilcoxon-rank-sum-test/#:~:text=The%20impact%20of%20ties%20means,p%2Dvalue%20with%20ties%E2%80%9D.

viTest <- wilcox.test(spec_rateDat2$VI, gen_rateDat2$VI, paired = TRUE,
                      exact = FALSE) 
#Cannot compute exact p values due to ties in the data
viTest #No sig difference

#Effect sizes for VI

#For this make dataset with both conditions in and with one rating per participant
 #and condition

specgen_rateDat <- rateDatamut %>%
  filter(Condition == "AOMIspec" | Condition == "AOMIgen") %>%
  droplevels %>% #filter doesn't actually drop the levels - we need to do this 
   #for wilcox_effsize to work
  group_by(ParticipantID, Condition) %>%
  slice(1) %>%
  mutate(ParticipantID = factor(ParticipantID)) %>%
  as.data.frame()

specgen_rateDat %>% 
                wilcox_effsize(VI ~ Condition, paired = TRUE)

#Now run for KI

kiTest <- wilcox.test(spec_rateDat2$KI, gen_rateDat2$KI, paired = TRUE,
                     exact = FALSE) 
kiTest #No sig difference

#Effect sizes for KI

specgen_rateDat %>% 
  wilcox_effsize(KI ~ Condition, paired = TRUE)

#Now compare spontaneous imagery ratings in base and control conditions

#Separate by condition

base_rateDat <- rateDatamut %>%
  filter(Condition == "base")
cont_rateDat <- rateDatamut %>%
  filter(Condition == "control")

#Filter by unique PIDs because we only need one rating for each person and 
#condition
base_rateDat2 <- base_rateDat %>%
  group_by(ParticipantID) %>%
  slice(1)

cont_rateDat2 <- cont_rateDat %>%
  group_by(ParticipantID) %>%
  slice(1)

imageryQTest <- wilcox.test(base_rateDat2$imageryQ, cont_rateDat2$imageryQ, 
                            paired = TRUE,
                            exact = FALSE) 
imageryQTest #No sig difference

#Get effect size

#Make joint dataset

baseCont_rateDat <- rateDatamut %>%
  filter(Condition == "base" | Condition == "control") %>%
  droplevels %>% #filter doesn't actually drop the levels - we need to do this 
  #for wilcox_effsize to work
  group_by(ParticipantID, Condition) %>%
  slice(1) %>%
  mutate(ParticipantID = factor(ParticipantID)) %>%
  as.data.frame()

baseCont_rateDat %>% 
  wilcox_effsize(imageryQ ~ Condition, paired = TRUE)


#### Visualise ratings ####

#Aggregate data by count

agg_ratePerf <- rateDatamut2 %>%
  group_by(Condition, ratePerf) %>%
  summarise(count = n()) 

ratePerf_plot <- ggplot(agg_ratePerf) +
  geom_col(aes(x = ratePerf, y = count, fill = Condition)) +
  ggtitle("Ratings of Typing Performance Across Conditions") +
  xlab("Typing Performance Rating") +
  xlim(1, NA) +
  ylab("Count of Ratings")  +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), 
        axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),
        axis.line = element_line(colour = "black")) +
  scale_fill_discrete(labels = c("Baseline", "Control", "AO+MIgen", "AO+MIspec")) 
                   


  
 
#### Compare IKIs with video speed ####

videoData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/120BPM_videoSpeedData.csv')

#Tidy and wrangle the video speed data

#get a list of column names in the dataset
colnames(videoData)

#make a new dataset based on just the columns we want
keyvData <- dplyr::select(videoData, pracWords, keyboardPrac1.keys, keyboardPrac1.rt)
head(keyvData)

#Get rid of empty first rows. The below removes all rows containing only NAs before
#a value appears in the column 'baseline_words'
keyvDataTidy <- keyvData %>%
  do(.[first(which(!is.na(.$pracWords))):nrow(.),])

#Separate RTs column so on separate rows
keyvDataTidy2 <- keyvDataTidy %>% 
  separate_rows(keyboardPrac1.keys, keyboardPrac1.rt, sep = ",")

#Remove punctuation and spaces from values in RTs column
keyvDataTidy2$keyboardPrac1.rt <- gsub("[","", keyvDataTidy2$keyboardPrac1.rt, fixed=TRUE)
keyvDataTidy2$keyboardPrac1.rt <- gsub("]","", keyvDataTidy2$keyboardPrac1.rt, fixed=TRUE)
keyvDataTidy2$keyboardPrac1.rt <- gsub("[:blank:]","", keyvDataTidy2$keyboardPrac1.rt, fixed=TRUE)

#Remove punctuation from Keys column as above
keyvDataTidy2$keyboardPrac1.keys <- gsub("[[:punct:][:blank:]]","", keyvDataTidy2$keyboardPrac1.keys)

#change RTs column to numeric instead of character
keyvDataTidy2$keyboardPrac1.rt <- as.numeric(keyvDataTidy2$keyboardPrac1.rt)

#find inter-key interval times (IKIs) of key presses from RTs and add in new column
#i.e. this is the difference between RTs for each word, where the first key
#press = 0
videoIKIdata <- keyvDataTidy2 %>%
  group_by(pracWords) %>%
  mutate(IKIs = keyboardPrac1.rt - lag(keyboardPrac1.rt, default = keyboardPrac1.rt[1]))

#Turn any negative numbers in IKIs column into 0 (this indicates the first key press
#of a word)
videoIKIdata[,4][videoIKIdata[, 4] < 0] <- 0

#Turn into ms
videoIKIdata$IKIs <- videoIKIdata$IKIs * 1000

#Find average IKI for each word

videoIKImean <- videoIKIdata %>%
  dplyr::group_by(pracWords) %>%
  dplyr::summarise(mean_iki = mean(IKIs))

#Overall mean
videoIKIdata %>%
  ungroup() %>%
  dplyr::summarise(mean_iki = mean(IKIs))

#Now we want to look at the overall mean IKIs for each condition out of the 
 #participant responses to see whether AO+MI conditions have key presses more 
 #similar to the videos

IKImeans #Defined in first section of script

#### Without HP typist (interim) ####

# We have just 1 HP typist, so tStyle wasn't included as a fixed effect
# Remove this participant and see if this changes the model results at all

fpDataTT <- subset(fpDatamut, ParticipantID!="13")
wwDataTT <- subset(wwDatamut, ParticipantID!="13")
ikiDataTT <- subset(ikiDatamut, ParticipantID!="13")
accDataTT <- subset(accDatamut, ParticipantID!="13")

# Check there's now 24 Ps in each dataset

count(unique(fpDataTT[c("ParticipantID")]))
count(unique(wwDataTT[c("ParticipantID")]))
count(unique(ikiDataTT[c("ParticipantID")]))
count(unique(accDataTT[c("ParticipantID")]))

# FIRST PRESS

gammaFPtime4_id <- glmer(firstPress ~ Condition * PHQ9 * Order + 
                           (1 | Word) + 
                           (1 + Condition * PHQ9 | ParticipantID), 
                         # additional (1 | Participant) term is not required:
                         # random intercepts for participants are captured as part of
                         # (1 + Condition | ParticipantID)
                         family = Gamma(link = "identity"),
                         nAGQ = 0,
                         glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                         data = fpDataTT)

#Warning message:
#In UseMethod("depth") :
 # no applicable method for 'depth' applied to an object of class "NULL"

#As it seems running the models might post issues (according to the above), let's 
 #visualise P13's data compared to the other Ps and see how it looks

P13_fp_plot <-fpDatamut %>%
  ggplot(aes(x = reorder(ParticipantID, firstPress), y = firstPress, colour = ParticipantID)) +
  ggtitle("Time of first key press for each participant") +
  xlab("Participant ID") +
  ylab("Time of first key press (ms)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#P13 is right in the middle so probably not skewing the findings

# Now let's check P13's performance across conditions

P13_fpcond_plot <-ggplot(data = fpDatamut[fpDatamut$ParticipantID == "13",],
                          aes(x = Condition, y = firstPress, colour = Condition)) +
  ggtitle("Time of first key press across conditions for P13") +
  xlab("Condition") +
  ylab("Time of first key press (ms)") +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.5) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#Aside from the baseline/control conditions, the general performance pattern is
 #the same as the mean for all Ps

# WHOLE WORD TIME

P13_ww_plot <-wwDatamut %>%
  ggplot(aes(x = reorder(ParticipantID, wwTime), y = wwTime, colour = ParticipantID)) +
  ggtitle("Time to type whole word for each participant") +
  xlab("Participant ID") +
  ylab("Time to type whole word (secs)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#Again, P13 unlikely to be affecting the findings

#Check P13's performance across conditions

P13_wwcond_plot <-ggplot(data = wwDatamut[wwDatamut$ParticipantID == "13",],
                         aes(x = Condition, y = wwTime, colour = Condition)) +
  ggtitle("Time to type whole word across conditions for P13") +
  xlab("Condition") +
  ylab("Time to type whole word (secs)") +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.5) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#P was slightly slower in AOMIspec compared to AOMIgen, which is opposite to the
 #overall Ps pattern, but differences are small. Should try a model without their
 #data to see if this impacts findings

gammaWW_noHP <- glmer(wwTime ~ Condition * PHQ9 +
                        (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                      family = Gamma, nAGQ = 0, data = wwDataTT)
summary(gammaWW_noHP)

emmeans(gammaWW_noHP, pairwise ~ Condition*PHQ9, adjust="tukey", lmer.df = 'satterthwaite')

# IKIs

P13_iki_plot <-ikiDatamut %>%
  ggplot(aes(x = reorder(ParticipantID, IKIs), y = IKIs, colour = ParticipantID)) +
  ggtitle("Inter-key-interval (IKI) times for each participant") +
  xlab("Participant ID") +
  ylab("IKIs (ms)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))

#Check P13's performance across conditions

P13_ikicond_plot <-ggplot(data = ikiDatamut[ikiDatamut$ParticipantID == "13",],
                         aes(x = Condition, y = IKIs, colour = Condition)) +
  ggtitle("Inter-key-interval times (IKIs) across conditions for P13") +
  xlab("Condition") +
  ylab("IKIs (ms)") +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.5) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#Pattern is the same as the overall Ps' means

# ACCURACY

P13_acc_plot <-accDatamut %>%
  ggplot(aes(x = reorder(ParticipantID, No.Errors), y = No.Errors, colour = ParticipantID)) +
  ggtitle("No. errors made for each participant") +
  xlab("Participant ID") +
  ylab("No. Errors")  +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#One of the more accurate Ps

#Check P13's performance across conditions

P13_acccond_plot <-ggplot(data = accDatamut[accDatamut$ParticipantID == "13",],
                          aes(x = Condition, y = No.Errors, colour = Condition)) +
  ggtitle("No. errors made across conditions for P13") +
  xlab("Condition") +
  ylab("No. Errors") +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  #geom_jitter(alpha = 0.5) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9))
#Very different to overall means - more errors made in control condition and none
 #in any others. Try to run model.

poisAcc_noHP <- glmer(No.Errors ~ Condition * PHQ9 * Order + 
                   (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                 family = poisson, nAGQ = 0, data = accDataTT)

summary(poisAcc_noHP)
summary(poisAcc_noHP)$coefficients

emmeans(poisAcc_noHP, pairwise ~ Condition*PHQ9*Order, adjust="tukey", lmer.df = 'satterthwaite')

#Overall the models excluding this P have very similar results to those including
 #them - decision made to include this P in interim analyses

#To get effect sizes: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html

#### Models excluding HP typists (final) ####

accDataTouch <- accDatamut %>%
  filter(tStyle == "touch")

#Test the above worked
unique(accDataTouch$tStyle)
count(unique(accDataTouch[c("tStyle")]))

#Count participants left in dataset
count(unique(accDataTouch[c("ParticipantID")])) #n=46

#ACCURACY

poisAccTouch <- glmer(No.Errors ~ Condition + PHQ9 
                  + Condition:PHQ9 + Order + Condition:Order + 
                    (1|Word) + (1 + Condition * PHQ9|ParticipantID),
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                  family = poisson, nAGQ = 0, data = accDataTouch)

summary(poisAccTouch)

#Pairwise comparisons
accPairwiseTouch <- emmeans(poisAccTouch, pairwise ~ Condition + PHQ9 
                        + Condition:PHQ9 + Order + Condition:Order, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")
#Very similar results to model including H&P typists


#Obtain Cohen's d effect sizes
lme.dscore(poisAccTouch,data= accDataTouch, type = "lme4")
