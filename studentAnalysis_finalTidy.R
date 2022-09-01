### INTERIM/FINAL ANALYSIS FOR STUDENT TYPING EXPERIMENT ###

library(tidyverse)
library(lmerTest) #needed for linear mixed models with p values in output
library(emmeans) #needed for pairwise comparisons
library(fitdistrplus) #for checking data distribution
library(performance) #needed for check_overdispersion
library(PMCMRplus) #needed for Nemenyi-test
library(coin) #needed for wilcox_effsize()
library(rstatix) #needed for friedman_effsize()

#### Import the data into R ####

#Change the below to data file links on GitHub

#Tidied and filtered data of final sample (51 Ps)

#Accuracy data
accData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_acc_v2.csv')
#First press data
fpData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_fp_v2.csv')
#Inter-key-interval times (IKIs)
ikiData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_iki_v2.csv')
#Whole word time
wwData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_v2_ww.csv')

#Should be 51 IDs for the below
count(unique(accData2[c("ParticipantID")]))
count(unique(fpData2[c("ParticipantID")]))
count(unique(ikiData2[c("ParticipantID")]))
count(unique(wwData2[c("ParticipantID")]))

# Change column types for analysis in each dataset
fpDatamut2 <- fpData2 %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
         #level the factor of condition according to treatment levels
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         firstPress = as.numeric(firstPress), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle))

ikiDatamut2 <- ikiData2 %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         IKIs = as.numeric(IKIs), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle))

wwDatamut2 <- wwData2 %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         wwTime = as.numeric(wholeWordTime), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle))

accDatamut2 <- accData2 %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")), 
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

#Count number of Hunt-and-Peck (HP) typists in each dataset
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

#First press data

fpPlot <-fpDatamut2 %>%
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

fpPlot

#Whole word data

wwPlot <-wwDatamut2 %>%
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

wwPlot

#IKIs data

ikiPlot <-ikiDatamut2 %>%
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

ikiPlot

#Accuracy data

accPlot <-accDatamut2 %>%
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

accPlot

#Check for missing data, as being flagged by ggplot
accMissing <- accDatamut2 %>%
  filter(is.na(No.Errors))

#Look at sum of total errors made in each condition

accSum <- accDatamut2 %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(totalErrors = sum(No.Errors))

accPlot2 <- accSum %>%
  ggplot(aes(x = Condition, y = totalErrors, fill = Condition)) +
  scale_fill_manual(values=c("tomato1", "yellowgreen", "mediumturquoise", "mediumpurple1")) +
  ggtitle("Total errors made in each condition") +
  xlab("Condition") +
  ylab("Sum of errors made by all participants")  +
  geom_col() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none")

accPlot2

#### Distribution checks ####

#See which distributions best fit the data

#Whole word time
wwDist <- descdist(wwDatamut2$wholeWordTime, discrete = FALSE)
#Gamma

#First press time
fpDist <- descdist(fpDatamut2$firstPress, discrete = FALSE)
#Gamma/lognormal

# IKIs
ikiDist <- descdist(ikiDatamut2$IKIs, discrete = FALSE)
#Gamma/lognormal

# Accuracy
accDist <- descdist(accDatamut2$No.Errors, discrete = TRUE)
#Poisson/negative binomial


#### Mixed effect models ####

#These are the final model structures chosen for maximum convergence, no singularity
 #warnings and maximal model fit according to AIC values

### Whole word time ###

gammaWWtime <- glmer(wwTime ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                      + Condition:PHQ9 + Order + Condition:Order  + 
                        (1|Word) + (1 + Condition | ParticipantID),
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                      family = Gamma(link = "log"), nAGQ = 1, data = wwDatamut2)

summary(gammaWWtime)

# #Build a null model
gammaWWtime_null <- glmer(wwTime ~ 1 + (1|Word) + (1 + Condition | ParticipantID),
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 500000)),
                           family = Gamma(link = "log"),
                           nAGQ = 1, data = wwDatamut2)

#Compare models
anova(gammaWWtime, gammaWWtime_null)


#Generate effect sizes for model output

#(Counterbalanced design) - note that participant X target intercept variance
#and participant X target intercept-slope covariance cannot be estimated because
#they are confounded with residual error variance (Westfall et al., 2014)

#Baseline X AOMIspec
effSize_ww_baseSpec <- (0.167634/sqrt(0.009620 + 0.005572 + 0.013582 + 0.009986 + 0.002562 + 0.055291)) #AMEND THESE 

#Try with base/control comparison
effSize_ww_baseCont <- (-0.046680/sqrt(0.009620 + 0.005572 + 0.013582 + 0.009986 + 0.002562 + 0.055291))

#Base X AOMIgen
effSize_ww_baseGen <- (0.137798/sqrt(0.009620 + 0.005572 + 0.013582 + 0.009986 + 0.002562 + 0.055291))


#Make pairwise comparisons
wwPairwise1 <- emmeans(gammaWWtime, pairwise ~ Condition, adjust="tukey",
                         lmer.df = 'satterthwaite', type = "response")

#Look at relevant interactions
wwPairwise2 <- emmeans(gammaWWtime, pairwise ~ Condition:tStyle, adjust="tukey", 
                         lmer.df = 'satterthwaite', type = "response")

#Generate effect sizes for post-hoc comparisons

#Get SDs for model
VarCorr(gammaWWtime)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_ww <- sqrt(0.050614 + 0.098080 + 0.074647 + 0.116542 + 0.099930 + 0.235141) #AMEND

wwPairwise1_eff <- eff_size(wwPairwise1$emmeans, sigma = totSD_ww, edf = 50)
wwPairwise2_eff <- eff_size(wwPairwise2$emmeans, sigma = totSD_ww, edf = 50)
#edf selected as ParticipantID degrees of freedom


### First press times ###

gammaFPtime <- glmer(firstPress ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                           + Condition:PHQ9 + Order + Condition:Order + (1 | Word) + 
                             (0 + Condition | ParticipantID), 
                           family = Gamma(link = "identity"),
                           nAGQ = 1,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                           data = fpDatamut2)
#Identity link can be suitable for reaction times when values are far from 0

summary(gammaFPtime)

#Compare to null model

gammaFPtime_null <- glmer(firstPress ~ 1 + (1 | Word) + 
                             (0 + Condition | ParticipantID), 
                           family = Gamma(link = "identity"),
                           nAGQ = 1,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                           data = fpDatamut2)

#Compare models
anova(gammaFPtime, gammaFPtime_null)


#Generate effect sizes for model output

#(Counterbalanced design) - note that participant X target intercept variance
#and participant X target intercept-slope covariance cannot be estimated because
#they are confounded with residual error variance (Westfall et al., 2014)

#Formula: Mean1 - mean2 (estimated variation from intercept)/ 
#square root of: participant intercept variance + participant slope variance for each level of factor + word intercept variance + residual variance

#Baseline X AOMIspec
effSize_fp_baseSpec <- (145.0227/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Try with base/control comparison
effSize_fp_baseCont <- (-37.4774/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Base X AOMIgen
effSize_fp_baseGen <- (106.3996/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Typing style
effSize_fp_tStyle <- (34.0019/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Order
effSize_fp_order <- (23.6134/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Interaction AO+MIspec:typing style
effSize_fp_spectStyle <- (-101.2758/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Interaction AO+MIgen:typing style
effSize_fp_gentStyle <- (-43.2619/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Interaction PHQ9:AO+MIspec
effSize_fp_specPHQ9 <- (4.9296/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Interaction PHQ9:AO+MIgen
effSize_fp_genPHQ9 <- (7.0904/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Interaction order:AO+MIspec
effSize_fp_specOrder <- (-13.1390/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Interaction order:AO+MIgen
effSize_fp_genOrder <- (-29.9286/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Run pairwise comparisons

fp_pairwise1 <- emmeans(gammaFPtime, pairwise ~ Condition,
                       adjust="tukey", lmer.df="satterthwaite")

fp_pairwise2 <- emmeans(gammaFPtime, pairwise ~ Condition:tStyle,
                        adjust="tukey", lmer.df="satterthwaite")

fp_pairwise3 <- emmeans(gammaFPtime, pairwise ~ Condition:Order,
                        adjust="tukey", lmer.df="satterthwaite")

#Get effect sizes for post-hoc comparisons

#Get SDs for model
VarCorr(gammaFPtime)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_fp <- sqrt(17.06470 + 58.87779 + 57.77817 + 75.58172 + 69.86056 + 0.26066)

fp_pairwise1_eff <- eff_size(fp_pairwise1$emmeans, sigma = totSD_fp, edf = 50)
fp_pairwise2_eff <- eff_size(fp_pairwise2$emmeans, sigma = totSD_fp, edf = 50)
fp_pairwise3_eff <- eff_size(fp_pairwise3$emmeans, sigma = totSD_fp, edf = 50)
#edf selected as ParticipantID degrees of freedom  


### IKIs ###

gammaIKIs <- glmer(IKIs ~ Condition + tStyle + Condition:tStyle + PHQ9 
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

summary(gammaIKIs)

#Make null model
gammaIKIs_null <- glmer(IKIs ~ 1 +
                              (1 | Word) + 
                              (1 + Condition | ParticipantID), 
                            # additional (1 | Participant) term is not required:
                            # random intercepts for participants are captured as part of
                            # (1 + Condition | ParticipantID)
                            family = Gamma(link = "log"),
                            nAGQ = 0,
                            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                            data = ikiDatamut2)

anova(gammaIKIs_null, gammaIKIs)

#Generate effect sizes

#Baseline X AOMIspec
effSize_iki_baseSpec <- (0.1010449/sqrt(0.010856 + 0.004171 + 0.016154 + 0.013654 + 0.002832 + 0.206463))

#Try with base/control comparison
effSize_iki_baseCont <- (-0.0676196/sqrt(0.010856 + 0.004171 + 0.016154 + 0.013654 + 0.002832 + 0.206463))

#Base X AOMIgen
effSize_iki_baseGen <- (0.0003012/sqrt(0.010856 + 0.004171 + 0.016154 + 0.013654 + 0.002832 + 0.206463))

#Typing style
effSize_iki_tStyle <- (-0.1442787/sqrt(0.010856 + 0.004171 + 0.016154 + 0.013654 + 0.002832 + 0.206463))

#Run pairwise comparisons

ikiPairwise1 <- emmeans(gammaIKIs, pairwise ~ Condition, adjust="tukey",
                       lmer.df = 'satterthwaite', type = "response")

ikiPairwise2 <- emmeans(gammaIKIs, pairwise ~ Condition:tStyle, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")

ikiPairwise3 <- emmeans(gammaIKIs, pairwise ~ tStyle, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")


#Effect sizes of post-hoc comparisons

#Get SDs for model
VarCorr(gammaIKIs)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_iki <- sqrt(0.053217 + 0.104192 + 0.064585 + 0.127099 + 0.116849 + 0.454382) #AMEND

ikiPairwise1_eff <- eff_size(ikiPairwise1$emmeans, sigma = totSD_iki, edf = 50)
ikiPairwise2_eff <- eff_size(ikiPairwise2$emmeans, sigma = totSD_iki, edf = 50)
ikiPairwise3_eff <- eff_size(ikiPairwise3$emmeans, sigma = totSD_iki, edf = 50)
#edf selected as ParticipantID degrees of freedom 


### Accuracy ###

poisAcc <- glmer(No.Errors ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                    + Condition:PHQ9 + Order + Condition:Order + 
                      (1|Word) + (1 + Condition | ParticipantID),
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                    family = poisson, nAGQ = 1, data = accDatamut2)

summary(poisAcc)

#Build a null model

poisAcc_null <- glmer(No.Errors ~ 1 + 
                         (1|Word) + (1 + Condition | ParticipantID),
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                       family = poisson, nAGQ = 1, data = accDatamut2)

anova(poisAcc_null, poisAcc)

#Check for overdispersion in the Poisson model - none found
check_overdispersion(poisAcc)

# Get effect sizes

#Find estimated residual variance of the model (not generated for non-Gaussian  
 #models)

#To do this we need the grand mean
grandMean <- ((accMeans$mean_acc[1] + accMeans$mean_acc[2] + accMeans$mean_acc[3] + accMeans$mean_acc[4])/4)

#Residual variance estimate:
#Based on formula: w · ln(1 / exp(B0) + 1)
# w = dispersion parameter
# B0 = grand mean
# See Nakagawa & Schielzeth (2010) and Q&A on Stack Exchange
poisResid <- 0.632 * log(1 / (exp(grandMean) + 1))

#Baseline X AOMIspec
effSize_acc_baseSpec <- (0.28156/sqrt(1.44215 + 0.23084 + 1.51265 + 0.76839 + 0.05827 + poisResid))

#Try with base/control comparison
effSize_acc_baseCont <- (-0.45869/sqrt(1.44215 + 0.23084 + 1.51265 + 0.76839 + 0.05827 + poisResid))

#Base X AOMIgen
effSize_acc_baseGen <- (0.18147/sqrt(1.44215 + 0.23084 + 1.51265 + 0.76839 + 0.05827 + poisResid))

#PHQ9
effSize_acc_phq9 <- (-0.09031/sqrt(1.44215 + 0.23084 + 1.51265 + 0.76839 + 0.05827 + poisResid))

#Do pairwise comparisons

accPairwise1 <- emmeans(poisAcc, pairwise ~ Condition, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")

accPairwise2 <- emmeans(poisAcc, pairwise ~ Condition:tStyle, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")


#Effect sizes from post-hoc comparisons

#Get SDs for model
VarCorr(poisAcc)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_acc <- sqrt(0.24138 + 1.20089 + 0.48046 + 1.22990 + 0.87658 + poisResid)

accPairwise1_eff <- eff_size(accPairwise1$emmeans, sigma = totSD_acc, edf = 50)
#edf selected as ParticipantID degrees of freedom 


#### Visualise PHQ-9 interactions ####

#Whole word time

condPHQ9_ww <- wwDatamut2 %>%
  ggplot(aes(x = PHQ9, y = wholeWordTime, colour = Condition)) +
  ggtitle("Correlation between depression and whole word time across conditions") +
  xlab("Depression (PHQ9) Score") +
  ylab("Time to type whole word (secs)") +
  geom_smooth(method='lm', formula= y~x) +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_colour_discrete(labels = c("base" = "Baseline", "control" = "Control",
                                   "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))

# First press time

condPHQ9_fp <- fpDatamut2 %>%
  ggplot(aes(x = PHQ9, y = firstPress, colour = Condition)) +
  ggtitle("Correlation between depression and first press time across conditions") +
  xlab("Depression (PHQ9) Score") +
  ylab("Time of first key press (secs)") +
  geom_smooth(method='lm', formula= y~x) +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_colour_discrete(labels = c("base" = "Baseline", "control" = "Control",
                                   "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))

# IKIs

condPHQ9_iki <- ikiDatamut2 %>%
  ggplot(aes(x = PHQ9, y = IKIs, colour = Condition)) +
  ggtitle("Correlation between depression and inter-key-interval times across conditions") +
  xlab("Depression (PHQ9) Score") +
  ylab("Inter-key-interval times (ms)") +
  geom_smooth(method='lm', formula= y~x) +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_colour_discrete(labels = c("base" = "Baseline", "control" = "Control",
                                   "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))

#Accuracy

condPHQ9_acc <- accDatamut2 %>%
  ggplot(aes(x = PHQ9, y = No.Errors, colour = Condition)) +
  ggtitle("Correlation between depression and number of errors across conditions") +
  xlab("Depression (PHQ9) Score") +
  ylab("No. Errors") +
  geom_smooth(method='lm', formula= y~x) +
  theme(plot.title = element_text(size=12, face="bold"), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_colour_discrete(labels = c("base" = "Baseline", "control" = "Control",
                                   "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))


#### Analyse rating scales data ####

rateDatamut <- wwDatamut2 %>%
  mutate(ratePerf = as.integer(ratePerf), imageryQ = as.integer(imageryQ),
         VI = as.integer(VI), KI = as.integer(KI))

rateMeans <- rateDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_bcImagery = mean(imageryQ), sd_bcImagery = sd(imageryQ),
                   mean_ratePerf = mean(ratePerf), sd_ratePerf = sd(ratePerf),
                   mean_VI = mean(VI), sd_VI = mean(VI), mean_KI = mean(KI),
                   sd_KI = sd(KI))

### Look at summaries ###

#Look at count of people who said they had imagined in the baseline condition
rateDatamut %>%
  group_by(Condition) %>%
  filter(Condition == "base" & imageryQ > 2) %>%
  dplyr::count(ParticipantID)

#Look at count of people who said they had imagined in the control condition
rateDatamut %>%
  group_by(Condition) %>%
  filter(Condition == "control" & imageryQ > 2) %>%
  dplyr::count(ParticipantID)

#Median imagery rating for baseline and control conditions
#Higher = more frequent imagery
rateDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(median_ima = median(imageryQ))

#Median imagery rating in AO+MI conditions
#Higher = more vivid/intense imagery
rateDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(median_vi = median(VI), median_KI = median(KI))

#Median performance rating in all conditions
#Higher = better self-rated performance
rateDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(median_perf = median(ratePerf))


### Run Friedman test on typing performance ratings ###

#First Make ParticipantID a factor and filter so we just have one rating per condition 
#per participant

rateDatamut2 <- rateDatamut %>%
  mutate(ParticipantID = factor(ParticipantID)) %>%
  group_by(ParticipantID, Condition) %>%
  slice(1) %>%
  as.data.frame()

perfFried <- friedman.test(ratePerf ~ Condition | ParticipantID, 
                           data = rateDatamut2)
perfFried

#Do post-hoc Nemenyi test to find which conditions have differences between them

frdAllPairsNemenyiTest(ratePerf ~ Condition | ParticipantID, 
                       data = rateDatamut2)

#Find effect size

rateDatamut2 <- as.data.frame(rateDatamut2)

friedman_effsize(ratePerf ~ Condition | ParticipantID, data = rateDatamut2)


### Run Wilcoxon signed rank test on imagery ratings ###

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

#Run test on visual imagery (VI) ratings

viTest <- wilcox.test(spec_rateDat2$VI, gen_rateDat2$VI, paired = TRUE,
                      exact = FALSE) 
#Cannot compute exact p values due to ties in the data
viTest

#Effect sizes for VI

#For this make dataset with both conditions in and with one rating per participant
#and condition

specgen_rateDat <- rateDatamut %>%
  filter(Condition == "AOMIspec" | Condition == "AOMIgen") %>%
  droplevels %>% 
  group_by(ParticipantID, Condition) %>%
  slice(1) %>%
  mutate(ParticipantID = factor(ParticipantID)) %>%
  as.data.frame()

specgen_rateDat %>% 
  wilcox_effsize(VI ~ Condition, paired = TRUE)

#Now run test for kinaesthetic imagery (KI) ratings

kiTest <- wilcox.test(spec_rateDat2$KI, gen_rateDat2$KI, paired = TRUE,
                      exact = FALSE) 
kiTest 

#Effect sizes for KI

specgen_rateDat %>% 
  wilcox_effsize(KI ~ Condition, paired = TRUE)


### Compare spontaneous imagery ratings in baseline and control conditions ###

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

#Run test
imageryQTest <- wilcox.test(base_rateDat2$imageryQ, cont_rateDat2$imageryQ, 
                            paired = TRUE,
                            exact = FALSE) 
imageryQTest

#Get effect size

#Make joint dataset

baseCont_rateDat <- rateDatamut %>%
  filter(Condition == "base" | Condition == "control") %>%
  droplevels %>% 
  group_by(ParticipantID, Condition) %>%
  slice(1) %>%
  mutate(ParticipantID = factor(ParticipantID)) %>%
  as.data.frame()

baseCont_rateDat %>% 
  wilcox_effsize(imageryQ ~ Condition, paired = TRUE)


#### Visualise performance ratings ####

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
