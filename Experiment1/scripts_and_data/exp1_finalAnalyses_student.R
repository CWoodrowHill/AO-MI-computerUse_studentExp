### INTERIM/FINAL ANALYSIS FOR STUDENT TYPING EXPERIMENT ###

library(tidyverse)
library(lmerTest) #needed for linear mixed models with p values in output
library(emmeans) #needed for pairwise comparisons
library(fitdistrplus) #for checking data distribution
library(performance) #needed for check_overdispersion and check_model
library(PMCMRplus) #needed for Nemenyi-test
library(coin) #needed for wilcox_effsize()
library(rstatix) #needed for friedman_effsize()
library(viridis) #for viridis colour palette in plots
library(MASS) #needed for glm.nb

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
  ggplot(aes(x = Condition, y = firstPress, colour = Condition, fill = Condition)) +
  ggtitle("Mean time of first key press on each trial by condition") +
  xlab("Condition") +
  ylab("Time of first key press (ms)\n")  +
  geom_violin(alpha = 0.3) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)


fpPlot

#Whole word data

wwPlot <-wwDatamut2 %>%
  ggplot(aes(x = Condition, y = wwTime, colour = Condition, fill = Condition)) +
  ggtitle("Mean time to type whole word by condition") +
  xlab("Condition") +
  ylab("Time to type whole word (secs)\n")  +
  geom_violin(alpha = 0.3) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)

wwPlot

#IKIs data

ikiPlot <-ikiDatamut2 %>%
  ggplot(aes(x = Condition, y = IKIs, colour = Condition, fill = Condition)) +
  ggtitle("Mean inter-key-interval times (IKIs) by condition") +
  xlab("Condition") +
  ylab("IKIs (ms)\n")  +
  geom_violin(alpha = 0.3) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.08) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none",
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)

ikiPlot

#Accuracy data

accPlot <-accDatamut2 %>%
  ggplot(aes(x = Condition, y = No.Errors, colour = Condition, fill = Condition)) +
  ggtitle("Mean number of errors on each trial by condition") +
  xlab("Condition") +
  ylab("No. Errors\n")  +
  geom_violin(alpha = 0.3) +
  ylim(0, max(accDatamut2$No.Errors)) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)

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

###### Whole word time #####

gammaWWtime <- glmer(wwTime ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                      + Condition:PHQ9 + Order + Condition:Order  + 
                        (1|Word) + (1 + Condition | ParticipantID),
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                      family = Gamma(link = "log"), nAGQ = 1, data = wwDatamut2)

summary(gammaWWtime)

#Re-level the condition factor to see if this changes results

wwDatamut3 <- wwDatamut2
wwDatamut3$Condition <- stats::relevel(wwDatamut2$Condition, "AOMIspec")

#Update model

gammaWWtime_rl <- stats::update(gammaWWtime, data = wwDatamut3)

summary(gammaWWtime_rl)

#Re-scale PHQ9 due to warning:
#"Model is nearly unidentifiable: very large eigenvalue - Rescale variables?""

wwDatamut3$PHQ9 <- scale(wwDatamut3$PHQ9)

#Update model
gammaWWtime_rl_scale <- stats::update(gammaWWtime_rl, data = wwDatamut3)
summary(gammaWWtime_rl_scale) #no warning!!

#Check assumptions
check_model(gammaWWtime_rl_scale)

# #Build a null model
gammaWWtime_null <- glmer(wwTime ~ 1 + (1|Word) + (1 + Condition | ParticipantID),
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 500000)),
                           family = Gamma(link = "log"),
                           nAGQ = 1, data = wwDatamut3)

#Compare models
anova(gammaWWtime_rl_scale, gammaWWtime_null)


#Generate effect sizes for model output

#(Counterbalanced design) - note that participant X target intercept variance
#and participant X target intercept-slope covariance cannot be estimated because
#they are confounded with residual error variance (Westfall et al., 2014)

#Baseline X AOMIspec
#effSize_ww_specBase <- (-0.2076851/sqrt(0.002562 + 0.011139 + 0.009985 + 0.007320 + 0.008512 + 0.055292)) 

#Try with base/control comparison
effSize_ww_specCont <- (-0.2358461/sqrt(0.002562 + 0.011139 + 0.009985 + 0.007320 + 0.008512 + 0.055292))

#Base X AOMIgen
#effSize_ww_specGen <- (-0.0005113/sqrt(0.002562 + 0.011139 + 0.009985 + 0.007320 + 0.008512 + 0.055292))


#Make pairwise comparisons
#df = Inf because we are using the Satterthwaite method, which tells emmeans to use
 #the z statistic instead of t

wwPairwise1 <- emmeans(gammaWWtime_rl_scale, pairwise ~ Condition, adjust="tukey",
                         lmer.df = 'satterthwaite', type = "response")

#Look at relevant interactions
# wwPairwise2 <- emmeans(gammaWWtime, pairwise ~ Condition:tStyle, adjust="tukey", 
#                          lmer.df = 'satterthwaite', type = "response")

#Generate effect sizes for post-hoc comparisons
#See here: https://rdrr.io/cran/emmeans/man/eff_size.html

#Get SDs for model
VarCorr(gammaWWtime_rl_scale)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_ww <- sqrt((0.050615^2) + (0.105539^2) + (0.099927^2) + (0.085559^2) + (0.092261^2) + (0.235142^2)) #AMEND

wwPairwise1_eff <- eff_size(wwPairwise1$emmeans, sigma = totSD_ww, edf = Inf)
#wwPairwise2_eff <- eff_size(wwPairwise2$emmeans, sigma = totSD_ww, edf = 50)
#edf selected as ParticipantID degrees of freedom


##### First press times ####

gammaFPtime <- glmer(firstPress ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                           + Condition:PHQ9 + Order + Condition:Order + (1 | Word) + 
                             (0 + Condition | ParticipantID), 
                           family = Gamma(link = "identity"),
                           nAGQ = 1,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                           data = fpDatamut2)
#Identity link can be suitable for reaction times when values are far from 0

summary(gammaFPtime)

#Check assumptions
check_model(gammaFPtime)

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
#effSize_fp_tStyle <- (34.0019/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Order
effSize_fp_order <- (23.6134/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Interaction AO+MIspec:typing style
#effSize_fp_spectStyle <- (-101.2758/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

#Interaction AO+MIgen:typing style
#effSize_fp_gentStyle <- (-43.2619/sqrt(3467 + 3338 + 5713 + 4880 + 291.2 + 0.06794))

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

#Try emtrends to explore PHQ-9 interaction: https://search.r-project.org/CRAN/refmans/grafify/html/posthoc_Trends_Pairwise.html

# fp_pairwise2 <- emmeans(gammaFPtime, pairwise ~ Condition:tStyle,
#                         adjust="tukey", lmer.df="satterthwaite")

fp_pairwise3 <- emmeans(gammaFPtime, pairwise ~ Condition:Order,
                        adjust="tukey", lmer.df="satterthwaite")

fp_trends <- emtrends(gammaFPtime, "Condition", var = "PHQ9",
                                     adjust = "bonferroni",
                                     lmer.df="satterthwaite")
pairs(fp_trends)


#Get effect sizes for post-hoc comparisons

#Get SDs for model
VarCorr(gammaFPtime)

#Combine SDs to get overall variance in model (this will form sigma)
#Square them before sqrt according to this post: 
#https://stackoverflow.com/questions/63848755/standardized-effect-sizes-interpretation-using-emmeans
totSD_fp <- sqrt((17.06470^2) + (58.87779^2) + (57.77817^2) + (75.58172^2) + (69.86056^2) + (0.26066^2))

fp_pairwise1_eff <- eff_size(fp_pairwise1$emmeans, sigma = totSD_fp, edf = Inf, method = "tukey")
#fp_pairwise2_eff <- eff_size(fp_pairwise2$emmeans, sigma = totSD_fp, edf = Inf)
fp_pairwise3_eff <- eff_size(fp_pairwise3$emmeans, sigma = totSD_fp, edf = Inf)
fp_trends_eff <- eff_size(fp_trends, sigma = totSD_fp, edf = Inf)

#edf = 50 selected as ParticipantID degrees of freedom  
#edf = Inf means sigma is exactly known


##### IKIs ####

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

#Re-level the condition factor to see if this changes results

ikiDatamut3 <- ikiDatamut2
ikiDatamut3$Condition <- stats::relevel(ikiDatamut2$Condition, "AOMIspec")

#Update model

gammaIKIs_rl <- stats::update(gammaIKIs, data = ikiDatamut3)

summary(gammaIKIs_rl)

#Check assumptions
check_model(gammaIKIs_rl)


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
                            data = ikiDatamut3)

anova(gammaIKIs_null, gammaIKIs_rl)

#Generate effect sizes

#(Counterbalanced design) - note that participant X target intercept variance
#and participant X target intercept-slope covariance cannot be estimated because
#they are confounded with residual error variance (Westfall et al., 2014)

#Formula: Mean1 - mean2 (estimated variation from intercept)/ 
#square root of: participant intercept variance + participant slope variance for each level of factor + word intercept variance + residual variance

#Baseline X AOMIspec
effSize_iki_specBase <- (-0.101045/sqrt(0.014581 + 0.013653 + 0.007330 + 0.004508 + 0.002832 + 0.206463))

#Try with base/control comparison
effSize_iki_specCont <- (-0.168665/sqrt(0.014581 + 0.013653 + 0.007330 + 0.004508 + 0.002832 + 0.206463))

#Base X AOMIgen
effSize_iki_specGen <- (-0.100744/sqrt(0.014581 + 0.013653 + 0.007330 + 0.004508 + 0.002832 + 0.206463))

#Typing style
#effSize_iki_tStyle <- (-0.1442787/sqrt(0.010856 + 0.004171 + 0.016154 + 0.013654 + 0.002832 + 0.206463))

#PHQ-9
effSize_iki_phq9 <- (0.006947/sqrt(0.014581 + 0.013653 + 0.007330 + 0.004508 + 0.002832 + 0.206463))

#Run pairwise comparisons

ikiPairwise1 <- emmeans(gammaIKIs_rl, pairwise ~ Condition, adjust="tukey",
                       lmer.df = 'satterthwaite', type = "response")

# ikiPairwise2 <- emmeans(gammaIKIs, pairwise ~ Condition:tStyle, adjust="tukey",
#                         lmer.df = 'satterthwaite', type = "response")

# ikiPairwise3 <- emmeans(gammaIKIs, pairwise ~ tStyle, adjust="tukey",
#                         lmer.df = 'satterthwaite', type = "response")

#Test main effect of PHQ-9 trend in each condition level
iki_trends <- emtrends(gammaIKIs_rl, ~Condition, var = "PHQ9",
                            adjust = "bonferroni",
                            lmer.df="satterthwaite")
#Get p values
iki_trends_p <- test(iki_trends)

#Effect sizes of post-hoc comparisons

#Get SDs for model
VarCorr(gammaIKIs_rl)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_iki <- sqrt((0.053217^2) + (0.120750^2) + (0.116848^2) + (0.085617^2) + (0.067141^2) + (0.454382^2)) #AMEND

ikiPairwise1_eff <- eff_size(ikiPairwise1$emmeans, sigma = totSD_iki, edf = Inf)
# ikiPairwise2_eff <- eff_size(ikiPairwise2$emmeans, sigma = totSD_iki, edf = 50)
# ikiPairwise3_eff <- eff_size(ikiPairwise3$emmeans, sigma = totSD_iki, edf = 50)
#edf selected as ParticipantID degrees of freedom 
#iki_trends_eff <- eff_size(iki_trends, sigma = totSD_iki, edf = Inf)

#Get correlation between IKIs in AO+MIgen and PHQ9 so we can get r as an effect size
# of the marginal main effect

ikiDatamut4 <- ikiDatamut3 %>%
  dplyr::filter(Condition == "AOMIgen")

cor(ikiDatamut4$IKIs, ikiDatamut4$PHQ9, method = "pearson")


##### Accuracy ####

poisAcc <- glmer(No.Errors ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                    + Condition:PHQ9 + Order + Condition:Order + 
                      (1|Word) + (1 + Condition | ParticipantID),
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                    family = poisson, nAGQ = 0, data = accDatamut2)
#Singular fit warning with nAGQ=1
summary(poisAcc)

#Re-level the condition factor to see if this changes results

accDatamut3 <- accDatamut2
accDatamut3$Condition <- stats::relevel(accDatamut2$Condition, "AOMIspec")

#Update model

poisAcc_rl <- stats::update(poisAcc, data = accDatamut3)

summary(poisAcc_rl)
#No significant effects

#Failed to converge - re-scale PHQ-9 and see if this helps

# accDatamut3$PHQ9 <- scale(accDatamut3$PHQ9)
# 
# #Update model
# poisAcc_rl_scale <- stats::update(poisAcc_rl, data = accDatamut3)
# summary(poisAcc_rl_scale) #no warning!! No significant effects

#Check assumptions
performance::check_model(poisAcc_rl)

#Check for overdispersion in the Poisson model
check_overdispersion(poisAcc_rl)
#Model has overdispersion

#Build negative binomial model instead

nbAcc <- lme4::glmer.nb(No.Errors ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                        + Condition:PHQ9 + Order + Condition:Order + 
                          (1|Word) + (1 + Condition | ParticipantID),
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                        nAGQ = 0, data = accDatamut2)
summary(nbAcc)

#Re-level to see whether this alters findings

nbAcc_rl <- stats::update(nbAcc, data = accDatamut3)

summary(nbAcc_rl) #Still no sig effects

#Build a null model

nbAcc_null <- lme4::glmer.nb(No.Errors ~ 1 + 
                               (1|Word) + (1 + Condition | ParticipantID),
                             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                             nAGQ = 0, data = accDatamut3)

anova(nbAcc_rl, nbAcc_null)
#No sig difference

#Check assumptions

performance::check_model(nbAcc_rl)
performance::check_zeroinflation(nbAcc_rl)


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
  ggplot(aes(x = PHQ9, y = firstPress, fill = Condition, colour = Condition)) +
  ggtitle("Relationship between depression and first press time across conditions") +
  xlab("Depression (PHQ-9) Score") +
  ylab("Time of first key press (ms)\n") +
  geom_smooth(method='lm', formula= y~x, alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_colour_viridis(discrete = TRUE, labels = c("base" = "Baseline", "control" = "Control",
                                                   "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, labels = c("base" = "Baseline", "control" = "Control",
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

PMCMRplus::frdAllPairsNemenyiTest(ratePerf ~ Condition | ParticipantID, 
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

#### Analyse participant variation ####

#We'll look at whether within-participant performance variation varies across 
 #conditions

##### Whole word times ####

#Make data
wwDatvar <- wwDatamut2 %>%
  dplyr::group_by(ParticipantID, Condition, Order, tStyle, PHQ9) %>%
  dplyr::summarise(sd_wwtimes = sd(wholeWordTime))

#Check distribution
fitdistrplus::descdist(wwDatvar$sd_wwtimes) #lognormal distibutrion, maybe gamma

#Linear/lognormal model
wwvar_mod <- lm(sd_wwtimes ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                + Condition:PHQ9 + Order + Condition:Order,
                data = wwDatvar)
summary(wwvar_mod)
AIC(wwvar_mod)

#Compare with gamma
wwvar_modGam <- glm(sd_wwtimes ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                + Condition:PHQ9 + Order + Condition:Order,
                family = Gamma(link = "log"),
                data = wwDatvar)
summary(wwvar_modGam)
#AIC is smaller, use gamma output. No sig effects

performance::check_model(wwvar_modGam)
#Assumptions look good.

##### First press times ####

#Make data
fpDatvar <- fpDatamut2 %>%
  dplyr::group_by(ParticipantID, Condition, Order, tStyle, PHQ9) %>%
  dplyr::summarise(sd_fptimes = sd(firstPress))

#Check distribution
fitdistrplus::descdist(fpDatvar$sd_fptimes) #Gamma distribution

fpvar_modGam <- glm(sd_fptimes ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                    + Condition:PHQ9 + Order + Condition:Order,
                    family = Gamma(link = "log"),
                    data = fpDatvar)
summary(fpvar_modGam)
#No sig effects

#Try with no additional fixed effects
fpvar_modGam2 <- glm(sd_fptimes ~ Condition,
                    family = Gamma(link = "log"),
                    data = fpDatvar)
summary(fpvar_modGam2)

performance::check_model(fpvar_modGam)
#Assumptions look good.

emmeans::emmeans(fpvar_modGam, pairwise ~ Condition, adjust="tukey",
                         lmer.df = 'satterthwaite', type = "response")

##### IKIs ####

#Make data
ikiDatvar <- ikiDatamut2 %>%
  dplyr::group_by(ParticipantID, Condition, Order, tStyle, PHQ9) %>%
  dplyr::summarise(sd_ikis = sd(IKIs))

#Check distribution
fitdistrplus::descdist(ikiDatvar$sd_ikis) #Gamma distribution

ikivar_modGam <- glm(sd_ikis ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                    + Condition:PHQ9 + Order + Condition:Order,
                    family = Gamma(link = "log"),
                    data = ikiDatvar)
summary(ikivar_modGam)

#Remove additional fixed effects
ikivar_modGam2 <- glm(sd_ikis ~ Condition,
                     family = Gamma(link = "log"),
                     data = ikiDatvar)
summary(ikivar_modGam2) #No sig differences, lower AIC

performance::check_model(ikivar_modGam2)
performance::check_homogeneity(ikivar_modGam2)
#Assumptions look good

##### Accuracy ####

#Make data
accDatvar <- accDatamut2 %>%
  dplyr::group_by(ParticipantID, Condition, Order, tStyle, PHQ9) %>%
  dplyr::summarise(sd_errors = sd(No.Errors))

#Check distribution
#Not discrete anymore because the variance is numeric, not integer
fitdistrplus::descdist(accDatvar$sd_errors) #Lognormal

accvar_mod <- lm(sd_errors ~ Condition + tStyle  + Condition:tStyle + PHQ9 
                     + Condition:PHQ9 + Order + Condition:Order,
                     data = accDatvar)
summary(accvar_mod)
AIC(accvar_mod)

#Remove other fixed effects

accvar_mod2 <- lm(sd_errors ~ Condition,
                 data = accDatvar)
summary(accvar_mod2) #No sig differences
AIC(accvar_mod2) #Lower AIC

performance::check_model(accvar_mod2)

#Gamma not suitable because we have valid 0s in data

#### Plot variation ####

fpvar_modGam %>%
  ggplot(aes(x = Condition, y = sd_fptimes, colour = Condition, fill = Condition)) +
  #ggtitle("Mean time of first key press on each trial by condition") +
  xlab("Condition") +
  ylab("Variation (SD) in time of first key press (ms)\n")  +
  geom_violin(alpha = 0.3) +
  # stat_summary(fun = "mean",
  #              width = 0.8,
  #              geom = "crossbar",
  #              color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("base" = "Baseline", "control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)

