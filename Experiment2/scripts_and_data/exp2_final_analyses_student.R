#Before installing: options("install.lock"=FALSE)

library(tidyverse)
library(lmerTest) #needed for linear mixed models with p values in output
library(emmeans) #needed for pairwise comparisons
library(performance) #needed for check_overdispersion and check_model
library(viridis) #for viridis colour palette in plots

## Final analysis for Experiment 2 student typing study ##

#### Import the data into R ####

#Whole word time
wwData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/dataForAnalyses/study3data_ww.csv')
#First press data
fpData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/dataForAnalyses/study3data_fp.csv')
#Inter-key-interval times (IKIs)
ikiData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/dataForAnalyses/study3data_iki.csv')
#Accuracy data
accData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/dataForAnalyses/study3data_acc.csv')

#Should be 20 IDs for the below
dplyr::count(unique(accData[c("participant")]))
dplyr::count(unique(fpData[c("participant")]))
dplyr::count(unique(ikiData[c("participant")]))
dplyr::count(unique(wwData[c("participant")]))

#Create a column to show which condition was completed first out of AOMIspec and 
 #AOMIgen according to group

wwData2 <- wwData %>%
  dplyr::mutate(firstCond = ifelse(group == "A" | group == "C" | group == "E" | group == "G", "AOMIspec", "AOMIgen"))

fpData2 <- fpData %>%
  dplyr::mutate(firstCond = ifelse(group == "A" | group == "C" | group == "E" | group == "G", "AOMIspec", "AOMIgen"))

ikiData2 <- ikiData %>%
  dplyr::mutate(firstCond = ifelse(group == "A" | group == "C" | group == "E" | group == "G", "AOMIspec", "AOMIgen"))

accData2 <- accData %>%
  dplyr::mutate(firstCond = ifelse(group == "A" | group == "C" | group == "E" | group == "G", "AOMIspec", "AOMIgen"))

#Change stim_speed from the actual speeds to 'fast' and 'slow' labels

wwData2$stim_speed[wwData2$stim_speed == 6.5] <- "fast"
wwData2$stim_speed[wwData2$stim_speed == 19.5] <- "slow"

fpData2$stim_speed[fpData2$stim_speed == 6.5] <- "fast"
fpData2$stim_speed[fpData2$stim_speed == 19.5] <- "slow"

ikiData2$stim_speed[ikiData2$stim_speed == 6.5] <- "fast"
ikiData2$stim_speed[ikiData2$stim_speed == 19.5] <- "slow"

accData2$stim_speed[accData2$stim_speed == 6.5] <- "fast"
accData2$stim_speed[accData2$stim_speed == 19.5] <- "slow"

# Change column types for analysis in each dataset

wwDatamut <- wwData2 %>%
  dplyr::mutate(participant = as.character(participant), firstCond = factor(firstCond), 
         Condition = factor(Condition, c("Baseline", "Control" ,"AOMIgen", "AOMIspec")),
         #level the factor of condition according to treatment levels
         Word = as.character(Word), stim_speed = factor(stim_speed),
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         wholeWordTime = as.numeric(wholeWordTime), PHQ9 = as.integer(PHQ9)
  )

fpDatamut <- fpData2 %>%
  dplyr::mutate(participant = as.character(participant), firstCond = factor(firstCond), 
         Condition = factor(Condition, c("Baseline", "Control" ,"AOMIgen", "AOMIspec")),
         #level the factor of condition according to treatment levels
         Word = as.character(Word), stim_speed = factor(stim_speed),
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         firstPress = as.numeric(firstPress), PHQ9 = as.integer(PHQ9)
  )

ikiDatamut <- ikiData2 %>%
  dplyr::mutate(participant = as.character(participant), firstCond = factor(firstCond), 
         Condition = factor(Condition, c("Baseline", "Control" ,"AOMIgen", "AOMIspec")),
         #level the factor of condition according to treatment levels
         Word = as.character(Word), stim_speed = factor(stim_speed),
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         IKIs = as.numeric(IKIs), PHQ9 = as.integer(PHQ9)
  )

accDatamut <- accData2 %>%
  dplyr::mutate(participant = as.character(participant), firstCond = factor(firstCond), 
         Condition = factor(Condition, c("Baseline", "Control" ,"AOMIgen", "AOMIspec")),
         #level the factor of condition according to treatment levels
         Word = as.character(Word), stim_speed = factor(stim_speed),
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         No.Errors = as.integer(No.Errors), PHQ9 = as.integer(PHQ9)
  )


#### Generate summary data ####

#Time to type whole word

WWmeans <- wwDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_WholeWord = mean(wholeWordTime, na.rm = TRUE), 
                   sd_WholeWord = sd(wholeWordTime, na.rm = TRUE))

WWmeans_speed <- wwDatamut %>%
  dplyr::group_by(Condition, stim_speed) %>%
  dplyr::summarise(mean_WholeWord = mean(wholeWordTime, na.rm = TRUE), 
                   sd_WholeWord = sd(wholeWordTime, na.rm = TRUE))

#Order
WWmeans_order <- wwDatamut %>%
  dplyr::group_by(Condition, firstCond) %>%
  dplyr::summarise(mean_WholeWord = mean(wholeWordTime, na.rm = TRUE), 
                   sd_WholeWord = sd(wholeWordTime, na.rm = TRUE))

#Next is first press time

fpMeans <- fpDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_firstPress = mean(firstPress), 
                   sd_firstPress = sd(firstPress))

fpMeans_speed <- fpDatamut %>%
  dplyr::group_by(Condition, stim_speed) %>%
  dplyr::summarise(mean_firstPress = mean(firstPress), 
                   sd_firstPress = sd(firstPress))

#Order
fpMeans_order <- fpDatamut %>%
  dplyr::group_by(Condition, firstCond) %>%
  dplyr::summarise(mean_WholeWord = mean(firstPress, na.rm = TRUE), 
                   sd_WholeWord = sd(firstPress, na.rm = TRUE))

#Now, IKIs

IKImeans <- ikiDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_IKIs = mean(IKIs, na.rm = TRUE), 
                   sd_IKIs = sd(IKIs, na.rm = TRUE))


IKImeans_speed <- ikiDatamut %>%
  dplyr::group_by(Condition, stim_speed) %>%
  dplyr::summarise(mean_IKIs = mean(IKIs, na.rm = TRUE), 
                   sd_IKIs = sd(IKIs, na.rm = TRUE))

#Order
IKImeans_order <- ikiDatamut %>%
  dplyr::group_by(Condition, firstCond) %>%
  dplyr::summarise(mean_WholeWord = mean(IKIs, na.rm = TRUE), 
                   sd_WholeWord = sd(IKIs, na.rm = TRUE))

#Accuracy data

accMeans <- accDatamut %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_acc = mean(No.Errors), sd_rt = sd(No.Errors))

accMeans_speed <- accDatamut %>%
  dplyr::group_by(Condition, stim_speed) %>%
  dplyr::summarise(mean_acc = mean(No.Errors), sd_rt = sd(No.Errors))

#Order
accMeans_order <- accDatamut %>%
  dplyr::group_by(Condition, firstCond) %>%
  dplyr::summarise(mean_WholeWord = mean(No.Errors, na.rm = TRUE), 
                   sd_WholeWord = sd(No.Errors, na.rm = TRUE))


#### Plot summary data ####

##### Without speed ####

#Whole word data

wwPlot <-wwDatamut %>%
  ggplot(aes(x = Condition, y = wholeWordTime, colour = Condition, fill = Condition)) +
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
  scale_x_discrete(labels=c("Baseline" = "Baseline", "Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)

#first press data

fpPlot <-fpDatamut %>%
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
  scale_x_discrete(labels=c("Baseline" = "Baseline", "Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)

#IKIs

ikiPlot <-ikiDatamut %>%
  ggplot(aes(x = Condition, y = IKIs, colour = Condition, fill = Condition)) +
  ggtitle("Mean inter-key-interval times by condition") +
  xlab("Condition") +
  ylab("Inter-key-interval times (ms)\n")  +
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
  scale_x_discrete(labels=c("Baseline" = "Baseline", "Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)

#Accuracy

accPlot <-accDatamut %>%
  ggplot(aes(x = Condition, y = No.Errors, colour = Condition, fill = Condition)) +
  ggtitle("Mean number of errors on each trial by condition") +
  xlab("Condition") +
  ylab("Number of typing errors\n")  +
  geom_violin(alpha = 0.3) +
  ylim(0, max(accDatamut$No.Errors)) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("Baseline" = "Baseline", "Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)


##### With speed ####

#filter out baseline condition from datasets as this does not have a speed aspect

wwDataSpeed <- wwDatamut %>%
  dplyr::filter(Condition != "Baseline") %>%
  droplevels()

fpDataSpeed <- fpDatamut %>%
  dplyr::filter(Condition != "Baseline") %>%
  droplevels()

ikiDataSpeed <- ikiDatamut %>%
  dplyr::filter(Condition != "Baseline") %>%
  droplevels()

accDataSpeed <- accDatamut %>%
  dplyr::filter(Condition != "Baseline") %>%
  droplevels()

#Re-label stim_speeds

speed_labels <- as_labeller(c("fast" = "Fast", "slow" = "Slow"))
#https://stackoverflow.com/questions/3472980/how-to-change-facet-labels

#Whole word data

wwPlot_speed <-wwDataSpeed %>%
  ggplot(aes(x = Condition, y = wholeWordTime, colour = Condition, fill = Condition)) +
  ggtitle("Mean time to type whole word by condition\n and stimulus speed") +
  xlab("Condition") +
  ylab("Time to type whole word (secs)\n")  +
  geom_violin(alpha = 0.3) +
  facet_wrap(~stim_speed, labeller = speed_labels) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.92)


#first press data

fpPlot_speed <- fpDataSpeed %>%
  ggplot(aes(x = Condition, y = firstPress, colour = Condition, fill = Condition)) +
  ggtitle("Mean time of first key press by condition and speed") +
  xlab("Condition") +
  ylab("First press time (ms)\n")  +
  geom_violin(alpha = 0.3) +
  facet_wrap(~stim_speed, labeller = speed_labels) +
  geom_jitter(alpha = 0.03) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.92)


#IKIs

ikiPlot_speed <- ikiDataSpeed %>%
  ggplot(aes(x = Condition, y = IKIs, colour = Condition, fill = Condition)) +
  ggtitle("Mean inter-key-interval times by condition and speed") +
  xlab("Condition") +
  ylab("Inter-key-interval times (ms)\n")  +
  geom_violin(alpha = 0.3) +
  facet_wrap(~stim_speed, labeller = speed_labels) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.92)

#Accuracy

accPlot_speed <- accDataSpeed %>%
  dplyr::filter(stim_speed == "slow" | stim_speed == "fast")  %>%
  ggplot(aes(x = Condition, y = No.Errors, colour = Condition, fill = Condition)) +
  ggtitle("Mean number of typing errors per trial by condition\n and speed") +
  xlab("Condition") +
  ylab("Number of typing errors\n")  +
  geom_violin(alpha = 0.3) +
  facet_wrap(~stim_speed, labeller = speed_labels) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)


##### Baseline vs AO+MI conditions in fast & slow conditions ####

#Make datasets with no control condition and re-jig the AO+MI speeds

wwDataSpeed_base <- wwDatamut %>%
  dplyr::filter(Condition != "Control") %>%
  tidyr::unite(conds_combd, c(Condition, stim_speed), remove = FALSE) %>%
  dplyr::select(-c(Condition, stim_speed)) #remove the old columns to prevent confusion

#Rename the baseline condition so we don't have the NA

wwDataSpeed_base$conds_combd[wwDataSpeed_base$conds_combd == "Baseline_NA"] <- "Baseline"

#Make sure the baseline condition is the intercept

wwDataSpeed_base <- wwDataSpeed_base %>%
  dplyr::mutate(conds_combd = factor(conds_combd, c("Baseline", "AOMIgen_slow", "AOMIgen_fast", "AOMIspec_slow", "AOMIspec_fast")))

wwDataSpeed_base$conds_combd <- stats::relevel(wwDataSpeed_base$conds_combd, "Baseline")

#Repeat for other datasets

fpDataSpeed_base <- fpDatamut %>%
  dplyr::filter(Condition != "Control") %>%
  tidyr::unite(conds_combd, c(Condition, stim_speed), remove = FALSE) %>%
  dplyr::select(-c(Condition, stim_speed))

fpDataSpeed_base$conds_combd[fpDataSpeed_base$conds_combd == "Baseline_NA"] <- "Baseline"

fpDataSpeed_base <- fpDataSpeed_base %>%
  dplyr::mutate(conds_combd = factor(conds_combd, c("Baseline", "AOMIgen_slow", "AOMIgen_fast", "AOMIspec_slow", "AOMIspec_fast")))

fpDataSpeed_base$conds_combd <- stats::relevel(fpDataSpeed_base$conds_combd, "Baseline")

ikiDataSpeed_base <- ikiDatamut %>%
  dplyr::filter(Condition != "Control") %>%
  tidyr::unite(conds_combd, c(Condition, stim_speed), remove = FALSE) %>%
  dplyr::select(-c(Condition, stim_speed))

ikiDataSpeed_base$conds_combd[ikiDataSpeed_base$conds_combd == "Baseline_NA"] <- "Baseline"

ikiDataSpeed_base <- ikiDataSpeed_base %>%
  dplyr::mutate(conds_combd = factor(conds_combd, c("Baseline", "AOMIgen_slow", "AOMIgen_fast", "AOMIspec_slow", "AOMIspec_fast")))

ikiDataSpeed_base$conds_combd <- stats::relevel(ikiDataSpeed_base$conds_combd, "Baseline")

accDataSpeed_base <- accDatamut %>%
  dplyr::filter(Condition != "Control") %>%
  tidyr::unite(conds_combd, c(Condition, stim_speed), remove = FALSE) %>%
  dplyr::select(-c(Condition, stim_speed))

accDataSpeed_base$conds_combd[accDataSpeed_base$conds_combd == "Baseline_NA"] <- "Baseline"  

accDataSpeed_base <- accDataSpeed_base %>%
  dplyr::mutate(conds_combd = factor(conds_combd, c("Baseline", "AOMIgen_slow", "AOMIgen_fast", "AOMIspec_slow", "AOMIspec_fast")))

accDataSpeed_base$conds_combd <- stats::relevel(accDataSpeed_base$conds_combd, "Baseline")


## Whole word time ##

wwPlot_speedBase <-wwDataSpeed_base %>%
  ggplot(aes(x = conds_combd, y = wholeWordTime, colour = conds_combd, fill = conds_combd)) +
  ggtitle("Mean time to type whole word by condition and stimulus speed") +
  xlab("\nCondition") +
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
  scale_x_discrete(labels=c("AOMIgen_slow" = "Slow AO+MIgen", "AOMIspec_slow" = "Slow AO+MIspec", "AOMIspec_fast" = "Fast AO+MIspec", "AOMIgen_fast" = "Fast AO+MIgen")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  # scale_fill_viridis(discrete = TRUE, option = "inferno", begin = 0.2, end = 0.8) +
  # scale_colour_viridis(discrete = TRUE, option = "inferno", begin = 0.2, end = 0.8)
  scale_fill_manual(values = c("#440154FF", "#20928CFF", "#25AB82FF", "#85D54AFF", "#C9E120FF")) +
  scale_colour_manual(values = c("#440154FF", "#20928CFF", "#25AB82FF", "#85D54AFF", "#C9E120FF"))


#First press

fpPlot_speedBase <-fpDataSpeed_base %>%
  ggplot(aes(x = conds_combd, y = firstPress, colour = conds_combd, fill = conds_combd)) +
  ggtitle("Mean time of first key press by condition and stimulus speed") +
  xlab("\nCondition") +
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
  scale_x_discrete(labels=c("AOMIgen_slow" = "Slow AO+MIgen", "AOMIspec_slow" = "Slow AO+MIspec", "AOMIspec_fast" = "Fast AO+MIspec", "AOMIgen_fast" = "Fast AO+MIgen")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_manual(values = c("#440154FF", "#20928CFF", "#25AB82FF", "#85D54AFF", "#C9E120FF")) +
  scale_colour_manual(values = c("#440154FF", "#20928CFF", "#25AB82FF", "#85D54AFF", "#C9E120FF"))

#IKIs

ikiPlot_speedBase <-ikiDataSpeed_base %>%
  ggplot(aes(x = conds_combd, y = IKIs, colour = conds_combd, fill = conds_combd)) +
  ggtitle("Mean inter-key-interval times by condition and stimulus speed") +
  xlab("\nCondition") +
  ylab("Inter-key-interval times (ms)\n")  +
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
  scale_x_discrete(labels=c("AOMIgen_slow" = "Slow AO+MIgen", "AOMIspec_slow" = "Slow AO+MIspec", "AOMIspec_fast" = "Fast AO+MIspec", "AOMIgen_fast" = "Fast AO+MIgen")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_manual(values = c("#440154FF", "#20928CFF", "#25AB82FF", "#85D54AFF", "#C9E120FF")) +
  scale_colour_manual(values = c("#440154FF", "#20928CFF", "#25AB82FF", "#85D54AFF", "#C9E120FF"))

#Accuracy
accPlot_speedBase <-accDataSpeed_base %>%
  ggplot(aes(x = conds_combd, y = No.Errors, colour = conds_combd, fill = conds_combd)) +
  ggtitle("Mean number of typing errors by condition and stimulus speed") +
  xlab("\nCondition") +
  ylab("Number of typing errors\n")  +
  geom_violin(alpha = 0.3) +
  ylim(0, max(accDataSpeed_base$No.Errors)) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("AOMIgen_slow" = "Slow AO+MIgen", "AOMIspec_slow" = "Slow AO+MIspec", "AOMIspec_fast" = "Fast AO+MIspec", "AOMIgen_fast" = "Fast AO+MIgen")) +
 # scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_manual(values = c("#440154FF", "#20928CFF", "#25AB82FF", "#85D54AFF", "#C9E120FF")) +
  scale_colour_manual(values = c("#440154FF", "#20928CFF", "#25AB82FF", "#85D54AFF", "#C9E120FF"))


##### Extra plots ####

#These are based on the significant model findings

order_labels <- as_labeller(c("AOMIgen" = "AO+MIgen First", "AOMIspec" = "AO+MIspec First"))

#IKIs, condition:order

ikiPlot_order <- ikiDatamut %>%
  ggplot(aes(x = Condition, y = IKIs, colour = Condition, fill = Condition)) +
  ggtitle("Mean inter-key-interval times by condition\n and condition order") +
  xlab("Condition") +
  ylab("Inter-key-interval times (ms)\n")  +
  geom_violin(alpha = 0.3) +
  facet_wrap(~firstCond, labeller = order_labels) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", end = 0.92)

#IKIs, PHQ-9

ikiPlot_phq9 <- ikiDatamut %>%
 # dplyr::filter(PHQ9 != 1) %>%
  ggplot(aes(x = PHQ9, y = IKIs, colour = Condition, fill = Condition)) +
  ggtitle("Correlation between depression and inter-key-interval times across conditions") +
  xlab("Depression (PHQ9) Score") +
  ylab("Inter-key-interval times (ms)") +
  geom_smooth(method='lm', formula= y~x, alpha = 0.1) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_colour_viridis(discrete = TRUE, labels = c("Control" = "Control",
                                                   "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, labels = c("Control" = "Control",
                                                 "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))


#Whole word time, condition:order

wwPlot_order <-wwDataSpeed %>%
  ggplot(aes(x = Condition, y = wholeWordTime, colour = Condition, fill = Condition)) +
  ggtitle("Mean time to type whole word by condition and order ") +
  xlab("Condition") +
  ylab("Time to type whole word (secs)\n")  +
  geom_violin(alpha = 0.3) +
  facet_wrap(~firstCond, labeller = order_labels) +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9), legend.position="none", 
        axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("Control" = "Control",
                            "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.92) +
  scale_colour_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.92)

#whole word time, PHQ-9

wwPlot_phq9 <- wwDatamut %>%
  ggplot(aes(x = PHQ9, y = wholeWordTime, colour = Condition, fill = Condition)) +
  ggtitle("Correlation between depression and whole word time across conditions") +
  xlab("Depression (PHQ9) Score") +
  ylab("Time to type whole word (secs)") +
  geom_smooth(method='lm', formula= y~x, alpha = 0.1) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_colour_viridis(discrete = TRUE, labels = c("Control" = "Control",
                                                   "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec")) +
  scale_fill_viridis(discrete = TRUE, labels = c("Control" = "Control",
                                                 "AOMIgen" = "AO+MIgen", "AOMIspec" = "AO+MIspec"))

#Whole word time, PHQ-9 with speed

wwPlot_phq9_speed <- wwDataSpeed_base %>%
  ggplot(aes(x = PHQ9, y = wholeWordTime, colour = conds_combd, fill = conds_combd)) +
  ggtitle("Correlation between depression and whole word time across conditions") +
  xlab("Depression (PHQ9) Score") +
  ylab("Time to type whole word (secs)") +
  geom_smooth(method='lm', formula= y~x, alpha = 0.1) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_colour_viridis(discrete = TRUE, labels = c("AOMIgen_slow" = "Slow AO+MIgen", "AOMIspec_slow" = "Slow AO+MIspec", "AOMIspec_fast" = "Fast AO+MIspec", "AOMIgen_fast" = "Fast AO+MIgen")) +
  scale_fill_viridis(discrete = TRUE, labels = c("AOMIgen_slow" = "Slow AO+MIgen", "AOMIspec_slow" = "Slow AO+MIspec", "AOMIspec_fast" = "Fast AO+MIspec", "AOMIgen_fast" = "Fast AO+MIgen"))

#Accuracy, PHQ-9 with speed

accPlot_phq9_speed <- accDataSpeed_base %>%
  ggplot(aes(x = PHQ9, y = No.Errors, colour = conds_combd, fill = conds_combd)) +
  ggtitle("Correlation between depression and number of typing errors across conditions") +
  xlab("Depression (PHQ-9) Score") +
  ylab("Number of errors") +
  geom_smooth(method='lm', formula= y~x, alpha = 0.1) +
  theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5), axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),  axis.line = element_line(colour = "black")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_colour_viridis(discrete = TRUE, labels = c("AOMIgen_slow" = "Slow AO+MIgen", "AOMIspec_slow" = "Slow AO+MIspec", "AOMIspec_fast" = "Fast AO+MIspec", "AOMIgen_fast" = "Fast AO+MIgen")) +
  scale_fill_viridis(discrete = TRUE, labels = c("AOMIgen_slow" = "Slow AO+MIgen", "AOMIspec_slow" = "Slow AO+MIspec", "AOMIspec_fast" = "Fast AO+MIspec", "AOMIgen_fast" = "Fast AO+MIgen"))


#### Mixed effect models - Without Speed Variable ####
#Model 1 in paper

###### Whole word time #####

gammaWWtime <- lme4::glmer(wholeWordTime ~ Condition + PHQ9  + Condition:PHQ9 + 
                             firstCond + Condition:firstCond  + 
                             (1|Word) + (1 + Condition | participant),
                           control = glmerControl(optimizer = "bobyqa", 
                                                  optCtrl = list(maxfun = 500000)),
                           family = Gamma(link = "log"), nAGQ = 1, data = wwDatamut)

summary(gammaWWtime)

#Re-level the condition factor to see if this changes results

wwDatamut2 <- wwDatamut
wwDatamut2$Condition <- stats::relevel(wwDatamut2$Condition, "AOMIspec")

#Update model

gammaWWtime_rl <- stats::update(gammaWWtime, data = wwDatamut2)

summary(gammaWWtime_rl)

#Check assumptions

performance::check_model(gammaWWtime)
#These look pretty good

# #Build a null model
gammaWWtime_null <- lme4::glmer(wholeWordTime ~ 1 + (1|Word) + (1 + Condition | participant),
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 500000)),
                          family = Gamma(link = "log"),
                          nAGQ = 1, data = wwDatamut2)

anova(gammaWWtime_rl, gammaWWtime_null)
#Null has lower AIC and BIC but no sig difference


##### First press times ####

gammaFPtime <- lme4::glmer(firstPress ~ Condition + PHQ9 + Condition:PHQ9 + 
                             firstCond + Condition:firstCond + (1 | Word) + 
                             (1 + Condition | participant), 
                           family = Gamma(link = "log"),
                           nAGQ = 1,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                           data = fpDatamut)

summary(gammaFPtime)

#Compare with a lognormal model

gammaFPtime_ln <- lmerTest::lmer(firstPress ~ Condition + PHQ9 + Condition:PHQ9 + 
                               firstCond + Condition:firstCond + (1 | Word) + 
                               (1 + Condition | participant), 
                             control = lmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 500000)),
                             data = fpDatamut)
summary(gammaFPtime_ln)
AIC(gammaFPtime_ln)
BIC(gammaFPtime_ln)

#Compare model fits

performance::check_model(gammaFPtime)
performance::check_model(gammaFPtime_ln) #lognormal model might be more affected by 
 #influential observations

#Gamma model has lower AIC and BIC, go with that one

#Compare to null model

gammaFPtime_null <- lme4::glmer(firstPress ~ 1 + (1 | Word) + 
                            (1 + Condition | participant), 
                          family = Gamma(link = "log"),
                          nAGQ = 1,
                          control = glmerControl(optimizer="bobyqa", 
                                                 optCtrl = list(maxfun = 500000)),
                          data = fpDatamut)

anova(gammaFPtime_null, gammaFPtime)
#Null model has lower AIC and BIC but no sig difference between models


#Generate effect sizes for model output

#(Counterbalanced design) - note that participant X target intercept variance
#and participant X target intercept-slope covariance cannot be estimated because
#they are confounded with residual error variance (Westfall et al., 2014)

#Formula: Mean1 - mean2 (estimated variation from intercept)/ 
#square root of: participant intercept variance + participant slope variance for each level of factor + word intercept variance + residual variance

#Try with base/control comparison
effSize_fp_baseCont <- (0.290028/sqrt(0.002132 + 0.015638  + 0.017240  + 0.010876  + 0.010509  + 0.057175))

#Base X AOMIgen
effSize_fp_baseGen <- (0.206936/sqrt(0.002132 + 0.015638  + 0.017240  + 0.010876  + 0.010509  + 0.057175))

#Baseline X AOMIspec
effSize_fp_baseSpec <- (0.209419/sqrt(0.002132 + 0.015638  + 0.017240  + 0.010876  + 0.010509  + 0.057175))

#Run pairwise comparisons

fp_pairwise <- emmeans::emmeans(gammaFPtime, pairwise ~ Condition,
                        adjust="tukey", lmer.df="satterthwaite", type = "response")

#Get effect sizes for post-hoc comparisons

#Get SDs for model
lme4::VarCorr(gammaFPtime)

#Combine SDs to get overall variance in model (this will form sigma)
#Square them before sqrt according to this post: 
#https://stackoverflow.com/questions/63848755/standardized-effect-sizes-interpretation-using-emmeans
totSD_fp <- sqrt((0.046179^2) + (0.125053^2) + (0.131300^2) + (0.104286^2) + (0.102513^2) + (0.239113^2))

fp_pairwise_eff <- emmeans::eff_size(fp_pairwise$emmeans, sigma = totSD_fp, edf = Inf, method = "tukey")

#edf = Inf means sigma is exactly known


##### IKIs ####

gammaIKIs <- lme4::glmer(IKIs ~ Condition + PHQ9 + Condition:PHQ9 + firstCond + 
                           Condition:firstCond + (1 | Word) + 
                           (1 + Condition | participant), 
                         family = Gamma(link = "log"),
                         nAGQ = 0,
                         control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                         data = ikiDatamut)
#'Very large eigenvalue' warning with naGQ = 1

summary(gammaIKIs)

#Check means of order
ikiMeans_order <- ikiDatamut %>%
  dplyr::group_by(firstCond) %>%
  dplyr::summarise(mean_order = mean(IKIs), sd_order = sd(IKIs))

#Compare with lognormal model
# 
# ln_IKIs <- lmerTest::lmer(IKIs ~ Condition + PHQ9 + Condition:PHQ9 + firstCond + 
#                         Condition:firstCond + (1 | Word) + 
#                         (1 + Condition | participant), 
#                       control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
#                       data = ikiDatamut)

#Gives singular fit warning - stick with gamma model

#Compare with null model

gammaIKIs_null <- lme4::glmer(IKIs ~ 1 + (1 | Word) + 
                                (1 + Condition | participant), 
                              family = Gamma(link = "log"),
                              nAGQ = 0,
                              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                              data = ikiDatamut)

anova(gammaIKIs, gammaIKIs_null)

#Null model has lower AIC and BIC. No sig difference.

#Check assumptions

performance::check_model(gammaIKIs)
#Fit looks pretty good

#Generate effect sizes

#Uses same formula described above.

#Baseline X AOMIspec
effSize_iki_phq9 <- (-1.980e-02/sqrt(0.003482 + 0.017530 + 0.005090 + 0.004122 + 0.005891 + 0.191668))

#Try with base/control comparison
effSize_iki_specFirst <- (1.295e-01/sqrt(0.003482 + 0.017530 + 0.005090 + 0.004122 + 0.005891 + 0.191668))

#Base X AOMIgen
effSize_iki_genFirstCond <- (-8.540e-02/sqrt(0.003482 + 0.017530 + 0.005090 + 0.004122 + 0.005891 + 0.191668))

#Get PHQ-9 correlation

cor(ikiDatamut$IKIs, ikiDatamut$PHQ9, method = "pearson")


#Run pairwise comparisons

ikiPairwise1 <- emmeans::emmeans(gammaIKIs, pairwise ~ Condition, adjust="tukey",
                        lmer.df = 'satterthwaite', type = "response")

ikiPairwise2 <- emmeans::emmeans(gammaIKIs, pairwise ~ Condition:firstCond, adjust="tukey",
                                 lmer.df = 'satterthwaite', type = "response")

#Also check plot of PHQ9 in each condition

#Effect sizes of post-hoc comparisons

#Get SDs for model
lme4::VarCorr(gammaIKIs)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_iki <- sqrt((0.059007^2) + (0.132400^2) + (0.071342^2) + (0.064204^2) + (0.076750^2) + (0.437798^2)) #AMEND

ikiPairwise1_eff <- emmeans::eff_size(ikiPairwise1$emmeans, sigma = totSD_iki, edf = Inf)
ikiPairwise2_eff <- emmeans::eff_size(ikiPairwise2$emmeans, sigma = totSD_iki, edf = Inf)


##### Accuracy ####

#Filter out missing values from accuracy data so we can use anova() to compare 
 #null model

accDatamut2 <- accDatamut %>%
  dplyr::filter(!is.na(Condition) & !is.na(PHQ9) & !is.na(firstCond) & !is.na(participant) & !is.na(Word))

# poisAcc <- lme4::glmer(No.Errors ~ Condition + PHQ9 + Condition:PHQ9 + firstCond + 
#                    Condition:firstCond + (1|Word) + (1 + Condition | participant),
#                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
#                  family = poisson, nAGQ = 0, data = accDatamut2)
#Singular fit warning with naGQ = 1

#Check for overdispersion in the Poisson model
#performance::check_overdispersion(poisAcc)
#Overdispersion in model

#Run negative binomial model instead

nbAcc <- lme4::glmer.nb(No.Errors ~ Condition + PHQ9 + Condition:PHQ9 + firstCond + 
                          Condition:firstCond + (1|Word) + (1 + Condition | participant),
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)),
                        nAGQ = 0, data = accDatamut2)

summary(nbAcc)
#Marginal interaction with PHQ-9

#Re-level the model to see if there are sig effects to justify pairwise comparisons

accDatamut3 <- accDatamut2
accDatamut3$Condition <- stats::relevel(accDatamut3$Condition, "AOMIspec")

#Update model

nbAcc_rl <- stats::update(nbAcc, data = accDatamut3)

summary(nbAcc_rl)
#Same marginal interaction

#Check assumptions/fit
performance::check_model(nbAcc_rl)
#Model fit is not amazing. Homogeneity of variance and normality of residuals look a bit strange
dispersion <- performance::check_overdispersion(nbAcc_rl)
performance::check_zeroinflation(nbAcc_rl)

#Build a null model

nbAcc_rl_null <- lme4::glmer.nb(No.Errors ~ 1 + (1|Word) + (1 + Condition | participant),
                                control = glmerControl(optimizer = "bobyqa", 
                                                       optCtrl = list(maxfun = 1000000)),
                                nAGQ = 0, data = accDatamut3)

anova(nbAcc_rl, nbAcc_rl_null)
#Null has lower AIC and BIC. No sig difference.

#Compare PHQ-9 trends

acc_trends <- emmeans::emtrends(nbAcc_rl, "Condition", var = "PHQ9",
                                adjust = "bonferroni",
                                lmer.df="satterthwaite")
pairs(acc_trends)

#Effect size

#Model output does not provide residual variance.
#Carrasco (2009) describes conditional variance for a negative binomial model as:

#Î¼ij+rÎ¼ij^2

#Î¼ = conditional mean
#r = negative binomial dispersion parameter
#i = subject
#j = observer (not relevant here)

#I think this is assuming the model has just one random intercept of subject

#Extract conditional means from model for each random effect

cond_Means <- lme4::ranef(nbAcc_rl, condVar=TRUE)
#Extract the participant-related values
p_cond_means <- cond_Means$participant
#Extract word-related values
word_cond_means <- cond_Means$Word

p_intercept <- colMeans(p_cond_means[1])
p_base <- colMeans(p_cond_means[2])
p_control <- colMeans(p_cond_means[3])
p_gen <- colMeans(p_cond_means[4])
word_intercept <- colMeans(word_cond_means)

#Extract dispersion ratio
dis_ratio <- dispersion$dispersion_ratio

#Construct equation

nb_resid <- ((p_intercept + ((dis_ratio*p_intercept)^2)) + (p_base + ((dis_ratio*p_base)^2)) + (p_control + ((dis_ratio*p_control)^2)) + (p_gen + ((dis_ratio*p_gen)^2)) + (word_intercept + ((dis_ratio*word_intercept)^2)))

#Work out effect size
effSize_acc_basePHQ9 <- (-0.07005/sqrt(0.01964 + 0.36302  + 0.01635  + 0.08610  +0.01348 + nb_resid))


#### Mixed effect models - With Speed Variable ####
#Model 2 in paper

##### ARCHIVE ####

#Model attempts with different random effects structures. Final models chosen had
 #lowest AIC values

#Whole word time

gammaWW_speed <- lme4::glmer(wholeWordTime ~ stim_speed + 
                               Condition:stim_speed + PHQ9 +
                               Condition:stim_speed:PHQ9 + firstCond + 
                               Condition:firstCond +
                               (1|Word) +  (1 + Condition | participant),
                             control = glmerControl(optimizer = "bobyqa", 
                                                    optCtrl = list(maxfun = 500000)),
                             family = Gamma(link = "log"), 
                             nAGQ = 0, 
                             data = wwDataSpeed)
#Model failed to converge with naGQ = 1

summary(gammaWW_speed)

gammaWW_speed2 <- lme4::glmer(wholeWordTime ~ stim_speed + 
                                Condition:stim_speed + PHQ9 +
                                Condition:stim_speed:PHQ9 + firstCond + 
                                Condition:firstCond +
                                (1|Word) +  (1 + Condition:stim_speed | participant),
                              control = glmerControl(optimizer = "bobyqa", 
                                                     optCtrl = list(maxfun = 500000)),
                              family = Gamma(link = "log"), 
                              nAGQ = 0, 
                              data = wwDataSpeed)
summary(gammaWW_speed2)

#First press times

gammaFP_speed <- lme4::glmer(firstPress ~ stim_speed + 
                               Condition:stim_speed + PHQ9 +
                               Condition:stim_speed:PHQ9 + firstCond + 
                               Condition:firstCond  +
                               (1|Word) + (1 + Condition| participant),
                             control = glmerControl(optimizer = "bobyqa", 
                                                    optCtrl = list(maxfun = 500000)),
                             family = Gamma(link = "log"), 
                             nAGQ = 1, 
                             data = fpDataSpeed)

summary(gammaFP_speed) #21826


gammaFP_speed3 <- lme4::glmer(firstPress ~ stim_speed + 
                                Condition:stim_speed + PHQ9 +
                                Condition:stim_speed:PHQ9 + firstCond + 
                                Condition:firstCond  +
                                (1|Word) + (1 + Condition | participant) +
                                (1 + stim_speed | participant),
                              control = glmerControl(optimizer = "bobyqa", 
                                                     optCtrl = list(maxfun = 500000)),
                              family = Gamma(link = "log"), 
                              nAGQ = 0, 
                              data = fpDataSpeed)

summary(gammaFP_speed3) #21816.9

#Compare with a lognormal model

lnFP_speed <- lmerTest::lmer(firstPress ~ stim_speed + 
                               Condition:stim_speed + PHQ9 +
                               Condition:stim_speed:PHQ9 + firstCond + 
                               Condition:firstCond  
                             + (1 | Word) + (1 + Condition | participant), 
                             control = lmerControl(optimizer="bobyqa", 
                                                   optCtrl = list(maxfun = 500000)),
                             data = fpDataSpeed)
summary(lnFP_speed)
AIC(lnFP_speed)
BIC(lnFP_speed)
#Gamma has lower AIC and BIC, go with this one


#IKIs

gammaIKIs_speed <- lme4::glmer(IKIs ~ stim_speed + 
                                 Condition:stim_speed + PHQ9 +
                                 Condition:stim_speed:PHQ9 + firstCond + 
                                 Condition:firstCond +
                                 (1 | Word) + 
                                 (1 + Condition | participant), 
                               family = Gamma(link = "log"),
                               nAGQ = 0,
                               control = glmerControl(optimizer="bobyqa", 
                                                      optCtrl = list(maxfun = 500000)),
                               data = ikiDataSpeed)
#"very large eigenvalue" warning with naGQ=1

summary(gammaIKIs_speed) #94854.7

gammaIKIs_speed2 <- lme4::glmer(IKIs ~ stim_speed + 
                                  Condition:stim_speed + PHQ9 +
                                  Condition:stim_speed:PHQ9 + firstCond + 
                                  Condition:firstCond +
                                  (1 | Word) + 
                                  (1 + Condition:stim_speed | participant), 
                                family = Gamma(link = "log"),
                                nAGQ = 0,
                                control = glmerControl(optimizer="bobyqa", 
                                                       optCtrl = list(maxfun = 500000)),
                                data = ikiDataSpeed)
summary(gammaIKIs_speed2)

#Compare with lognormal model

lnIKIs_speed <- lmerTest::lmer(IKIs ~ stim_speed + 
                                 Condition:stim_speed + PHQ9 +
                                 Condition:stim_speed:PHQ9 + firstCond + 
                                 Condition:firstCond  +
                                 (1 | Word) + 
                                 (1 + Condition | participant), 
                               control = lmerControl(optimizer="bobyqa", 
                                                     optCtrl = list(maxfun = 500000)),
                               data = ikiDataSpeed)
#singular fit warning
summary(lnIKIs_speed)
AIC(lnIKIs_speed)
BIC(lnIKIs_speed)
#AIC and BIC lower for gamma model, go with that one


#Accuracy

poisAcc_speed2 <- lme4::glmer(No.Errors ~ stim_speed + 
                                Condition:stim_speed + PHQ9 +
                                Condition:stim_speed:PHQ9 + firstCond + 
                                Condition:firstCond + (1|Word) + 
                                (1 + Condition:stim_speed | participant),
                              control = glmerControl(optimizer = "bobyqa", 
                                                     optCtrl = list(maxfun = 500000)),
                              family = poisson, nAGQ = 0, data = accDataSpeed2)

summary(poisAcc_speed2)
#New marginal interaction between speed X condition, larger AIC

poisAcc_speed3 <- lme4::glmer(No.Errors ~ stim_speed + 
                                Condition:stim_speed + PHQ9 +
                                Condition:stim_speed:PHQ9 + firstCond + 
                                Condition:firstCond + (1|Word) + 
                                (1 + Condition | participant) +
                                (1 + stim_speed | participant),
                              control = glmerControl(optimizer = "bobyqa", 
                                                     optCtrl = list(maxfun = 500000)),
                              family = poisson, nAGQ = 0, data = accDataSpeed2)
summary(poisAcc_speed3)
#Still higher AIC than first model, same effects

#Compare with negative binomial model

nbAcc <- lme4::glmer.nb(No.Errors ~ stim_speed + 
                          Condition:stim_speed + PHQ9 +
                          Condition:stim_speed:PHQ9 + firstCond + 
                          Condition:firstCond + (1|Word) + 
                          (1 + Condition | participant),
                        control = glmerControl(optimizer = "bobyqa", 
                                               optCtrl = list(maxfun = 1000000)),
                        nAGQ = 0, data = accDataSpeed2)
#With naGQ=1, large eigenvalue warning and iteration limit reached
#naGQ=0, iteration limit reached
#We'll stick with Poisson model


##### Make speed data ####

#filter out baseline condition from datasets as this does not have a speed aspect

wwDataSpeed <- wwDatamut %>%
  dplyr::filter(Condition != "Baseline") %>%
  droplevels()

fpDataSpeed <- fpDatamut %>%
  dplyr::filter(Condition != "Baseline") %>%
  droplevels()

ikiDataSpeed <- ikiDatamut %>%
  dplyr::filter(Condition != "Baseline") %>%
  droplevels()

accDataSpeed <- accDatamut %>%
  dplyr::filter(Condition != "Baseline") %>%
  droplevels()


##### Whole word time ####

gammaWW_speed3 <- lme4::glmer(wholeWordTime ~ stim_speed + 
                                Condition:stim_speed + PHQ9 +
                                Condition:stim_speed:PHQ9 + firstCond + 
                                Condition:firstCond +
                                (1|Word) +  (1 + Condition | participant) +
                                (1 + stim_speed | participant),
                              control = glmerControl(optimizer = "bobyqa", 
                                                     optCtrl = list(maxfun = 500000)),
                              family = Gamma(link = "log"), 
                              nAGQ = 0, 
                              data = wwDataSpeed)
summary(gammaWW_speed3)

#Build null model

gammaWW_speed_null <- lme4::glmer(wholeWordTime ~ 1 + (1|Word) + 
                                    (1 + Condition | participant) +
                                    (1 + stim_speed | participant),
                                  control = glmerControl(optimizer = "bobyqa", 
                                                         optCtrl = list(maxfun = 500000)),
                                  family = Gamma(link = "log"), 
                                  nAGQ = 0, 
                                  data = wwDataSpeed)           

#Compare models
anova(gammaWW_speed3, gammaWW_speed_null)
#Null has lower AIC and BIC. No sig difference (but marginal, p = .087)

#Get effect size

#AOMIgen*firstCond
effSize_ww_AOMIgenFirstCond <- (-0.0961627/sqrt(0.003923 + 0.006841 + 0.004781 + 0.005719 + 0.006295 + 0.002154 + 0.048830))

#PHQ9
effSize_ww_PHQ9 <- (-0.0110900/sqrt(0.003923 + 0.006841 + 0.004781 + 0.005719 + 0.006295 + 0.002154 + 0.048830))

#Check model fit

performance::check_model(gammaWW_speed3)
#fit looks pretty good

#Get PHQ-9 correlation

cor(wwDataSpeed$wholeWordTime, wwDataSpeed$PHQ9, method = "pearson")

#Pairwise comparisons

wwPairwise_speed <- emmeans::emmeans(gammaWW_speed3, 
                                     pairwise ~ Condition:firstCond,
                                     adjust="tukey",
                                     lmer.df = 'satterthwaite', 
                                     type = "response")
#Sig difference between AO+MIgen and AO+MIspec when AO+MIspec completed first?

#Get effect sizes

#Get SDs for model
lme4::VarCorr(gammaWW_speed3)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_ww_speed <- sqrt((0.062635^2) + (0.082707^2) + (0.069148^2) + (0.075622^2) + (0.079341^2) + (0.046410^2) + (0.220975^2))

wwPairwise_speedEff <- emmeans::eff_size(wwPairwise_speed$emmeans, 
                                         sigma = totSD_ww_speed, 
                                         edf = Inf)

##### First press time ####

gammaFP_speed2 <- lme4::glmer(firstPress ~ stim_speed + 
                                Condition:stim_speed + PHQ9 +
                                Condition:stim_speed:PHQ9 + firstCond + 
                                Condition:firstCond  +
                                (1|Word) + (1 + Condition:stim_speed | participant),
                              control = glmerControl(optimizer = "bobyqa", 
                                                     optCtrl = list(maxfun = 500000)),
                              family = Gamma(link = "log"), 
                              nAGQ = 0, 
                              data = fpDataSpeed)
#Convergence failure on above with naGQ=1

summary(gammaFP_speed2)

#check assumptions
performance::check_model(gammaFP_speed2)
#Looks a decent fit

#build null model

gammaFP_speed_null2 <- lme4::glmer(firstPress ~ 1 + (1|Word) + 
                               (1 + Condition:stim_speed | participant),
                             control = glmerControl(optimizer = "bobyqa", 
                                                    optCtrl = list(maxfun = 500000)),
                             family = Gamma(link = "log"), 
                             nAGQ = 0, 
                             data = fpDataSpeed)

anova(gammaFP_speed2, gammaFP_speed_null2)
#null model has lower AIC and BIC. No sig difference

#Get effect sizes of output

#stim_speed:AO+MIgen
effSize_fp_speed_AOMIgen <- (-0.160065/sqrt(0.002407 + 0.013018 + 0.004669 + 0.016840 + 0.004392 + 0.003782 + 0.004570 + 0.003519 + 0.053811))

#stim_speed:AO+MIspec
effSize_fp_speed_AOMIspec <- (-0.106389/sqrt(0.002407 + 0.013018 + 0.004669 + 0.016840 + 0.004392 + 0.003782 + 0.004570 + 0.003519 + 0.053811))

#check means of speed overall

# fpSpeed_means <- fpDataSpeed %>%
#   dplyr::group_by(stim_speed) %>%
#   dplyr::summarise(speed_means = mean(firstPress), speed_sd = sd(firstPress))


#Run pairwise comparisons


fp_pairwise_speed <- emmeans::emmeans(gammaFP_speed2, pairwise ~ stim_speed:Condition,
                                       adjust="tukey", lmer.df="satterthwaite",
                                       type = "response")

# #Effect sizes

#Get SDs for model
lme4::VarCorr(gammaFP_speed2)

#Create sigma
totSD_fp_speed <- sqrt((0.049061^2) + (0.114097^2) + (0.068328^2) + (0.129770^2) + (0.066271^2) + (0.061499^2) + (0.067603^2) + (0.059324^2) + (0.231973^2))

fp_pairwise2_eff <- emmeans::eff_size(fp_pairwise_speed$emmeans, sigma = totSD_fp_speed,
                                      edf = Inf, method = "tukey")


##### IKIs ####


#Similar effects and larger AIC than first model

gammaIKIs_speed3 <- lme4::glmer(IKIs ~ stim_speed + 
                                  Condition:stim_speed + PHQ9 +
                                  Condition:stim_speed:PHQ9 + firstCond + 
                                  Condition:firstCond +
                                  (1 | Word) + 
                                  (1 + Condition | participant) +
                                  (1 + stim_speed | participant) , 
                                family = Gamma(link = "log"),
                                nAGQ = 0,
                                control = glmerControl(optimizer="bobyqa", 
                                                       optCtrl = list(maxfun = 500000)),
                                data = ikiDataSpeed)
summary(gammaIKIs_speed3)
#Lowest AIC, similar effects

#Compare to null model

gammaIKIs_speed_null <- lme4::glmer(IKIs ~ 1 +
                                      (1 | Word) + 
                                      (1 + Condition | participant)+
                                      (1 + stim_speed | participant), 
                                    family = Gamma(link = "log"),
                                    nAGQ = 0,
                                    control = glmerControl(optimizer="bobyqa", 
                                                           optCtrl = list(maxfun = 500000)),
                                    data = ikiDataSpeed)

anova(gammaIKIs_speed3, gammaIKIs_speed_null)
#Exp model has lower AIC but higher BIC. We go with AIC in this case because it 
 #shouldn't penalise differences in no. of parameters as much. Sig difference
 #between models (p = .002)

#Check model fit
performance::check_model(gammaIKIs_speed)
#fit looks quite good

#Effect sizes

#PHQ9
effSize_iki_phq9 <- (-0.0129489/sqrt(0.004057 + 0.008893 + 0.001941 + 0.004620 + 0.005092 + 0.001251 + 0.193136))

#firstCond
effSize_iki_firstCond <- (0.1352263/sqrt(0.004057 + 0.008893 + 0.001941 + 0.004620 + 0.005092 + 0.001251 + 0.193136))

#stim_speed:AOMIgen
effSize_iki_speed_Cond <- (0.0782278/sqrt(0.004057 + 0.008893 + 0.001941 + 0.004620 + 0.005092 + 0.001251 + 0.193136))

#AOMIgen:firstCond
effSize_iki_cond_firstCond <- (-0.0839574/sqrt(0.004057 + 0.008893 + 0.001941 + 0.004620 + 0.005092 + 0.001251 + 0.193136))
  

#Pairwise comparisons 

#stim_speed:Condition

ikiPairwise1 <- emmeans::emmeans(gammaIKIs_speed3, 
                 pairwise ~ Condition:stim_speed,
                 adjust="tukey",
                 lmer.df = 'satterthwaite', 
                 type = "response")

#condition:firstCond

# ikiPairwise2 <- emmeans::emmeans(gammaIKIs_speed3, 
#                                  pairwise ~ Condition:firstCond,
#                                  adjust="tukey",
#                                  lmer.df = 'satterthwaite', 
#                                  type = "response")

#Get effect sizes

lme4::VarCorr(gammaIKIs_speed3)

totSD_iki_speed <- sqrt((0.063698^2) + (0.094301^2) + (0.044053^2) + (0.067970^2) + (0.071359^2) + (0.035364^2) + (0.439472^2))

effSize_ikiPairwise1 <- emmeans::eff_size(ikiPairwise1$emmeans,
                                          sigma = totSD_iki_speed,
                                          edf = Inf, 
                                          method = "tukey")

# effSize_ikiPairwise2 <- emmeans::eff_size(ikiPairwise2$emmeans,
#                                           sigma = totSD_iki_speed,
#                                           edf = Inf, 
#                                           method = "tukey")

##### Accuracy ####


#Filter out missing/incorrect values from accuracy data so we can use anova() to compare 
#null model

accDataSpeed2 <- accDataSpeed %>%
  dplyr::filter(!is.na(Condition) & !is.na(PHQ9) & !is.na(firstCond) & !is.na(participant) & !is.na(Word)) %>%
  dplyr::filter(stim_speed == "fast" | stim_speed == "slow") %>%
  droplevels()

poisAcc_speed <- lme4::glmer(No.Errors ~ stim_speed + 
                               Condition:stim_speed + PHQ9 +
                               Condition:stim_speed:PHQ9 + firstCond + 
                               Condition:firstCond + (1|Word) + 
                               (1 + Condition | participant),
                             control = glmerControl(optimizer = "bobyqa", 
                                                    optCtrl = list(maxfun = 500000)),
                             family = poisson, nAGQ = 0, data = accDataSpeed2)
#Large eigenvalue warning with naGQ = 1

summary(poisAcc_speed)

#Check for overdispersion in the Poisson model - none found
performance::check_overdispersion(poisAcc_speed)

#Build null model

poisAcc_speed_null <- lme4::glmer(No.Errors ~ 1 + (1|Word) + 
                               (1 + Condition | participant),
                             control = glmerControl(optimizer = "bobyqa", 
                                                    optCtrl = list(maxfun = 500000)),
                             family = poisson, nAGQ = 0, data = accDataSpeed2)

anova(poisAcc_speed, poisAcc_speed_null)
#Null has lower AIC and BIC. No sig difference.

#check model fit

performance::check_model(poisAcc_speed)
#Mostly okay but residuals and influential obs looks a bit strange


#Get effect sizes of significant output


#Find estimated residual variance of the model (not generated for non-Gaussian  
#models)
#https://stats.stackexchange.com/questions/545947/how-to-calculate-the-standard-errors-of-the-variance-components-of-a-mixed-model

#To do this we need the grand mean
grandMean <- ((accMeans$mean_acc[2] + accMeans$mean_acc[3] + accMeans$mean_acc[4])/3)

#Residual variance estimate:
#Based on formula: w · ln(1 / exp(B0) + 1)
# w = dispersion parameter
# B0 = grand mean
# See Nakagawa & Schielzeth (2010) and Q&A on Stack Exchange
poisResid <- 0.696 * log(1 / (exp(grandMean) + 1))

#Stim_speed:AO+MIspec

effsize_acc_specSpeed <- (-0.7068953/sqrt(0.08801 + 0.30372 + 0.21074 + 0.10816 + poisResid))

#stim_speed:AOMIgen:PHQ9

effsize_acc_speedCondPHQ9 <-  (0.0782198/sqrt(0.08801 + 0.30372 + 0.21074 + 0.10816 + poisResid))
  
#Pairwise comparisons

accPairwise1 <- emmeans::emmeans(poisAcc_speed,
                                 pairwise ~ stim_speed:Condition,
                                 adjust="tukey",
                                 lmer.df = 'satterthwaite', 
                                 type = "response")

accPairwise2 <- emmeans::emmeans(poisAcc_speed,
                                pairwise ~ stim_speed:Condition:PHQ9,
                                adjust="tukey",
                                lmer.df = 'satterthwaite', 
                                type = "response")

#Look at trends to better interpret PHQ9 interaction

acc_trends <- emtrends(poisAcc_speed, ~stim_speed:Condition:PHQ9, var = "PHQ9",
                       adjust = "bonferroni",
                       lmer.df="satterthwaite")
acc_trends_p <- test(acc_trends)
pairs(acc_trends)

acc_trends2 <- emtrends(poisAcc_speed, ~stim_speed:PHQ9, var = "PHQ9",
                       adjust = "bonferroni",
                       lmer.df="satterthwaite")
acc_trends_p2 <- test(acc_trends2)

acc_trends3 <- emtrends(poisAcc_speed, ~Condition:PHQ9, var = "PHQ9",
                        adjust = "bonferroni",
                        lmer.df="satterthwaite")
acc_trends_p3 <- test(acc_trends3)
#Not significant


#### Mixed effect models - comparing baseline to speeds ####
#Model 3 in paper

#First alter the datasets so we exclude the control condition and code the AO+MI speeds
 #as different levels of the same factor

#ONLY run this if you haven't already made this dataset for the plots above

wwDataSpeed_base <- wwDatamut %>%
  dplyr::filter(Condition != "Control") %>%
  tidyr::unite(conds_combd, c(Condition, stim_speed), remove = FALSE) %>%
  dplyr::select(-c(Condition, stim_speed)) #remove the old columns to prevent confusion

#Rename the baseline condition so we don't have the NA

wwDataSpeed_base$conds_combd[wwDataSpeed_base$conds_combd == "Baseline_NA"] <- "Baseline"

#Make sure the baseline condition is the intercept

wwDataSpeed_base <- wwDataSpeed_base %>%
  dplyr::mutate(conds_combd = factor(conds_combd, c("Baseline", "AOMIspec_slow", "AOMIspec_fast", "AOMIgen_slow", "AOMIgen_fast")))

wwDataSpeed_base$conds_combd <- stats::relevel(wwDataSpeed_base$conds_combd, "Baseline")

#Repeat for other datasets

fpDataSpeed_base <- fpDatamut %>%
  dplyr::filter(Condition != "Control") %>%
  tidyr::unite(conds_combd, c(Condition, stim_speed), remove = FALSE) %>%
  dplyr::select(-c(Condition, stim_speed))

fpDataSpeed_base$conds_combd[fpDataSpeed_base$conds_combd == "Baseline_NA"] <- "Baseline"

fpDataSpeed_base <- fpDataSpeed_base %>%
  dplyr::mutate(conds_combd = factor(conds_combd, c("Baseline", "AOMIspec_slow", "AOMIspec_fast", "AOMIgen_slow", "AOMIgen_fast")))

fpDataSpeed_base$conds_combd <- stats::relevel(fpDataSpeed_base$conds_combd, "Baseline")

ikiDataSpeed_base <- ikiDatamut %>%
  dplyr::filter(Condition != "Control") %>%
  tidyr::unite(conds_combd, c(Condition, stim_speed), remove = FALSE) %>%
  dplyr::select(-c(Condition, stim_speed))

ikiDataSpeed_base$conds_combd[ikiDataSpeed_base$conds_combd == "Baseline_NA"] <- "Baseline"

ikiDataSpeed_base <- ikiDataSpeed_base %>%
  dplyr::mutate(conds_combd = factor(conds_combd, c("Baseline", "AOMIspec_slow", "AOMIspec_fast", "AOMIgen_slow", "AOMIgen_fast")))

ikiDataSpeed_base$conds_combd <- stats::relevel(ikiDataSpeed_base$conds_combd, "Baseline")

accDataSpeed_base <- accDatamut %>%
  dplyr::filter(Condition != "Control") %>%
  tidyr::unite(conds_combd, c(Condition, stim_speed), remove = FALSE) %>%
  dplyr::select(-c(Condition, stim_speed))

accDataSpeed_base$conds_combd[accDataSpeed_base$conds_combd == "Baseline_NA"] <- "Baseline"  

accDataSpeed_base <- accDataSpeed_base %>%
  dplyr::mutate(conds_combd = factor(conds_combd, c("Baseline", "AOMIspec_slow", "AOMIspec_fast", "AOMIgen_slow", "AOMIgen_fast")))

accDataSpeed_base$conds_combd <- stats::relevel(accDataSpeed_base$conds_combd, "Baseline")
  
##### Whole word time ####

gammaWW_speedBase <- lme4::glmer(wholeWordTime ~ conds_combd + PHQ9 +
                                   conds_combd:PHQ9 + firstCond + 
                                   conds_combd:firstCond +
                                   (1|Word) +  (1 + conds_combd | participant),
                                 control = glmerControl(optimizer = "bobyqa", 
                                                        optCtrl = list(maxfun = 500000)),
                                 family = Gamma(link = "log"), 
                                 nAGQ = 0, 
                                 data = wwDataSpeed_base)
#Singular fit warning with naGQ = 1

summary(gammaWW_speedBase)

#Compare to null

gammaWW_speedBase_null <- lme4::glmer(wholeWordTime ~ 1 +
                                   (1|Word) +  (1 + conds_combd | participant),
                                 control = glmerControl(optimizer = "bobyqa", 
                                                        optCtrl = list(maxfun = 500000)),
                                 family = Gamma(link = "log"), 
                                 nAGQ = 0, 
                                 data = wwDataSpeed_base)

anova(gammaWW_speedBase, gammaWW_speedBase_null)

#Get effect size

#AOMIgen slow
effSize_ww_AOMIspec_slow <- (0.086642/sqrt(0.004282 + 0.013429 + 0.005396 + 0.003090 + 0.004607 + 0.007024 + 0.049294))

effSize_ww_AOMIgen_slow <- (0.144666/sqrt(0.004282 + 0.013429 + 0.005396 + 0.003090 + 0.004607 + 0.007024 + 0.049294))

#PHQ-9 main
effSize_ww_phq9_base <- (-0.018719/sqrt(0.004282 + 0.013429 + 0.005396 + 0.003090 + 0.004607 + 0.007024 + 0.049294))

#Fast AO+MIspec * PHQ-9
effSize_ww_phq9_AOMIspec_fast <- (0.012501/sqrt(0.004282 + 0.013429 + 0.005396 + 0.003090 + 0.004607 + 0.007024 + 0.049294))

#Slow AO+MIgen * Order
effSize_ww_order_AOMIgen_slow <- (-0.140516/sqrt(0.004282 + 0.013429 + 0.005396 + 0.003090 + 0.004607 + 0.007024 + 0.049294))

#Check model fit

performance::check_model(gammaWW_speedBase)

#Look at trends with PHHQ-9

ww_condition_phq9 <- emtrends(gammaWW_speedBase, "conds_combd", var = "PHQ9",
                      adjust = "bonferroni",
                      lmer.df="satterthwaite")
#Get p values for each condition
ww_condition_phq9_p <- test(ww_condition_phq9)
#Pairwise comparisons
pairs(ww_condition_phq9)

#Get effect size

#Get SDs for model
VarCorr(gammaWW_speedBase)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_ww_baseSpeed <- sqrt((0.065436^2) + (0.115882^2) + (0.073458^2) + (0.055586^2) + (0.067878^2) + (0.083811^2) + (0.222022^2))

wwPairwisebaseSpeed_eff <- emmeans::eff_size(ww_condition_phq9, sigma = totSD_ww_baseSpeed, edf = Inf)

#Get r of correlations

ww_corrDat <- wwDataSpeed_base %>%
  dplyr::filter(conds_combd == "Baseline")

cor(ww_corrDat$wholeWordTime, ww_corrDat$PHQ9, method = "pearson")

ww_corrDat2 <- wwDataSpeed_base %>%
  dplyr::filter(conds_combd == "AOMIspec_fast")

stats::cor(ww_corrDat2$wholeWordTime, ww_corrDat2$PHQ9, method = "pearson")

#Pairwise comparisons

wwPairwise_speedBase <- emmeans::emmeans(gammaWW_speedBase, pairwise~conds_combd,
                                        adjust="tukey", lmer.df="satterthwaite",
                                        type="response")

wwPairwise_speedBase2 <- emmeans::emmeans(gammaWW_speedBase, 
                                          pairwise~conds_combd:firstCond,
                                          adjust="tukey", lmer.df="satterthwaite",
                                          type="response")

#Effect sizes

wwPairwisebaseSpeed_eff2 <- emmeans::eff_size(wwPairwise_speedBase$emmeans, sigma = totSD_ww_baseSpeed, edf = Inf)

wwPairwisebaseSpeed_eff3 <- emmeans::eff_size(wwPairwise_speedBase2$emmeans, sigma = totSD_ww_baseSpeed, edf = Inf)


##### first press time ####

gammaFP_speedBase <- lme4::glmer(firstPress ~ conds_combd + PHQ9 + conds_combd:PHQ9 + 
                             firstCond + conds_combd:firstCond + (1 | Word) + 
                             (1 + conds_combd | participant), 
                           family = Gamma(link = "log"),
                           nAGQ = 1,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                           data = fpDataSpeed_base)

summary(gammaFP_speedBase)

#Compare to null model

gammaFP_speedBase_null <- lme4::glmer(firstPress ~ 1 + (1 | Word) + 
                                   (1 + conds_combd | participant), 
                                 family = Gamma(link = "log"),
                                 nAGQ = 1,
                                 glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                                 data = fpDataSpeed_base)

anova(gammaFP_speedBase, gammaFP_speedBase_null)

#Get effect sizes

#Slow AO+MIspec
effSize_fp_AOMIspec_slow <- (0.19359/sqrt(0.002651 + 0.015981 + 0.011097 + 0.014037 + 0.014524 + 0.013405 + 0.055852))

#Fast AO+MIspec
effSize_ww_AOMIspec_fast <- (0.21727/sqrt(0.004282 + 0.013429 + 0.005396 + 0.003090 + 0.004607 + 0.007024 + 0.049294))

#Slow AO+MIgen
effSize_ww_AOMIgen_slow <- (0.27626/sqrt(0.004282 + 0.013429 + 0.005396 + 0.003090 + 0.004607 + 0.007024 + 0.049294))

#Check model fit
performance::check_model(gammaFP_speedBase)

#Pairwise comparisons

fp_pairwise_condsCombd <- emmeans::emmeans(gammaFP_speedBase, pairwise ~ conds_combd,
                                               adjust="tukey", lmer.df="satterthwaite",
                                               type = "response")

#Effect sizes

#Get SDs for model
VarCorr(gammaFP_speedBase)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_fp_baseSpeed <- sqrt((0.051485^2) + (0.126416^2) + (0.105342^2) + (0.118478^2) + (0.120514^2) + (0.115780^2) + (0.236330^2))

fpPairwisebaseSpeed_eff <- eff_size(fp_pairwise_condsCombd$emmeans, sigma = totSD_fp_baseSpeed, edf = Inf)


##### IKIs ####

gammaIKIs_speedBase <- lme4::glmer(IKIs ~ conds_combd + PHQ9 + conds_combd:PHQ9 + firstCond + 
                           conds_combd:firstCond + (1 | Word) + 
                           (1 + conds_combd | participant), 
                         family = Gamma(link = "log"),
                         nAGQ = 0,
                         control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                         data = ikiDataSpeed_base)
#Singular fit warning with naGQ=1

summary(gammaIKIs_speedBase)

#Null model

gammaIKIs_speedBase_null <- lme4::glmer(IKIs ~ 1 + (1 | Word) + 
                                     (1 + conds_combd | participant), 
                                   family = Gamma(link = "log"),
                                   nAGQ = 0,
                                   control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)),
                                   data = ikiDataSpeed_base)

anova(gammaIKIs_speedBase, gammaIKIs_speedBase_null)

#Effect sizes

#PHQ-9
effSize_iki_phq9_baseSpeed <- (-0.019907/sqrt(0.003585 + 0.017609 + 0.006111 + 0.006397 + 0.006245 + 0.003226 + 0.190307))

#Order
effSize_iki_order_baseSpeed <- (0.134738/sqrt(0.003585 + 0.017609 + 0.006111 + 0.006397 + 0.006245 + 0.003226 + 0.190307))

#Slow AO+MIgen:order
effSize_iki_AOMIgenOrder_baseSpeed <- (-0.114494/sqrt(0.003585 + 0.017609 + 0.006111 + 0.006397 + 0.006245 + 0.003226 + 0.190307))

#Fast AO+MIgen:PHQ-9
effSize_iki_AOMIgen_phq9 <- (0.009297/sqrt(0.003585 + 0.017609 + 0.006111 + 0.006397 + 0.006245 + 0.003226 + 0.190307))

#check model fit
performance::check_model(gammaIKIs_speedBase)

#Pairwise trends

iki_pairwise_trends <- emmeans::emtrends(gammaIKIs_speedBase, "conds_combd", 
                                         var = "PHQ9",
                                         adjust = "bonferroni",
                                         lmer.df="satterthwaite")
#Get p values for each condition
iki_pairwise_trends_p <- test(iki_pairwise_trends)
#Pairwise comparisons
pairs(iki_pairwise_trends)

#Pairwise comparisons(?)

iki_pairwise_condComb <- emmeans::emmeans(gammaIKIs_speedBase, 
                                          pairwise~conds_combd:firstCond,
                                          adjust="tukey", lmer.df="satterthwaite",
                                          type="response")


##### Accuracy ####

accDataSpeed_base2 <- accDataSpeed_base %>%
  dplyr::filter(!is.na(conds_combd) & !is.na(PHQ9) & !is.na(firstCond) & !is.na(participant) & !is.na(Word))

poisAcc_speedBase <- lme4::glmer(No.Errors ~ conds_combd + PHQ9 + conds_combd:PHQ9 + firstCond + 
                         conds_combd:firstCond + (1|Word) + (1 + conds_combd | participant),
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                       family = poisson, nAGQ = 0, data = accDataSpeed_base2)
#Singular fit warning with naGQ=1

#check overdispersion - none detected
performance::check_overdispersion(poisAcc_speedBase)

summary(poisAcc_speedBase)

#Null model

poisAcc_speedBase_null <- lme4::glmer(No.Errors ~ 1 + (1|Word) + (1 + conds_combd | participant),
                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                                 family = poisson, nAGQ = 0, data = accDataSpeed_base2)

anova(poisAcc_speedBase,poisAcc_speedBase_null)

#Check model fit
performance::check_model(poisAcc_speedBase)

#Effect size

#First work out residual error

#To do this we need the grand mean

accMeans_speedBase <- accDataSpeed_base2 %>%
  dplyr::group_by(conds_combd) %>%
  dplyr::summarise(mean = mean(No.Errors))

grandMean2 <- ((accMeans_speedBase$mean[1] + accMeans_speedBase$mean[2] + accMeans_speedBase$mean[3] + accMeans_speedBase$mean[4] + accMeans_speedBase$mean[5])/5)

#Residual variance estimate:
#Based on formula: w · ln(1 / exp(B0) + 1)
# w = dispersion parameter
# B0 = grand mean
# See Nakagawa & Schielzeth (2010) and Q&A on Stack Exchange
poisResid <- 0.689 * log(1 / (exp(grandMean2) + 1))

#Slow AO+MIgen
effSize_acc_specPHQ9 <- (0.119212/sqrt(0.08474 + 0.52831 + 0.22425  + 0.02084  + 0.01264 + 0.11479 + poisResid))

#Pairwise comparisons

accPairwise_condsCombd <- emmeans::emmeans(poisAcc_speedBase, pairwise~conds_combd,
                                           adjust="tukey", lmer.df="satterthwaite",
                                           type="response")

accPairwise_cond_phq9 <- emmeans::emtrends(poisAcc_speedBase, "conds_combd", 
                                           var = "PHQ9",
                                           adjust = "bonferroni",
                                           lmer.df="satterthwaite")

accPairwise_cond_phq9_p <- test(accPairwise_cond_phq9)
pairs(accPairwise_cond_phq9)

#Get effect sizes

#Get SDs for model
VarCorr(poisAcc_speedBase)

#Combine SDs to get overall variance in model (this will form sigma)
totSD_acc_baseSpeed <- sqrt((0.29111^2) + (0.72685^2) + (0.47355^2) + (0.14437^2) + (0.11241^2) + (0.33881^2))

accPairwisebaseSpeed_eff <- emmeans::eff_size(accPairwise_cond_phq9, sigma = totSD_acc_baseSpeed, edf = Inf)

#Get correlations

#Filter on the separate conditions

accDataSpeed_base_cor1 <- accDataSpeed_base %>%
  dplyr::filter(conds_combd == "AOMIspec_fast") %>%
  dplyr::filter(!is.na(No.Errors) & !is.na(conds_combd) & !is.na(PHQ9)) %>%
  droplevels()

stats::cor(accDataSpeed_base_cor1$No.Errors, accDataSpeed_base_cor1$PHQ9, 
           method = "pearson")

accDataSpeed_base_cor2 <- accDataSpeed_base %>%
  dplyr::filter(conds_combd == "Baseline") %>%
  dplyr::filter(!is.na(No.Errors) & !is.na(conds_combd) & !is.na(PHQ9)) %>%
  droplevels()

stats::cor(accDataSpeed_base_cor2$No.Errors, accDataSpeed_base_cor2$PHQ9, 
           method = "pearson")
