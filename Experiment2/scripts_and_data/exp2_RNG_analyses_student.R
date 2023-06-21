#Before installing:
#options("install.lock"=FALSE)

library(tidyverse)

## Script to analyse group statistics of random number generation task in Experiment 2
 #student typing study ##

#### Import and tidy ####

#Import RNG data

rngData <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/study3/rawData/RNG/RNG_Data_study3_v2.csv')

#Remove participants that were for piloting, i.e. IDs that are a name, not a number

rngData2 <- rngData %>%
  dplyr::filter(participantID !="aklima" & participantID !="Eve") %>%
  dplyr::mutate(Trial = as.integer(Trial)) %>%
  dplyr::filter(!is.na(Trial)) #remove practice trials

#Remove % signs from data
rngData2[] <- lapply(rngData2, gsub, pattern = "%", fixed = TRUE, replacement = "")

#Create separate data set with just the randomisation statistics
rngRandDat <- rngData2 %>%
  dplyr::select(-c(Number)) %>%
  dplyr::filter(!is.na(R) & !is.na(Adjacency_combined) & !is.na(RNG) & !is.na(NSQ) & !is.na(NSQ) & !is.na(RNG2) & !is.na(TPI) & !is.na(`Repetition gap (mean)`)) %>%
  dplyr::mutate(Adjacency_combined = as.numeric(Adjacency_combined),
                R = as.numeric(R),
                RNG = as.numeric(RNG),
                NSQ = as.numeric(NSQ),
                RNG2 = as.numeric(RNG2),
                TPI = as.numeric(TPI),
                `Repetition gap (mean)` = as.numeric(`Repetition gap (mean)`))


#### Visualise spread of data and check for outliers ####

#Adjacency
hist(rngRandDat$Adjacency_combined)
boxplot(rngRandDat$Adjacency_combined,
        ylab = "Adjacency_combined"
)
#Find row numbers of outliers
out <- boxplot.stats(rngRandDat$Adjacency_combined)$out
out_ind <- which(rngRandDat$Adjacency_combined %in% c(out))
out_ind

#R
hist(rngRandDat$R)
boxplot(rngRandDat$R,
        ylab = "R"
)

#Find row numbers of outliers
out1 <- boxplot.stats(rngRandDat$R)$out
out1_ind <- which(rngRandDat$R %in% c(out1))
out1_ind

#RNG
hist(rngRandDat$RNG)
boxplot(rngRandDat$RNG,
        ylab = "RNG"
)

#Find row numbers of outliers
out2 <- boxplot.stats(rngRandDat$RNG)$out
out2_ind <- which(rngRandDat$RNG %in% c(out2))
out2_ind

#NSQ
hist(rngRandDat$NSQ)
boxplot(rngRandDat$NSQ,
        ylab = "NSQ"
)

#Find row numbers of outliers
out3 <- boxplot.stats(rngRandDat$NSQ)$out
out3_ind <- which(rngRandDat$NSQ %in% c(out3))
out3_ind

#RNG2
hist(rngRandDat$RNG2)
boxplot(rngRandDat$RNG2,
        ylab = "RNG2"
)

#Find row numbers of outliers
out4 <- boxplot.stats(rngRandDat$RNG2)$out
out4_ind <- which(rngRandDat$RNG2 %in% c(out4))
out4_ind

#TPI  
hist(rngRandDat$TPI)
boxplot(rngRandDat$TPI,
        ylab = "TPI"
)

#Find row numbers of outliers
out5 <- boxplot.stats(rngRandDat$TPI)$out
out5_ind <- which(rngRandDat$TPI %in% c(out5))
out5_ind

#Repetition Gap
hist(rngRandDat$`Repetition gap (mean)`)
boxplot(rngRandDat$`Repetition gap (mean)`,
        ylab = "`Repetition gap (mean)`"
)

#Find row numbers of outliers
out6 <- boxplot.stats(rngRandDat$`Repetition gap (mean)`)$out
out6_ind <- which(rngRandDat$`Repetition gap (mean)` %in% c(out6))
out6_ind

#No participants stand out as consistent outliers

#### Extra tidying ####

#Add to df how many numbers were produced on each trial

rngData3 <- rngData2 %>%
  dplyr::group_by(participantID, Trial) %>%
  dplyr::count() 

#Add this new column onto our data with the randomness statistics

rngRandDat2 <- dplyr::left_join(rngRandDat, rngData3) %>%
  dplyr::filter(n > 2) #Remove trials with < 3 numbers


#### Descriptive statistics ####

descStats <- rngRandDat2 %>%
  dplyr::group_by(Speed) %>%
  dplyr::summarise(mean_n = mean(n),
                   sd_n = sd(n),
                   mean_adjacency = mean(Adjacency_combined),
                   sd_adjacency = sd(Adjacency_combined),
                   mean_R = mean(R),
                   sd_R = sd(R),
                   mean_RNG = mean(RNG),
                   sd_RNG = sd(RNG),
                   mean_RNG2 = mean(RNG2),
                   sd_RNG2 = sd(RNG2),
                   mean_TPI = mean(TPI),
                   sd_TPI = sd(TPI),
                   mean_repGap = mean(`Repetition gap (mean)`),
                   sd_repGap = sd(`Repetition gap (mean)`)
  )

