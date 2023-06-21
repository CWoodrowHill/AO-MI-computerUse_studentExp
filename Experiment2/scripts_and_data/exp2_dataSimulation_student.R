# Before installing: options("install.lock"=FALSE)

### Script to simulate an additional independent variable for the student
### typing study of stimulus speed

library(tidyverse)
library(lmerTest)
library(emmeans)
library(mefa) #needed for rep() to repeat a dataframe

#### Import existing student data (from Experiment 1) ####

#Whole word time
wwData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_v2_ww.csv')

#Tidy data

wwDatamut2 <- wwData2 %>%
  mutate(ParticipantID = as.character(ParticipantID), Order = factor(Order.x), 
         Condition = factor(Condition, c("base", "control" ,"AOMIgen", "AOMIspec")),
         Word = as.character(Word), 
         typedWord = as.character(typedWord), Keys = as.character(Keys),
         wwTime = as.numeric(wholeWordTime), PHQ9 = as.integer(PHQ9), 
         tStyle = factor(tStyle))

#### Simulate some data for an additional independent variable ####

#We need to create new columns which add a SD of difference onto the 1.5X
#and 2X speed columns to simulate differences we might find in these conditions
#This won't simulate any interaction between speed and condition (i.e. whether
#differences between conditions are smaller or larger at certain speeds) but
#as we are more interested in general power, we won't worry too much about this

#Note that at the time of experiment design we contemplated 2 additional stimulus
 #speeds but only ended up adding 1, hence the simulation of 2
 #different speeds.

#Aoyama et al. (2020) also looked at speed of AO video
#Results given as mean improvement rates:
#1X = 9.4%, SD: 4.4%
#1.5X = 15.4%, SD: 8.4%
#2.25X = 5.6%, SD: 10.6%

#Work out % difference between conditions. These are the values from the paper

diff1mean <- 9.4/15.4
diff2mean <- 9.4/5.6

diff1sd <- 4.4/8.4
diff2sd <- 4.4/10.6

#Or use altered values to play around with the effect size. We can keep the means
 #the same and just increase the SDs to decrease the effect size
# diff1mean <- 9.4/9.4
# diff2mean <- 9.4/9.4
# 
# diff1sd <- 4.4/4.4
# diff2sd <- 4.4/4.4

#Now get the mean for each condition from the existing data

WWmeans <- wwDatamut2 %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(mean_WholeWord = mean(wwTime), sd_WholeWord = sd(wwTime))

#Adjust the means for each condition according to values from Aoyama et al. (2020)
#AOMIspec first
#1.5X

spec1.5mean <- WWmeans[4,2]*diff1mean
spec1.5sd <- WWmeans[4,3]*diff1sd

wwDatSim <- wwDatamut2 %>%
  filter(Condition == "AOMIgen" | Condition == "AOMIspec") %>%
  mutate(`1X` = wwTime) %>%
  select(-wwTime)

condCount <- plyr::count(wwDatSim$Condition) #find how many rows there are for each 
#condition

#Simulate data based on our simulated means and SDs

spec1.5_sim <- rnorm(condCount[2,2], spec1.5mean[1,1], spec1.5sd[1,1])

#2.25X

spec2.25mean <- WWmeans[4,2]*diff2mean
spec2.25sd <- WWmeans[4,3]*diff2sd

spec2.25_sim <- rnorm(condCount[2,2], spec2.25mean[1,1], spec2.25sd[1,1])

#Now AOMIgen

gen1.5mean <- WWmeans[3,2]*diff1mean
gen1.5sd <- WWmeans[3,3]*diff1sd

gen1.5_sim <- rnorm(condCount[1,2], gen1.5mean[1,1], gen1.5sd[1,1])

#2.25X

gen2.25mean <- WWmeans[3,2]*diff2mean
gen2.25sd <- WWmeans[3,3]*diff2sd

gen2.25_sim <- rnorm(condCount[1,2], gen2.25mean[1,1], gen2.25sd[1,1])

#NOTE THAT YOU ARE SIMULATING THE NORMAL DISTRIBUTION, NOT GAMMA WHICH WOULD BE
#MORE ACCURATE

genConds <- rep("AOMIgen", times = length(gen2.25_sim))
specConds <- rep("AOMspec", times = length(spec2.25_sim))

#Combine simulated data
genSim <- data.frame(genConds, gen1.5_sim, gen2.25_sim) %>%
  mutate(Condition = genConds,
         `1.5X` = gen1.5_sim,
         `2.25X` = gen2.25_sim) %>%
  select(-c(genConds, gen1.5_sim, gen2.25_sim))

specSim <- data.frame(specConds, spec1.5_sim, spec2.25_sim) %>%
  mutate(Condition = specConds,
         `1.5X` = spec1.5_sim,
         `2.25X` = spec2.25_sim) %>%
  select(-c(specConds, spec1.5_sim, spec2.25_sim))

AOMIsim <- merge(genSim, specSim, all = TRUE)

#Combine simulated and existing data

wwDatSim2 <- cbind(wwDatSim, `1.5X` = AOMIsim$`1.5X`,
                   `2.25X` = AOMIsim$`2.25X`)


#### Test analysis works with one simulation version ####

#Put in long format

wwDatSim_Long <- wwDatSim2 %>%
  pivot_longer(cols = c(`1.5X`, `1X`, `2.25X`), 
               names_to = "videoSpeed",
               values_to = "wwTime") %>%
  #remove wwTimes that are < 0.1 as these would never be real values in actuality
  #and filter on touch typists
  dplyr::filter(wwTime >= 0.1 & tStyle == "touch") %>%
  droplevels()

#Check no. PIDs now
plyr::count(unique(wwDatSim_Long$ParticipantID)) #47 is correct

wwDatSim_long2 <- wwDatSim_Long %>%
  mutate(videoSpeed = factor(videoSpeed, c("1X", "1.5X", "2.25X")))

negCheck <- wwDatSim_long2 %>%
  filter(wwTime < 0)

wwDatSim_long3 <- wwDatSim_long2 %>%
  filter(wwTime > 0.1)

#Run model on simulated data
wwSimMod <- glmer(wwTime ~ Condition + videoSpeed + Condition:videoSpeed + PHQ9
                  + Condition:PHQ9 + Order + Condition:Order  +
                    (1|Word) + (1 + Condition | ParticipantID),
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                  family = Gamma(link = "log"), nAGQ = 0, data = wwDatSim_long3)

wwSimMod_sum <- summary(wwSimMod)

wwsimMod_comps <- emmeans(wwSimMod, pairwise ~ videoSpeed, adjust = "tukey",
                          lmer.df = 'satterthwaite', type = "response")

#### Plot ####
wwSimPlot <-wwDatSim_long2 %>%
  ggplot(aes(x = videoSpeed, y = wwTime, colour = Condition)) +
  ggtitle("Mean time to type whole word by video speed") +
  xlab("Video Speed") +
  ylab("Time to type whole word (secs)")  +
  geom_violin() +
  stat_summary(fun = "mean",
               width = 0.8,
               geom = "crossbar",
               color = "black") +
  geom_jitter(alpha = 0.1) +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), 
        axis.title.x = element_text(size=9),
        axis.title.y = element_text(size=9),
        axis.line = element_line(colour = "black"))
wwSimPlot

#### Decrease number of participants ####

#Our existing dataset has 47 Ps. To reduce this, use below

numPs <- 20 #no. participants you want. Must be < 47 - for more than this, see
 #below section

wwDatSim3 <- wwDatSim2 %>%
  filter(tStyle == "touch")

keepPs <- head(unique(wwDatSim3$ParticipantID), numPs)

wwDatSim3 <- wwDatSim3 %>%
  filter(ParticipantID %in% keepPs)

#Check you have the number of Ps you intended
plyr::count(unique(wwDatSim3$ParticipantID))

#Rename so this works with the simulation code
wwDatSim_ex2 <- wwDatSim3

condCount_ex <- plyr::count(wwDatSim_ex2$Condition)
condCount_ex


#### Increase number of participants ####

#This is optional - to increase the dataset from n=47

#Replicate data in wide format so we can increase the number of participants

reps <- 0 #when = 0
loops <- -1 #and when loops also = -1, we are running this code but keeping the 
#number of participant as the original sample
PID <- wwDatSim2$ParticipantID
wwDatSim_ex <- wwDatSim2 %>%
  filter(tStyle == "touch")

repeat {
  loops <- loops + 1 #add 1 to the variable 'loops' on each repeat so we go down
  #the rows on each loop iteration
  PID <- append(PID, paste(wwDatSim2$ParticipantID, LETTERS[loops]))
  wwDatSim_ex <- rbind(wwDatSim_ex, wwDatSim2) #rbind the dataset onto itself
  #so we are repeating the dataframe
  if (loops == reps) {
    break
  }
}

wwDatSim_ex2 <- cbind(wwDatSim_ex, PID = PID)

wwDatSim_ex2$Condition <- sort(wwDatSim_ex2$Condition)

#find how many rows there are for each condition
condCount_ex <- plyr::count(wwDatSim_ex2$Condition)
condCount_ex


#### Run simulated data through a model n times ####

#If you have NOT changed the number of participants in the sections above, (i.e. n=47)
 #run the below commented lines down to condCount_ex

# wwDatSim_ex2 <- wwDatSim2 %>%
#   filter(tStyle == "touch")
# condCount_ex <- plyr::count(wwDatSim_ex2$Condition)
# condCount_ex

reps1 <- 100 #how many simulations do we want to do?
loops1 <- 0
contrast = vector() #Create empty vectors ready for filling
p.value = vector()
wwPs <- data.frame(contrast, p.value) #Build empty df ready for filling

repeat {
  loops1 <- loops1 + 1
  set.seed(1234+loops1) #make random generation reproducible
  
  #Generate random data for new video speeds (normal distribution)
  spec1.5_sim <- rnorm(condCount_ex[2,2], spec1.5mean[1,1], spec1.5sd[1,1])
  spec2.25_sim <- rnorm(condCount_ex[2,2], spec2.25mean[1,1], spec2.25sd[1,1])
  gen1.5_sim <- rnorm(condCount_ex[1,2], gen1.5mean[1,1], gen1.5sd[1,1])
  gen2.25_sim <- rnorm(condCount_ex[1,2], gen2.25mean[1,1], gen2.25sd[1,1])
  genConds_ex <- rep("AOMIgen", times = length(gen2.25_sim))
  specConds_ex <- rep("AOMspec", times = length(spec2.25_sim))
  
  #Combine our simulated data
  genSim <- data.frame(genConds_ex, gen1.5_sim, gen2.25_sim) %>%
    mutate(Condition = genConds_ex, `1.5X` = gen1.5_sim, `2.25X` = gen2.25_sim) %>%
    select(-c(genConds_ex, gen1.5_sim, gen2.25_sim))
  
  specSim <- data.frame(specConds_ex, spec1.5_sim, spec2.25_sim) %>%
    mutate(Condition = specConds_ex, `1.5X` = spec1.5_sim, `2.25X` = spec2.25_sim) %>%
    select(-c(specConds_ex, spec1.5_sim, spec2.25_sim))
  
  AOMIsim <- merge(genSim, specSim, all = TRUE)
  
  #Now cbind to main df
  wwDatSim_ex3 <- cbind(wwDatSim_ex2, `1.5X` = AOMIsim$`1.5X`,
                        `2.25X` = AOMIsim$`2.25X`)
  #Now put into long format
  wwDatSim_ex3_long <- wwDatSim_ex3  %>%
    pivot_longer(cols = c(`1X`, `1.5X`, `2.25X`),
                 names_to = "videoSpeed",
                 values_to = "wwTime")
  
  #Remove negative values
  wwDatSim_ex3_long  <- wwDatSim_ex3_long  %>%
    filter(wwTime > 0.1)
  
  #Run model
  wwMod_ex <- glmer(wwTime ~ Condition + videoSpeed + Condition:videoSpeed + PHQ9
                    + Condition:PHQ9 + Order + Condition:Order  +
                      (1|Word) + (1 + Condition | ParticipantID),
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
                    family = Gamma(link = "log"), nAGQ = 0, data = wwDatSim_ex3_long)
  
  #Look at main effect of videoSpeed
  wwsimMod_compsEx <- emmeans(wwMod_ex, pairwise ~ videoSpeed, adjust = "tukey",
                              lmer.df = 'satterthwaite', type = "response")
  
  #Extract contrasts from emmList object
  wwsimMod_df <- as.data.frame(wwsimMod_compsEx$contrasts)
  
  #Select columns we want and keep track of which value is associated with which
   #simulation
  wwsimMod_p <- wwsimMod_df %>%
    select(contrast, p.value) %>%
    mutate(simulation = rep(loops1, times = 3))
  
  #Combine different simulations into one df
  wwPs <- rbind(wwPs, wwsimMod_p)
  
  if (loops1 == reps1) {
    break
  }
}

#There are 3 comparisons, so each simulation gives 3 p values
wwPs %>%
  filter(p.value < .05) %>%
  count()








