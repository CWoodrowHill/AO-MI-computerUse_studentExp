#options("install.lock"=FALSE)

library(tidyverse)

#### Import and tidy data ####

#Import KVIQ-10 Data
kviqdat <- read_csv("//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/studentRawData/KVIQ/ZoomSessionStudentData.csv")

#Import full sample for analysis (only need one measure)
wwData2 <- read_csv('//nask.man.ac.uk/home$/Documents/R_PhD/dataAnalysis/StudentCompExp/dataForAnalyses/finalStudentData_v2_ww.csv')

#Tidy the data a bit

kviqdat_tidy <- kviqdat %>%
  dplyr::filter(ParticipantID != "Example") 

#Filter data so we only have our included final sample of Ps. Use wwData2 as reference

#Remove 0s first so the IDs match up
kviqdat_tidy$ParticipantID <- str_remove(kviqdat_tidy$ParticipantID, "^0+")

kviqdat_tidy2 <- kviqdat_tidy[kviqdat_tidy$ParticipantID %in% wwData2$ParticipantID,]


#### Handedness ####

ehi_dat <- kviqdat_tidy2 %>%
  dplyr::select(ParticipantID, `EHI-Laterality`) %>%
  dplyr::distinct() #one row for each participant. Should be 51 obs

#Count how many of each handedness

ehi_dat %>%
  dplyr::group_by(`EHI-Laterality`) %>%
  dplyr::summarise(n())


#### KVIQ-10 ####

kviq_dat <- kviqdat_tidy2 %>%
  dplyr::select(ParticipantID, `KVIQ-VI_total(max=45)`, `KVIQ-KI_total(max=45)`) %>%
  dplyr::filter(!is.na(`KVIQ-VI_total(max=45)`) & !is.na(`KVIQ-KI_total(max=45)`)) %>% #remove rows with NAs
  dplyr::mutate(VIscore = as.integer(`KVIQ-VI_total(max=45)`),
                KIscore = as.integer(`KVIQ-KI_total(max=45)`)) 
#Should be 51 obs

#Get summaries
kviq_dat %>%
  dplyr::summarise(VImean = mean(VIscore),
                   KImean = mean(KIscore),
                   VIsd = sd(VIscore),
                   KIsd = sd(KIscore))

#### CRT #### 

crt_dat <- kviqdat_tidy2 %>%
  dplyr::select(ParticipantID, `CRT-Rhand`, `CRT_Lhand`) %>%
  dplyr::rename(CRT_right = `CRT-Rhand`,
                CRT_left = `CRT_Lhand`) %>%
  dplyr::filter(!is.na(CRT_right) & !is.na(CRT_left)) #remove rows with NAs

#Get summaries
crt_dat %>%
  dplyr::summarise(mean_right = mean(CRT_right),
                   sd_right = sd(CRT_right),
                   mean_left = mean(CRT_left),
                   sd_left = sd(CRT_left))
