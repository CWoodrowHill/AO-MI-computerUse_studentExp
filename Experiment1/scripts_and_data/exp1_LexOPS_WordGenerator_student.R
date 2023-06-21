library(LexOPS)

LexOPS::run_shiny() #run user interface

stim <- LexOPS::lexops %>%
  subset(Zipf.SUBTLEX_UK >= 1.1 & Zipf.SUBTLEX_UK <= 4) %>% 
  #filter word frequency according to Zipf score between 1.1 and 3.5
  subset(Length >= 6 & Length <= 6) %>%
  #filter word length of 6 letters
  subset(AoA.Kuperman >= 10 & AoA.Kuperman <= 25) %>%
  #filter age of acquisition as above 10 years
  subset(VAL.Glasgow_Norms >= 3 & VAL.Glasgow_Norms <= 6.6) %>%
  #filter valence score as between 3 and 6.6
  subset(AROU.Glasgow_Norms >= 2.8 & AROU.Glasgow_Norms <= 6.1) %>%
  #filter arousal score as between 2.8 and 6
  split_random(3) %>%
  #split into three groups (conditions)
  control_for(Phonemes.CMU, -2:3) %>%
  #control for number of phonemes with a tolerance of -2 to 2
  control_for(AoA.Kuperman, -2.5:2.5) %>%
  #control for age of acquisition with a tolerance of -2 to 2
  generate("all", "inclusive")
#generate as many words as possible, according to the above conditions 
 #inclusive of each word