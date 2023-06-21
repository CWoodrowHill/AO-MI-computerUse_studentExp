Scripts used for the main analyses are run in the following order:

1. exp1_dataWrangling_student.R - this is used on each individual raw data file to tidy the data into a manageable format
2. exp1_combine_exclusions_student.R - this combines all tidied indiviudal data into one file and applies data exclusions
3. exp1_finalAnalyses_student.R - this takes the combined datasets after exclusions applied and conducts the main analyses

Other scripts in this folder:

exp1_interimAnalyses_student.R - this replaced the finalAnalyses script described above when the interim analyses was performed after the first 25
                                 participants

exp1_kviq_ehi_student.R - this is used to analyse data from the KVIQ-10 and Edinburgh Handedness Inventory

exp1_LexOPS_WordGenerator_student.R - this was used to generate the word stimuli for both experiments

exp1_power_analysis.R - used to conduct the power analysis to estimate the sample size for Experiment 1

exp1_practiceTrials_student.R - this was used to see how many participants had needed to repeat the practice trials for the baseline condition

exp1_questionnaire_data_student.R - Analyses of questionnaire data, including the PHQ-9, CPQ-12 and demographic information

exp1_sequentialAnalysisDesign.R - how efficacy and futility thresholds were determined for sequential analyses to perform an interim 'look'