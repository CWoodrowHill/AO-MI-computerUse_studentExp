library(pwr)

## Power analyses for computer AO+MI studies in students and people with Parkinson's ##

# Parkinson's keyboard experiment

pwr.anova.test(k = 5, n = 50, f = 0.25, sig.level = 0.05)

# where k is the number of groups/conditions and n is the common sample size in 
 #each group.
# Cohen suggests that f values of 0.1, 0.25, and 0.4 represent small, medium, 
 #and large effect sizes respectively.

# Parkinson's mouse experiment

pwr.anova.test(k = 4, n = 50, f = 0.25, sig.level = 0.05)

# Student experiment

pwr.anova.test(k = 4, n = 30, f = 0.25, sig.level = 0.05)
