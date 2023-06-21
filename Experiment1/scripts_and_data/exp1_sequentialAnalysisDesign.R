library(rpact)
library(gsDesign)
library(ggplot2)
help(package = "rpact")

## Sequential analysis design for student and Parkinson's computer AO+MI studies ##

#### RPACT ####

# Example: non-binding futility boundary at each interim in case
# estimated treatment effect is null or goes in "the wrong direction"
design <- getDesignGroupSequential(sided = 2, alpha = 0.05,
                                   informationRates = c(0.5, 1), typeOfDesign = "asOF",
                                   futilityBounds = c(0,0), bindingFutility = FALSE)
# Formal beta-spending function
# Example: non-binding futility boundary at each interim in case
# estimated treatment effect is null or goes in "the wrong direction"
design <- getDesignGroupSequential(sided = 2, alpha = 0.025, beta = 0.2,
                                   informationRates = c(0.5, 1), typeOfDesign = "asOF",
                                   typeBetaSpending = "bsP")

# Example: non-binding futility boundary using an O'Brien & Fleming type
# beta spending function. No early stopping for efficacy (i.e., all alpha
# is spent at the final analysis).
design <- getDesignGroupSequential(sided = 2, alpha = 0.025, beta = 0.2,
                                   informationRates = c(0.5, 1), typeOfDesign = "asUser",
                                   userAlphaSpending = c(0, 0.025), typeBetaSpending = "bsOF",
                                   bindingFutility = FALSE)

#### gsDesign ####

# Derive Group Sequential Design 
Design <- gsDesign (k = 2 , test.type = 4 , alpha = 0.05 , beta = 0.1 ,
                    astar = 0 , timing = c( 1 ) , sfu = sfLDOF , sfupar = c( 0 ) , 
                    sfl = sfLDOF , sflpar = c( 0 ) , delta = 0 , delta1 = 0.25 , 
                    delta0 = 0 , endpoint = 'user' , n.fix = 50 ) 
# k = number of looks (?)
# test.type = 4 = two-sided, asymmetric, beta-spending with non-binding lower bound
# alpha is always one-sided (default is 0.025?)
# beta = 1-power (e.g. 1 - 0.9 = 0.1)
# delta = Effect size for theta under alternative hypothesis. This can be set to
 # the standardized effect size to generate a sample size if n.fix=NULL. See details and
 # examples
# n.fix = Sample size for fixed design with no interim; used to find maximum group 
 #sequential sample size. 
# timing = Sets relative timing of interim analyses. Default of 1 produces equally spaced
 #analyses.
# sfu (spending function upper) = A spending function or a character string indicating a boundary type (that 
 #is, "WT" for Wang-Tsiatis bounds, "OF" for O'Brien-Fleming bounds and "Pocock"
 #for Pocock bounds). For one-sided and symmetric two-sided testing is used
 #to completely specify spending (test.type=1,2), sfu. The default value is
 #sfHSD which is a Hwang-Shih-DeCani spending function
# sfupar = Real value, default is ???4 which is an O'Brien-Fleming-like conservative 
 #bound when used with the default Hwang-Shih-DeCani spending function. This is
 #a real-vector for many spending functions. The parameter sfupar specifies
 #any parameters needed for the spending function specified by sfu; this will be
 #ignored for spending functions (sfLDOF, sfLDPocock) or bound types ("OF",
 # "Pocock") that do not require parameters
# sfl = spending function lower
# endpoint = how the data collection endpoint is determined

# Plot Design: Boundaries (Z) 
plot ( Design , plottype = 1 , xlab = 'Sample size' , ylab = 'Normal critical value' ) 

# Tabular Output 
gsBoundSummary ( Design ) 

# Design Summary 
cat ( summary ( Design ) ) 
