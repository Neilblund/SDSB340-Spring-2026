### Using the PHQ-9 item depression inventory
library(tidyverse)
library(lavaan)
library(lavaanPlot)
library(effectsize)

loc<-url('https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/depression_scores.rds')
scores<-readRDS(loc)



# setup the one factor model. All items load onto a single latent variable called "depression"
oneModel <- 'depression =~ DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090'
# run the factor analysis:
fit1 <- cfa(model = oneModel, data = scores)

# View results
summary(fit1, fit.measures = TRUE)

# view a bunch of metrics at once, along with reminders about their conventional
# thresholds:
effectsize::interpret(fit1)


# Examine parameter estimates (loadings and variances)
parameterEstimates(fit1)


AVE(fit1)

# set up the 2-factor model. 
twoModel <- '
    cognitive_affective =~ DPQ010 + DPQ020 + DPQ060 + DPQ070 +  DPQ090
    somatic =~  DPQ030 + DPQ040 + DPQ050  + DPQ080
'

# running the factor analysis
fit2<-cfa(model=twoModel, data=scores)
# evaluate fit of model 2
summary(fit2, 
        fit.measures=TRUE, 
        standardized=TRUE) # showing standardized fits

# view a bunch of metrics at once, along with reminders about their conventional
# thresholds:
effectsize::interpret(fit2)

# Examine parameter estimates (loadings and variances)
parameterEstimates(fit2)



# Plotting results--------------------------------------------------------------


## Model 1 ---------------------------------------------------------------------

labels1 <- list(DPQ010 = "Anhedonia",
               DPQ020 = "Depressed mood",
               DPQ030 = "Sleep disturbance",
               DPQ040 = "Fatigue",
               DPQ050 = "Change in appetite",
               DPQ060 = "Low self-esteem",
               DPQ070 = "Trouble concentrating",
               DPQ080 = "Psychomotor disturbances",
               DPQ090 = "Suicidal ideation",
               depression = "Depression"
)


lavaanPlot(fit1, labels=labels1, 
           coefs=TRUE,  # Showing factor loadings
           stand = TRUE, # Standardized values
           graph_options = list(rankdir = "LR") # arrange horizontally
           
           )


### Model 2 --------------------------------------------------------------------
labels2 <- list(DPQ010 = "Anhedonia",
               DPQ020 = "Depressed mood",
               DPQ030 = "Sleep disturbance",
               DPQ040 = "Fatigue",
               DPQ050 = "Change in appetite",
               DPQ060 = "Low self-esteem",
               DPQ070 = "Trouble concentrating",
               DPQ080 = "Psychomotor disturbances",
               DPQ090 = "Suicidal ideation",
               cognitive_affective = "Cognitive/Affective",
               somatic = "Somatic"
               )


lavaanPlot(fit2, labels=labels2, 
           coefs=TRUE, 
           stand = TRUE,
           graph_options = list(rankdir = "LR")
           )


################################################################################
# Comparing models--------------------------------------------------------------
# If the models are nested (one model is a simpler version of the other) then
# you can use a likelihood ratio test to compare both models to each other:
lavTestLRT(fit1, fit2)
# the null hypothesis is that the models are equally good fits for the data,
# so a significant p-value (<.05) indicates that the more complex model does a 
# better job of explaining the data.


# If models are NOT nested, they can still be compared using AIC/BIC values, as 
# long as both models are using the same data. Remember that lower values indicate
# better fit:
BIC(fit1)
BIC(fit2)
# A rule of thumb when reading BIC values based on comparing their absolute
# differences:
# Absolute difference in BICs:
#   0-2: weak evidence for the model with the lower BIC
#   3-5:  moderate evidence for the model with the lower BIC
#   6-10: strong evidence for the model with the lower BIC
#   10+ very strong evidence for the model with the lower BIC
# 
abs(BIC(fit2) - BIC(fit1)) # Very strong evidence!



# What about a four-factor model?
fourModel <- '
    affective =~ DPQ010 + DPQ020
    somatic =~  DPQ030 + DPQ040 + DPQ050
    internalizing =~ DPQ060 + DPQ090
    sensorimotor =~DPQ070 + DPQ080
'

fit4<-cfa(model=fourModel, data=scores)
# The LRT indicates that this model fits better
lavTestLRT(fit4,  fit2, fit1)

# as does the BIC estimate:
abs(BIC(fit2) - BIC(fit4)) 
abs(BIC(fit1) - BIC(fit4)) 


