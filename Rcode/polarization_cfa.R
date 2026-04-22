# Example using the depression data
library(tidyverse)
library(lavaan)
library(lavaanPlot)
library(effectsize)
library(semTools)

datafile<-'https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/polarization3.rds'
dat3<-readRDS(url(datafile))|>
  drop_na()
threeModel <- 'othering =~ o1 + o2 + o3
             aversion =~ a1 + a2 + a3
             moralization =~m1 + m2 +m3
'
fit<-cfa(threeModel, data =dat3)

# View results
summary(fit, fit.measures = TRUE)



# view a bunch of metrics at once, along with reminders about their conventional
# thresholds:
effectsize::interpret(fit)

#Average Variance Extracted is the average percentage of variation explained by
#a set of questions within a latent construct compared to the variance due to
#measurement error
semTools::AVE(fit)


fitpreds<-predict(fit, newdata=dat3)

# do these values correlate with the conventional measure of affective
# polarization (warmth bias)?
cor(cbind(fitpreds, "bias"= dat3$wbias))
# we can explore further in a linear model. Coefficients indicate the effect of
# a one unit increase in that dimension on the expected value of the dependent
# variable.
summary(lm(wbias  ~ fitpreds, data=dat3))

# what happens if we control for "Male"?
summary(lm(wbias  ~ fitpreds + male, data=dat3))

# what about the effect on support for political violence?
summary(lm(pv1 ~ fitpreds, data=dat3))
summary(lm(pv2 ~ fitpreds, data=dat3))



