library(tidyverse)
library(labelled)
library(gt)
library(ggcorrplot)
library(parameters)
library(performance)
library(psych)


## Data -------
datafile<-'https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/polarization1.rds'
dat1<-readRDS(url(datafile))
labelfile<-'https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/polarization_labels.rds'
labels_frame<-readRDS(url(labelfile))


# Getting only the scale columns
scale_cols<-labels_frame$Variable
scale_data<-dat1|>
  select(all_of(scale_cols))


################################################################################

# Descriptive stats

# Correlations
cmat<-cor(scale_data)  # Get initial correlations (Why are these all NA?)


# Getting correlations for pairwise observations

cmat<-cor(scale_data, 
          use= 'pairwise.complete.obs')

ggcorrplot(cmat, 
           lab = TRUE,  # add labels
           lab_size = 2 # shrink them to make them fit
           ) 


# Are any items highly correlated? Really strong correlates might need to be
# combined for EFA to give reasonable results. (if the items correlate
# perfectly, their effects loadings can't really be distinguished from one
# another)

# there's one correlation > .8 we *could* consider combining these (although
# the authors of the original paper ultimately don't do this)
cmat[upper.tri(cmat)]|>
  sort(decreasing=T)|>
  head(n=15)

# putting this in long format to help identify strong correlates:
cor_df<-data.frame(cmat)|>
  rownames_to_column(var = 'item1')|>
  pivot_longer(cols=-item1, names_to='item2')|>
  filter(item1!=item2)|>  
  mutate(pairing =  paste(pmin(item1, item2), pmax(item1, item2)))|>
  distinct(pairing, .keep_all = TRUE)|>
  arrange(-value)
  
# if we decided to combine these, we could do something like this: 
#scale_data_combined <- scale_data |>
#  mutate(
#    a34 = (a3 + a4) / 2,
#    m89 = (m8 + m9) / 2,
#    m26 = (m2 + m6 + 2) / 3,
#  ) |>
#  select(-a3, -a4, m8, m9, m2, m6) # and then drop the originals

# alpha (measure of scale reliability assuming tau equivalence and a single dimension)

alpha(scale_data)

# we should be skeptical of this estimate, though (why?)

################################################################################


#EFA ------------

# How much variance is common variance? 
# (ie.: Variance that comes from one or more shared latent factors)
# Higher scores = more common variance. A rule of thumb suggests overall scores
# should be > .6, and item scores below that threshold might need to be dropped

performance::check_factorstructure(scale_data)


# scree plot (how many latent factors do we have?)

scree(scale_data, 
      pc =FALSE) # default also shows scree plot for PCA


# maybe not! 
n_factors(scale_data, type='FA',
          rotation ='oblimin',
          algorithm = 'minres',
          n_max = 10
          )


# Considerations:

# You typically need at least 3 or 4 questions per latent factor in EFA. If you exceed
# this, you often won't be able to get valid statistics for EFA,

## Exploratory factor analysis:
efa<-fa(scale_data,       # the original data (cleaned and coded as numeric!)
            nfactors=3,       # number of factors based on analysis
            rotate='oblimin') # default oblique rotation (allows factors to correlate)
# This shows a lot of stuff! 
print(efa) 

# The diagram here is messy because we have a lot of items, but it can give you
# a quick visual for the pattern of loadings
diagram(efa)

### Top section: the pattern matrix________
# Loadings: 
#  The first three columns (MR1, MR2, MR3) represent the latent
#  factors, so they tell us the loading for each item onto each latent factor
# H2, U2, com:
#   h2 (communality): the proportion of variance explained by the latent factors.
#   u2 (uniqueness): 1 - h2. The variance not explained by the factors. 
#   com (complexity): values much higher than 1 = item may measure multiple factors.

### Factor Fit Statistics________
##  SS Loadings: sum of squared loadings per factor
#  Proportion var: variance explained by this factor
#  Cumulative var: cumulative variance explained by this factor
#  Proportion explained: Factors share of the common variance
#  Cumulative proportion explained: Factors cumulative share of the common variance

### Factor correlations _________
# For oblique rotations (like the one we used) this shows the correlation of each
# latent factor assumed by the model

### Model Fit Indices________
# Mean item complexity: values near 1 mean that most items load onto a single factor
# RMSEA (Root Mean Square Error of Approximation): lower values = better fit
# TLI (Tucker-Lewis Index): values > 0.95 suggest good fit.
# BIC: lower is better; useful for comparing models with different numbers of factors.
# RMSR (Root Mean Square of Residuals): average residual correlation; < 0.05 is ideal.
# Chi-square test: tests whether the factor model fits the data. 
#   A non-significant p-value means you can't reject the model
# Tucker Lewis Index: measures how well the model compares to a null model, ranges
# from 0 to 1

# Measures of factor score adequacy:
# Factor scores are basically a measure of how much of each latent factor an
# individual respondent has. These measures tell us how well the factor scores
# would capture the latent factors.


# Try comparing different numbers of factors: 
# Does adding additional factors improve the variance explained? 
# Does the BIC score improve with more/fewer factors? (BIC scores penalize
# non-parsimonious models, so we should expect to see a worse result if we
# include additional factors beyond what can be justified by the data)
# Are there conceptual reasons to prefer a factor structure that is simpler? For
# instance: if five factors perform better, might we still prefer 3 factors
# based on intuition or research goals?


### Visualizing factor loading matrix with a table -----------------------------
# The factor loadings can be viewed by just printing the efa object, but with
# 45 items this can be tough to read. We can do some reformatting to put this in 
# a table and then color code the results based on the factor loadings.
 
# we need to do some data prep to put this in an appropriate format...

# this does some of this processing automatically:
model_parameters(efa)

# we'll convert the loadings to a data frame, then use a join to add the labels
# back in for a table:
loadings_frame<-model_parameters(efa)|>
  right_join(labels_frame, by = join_by(Variable==Variable))

# Creating the table

gt(loadings_frame, rowname_col = 'Label') |> # use the labels as row names
  cols_hide(Variable) |>          # Hide the variable name since its already in the label
  data_color(
    # color code the factor loadings based on their values
    palette = "PuOr",
    columns = starts_with("MR"),
    domain = c(-1, 1)                     
  ) |>
  fmt_number(decimals = 2)                  # format numeric values


# What to do with the loading matrix in EFA:
# 1. If you don't have an a-priori expectation about what each dimension
# represents you can use the loadings to try to identify it.

# 2. You can also use the loadings to identify questions that might measure a
# dimension you didn't anticipate: M11 seems like it might actually be better
# for measuring the "othering" dimension rather than the "moralizing" one.

# 3. If we're interested in distinguishing the different dimensions, we should
# probably consider dropping items that load strongly onto more than one factor,
# since these won't do a good job of distinguishing those constructs. (you can
# see this in the loadings, but you could also look at the complexity scores)

# Based on the results: should any questions be dropped or re-categorized? The
# authors reduce this initial set to 20 items, and then reduce it further to 9
# items based on more data/analyses. If you had to pick 20, which would you
# choose?


# Calculating scores for each dimension for each respondent based on their responses
# weighted by the factor loadings
predicted_scores<-predict(efa, 
                          names=c("Othering", "Aversion", "Moralizing"),
                          data = scale_data
                          )

# Viewing histograms of factor loadings
hist(predicted_scores[,1])
hist(predicted_scores[,2])
hist(predicted_scores[,3])


### Using the PHQ-9 item depression inventory

loc<-url('https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/depression_scores.rds')
scores<-readRDS(loc)

score_labels<-sapply(scores, function(x) attr(x, "label"))|>
  unname()

labels_frame <- data.frame(Variable =  colnames(scores) , 
                           Label = score_labels)

# Use a scree plot to check the number of potential factors

scree(scores)
n_factors(scores)


# Assess the adequacy for factor analysis using check_factorstructure

performance::check_factorstructure(scores)


# Create a correlation matrix and plot it

cmat<-cor(scores)
ggcorrplot(cmat, lab = TRUE, lab_size = 2)


# Run a factor analysis with 2 latent dimensions


efa<-fa(scores, 
        nfactors=2) # exploratory factor analysis with 2 latent factors
print(efa)

parms<-model_parameters(efa)
print(parms)

loadings_frame<-model_parameters(efa)|>
  right_join(labels_frame, by = join_by(Variable==Variable))


gt(loadings_frame, rowname_col = 'Label') |> # use the labels as row names
  cols_hide(c(Variable)) |>          # Hide items and dimension columns
  data_color(
    # color code the factor loadings based on their values
    palette = "PuOr",
    columns = starts_with("MR"),
    domain = c(-1, 1)                     
  ) |>
  fmt_number(decimals = 2)                  # format numeric values





# Using scaled data in a model


datafile<-'https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/polarization2.rds'
dat2<-readRDS(url(datafile))

model_data<-dat2|>
  select(o1:m6, ftinparty, ftoutparty, wbias)

scale_data<-dat2|>
  select(o1:m6)

fmodel<-fa(scale_data, nfactors=3)
scale_values<-predict(fmodel, scale_data)

lm(wbias ~ scale_values,data=dat2)|>summary()






