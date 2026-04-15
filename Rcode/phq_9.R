### Using the PHQ-9 item depression inventory
library(tidyverse)
library(labelled)
library(gt)
library(ggcorrplot)
library(parameters)
library(performance)
library(psych)

loc<-url('https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/depression_scores.rds')
scores<-readRDS(loc)


# Creating a list of labels from the data
score_labels<-sapply(scores, function(x) attr(x, "label"))|>
  unname()

labels_frame <- data.frame(Variable =  colnames(scores) , 
                           Label = score_labels)


# Assess the adequacy for factor analysis using check_factorstructure


# Create a correlation matrix and plot it


# Run a factor analysis with 2 latent dimensions


# Analyze your results (make a table)
# and see if you can identify or name the two (potential) dimensions based on the loadings







