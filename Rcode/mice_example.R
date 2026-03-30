library(tidyverse)
library(mice)

# this string is just a link to a web page that holds the data: 
loc <- url('https://github.com/Neilblund/GVPT201-Spring-2026/raw/refs/heads/master/Data/survey_data.rds')

# read RDS reads the data from the URL into R:
gvpt_survey<-readRDS(loc)

# filtering out data where most of the feeling thermometers are missing
survey<-gvpt_survey|>
  rowwise()|>
  mutate(na_count = sum(is.na(x= c_across(starts_with("ft_")))))|>
  filter(na_count<10)

# create a list containing only the variables you want to impute/use for imputation
variables <- c(
  "pid_summary",
  "age",
  "ft_noem",
  "ft_trump",
  "ft_ice",
  "ft_natguard",
  "ft_vance",
  "ft_immigrants",
  "pol_news"
)

# making a predictor matrix for imputations
predMat <- survey |>           
  select(all_of(variables)) |> # select only the columns 
  make.predictorMatrix()       # create a predictor matrix

# The row name is the target variable, the columns indicate the predictors. 
# Changing x[i,j] to 0 would prevent variable j from being used to predict row i

print(predMat)

imputedData<- mice(
  survey_select,             # original data 
  predictorMatrix = predMat, # Matrix of predictors
  m=5,                       # number of imputed data sets to create
  maxit=50,                  # maximum iterations before quitting
  meth='pmm',                # predictive mean matching
  seed=500                   # random number seed for replicability
  
)
bwplot(imputedData)           # box plots comparing imputations to original data

fit <- with(imputedData, 
            lm(ft_noem ~ 1))|>     # estimating the mean of ft_noem across each imputed data set
  pool()|>                         # pooling the fits
  summary(conf.int = TRUE)         # getting a pooled estimate with a confidence interval

fit
# comparison with original:

t.test(survey$ft_noem, na.rm=T)|>
  broom::tidy()


# note that there's not a huge difference here, but the estimated support for Noem
# is *slightly* increased under the imputation model





