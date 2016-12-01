## Logistic Regression Exercises

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

## Install and Load in all the packages below

install.packages("effects")
install.packages("dplyr")

library(effects)
library(dplyr)

#load in the data, remember to set your WD to the correct location:
NH11 <- readRDS("NatHealth2011.rds")

## Structure and Summary of the data
summary(NH11)
str(NH11)

## Answer to Exercise 1

## First let's look at the everwrk variable to understand the data
summary(NH11$everwrk)

## Looks like it's a factor with 5 levels and a lot of missing values
## Since we are only interested in the binary outcome, let's subset the data

## select only the everwrk, age_p, and maritl_r variables
logModelData <- select(NH11, everwrk, age_p, r_maritl)

## Filter the dataset so that we only have the binary outcomes
logModelData2 <- filter(logModelData, everwrk == "1 Yes" | everwrk == "2 No")

## Checkout a summary of the data
summary(logModelData2)

## Let's drop the levels that aren't needed/used in the logit model
logitModelData <- droplevels(logModelData2)
summary(logitModelData)

## Let's create our model
logitModel <- glm(everwrk ~ age_p + r_maritl, data = logitModelData, family = "binomial")
summary(logitModel)

## Answer to Exercise #2

## plotted version:
plot(allEffects(logitModel))

## data in "table"
data.frame(Effect("r_maritl", logitModel))

## Bonus--let's look at the probabilities for the age categories
data.frame(Effect("age_p", logitModel))
