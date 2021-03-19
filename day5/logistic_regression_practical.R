library(tidyverse)
library(epiDisplay)
## read in the data
# Read in CSV data 
onch <-read_csv("onchall.csv")

## fit the logistic model
m1 <- glm(mf~area, data=onch,family=binomial) # Run model
# Show model results
summary(m1) 

#transform the coeffs into ORs #
exp(coef(m1))  
# and show their CIs   #
exp(confint(m1))  

## display
logistic.display(m1)

## model 2 adjusting for the age as a confounder
m2 <- glm(mf ~ area  + as.factor(agegrp), data=onch, family=binomial) # Fit the model
# Show model results
summary(m2)

# transform the coeffs into ORs #
exp(coef(m2))
# and show their CIs
exp(confint(m2))  

logistic.display(m2)
