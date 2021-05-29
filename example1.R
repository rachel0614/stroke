# https://rpubs.com/huiziy/544379
# how to select variables
# use the model selection method of “bestsubset”, “forward”
# and “backward” in the package “leaps”. 
# select the top predictors without losing too much of the model’s
# predicability
library(leaps)
library(sp)
data(meuse)
meuse <- na.omit(meuse)
model_bench <- lm(lead~.,data = meuse) ## notice: the "." indicates that we are using all other variables in the dataset except the response variable lead
summary(model_bench)

meuse$ffreq <- as.numeric(meuse$ffreq)
meuse$soil <- as.numeric(meuse$soil)
meuse$lime <- as.numeric(meuse$lime)
meuse$landuse <- as.numeric(meuse$landuse)

# best subset
# Best Subsets compares all possible models using a specified set of 
# predictors, and displays the best-fitting models that contain 
# one predictor, two predictors, and so on. 
# We begin by building a complete model with all the predictors. 
# Before using regsubset, let’s convert the categorical 
# variables into dummy


variables <- meuse[ , -which(names(meuse) == "lead")]
outcome <- meuse$lead
out_1 <- regsubsets(x = variables, y = outcome, method = "exhaustive", nvmax = dim(variables)[2])

## We output the summary based on best subset
summary(out_1)

# The summary plot shows which variables we want to select 
# given the number of desired variables we want to select 
# to be in our model. 
# However, how do we know how many variables we want to have in our model? 
# We need to establish some criteria for selecting.

# Criteria
# BIC: a criterion for model selection among a finite set of models; 
# the model with the lowest BIC is preferred
# cp: Cp is a ratio of the specification spread to the process spread.
# AdjRsquared: a modified version of R-squared that has been 
# adjusted for the number of predictors in the model. 
# The adjusted R-squared increases only if the new term
# improves the model more than would be expected by chance.


## plotting cp
plot(1:dim(variables)[2],summary(out_1)$cp)
lines(1:dim(variables)[2],summary(out_1)$cp)

## plotting BIC
plot(1:dim(variables)[2],summary(out_1)$bic)
lines(1:dim(variables)[2],summary(out_1)$bic)

## plotting adjRsquared
plot(1:dim(variables)[2],summary(out_1)$adjr2)
lines(1:dim(variables)[2],summary(out_1)$adjr2)

## choosing the variables that lead to lowest BIC, 
# Adjusted Rsquared and cp
which.min(summary(out_1)$bic)
which.min(summary(out_1)$cp)
which.max(summary(out_1)$adjr2)

# Based on bic and cp, 
# we should select the model with 5 variablesm and 
# according to adjusted Rsquared, we should select model with 9 variables.
# However, a closer inspection of the graph reveals 
# that the adjusted Rsquared reaches plateau after var = 5.
# So we select top 5 variables and construct a new linear model.

var_list <- rep(NA,dim(variables)[2] )
for (i in 1: dim(variables)[2]) {
  if (summary(out_1)$outmat[which.min(summary(out_1)$bic),i] == "*") {var_list[i] = TRUE}
  else {var_list[i] = FALSE}
}
var_selec <- variables[,var_list]

# Now we have selected the best 5 variables based on the best subset output,
# we build the model again
data_selec <- cbind(var_selec, meuse$lead)
colnames(data_selec)[6] <- "lead"
model_1 <- lm(lead~. ,data = data_selec) 
summary(model_1)
# Multiple R-squared:  0.9597,	Adjusted R-squared:  0.9584 

# From the above summary, we can see that all of our variables 
# are significant (we can see that from the * at the summary).
# Moreover, our Rsquared is 0.9597, not an extreme drop 
# from the Rsquared in model1 (0.9664), but we have sigificantly 
# reduced the number of variables.


# Part TWO: Forward and backward stepwise:
# Besides best subset, we can also use the forward and backward stepwise
# selection method in the leap package. 
# We demonstrate the variables selected by using forward and backward 
# stepwise selection below following similar procedures as best subset 
# function

## Forward stepwise
out_2 <- regsubsets(x = variables, y = outcome, method = "forward", nvmax = dim(variables)[2])
summary(out_2)


## plotting BIC
plot(1:dim(variables)[2],summary(out_2)$bic)
lines(1:dim(variables)[2],summary(out_2)$bic)

## plotting cp
plot(1:dim(variables)[2],summary(out_2)$cp)
lines(1:dim(variables)[2],summary(out_2)$cp)

## plotting adjRsquared
plot(1:dim(variables)[2],summary(out_2)$adjr2)
lines(1:dim(variables)[2],summary(out_2)$adjr2)

## choosing the variables that lead to lowest BIC, 
# Adjusted Rsquared and cp
which.min(summary(out_2)$bic)
which.min(summary(out_2)$cp)
which.max(summary(out_2)$adjr2)
out_3 <- regsubsets(x = variables, y = outcome, method = "backward", nvmax = dim(variables)[2])
summary(out_3)

## plotting BIC
plot(1:dim(variables)[2],summary(out_3)$bic)
lines(1:dim(variables)[2],summary(out_3)$bic)

## plotting cp
plot(1:dim(variables)[2],summary(out_3)$cp)
lines(1:dim(variables)[2],summary(out_3)$cp)

## plotting adjRsquared
plot(1:dim(variables)[2],summary(out_3)$adjr2)
lines(1:dim(variables)[2],summary(out_3)$adjr2)
## choosing the variables that lead to lowest BIC, Adjusted Rsquared and cp
which.min(summary(out_3)$bic)

# Similarly, forward and backward stepwise selection also 
# selects 5 variable (Ajusted Rsquared suggests 10 but we reject 
# it and choose 5 because of similar reason as best subset) 
# However, do we choose the same variables based on these methods?
  
var_list <- rep(NA,dim(variables)[2] )
for (i in 1: dim(variables)[2]) {
  if (summary(out_2)$outmat[which.min(summary(out_2)$bic),i] == "*") {var_list[i] = TRUE}
  else {var_list[i] = FALSE}
}
var_selec_2 <- variables[,var_list]

var_list <- rep(NA,dim(variables)[2] )
for (i in 1: dim(variables)[2]) {
  if (summary(out_3)$outmat[which.min(summary(out_3)$bic),i] == "*") {var_list[i] = TRUE}
  else {var_list[i] = FALSE}
}
var_selec_3 <- variables[,var_list]

colnames(var_selec)  
colnames(var_selec_2)
colnames(var_selec_3)

# We found that the best 5 are all the same across the three different 
# variable selection methods. In next section, we investigate 
# whether we need to conduct any more variable transformations.