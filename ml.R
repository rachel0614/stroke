# my os (windows Chinese version)
# the date format converts incorrectly without setting the locale
Sys.setlocale("LC_TIME", "English")
# load and convert the below columns 
stroke_data <- within(read.csv("data/stroke.csv", na = "N/A", 
                               stringsAsFactors = FALSE),{
                                 gender <- factor(gender)
                                 hypertension <- factor(hypertension)
                                 heart_disease <- factor(heart_disease)     
                                 ever_married <- factor(ever_married)    
                                 work_type <- factor(work_type) 
                                 Residence_type <- factor(Residence_type) 
                                 smoking_status <- factor(smoking_status)
                                 stroke <- factor(stroke)
                                 Date <- as.Date(Date,"%A %d %B %Y")
                               })
# have a look at the data, to make sure the data type conversion has been done correctly.
head(stroke_data, 2)
str(stroke_data)
# lowercase the variable name
colnames(stroke_data)[colnames(stroke_data) == "Residence_type"] <- "residence_type"
colnames(stroke_data)[colnames(stroke_data) == "Date"] <- "date"

# summary of the dataset
summary(stroke_data)

# we're not going to do a time related predictive model,
# so we remove it first
stroke_data <- subset(stroke_data, select = -c(date))

# gender
# remove gender=='Other', just one row
stroke_data <- stroke_data[stroke_data$gender != 'Other', ]
# convert from Female/Male to 0/1
gender_col <- ifelse(stroke_data$gender=='Female', 0, 1)
stroke_data$gender <- as.factor(gender_col)

# processing missing values
# install.packages("VIM")
library(VIM) 
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
summary(missing_values)
# remove missing values (only bmi has null values from the above summary)
stroke_data <- na.omit(stroke_data)

# ever_married
# convert from No/Yes to 0/1
marital_status_col <- ifelse(stroke_data$ever_married=='No', 0, 1)
stroke_data$ever_married <- as.factor(marital_status_col)


# Residence_type
# convert from Rural/Urban to 0/1
residence_type_col <- ifelse(stroke_data$residence_type=='Rural', 0, 1)
stroke_data$residence_type <- as.factor(residence_type_col)


# new variable : overweight
overweight_col <- cut(stroke_data$bmi,
                      breaks = c(0, 25, Inf),
                      labels = c(0, 1),
                      right = FALSE,
                      order = FALSE)
stroke_data$overweight <- as.factor(overweight_col)

# work_type
# convert labeled factor to 
stroke_data$work_type <- as.factor(unclass(stroke_data$work_type))

# represent smoking_status by number
stroke_data$smoking_status <- as.factor(unclass(stroke_data$smoking_status))

# in order to see the value distribution of the categorical variables by summary function,
# we keep these variables as factor type when doing cleansing & transformation
summary(stroke_data)

# from the summary, we can see all the dichotomous categorical variables (only 2 types)  
# have been converted to 0/1, including  
dich_cols = c("gender", "hypertension", "heart_disease", "ever_married", 
              "residence_type", "stroke", "overweight")

# convert these dichotomous variables to numeric
stroke_data[, dich_cols] <-
  apply(stroke_data[, dich_cols], 2, function(x) as.numeric(x))

# norminal variables 
dummy_cols <- c("work_type", "smoking_status")

# now dummy encode those categorical variables which have over three levels
library(fastDummies)
# keep the dummy variables
stroke_data_dummy <- dummy_cols(stroke_data, 
                                select_columns = dummy_cols, 
                                remove_first_dummy = FALSE,
                                remove_selected_columns = TRUE)
str(stroke_data_dummy)
summary(stroke_data_dummy)
# set rownames as id
rownames(stroke_data_dummy) <- stroke_data_dummy$id 
###############################data variables have been prepared################
attach(stroke_data_dummy)
opar <- par(no.readonly = TRUE)
library(data.table)
library(dplyr)
library(tidyr)
library(corrplot)
library(scorecard)

# training and testing set
# use the same seed later to make sure getting the same datase
# data <- split_df(stroke_data_dummy, y=age, ratio = 0.7, seed = 1,
# replace = FALSE)
set.seed(1)
no_rows_data <- nrow(stroke_data_dummy)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = 
                   , FALSE)
data_train <- stroke_data_dummy[sample, ]
data_test <- stroke_data_dummy[-sample, ]

# building the MLR model
# initial model
# attach(data$train)
# detach(data$train)
fit <- lm(age ~ 
            hypertension + heart_disease + ever_married +    
            stroke + bmi + avg_glucose_level +  
            work_type_1 + work_type_2 + work_type_3 + work_type_4 + 
            work_type_5 +
            smoking_status_1 + smoking_status_2 + smoking_status_3 + 
            smoking_status_4 , 
          data = data_train)
summary(fit)
confint(fit, "(Intercept)", level = 0.95)

# assumption - linearity
library(car)
# residuals & fitted
plot(fit, which = 1)

# outliers
qqPlot(fit, id.method="identify", 
       label = row.names(id),
       simulate=TRUE, which = 2,
       main="Q-Q Plot")

# histogram of the studentized
# residuals and superimposes a normal curve, kernel-density curve, and rug plot
student_fit <- rstudent(fit)
hist(student_fit,
     breaks = 10,
     freq = FALSE,
     xlab = "studentized residual",
     main = "distribution of errors")
rug(jitter(student_fit), col = "brown")
curve(dnorm(x, mean = mean(student_fit), 
            sd = sd(student_fit)),
      add = TRUE, col = "blue", lwd = 2)
lines(density(student_fit)$x, density(student_fit)$y, 
      col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("normal curve", "kernel density curve"), 
       lty = 1:2, col = c("blue", "red"), cex = 0.7)

# Influential observations
plot(fit, pch = 10, cex = 2, main="Influential observations ") 
abline(h = 4 * mean(fit, na.rm=T), col="red")  # add cutoff line
text(x = 1:length(fit) + 1, y = fit, 
     labels=ifelse(fit > 4 * mean(fit, na.rm = T), 
                   names(fit),""),col="red")


# influential observations
cutoff <- 4/((nrow(data_train) - length(fit$coefficients) - 2)) 
plot(fit, which = 4, cook.levels = cutoff)

# test outliers
outlierTest(fit)
# plot bmi & avg_glucose_level
plot(stroke_data_dummy$bmi, main = "bmi")
plot(stroke_data_dummy$avg_glucose_level, main = "average glucose level")

# remove wrong values of bmi
subset(data_train , bmi > 90, 
       select = c('id', 'age', 'bmi', 'avg_glucose_level'))

# list outliers' value
subset(data_train , id %in% c(40704, 54724,47917 ,29552), 
       select = c('id', 'age', 'bmi', 'avg_glucose_level'))
# remain outliers because the p-value is less than 0.05


# remove outlier
# stroke_data_dummy <- 
stroke_data_dummy <- (stroke_data_dummy[stroke_data_dummy$id != 56420 & 
                                          stroke_data_dummy$id !=51856, ])



# resplit training and testing set
# training and testing set
# use the same seed later to make sure getting the same datase
set.seed(1)
no_rows_data <- nrow(stroke_data_dummy)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = 
                   , FALSE)
data_train <- stroke_data_dummy[sample, ]
data_test <- stroke_data_dummy[-sample, ]

fit <- lm(age ~ 
            hypertension + heart_disease + ever_married +    
            stroke + bmi + avg_glucose_level +  
            work_type_1 + work_type_2 + work_type_3 + work_type_4 + 
            work_type_5 +
            smoking_status_1 + smoking_status_2 + smoking_status_3 + 
            smoking_status_4 , 
          data = data_train)
summary(fit)
str(stroke_data_dummy)
# assumption - linearity
# par(opar)
# par(mfrow=c(2, 2))
avPlots(fit)
# 
# termplot(fit, terms = "value1", 
#          partial.resid = TRUE, 
#          se = TRUE, ask = FALSE, las = 1, 
#          col.res = "black")

influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

# list outliers' value
subset(data_train , id %in% c(54724,5387 ,18605,46136,26191,13861), 
       select = c('id', 'age', 'bmi', 'avg_glucose_level'))

# resplit training and testing set
# training and testing set
# use the same seed later to make sure getting the same datase
# set.seed(1)
# no_rows_data <- nrow(stroke_data_dummy)
# sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = 
#                    , FALSE)
# data_train <- stroke_data_dummy[sample, ]
# data_test <- stroke_data_dummy[-sample, ]
# 
# fit <- lm(age ~ 
#             hypertension + heart_disease + ever_married +    
#             stroke + bmi + avg_glucose_level +  
#             work_type_1 + work_type_2 + work_type_3 + work_type_4 + 
#             work_type_5 +
#             smoking_status_1 + smoking_status_2 + smoking_status_3 + 
#             smoking_status_4 , 
#           data = data_train)
# summary(fit)

# rsquared value decreased a little


# check correlation of variables
continuous_data <- subset(stroke_data_dummy, select = c(age, avg_glucose_level, bmi))
dichotomous_data <- subset(stroke_data_dummy, select = -c(avg_glucose_level, bmi, id))

# correlation check of continuous variables
library(psych)
pairs.panels(continuous_data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             main = "correlation of continuous variables"
)
# dichotomous correlation
stroke_matrix <- cor(dichotomous_data)
corrplot(stroke_matrix, type = "upper")
# show the four highest correlation values
cor(subset(dichotomous_data, select = c(age, ever_married, work_type_1, smoking_status_4)))

# variance inflation factor
# vif(fit)
# normality check
library(ggplot2)

# Linearity
# crPlots(fit) #Model has aliased term(s); df ambiguous.

# assumption 5 - test normality of the model
stdres = rstudent(fit)
qqnorm(stdres, 
       ylab="Standardized Residuals",  
       xlab="Normal Scores", 
       main="age of patient model") 
qqline(stdres, col = 2)

#create histogram of residuals
ggplot(data = data_train, 
       aes(x = fit$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'darkblue') +
  labs(title = 'Histogram of Residuals', 
       x = 'Residuals', 
       y = 'Frequency')
summary(fit)

# homescedasticity
ncvTest(fit)
# plot(fit ,which=1)

# p-value is greater than 0.05, we would assume that the error variance
# unchanges with the level of the fitted values
opar <- par(no.readonly = TRUE)
par(mfrow=c(2, 2), mar=c(2,2,2,2) )
plot(fit)
par(opar)
# scatter plot of the absolute standardized residuals
# versus the fitted values and superimposes a line of best fit
spreadLevelPlot(fit)
# Suggested power transformation:  0.5409144 
# a transformation is required
library(lmtest)
bptest(fit, studentize = FALSE)
# show different conclusion with ncvTest
# shapiro.test(residuals(fit))


# global validation
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)
# Heteroscedasticity    Assumptions acceptable

gobble1 <- Boot(fit, R=1000)
summary(gobble1)

# 
library(car)
# vif(fit)
# sqrt(vif(fit)) > 2
alias(fit)
# work_type_1 + work_type_4  + 
fit <- lm(age ~ 
            hypertension + heart_disease + ever_married +    
            stroke + bmi + avg_glucose_level +  
            work_type_2 + work_type_3 + 
            smoking_status_1 + smoking_status_2 + smoking_status_3 , 
          data = data_train)
summary(fit)

summary(powerTransform(data_train$age))
# power 0.8268
# apply a square-root transformation to improve the
# model’s fit to normality
# p-val 2.22e-16, that means there’s no strong
# evidence that a transformation is needed in this case

sqr_age <- sqrt(data_train$age)
data_train$sqr_age <- sqr_age

# compare
model <- lm(age ~ 
              hypertension + heart_disease + ever_married +    
              stroke + bmi + avg_glucose_level +  
              work_type_2 + work_type_3 + 
              smoking_status_1 + smoking_status_2 + smoking_status_3 , 
            data = data_train)
model_sqr <- lm(sqr_age ~ 
                  hypertension + heart_disease + ever_married +    
                  stroke + bmi + avg_glucose_level +  
                  work_type_2 + work_type_3 + 
                  smoking_status_1 + smoking_status_2 + smoking_status_3 , 
                data = data_train)
AIC(model, model_sqr)
# Durbin Waston test
durbinWatsonTest(model_sqr)

# check spread
spreadLevelPlot(model_sqr)
# Suggested power transformation:  1.367796 
vif(model)
vif(model_sqr)
# STEPWISE REGRESSION
library(MASS)
fit_test <- lm(sqr_age ~ 
                 hypertension + heart_disease + ever_married +    
                 stroke + bmi + avg_glucose_level +  
                 work_type_2 + work_type_3 + 
                 smoking_status_1 + smoking_status_2 + smoking_status_3 , 
               data = data_train)
stepAIC(fit_test, direction="backward")
# worktype_3 is dropped
# re-backward by replacing bmi with overweight 
fit_test <- lm(sqr_age ~ 
                 hypertension + heart_disease + ever_married +    
                 stroke + overweight + avg_glucose_level +  
                 work_type_2 , 
               data = data_train)
stepAIC(fit_test, direction="backward")


# leaps
library(leaps)
leaps <-regsubsets(sqr_age ~ 
                     hypertension + heart_disease + ever_married +    
                     stroke + overweight + avg_glucose_level +  
                     work_type_2 ,
                   data = data_train,
                   nbest=4)
plot(leaps, scale="adjr2")
summary(fit_test)

# examine accuracy
fit_model <- lm(age ~ 
                  hypertension + heart_disease + ever_married +    
                  stroke + bmi + avg_glucose_level +  
                  work_type_2 +  
                  smoking_status_1 + smoking_status_2 + smoking_status_3,
                data = data_train)

fit_model_sqr <- lm(sqr_age ~ 
                      hypertension + heart_disease + ever_married +    
                      stroke + bmi + avg_glucose_level +  
                      work_type_2 +  
                      smoking_status_1 + smoking_status_2 + smoking_status_3,
                    data = data_train)

fit_model_sqr_simpler <- lm(sqr_age ~ 
                              hypertension + heart_disease + ever_married +    
                              stroke + overweight + avg_glucose_level +  
                              work_type_2  ,
                            data = data_train)

predicted <- predict(fit_model, data_test)
predicted_sqr <- predict(fit_model_sqr, data_test)
predicted_simpler <- predict(fit_model_sqr_simpler, data_test)
# model1 - age ~ .. + smoking_status
actuals_predictions <- data.frame(cbind(actuals = data_test$age, 
                                        predicted = predicted))
head(actuals_predictions)
summary(fit_model)
# model2 - sqr age + smoking_status
actuals_predictions_sqr <- data.frame(cbind(actuals = data_test$age, 
                                            predicted = predicted_sqr))
head(actuals_predictions_sqr)
summary(fit_model_sqr)

# model3 - sqr age + simpler variables
actuals_predictions_simpler <- data.frame(cbind(actuals = test_data$age, 
                                                predicted = predicted_simpler))
head(actuals_predictions_simpler)
summary(fit_model_sqr_simpler)

# 
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
# MAPE
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / 
                           apply(actuals_predictions, 1, max))
min_max_accuracy
sigma(fit_model)/ mean(actuals_predictions$age)
