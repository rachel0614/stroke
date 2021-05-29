# examples
# http://rstudio-pubs-static.s3.amazonaws.com/74431_8cbd662559f6451f9cd411545f28107f.html
# https://zhuanlan.zhihu.com/p/31642344
# https://bvasiles.github.io/empirical-methods/slides/15-zscore.html
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



# id is meaningless for prediction, and 
# we're not going to do a time related predictive model,
# so we remove these two columns first

stroke_data <- subset(stroke_data, select = -c(id, date))

# gender
# remove gender=='Other', just one row
stroke_data <- stroke_data[stroke_data$gender != 'Other', ]
# convert from Female/Male to 0/1
gender_col <- ifelse(stroke_data$gender=='Female', 0, 1)
stroke_data$gender <- as.factor(gender_col)

levels(stroke_data$smoking_status)
# processing missing values
#install.packages("VIM")
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

# new variable: diabetes
# 1 normal <= 140
# 2 pre-diabetes  <= 199
# 3 diabetes >199
# create a new variable diabetes to indicate the diabetes status
diabetes_col <- cut(stroke_data$avg_glucose_level,
                    breaks = c(0, 140, 199, Inf), 
                    labels = c(1, 2, 3), 
                    right = FALSE,
                    order = TRUE)
stroke_data$diabetes <- as.factor(diabetes_col)

# generate a new variable: bmi_level
# 1 underweight < 18.5
# 2 normal >=18.5
# 3 overweight >=25
# 4 obese >= 30
bmi_level_col <- cut(stroke_data$bmi,
                    breaks = c(0, 18.5, 25, 30, Inf), 
                    labels = c(1, 2, 3, 4), 
                    right = TRUE,
                    order = TRUE)
stroke_data$bmi_level <- as.factor(bmi_level_col)

# new variable : overweight
overweight_col <- cut(stroke_data$bmi,
                     breaks = c(0, 25, Inf), 
                     labels = c(0, 1), 
                     right = TRUE,
                     order = FALSE)
stroke_data$overweight <- as.factor(overweight_col)

# work_type
# convert labeled factor to 
stroke_data$work_type <- as.factor(unclass(stroke_data$work_type))

# smoking_status
stroke_data$smoking_status <- as.factor(unclass(stroke_data$smoking_status))

# in order to see the value distribution of the categorical variables by summary function,
# we keep these variables as factor type when doing cleansing & transformation
summary(stroke_data)
# from the summary, we can see all the dichotomous categorical variables (only 2 types)  
# have been converted to 0/1, including  
dich_cols = c("gender", "hypertension", "heart_disease", "ever_married", 
              "residence_type", "stroke", "overweight")

# convert these variables to numeric
stroke_data[, dich_cols] <-
  apply(stroke_data[, dich_cols], 2, function(x) as.numeric(x))

# categorical variables with more than 2 levels
dummy_cols <- c("work_type", "smoking_status", "diabetes", "bmi_level")
# now dummy encode those categorical variables which have over three levels
# including work_type,smoking_status,diabetes,bmi_level
# dummy these categorical columns 
library(fastDummies)
# keep the dummy variables
stroke_data_dummy <- dummy_cols(stroke_data, 
           select_columns = dummy_cols, 
           remove_first_dummy = FALSE,
           remove_selected_columns = FALSE)
str(stroke_data_dummy)
summary(stroke_data_dummy)
# convert these dummied variables to numeric
stroke_data_dummy[, dummy_cols] <-
  apply(stroke_data_dummy[, dummy_cols], 2, function(x) as.numeric(x))

# new generated dummy column names 
# convert to factor
# stroke_data_dummy[,dummy_cols] <- lapply(stroke_data_dummy[,dummy_cols] , factor)

str(stroke_data_dummy)
summary(stroke_data_dummy)
# stroke_data_dummy <- stroke_data_dummy[stroke_data_dummy$age >= 1, ]
###############################data variables have been prepared################
attach(stroke_data_dummy)

library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(corrplot)
# check correlation of variables
stroke_matrix <- cor (stroke_data_dummy[1:14])
round(stroke_matrix, 2)
corrplot(stroke_matrix, type = "upper", 
         number.cex=0.30)

attach(stroke_data_dummy)

# initial model
# fit <- lm(age ~ 
#              hypertension + heart_disease +     
#              overweight + ever_married +  work_type + 
#              stroke + diabetes,
#                      data = stroke_data_dummy)
# 
# summary(fit)
fit <- lm(age ~ 
            hypertension + heart_disease +     
            bmi + avg_glucose_level +  
            work_type_1 + work_type_2 + work_type_3 + work_type_4 + work_type_5 +
            stroke + 
            smoking_status_1 + smoking_status_2 + smoking_status_3 + 
            smoking_status_4 ,
          data = stroke_data_dummy)

summary(fit)

# outliers
library(qqplot)
library("car")
library(boxplot)
# qqplot continous variables
qqPlot(stroke_data_dummy$bmi,
              main = "bmi")
qqPlot(stroke_data_dummy$avg_glucose_level,
       main = "avg_glucose_level")


plot(fit, pch = 10, cex = 2, main="Influential observations ") 
abline(h = 4 * mean(fit, na.rm=T), col="red")  # add cutoff line
text(x = 1:length(fit) + 1, y = fit, 
     labels=ifelse(fit > 4 * mean(fit, na.rm = T), 
                   names(lm),""),col="red")

# list outliers' value
# stroke_data_dummy[c(4030,2020, 181), c("bmi") ]
stroke_data_dummy[c(4030, 2020, 181), c('age', 'bmi', 'avg_glucose_level')]
stroke_data_dummy[c(873, 100, 18), c('age', 'bmi', 'avg_glucose_level')]

# remove the clearly wrong collected data
# two rows are removed
stroke_data_dummy <- stroke_data_dummy[-c(4030, 2020, 181), ]
# the model improved a little
# replace the diabetes with dummied variables

# recheck the model
fit <- lm(age ~ 
            hypertension + heart_disease +     
            bmi + avg_glucose_level +  
            work_type_1 + work_type_2 + work_type_3 + work_type_4 + work_type_5 +
            stroke + 
            smoking_status_1 + smoking_status_2 + smoking_status_3 + 
            smoking_status_4 ,
          data = stroke_data_dummy)

summary(fit)

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

# 
# 
# visualization
library(car)
outlierTest(fit)
# p-value is 2.7031e-05 
# delete this outlier
stroke_data_dummy[c(518), ]
stroke_data_dummy <- stroke_data_dummy[-c(518), ]

# rebuild model
fit <- lm(age ~ 
            hypertension + heart_disease +     
            ever_married + 
            work_type_1 + work_type_2 + work_type_3 + work_type_4 + 
            smoking_status_1 +  smoking_status_3 + 
            overweight + stroke + diabetes_1 + diabetes_2 ,
          data = stroke_data_dummy)
summary(fit)
# test outliers
outlierTest(fit)
# check if the outliers has been removed

# test normality of the model
qqnorm(rstudent(fit))
qqline(rstudent(fit))
# upper end improved but the lower end is highly skewes

# test age distribution
hist(stroke_data_dummy$age)
summary(stroke_data_dummy$age)
detach(stroke_data_dummy)

# subset
# many independent in this dataset are obviously adult related features,
# such as marital status, smoking status
# under age patients only account for a small proportion
str(stroke_data_dummy[stroke_data_dummy$age >= 18, ])

# so we adjust the dataset to adult subset
stroke_data_adult <- stroke_data_dummy[stroke_data_dummy$age >= 18, ]
hist(stroke_data_adult$age)

attach(stroke_data_adult)
# rebuild the model
stroke_data_adult$log_age <- log(stroke_data_adult$age)
fit <- lm(log_age ~ 
            hypertension + heart_disease +     
            ever_married + 
            work_type_1 + work_type_2 + work_type_3 + work_type_4 + 
            smoking_status_1 +  smoking_status_3 + 
            overweight + stroke + diabetes_1 + diabetes_2 ,
          data = stroke_data_adult)
summary(fit)
# test outliers
outlierTest(fit)
# check if the outliers has been removed

# test normality of the model
qqnorm(rstudent(fit))
qqline(rstudent(fit))

