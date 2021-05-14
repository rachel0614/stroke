# my os (windows Chinese version)
# the date format converts incorrectly without setting the locale
Sys.setlocale("LC_TIME", "English")
# predefine the factor 
no_yes_factor <- c("No","Yes")
# load and convert the below columns 
stroke_data <- within(read.csv("data/stroke.csv", na = "N/A", 
                               stringsAsFactors = FALSE),{
     gender <- factor(gender)
     hypertension <- factor(hypertension,labels = no_yes_factor)
     heart_disease <- factor(heart_disease, labels = no_yes_factor)     
     ever_married <- factor(ever_married)    
     work_type <- factor(work_type) 
     Residence_type <- factor(Residence_type) 
     smoking_status <- factor(smoking_status)
     stroke <- factor(stroke,labels = no_yes_factor)
     stroke <- factor(stroke)
     Date <- as.Date(Date,"%A %d %B %Y")
})
# have a look at the data, to make sure the data type conversion has been done correctly.
head(stroke_data, 2)
str(stroke_data)
# check the data amount - 5110 rows  13 cols
dim(stroke_data)

# processing missing values
#install.packages("VIM")
library(VIM) 
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
summary(missing_values)
# remove missing values (only bmi has null values from the above summary)
stroke_data <- na.omit(stroke_data)

# 4909 rows 13 columns
dim(stroke_data)
# see summary of the dataset
summary(stroke_data)

# according to the summary, remove outliers of gender & smoking_status
# and discard id & Date variables.
stroke_data_modified <- subset(stroke_data, 
                               select = c(-id, -Date), 
                               gender %in% c("Female", "Male") & 
                               smoking_status != "Unknown")

# create a new variable obesity with bmi>=30 is Yes else if No
stroke_data_modified$obesity <- 
  factor(ifelse(stroke_data_modified$bmi>=30, "Yes", "No"))

str(stroke_data_modified)
# drop the non-existed factor of gender 
stroke_data_modified$gender <- 
  factor(as.character(stroke_data_modified$gender))
stroke_data_modified$smoking_status <- 
  factor(as.character(stroke_data_modified$smoking_status))
str(stroke_data_modified)
# check summary to make sure ourlier has been processed properly
summary(stroke_data_modified)

# we create a new variable which represents whether the patient is or was a smoker
# according to the smoking status variable
# smoker_col <- 
#   (stroke_data_modified$smoking_status = 
#      ifelse(stroke_data_modified$smoking_status %in% c("smokes"),"Yes","No"))  
# stroke_data_modified$smoker <- factor(smoker_col)
str(stroke_data_modified)
dim(stroke_data_modified)
######################  data has been prepared##########
# Lets look at the correlation between both of these variables
# to evaluate the strength of the relationship
# and whether it is negative or positive

summary(stroke_data_modified)
str(stroke_data_modified)
# plot the distribution of patients by age
library(ggplot2)
library(ggpubr)
# mean value of age by stroke status
# ggdensity(stroke_data_modified, x = "age",
#           add = "mean", rug = TRUE,
#           title = "age distribution in stroke and non-stroke group",
#           color = "stroke", fill = "stroke",
#           palette = c("#0073C2FF", "#FC4E07"))

# plot the distribution of patients by average glucose level
# distributioaverage glucose level vs stroke
# ggdensity(stroke_data_modified, x = "avg_glucose_level",
#           add = "mean", rug = TRUE,
#           title = "average glucose level distribution \n   in stroke and non-stroke group",
#           color = "stroke", fill = "stroke",
#           palette = c("#0073C2FF", "#FC4E07"))

# proportion
prop.table(table(stroke_data_modified$stroke, 
                 stroke_data_modified$heart_disease))
prop.table(table(stroke_data_modified$stroke, 
                 stroke_data_modified$ever_married))

cor(stroke_data_modified,method="pearson")
print(corr.test(stroke_data_modified,method = "pearson"),short = FALSE)


# personal information
pairs.panels(stroke_data_modified[,
                                  c("gender", "age", "stroke")], 
             main = "personal information related variable vs. stroke", 
             smooth = TRUE,# If TRUE, draws loess smooths    
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman", # Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color  # 
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) 
attach(stroke_data_modified)
#####################################################
# gender hypothesis
# H0 - gender has no correlation with stroke
# H1 - gender has correlation with stroke

# do visualization first................
mytable <- xtabs( ~ gender + stroke, 
                  data = stroke_data_modified)
mytable

# summation of gender and stroke
table(gender, stroke)
# hypothesis test
chisq.test(table(gender, stroke))
# P-VALUE = 0.517, > 0.05, so we failed to reject H0 and accept H1 
# gender has correlation with stroke

# obesity hypothesis
# H0 - obesity has no correlation with stroke
# H1 - obesity has correlation with stroke

# visualization......

table(stroke, obesity)
chisq.test(table(stroke, obesity) ,correct=F)
# p-value = 0.4213, less than 0.05, we failed to rejected H0
# so that we accept H1 that obesity has correlation with stroke

# age hypothesis
# H0 - age has no correlation with stroke 
# H1 - age has correlation with stroke

library("lattice")
# boxplot to see data distribution in stroke and non-stroke patient group
plot(stroke, age, pch=9,col='lightblue', 
     main = "stroke status vs age",
     ylab = "age",
     xlab = "group by stroke status")

# compare age distribution for non-stroke and stroke patient group
ggdensity(stroke_data_modified, 
          x = "age",
          add = "mean", rug = TRUE,
          title = "age distribution in stroke and non-stroke group",
          color = "stroke", 
          fill = "stroke",
          palette = c("#0073C2FF", "#FC4E07"))


# mean age in non-stroke and stroke group
tapply(age, stroke, mean)

# hist of age to check normality
histogram(~age, data = stroke_data_modified,
          main = "distribution of age data",
          xlab = "age")

# hist of age by stroke status to check normality
histogram(~age | stroke, data = stroke_data_modified,
          main = "distribution of stroke & age data",
          xlab = "stroke status",
          ylab = "age")

# use Q-Q plot to check if age is normally distributed
qqnorm(age, main = "age distribution")
qqline(age, col = "red")

# get precise result of whether age is normally distributed in the below population
# Shapiro-Wilk test - H0 - the variable is normally distributed
# if p<0.05 = H0 is rejected, so that the data is not normality distributed

# normality check of three group

# 1. population of non-stroke patient
normality_test <- shapiro.test(stroke_data_modified[stroke == 'No', ]$age)
normality_test$p.value

# 2. population of stroke patient
normality_test <- shapiro.test(stroke_data_modified[stroke == 'Yes', ]$age)
normality_test$p.value

# 3. entire population
normality_test <- shapiro.test(stroke_data_modified$age)
normality_test$p.value

# p-value of stroke, non-stroke and entire population are all far smaller than 0.05
# so that the age are not normally distributed.

# hypothesis test 
# continuous independent variable - age
# categorical variable - stroke (Yes/No)
chisq.test(table(age, stroke))

# p-value of chisq test is far smaller than 0.05, so we accept H1 that
# age has correlation with stroke

# H0 - mean age of stroke and non-stroke patients are identical
# H1 - mean age of stroke and non-stroke patients are not identical

# independent continuous variable - age 
# dependent categorical variable - stroke (Yes/No)
wilcox.test(age ~  stroke)

# p-value < 2.2e-16
# At 0.05 significance level, we conclude 
# that the age of stroke and non-stroke patients are different.

# bmi & average glucose level hypothesis

qqnorm(avg_glucose_level)
qqline(avg_glucose_level, col = "red")

# group
with(stroke_data_modified, { 
  qqnorm(age[stroke == 'Yes'], main = 'stroke data')  
  qqline(age[stroke == 'Yes'], col = "red")
})

#
with(stroke_data_modified,
     qqplot(stroke_data_modified$bmi[stroke_data_modified$stroke_status == "Yes"],
            stroke_data_modified$bmi[stroke_data_modified$stroke_status == "No"],
            main = "compare 2 samples of patients data",
            xlab = "stroke = Yes",
            ylab = "stroke = No"))

# most of the stroke patients data is normally distributed
with(stroke_data_modified, { 
  qqnorm(age[stroke_status == 'Yes'], main = 'Stroke patients')  
  qqline(age[stroke_status == 'Yes']) 
})

with(stroke_data_modified, { 
  qqnorm(age[stroke_status == 'No'], main = 'Stroke patients')  
  qqline(age[stroke_status == 'No']) 
})

normality_test <- shapiro.test(stroke_data_modified$age)
normality_test$p.value
# p-value = 1.256842e-31
# run man-whinney test
wilcox.test(age ~ stroke)
#  p-value < 2.2e-16 so this indicates the Null hypothesis (H0) is rejected

# not being tested yet???????????????????
# Whether there is significant difference of patient in mean age
# H0 - there is no significant age difference between patients of stroke and non-stroke groups
# H1 - there is significant age difference between patients of stroke and non-stroke groups
stroke_data <- stroke_data_modified[stroke_data_modified$stroke
                                    ]
library(FSA)
Summarize(age ~ stroke,
          data = stroke_data_modified)
histogram(~ age | stroke,
          data = stroke_data_modified,
          layout = c(1,2), 
          xlab = "age",
          main = "distribution of age in stroke and non-stroke group")
boxplot(age ~ stroke,
        data = stroke_data_modified,
        ylab="age",
        xlab="stroke")
# from the boxplot and histogram, there is a significant difference in the distribution of age
# among stroke patients and non-stroke patients

# p-value is significant small, 
# in this case, the distribution of age in both stroke and non-stroke groups  
# has significant difference

# not being tested yet ??????????????
library(FSA)
boxplot(avg_glucose_level,
        data = stroke_data_modified,
        ylab="avg_glucose_level",
        xlab="stroke")
# from the boxplot and histogram, there is a significant difference in the distribution of age
# among stroke patients and non-stroke patients

kruskal.test(avg_glucose_level ~ stroke, data = stroke_data_modified)
kruskal.test(avg_glucose_level ~ stroke, data = tmp_data)

with(stroke_data_modified,
     qqplot(stroke_data_modified$avg_glucose_level[stroke_data_modified$stroke == "Yes"],
            stroke_data_modified$avg_glucose_level[stroke_data_modified$stroke == "No"],
            main = "compare 2 samples of patients data",
            xlab = "stroke = Yes",
            ylab = "stroke = No"))
# from the boxplot and histogram, there is a significant difference in the distribution of age
# among stroke patients and non-stroke patients
with(stroke_data_modified, { 
  qqnorm(avg_glucose_level[stroke == 'No' ], main = 'Stroke patients')  
  qqline(avg_glucose_level[stroke == 'No' ]) 
})

with(stroke_data_modified, tapply(avg_glucose_level, stroke, shapiro.test))
kruskal.test(bmi ~ stroke, data = stroke_data_modified)

normality_test <- shapiro.test(stroke_data_modified$avg_glucose_level)
normality_test$p.value
normality_test <- shapiro.test(stroke_data_modified$bmi)
normality_test$p.value
cor.test(stroke_data_modified$avg_glucose_level
         , stroke_data_modified$bmi, 
         method = "spearman")
