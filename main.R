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

dim(stroke_data)
# see summary of the dataset
summary(stroke_data)

# according to the summary 
# avg_glucose_level is between 55.12 and 271.74 in this dataset
# we cut it into three level follow the rule :
# normal = avg_glucose_level <= 140
# prediabetes = avg_glucose_level <= 199
# diabetes =  avg_glucose_level >199
# create a new variable diabetes to indicate the diabetes status
# categorical ordinal
diabetes_col <- cut(stroke_data$avg_glucose_level,
                    breaks = c(0, 140, 199, Inf), 
                    labels = c('normal', 'prediabetes', "diabetes"), 
                    right = FALSE,
                    order = TRUE)
stroke_data$diabetes <- diabetes_col

# age level
age_col <- cut(stroke_data$age,
                    breaks = c(0, 30, 40, 50, 60, 70,Inf), 
                    labels = c('under 30', '31-40', "41-50", "51-60","61-70","71 or older"), 
                    right = FALSE,
                    order = TRUE)
stroke_data$age_level <- age_col


# according to the summary, remove outliers of gender & smoking_status
# and discard id & Date variables.
stroke_data_modified <- subset(stroke_data, 
                               select = c(age, 
                                          bmi, 
                                          smoking_status, 
                                          avg_glucose_level,
                                          diabetes,
                                          heart_disease,
                                          ever_married,
                                          age_level,
                                          stroke), 
                               smoking_status != "Unknown")

# create a new variable obesity with bmi>=30 is Yes else if No
stroke_data_modified$obesity <- 
  factor(ifelse(stroke_data_modified$bmi>=30, "Yes", "No"))

# create variable to indicate the patients smokes or not
smoke_col <- factor(ifelse(stroke_data_modified$smoking_status == "smokes", 
                           "Yes", "No"))
stroke_data_modified$smoke <- smoke_col


# drop the non-existed level of smoking_status factor 
stroke_data_modified$smoking_status <- 
  factor(as.character(stroke_data_modified$smoking_status))

# show levels of smoking status variable
levels(stroke_data_modified$smoking_status)

# generate oridinal variable based on smoking status
# 
smoking_col <- stroke_data_modified$smoking_status
smoking_level_col <- 
  factor(smoking_col, order = TRUE, levels =c('never smoked', 
                                              'formerly smoked', 
                                              'smokes'))

stroke_data_modified$smoking_status <- smoking_level_col
# check summary to make sure data has been processed properly
summary(stroke_data_modified)

str(stroke_data_modified)
dim(stroke_data_modified)
data_backup <- stroke_data_modified
# View(stroke_data_modified)
######################  data has been prepared##########

# overview
library(psych)
pairs.panels(stroke_data_modified, 
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
             hist.col = 4, # Histograms color  # stars = TRUE, # If TRUE, adds significance level with stars    
             stars = TRUE, 
             ci = TRUE) 

########################################################
# install.packages("ggplot2")
# install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
library(scales)
library(dplyr)
########################################################   
attach(stroke_data_modified)

# age hypothesis

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

# install.packages("histogram")
library(histogram)
# hist of age to check normality
histogram(~age, data = stroke_data_modified,
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

# p-value of stroke, non-stroke and entire population 
# are all far smaller than 0.05
# so that the age are not normally distributed.

# 3. entire population
normality_test <- shapiro.test(stroke_data_modified$age)
normality_test$p.value

# mean by group
tapply(age , stroke , summary)
# stroke group = 68.06, non-stroke group 47.57


# hypothesis test 
# categorical ordinal independent variable - age_level
# categorical dichotomous variable - stroke (Yes/No)
# H0 - age has no correlation with stroke
# H1 = age has a correlation with stroke
# check test data
# chisq test

# summary with row column total values
age_table <- table(stroke, age_level)
addmargins(age_table)

barplot(prop.table(age_table,2)*100, 
        ylab='Percentages',
        main="percentage stroke by age groups", 
        beside=F, 
        col=c("#0073C2FF", "#FC4E07"),
        legend=rownames(age_table), 
        args.legend = list(x = "bottomleft"))

chisq.test(age_table)
# p-value < 2.2e-16, which is far smaller than 0.05
# so we accept H1, that age has correlation with stroke

# H0 - patient age in stroke and non-stroke patients group are equal
# H1 - patient age in stroke and non-stroke patients group are significant different

# independent continuous variable - age 
# dependent categorical variable - stroke (Yes/No)
# Mann-Withney-Wilcoxon test (use same function with wilcox)
# view data
table(age, stroke)
# test 
wilcox.test(age ~  stroke)
# p-value < 2.2e-16, accept H1, age are significantly different

# check variance of both groups 
var.test(age ~ stroke, 
         data = stroke_data_modified )
# p-value = 5.485e-13, variance is significantly different
# so that we don't do one-tailed test to prove stroke patients is older

# smoking hypothesis 

# H0 - smoking status has correlation with stroke 
# H1 - smoking status has correlation with stroke
chisq.test(table(smoking_status, stroke))
# p-value = 0.04996
# stroke group data 
smoking_stroke_data <- stroke_data_modified[stroke == "Yes", ]
# plot age of stroke patient by smoking status
qplot(x = smoking_status, 
      y = age,
      geom = "boxplot", 
      data = smoking_stroke_data,
      xlab = "smoking status", 
      ylab = "age",
      fill = I("lightblue"))

# check distribution shape
histogram(~age | smoke , data = smoking_stroke_data,
          main = "distribution of smoking status & age",
          xlab = "smoking status",
          ylab = "age")

# check variance between both groups
var.test(age ~ smoke, data = smoking_stroke_data)
# the variance = 0.8310142 


# mean age in stroke patients by smoking status
# smoking_status - never smoke / formerly somoke / smokes
# tapply(smoking_stroke_data$age , smoking_stroke_data$smoking_status , summary)
# smoke - Yesy / No
tapply(smoking_stroke_data$age , smoking_stroke_data$smoke , summary)

# H0 - age is equal in smoking status group of the stroke patients
# H1 - age is not equal in smoking status group of the stroke patients 
# age - continuous 
# smoking - categorical dichotomous
wilcox.test(smoking_stroke_data$age ~  smoking_stroke_data$smoke)

# p-value = 0.002014, age in both groups are significantly different

# check variance 
var.test(smoking_stroke_data$age ~ smoking_stroke_data$smoke, 
         data = smoking_stroke_data )
# p-value = 0.4383, ratio of variances is equal
# so that we do one-tailed test to prove that
# non-smoker is older
# H0 - age of non-smoker is equal to smoker
# H1 - age is greater in smoking group of the stroke patients

wilcox.test(smoking_stroke_data$age ~  smoking_stroke_data$smoke,
            alternative = "greater")
# p-value = 0.001007, accept H1, non-smoker is older than smokers in stroke patients

# average glucose level hypothesis
# 
# plot the distribution of patients by average glucose level
# distribution of average glucose level vs stroke
ggdensity(stroke_data_modified, x = "avg_glucose_level",
          add = "mean", rug = TRUE,
          title = "average glucose level distribution \n   in stroke and non-stroke group",
          color = "stroke", fill = "stroke",
          palette = c("#0073C2FF", "#FC4E07"),
          xlab = "aver glucose level")

# it seems the glucose level under 120 distributed fewer stroke patients 
# than that of  above 120

# use Q-Q plot to check if average glucose level is normally distributed
qqnorm(avg_glucose_level, main = "average glucose level distribution")
qqline(avg_glucose_level, col = "red")

# install.packages(histogram)

# hist of average glucose level by stroke status to check normality
histogram(~avg_glucose_level | stroke, data = stroke_data_modified,
          main = "distribution of stroke & average glucose level data",
          xlab = "stroke status",
          ylab = "average glucose level")

qplot(sample = avg_glucose_level,
      data = stroke_data_modified,
      color = stroke) + theme(legend.position="right")
# both stroke and non-stroke not distributed in a straight line 
# it's unlike normally distributed

# get precise result of whether avg_glucose_level is normally distributed
# Shapiro-Wilk test - H0 - the variable is normally distributed
# if p<0.05 = H0 is rejected, so that the data is not normality distributed

# normality check of avg_glucose_level
normality_test <- shapiro.test(
  stroke_data_modified$avg_glucose_level)
normality_test$p.value

# p-value = 2.212745e-53
# avg_glucose_level is not normally distributed 

# bmi 
with(stroke_data_modified, { 
  qqnorm(bmi, main = 'bmi')  
  qqline(bmi) 
})

normality_test <- shapiro.test(bmi)
normality_test$p.value
# p-value = 2.855762e-35 not normally distributed 

# avg_glucose_level hypothesis
ggqqplot(stroke_data_modified$avg_glucose_level, 
         ylab = "avg_glucose_level")

shapiro.test(stroke_data_modified$avg_glucose_level)

# correlation between avg_glucose_level & bmi
# H0 - there is no correlation between avg_glucose_level & bmi
# H1 - ther is correlation between avg_glucose_level & bmi
data <- stroke_data_modified[stroke=="Yes", ]
cor.test(data$avg_glucose_level,
         data$bmi,
         method = "spearman")
# stroke group p-value = 6.81e-05 rho = 0.2923862
data <- stroke_data_modified[stroke=="No", ]
cor.test(data$avg_glucose_level,
         data$bmi,
         method = "spearman")
# non-stroke group p-value = 2.669e-08, rho=0.09
detach(stroke_data_modified)
str(stroke_data_modified)
