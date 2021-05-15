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


# according to the summary, remove outliers of gender & smoking_status
# and discard id & Date variables.
stroke_data_modified <- subset(stroke_data, 
                               select = c(age, bmi, 
                                          smoking_status, 
                                          avg_glucose_level, stroke), 
                               smoking_status != "Unknown")

# create a new variable obesity with bmi>=30 is Yes else if No
stroke_data_modified$obesity <- 
  factor(ifelse(stroke_data_modified$bmi>=30, "Yes", "No"))

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
# H0 - there is no relationship with smoking and stroke
# H1 - there is relationship between smoking and stroke
chisq.test(table(stroke_data_modified$smoking_level, 
                 stroke_data_modified$stroke))
str(stroke_data_modified)
# p-value = 0.04906, accept H1
########################################################   
attach(stroke_data_modified)

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

# install.packages(histogram)
library(histogram)
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
# continuous independent variable - age
# categorical variable - stroke (Yes/No)
chisq.test(table(age, stroke))

# p-value < 2.2e-16, which is far smaller than 0.05
# so we accept H1 that age has correlation with stroke

# H0 - patient age in stroke and non-stroke patients group are similar
# H1 - patient age in stroke and non-stroke patients group are significant different

# independent continuous variable - age 
# dependent categorical variable - stroke (Yes/No)
wilcox.test(age ~  stroke)
# p-value < 2.2e-16, at 0.05 significance level, we conclude that 
# age of stroke and non-stroke patients are significantly different.


# from the above boxplot showing patient age by stroke status,
# It seems the mean age of stroke patient is higher than that of the stroke group 
# this can be tested by adding alternative = "less" 
# we keep doing on-tailed hypothesis test as below

# H0 - age in stroke group is higher than non-stroke group
# H1 - age in stroke group is not higher than non-stroke group
wilcox.test(age ~  stroke,
            alternative = "less")

# p-value < 2.2e-16, at 0.05 significance level, we conclude that 
# age of stroke patient is significantly higher than non-stroke group.

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

# glucose level under 120 distributed fewer stroke patients 
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


# group
with(stroke_data_modified, { 
  qqnorm(avg_glucose_level[stroke == 'Yes'], main = 'stroke data')  
  qqline(avg_glucose_level[stroke == 'Yes'], col = "red")
  })

qplot(sample = avg_glucose_level,
      data = stroke_data_modified,
      color = stroke) + theme(legend.position="bottom")
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

# hypothesis test 
# continuous independent variable - avg_glucose_level
# categorical variable - stroke (Yes/No)
chisq.test(table(avg_glucose_level, stroke))

# BMI hypothesis
#
with(stroke_data_modified,
     qqplot(bmi[stroke == "Yes"],
            bmi[stroke == "No"],
            main = "compare 2 samples of patients data",
            xlab = "stroke = Yes",
            ylab = "stroke = No"))
# the line is quite straight except one outlier

# 
with(stroke_data_modified, { 
  qqnorm(bmi[stroke == 'Yes'], main = 'Stroke patients')  
  qqline(bmi[stroke == 'Yes']) 
})

with(stroke_data_modified, { 
  qqnorm(age[stroke == 'No'], main = 'Stroke patients')  
  qqline(age[stroke == 'No']) 
})

normality_test <- shapiro.test(stroke_data_modified$bmi)
normality_test$p.value
# p-value = 2.920174e-35 
# bmi is normally distributed

# H0 - bmi has no correlation with stroke
# H1 - bmi has correlation with stroke
chisq.test(table(bmi, stroke))

normality_test <- shapiro.test(bmi)
normality_test$p.value

cor.test(avg_glucose_level
         , bmi, 
         method = "spearman")

data <- stroke_data_modified[stroke == "Yes", ]
detach(stroke_data_modified)

attach(data)
# smoking status
# plot age of stroke patient by smoking status
qplot(x = smoking_status, 
      y = age,
      geom = "boxplot", 
      data = data,
      xlab = "smoking status", 
      ylab = "age",
      fill = I("lightblue"))


# mean age in stroke patients by smoking status
tapply(data$age , data$smoking_status , summary)

ggdensity(data, x = "age",
          add = "mean", 
          rug = TRUE,
          title = "age distribution \n   by smoking status in stroke patients",
          color = "smoking_status", fill = "smoking_status",
          palette = c("#0073C2FF", "#FC4E07", "#FF0000"),
          xlab = "age")
# it looks more older non-smokers, smokers tends to be younger stroke patients 

# independent continuous age
# independent categorical ordinal smoking level
# H0 - smoking has no correlation with mean age of stroke patient
# H1 - smoking has correlation with mean age of stroke patient
cor.test(data$age, 
         as.numeric(smoking_status), 
         exact = FALSE, 
         method = "spearman")

# p-value = 0.0009329 reject H0 which means that 
# smoking has correlation with mean age of stroke patient
# rho = -0.244659, it shows that 
# age of stroke has negative correlation with smoking level

