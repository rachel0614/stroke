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
     Date <- Date(Date,"%A %d %B %Y")
})

# have a look at the data
head(stroke_data, 2)
str(stroke_data)
# 5110 rows  13 cols
dim(stroke_data)
#install.packages("VIM")
library(VIM) 
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
# bmi - has null value
stroke_data <- na.omit(stroke_data)
# 4909 rows 13 columns
dim(stroke_data)
# see summary of the dataset
summary(stroke_data)
# from the summary, we can see 
# gender variable has outlier 
# smoking status has outlier of unknown smking status
levels(smoking_status)
stroke_data_modified <- subset(stroke_data, 
                               gender %in% c("Female", "Male") & 
                               smoking_status %in% c("formerly smoked", 
                                                     "never smoked",
                                                     "smokes"))
# the nonexisted factor of gender has to be dropped
stroke_data_modified$gender <- 
  factor(as.character(stroke_data_modified$gender))
stroke_data_modified$smoking_status <- 
  factor(as.character(stroke_data_modified$smoking_status))
str(stroke_data_modified)
# we create a new variable which represents whether the patient is or was a smoker
# according to the smoking status variable
levels(stroke_data_modified$smoking_status)
smoker_col <- 
  (stroke_data_modified$smoking_status = 
     ifelse(stroke_data_modified$smoking_status %in% c("smokes"),"Yes","No"))  
stroke_data_modified$smoker <- factor(smoker_col)
str(stroke_data_modified)
dim(stroke_data_modified)
######################  data has been prepared##########
attach(stroke_data_modified)
# age, bmi, avg_glucose_level is continuous, do it later
# gender, smoking status, marital status are categorical, do chisq test
##################### hypothesis test1  ################
# H0 - gender has no correlation with stroke
# H1 - gender has correlation with stroke
# summation of gender and stroke
table(gender, stroke)
# hypothesis test
chisq.test(table(gender, stroke))
# P-VALUE = 0.517, > 0.05, so we failed to reject H0
# no correlation between 
#######################################
# H0 - smoking has no correlation with stroke
# H1 - smoking has correlation with stroke
table(smoking_status, stroke)
chisq.test(table(smoking_status, stroke))
# p-value = 1, greater than 0.05, we failed to rejected H0
#######################################
table_data <- table(heart_disease, stroke)
chisq.test(table_data)
# work_type p-value = 0.0128
# ever_married p-value = 4.115e-05  < 0.05, reject H0
# heart_disease p-value = 1.915e-15
# 婚姻可有效延缓心脏病/中风的进展
# heart_disease is risk factor of stroke

##################### hypothesis test2  ################
# whether smoking there has correlation with stroke
# see levels of smoking status factor
# H0 - in general, no correlation between smoke and stroke 
# H1 - in general, there is correlation between smoke and stroke 
# get subset which includes only stroke patients
stroke_smoker_patient_data <- subset(stroke_data_modified, 
                              stroke == "Yes",  
                               select = c(id, smoker))

summary(stroke_smoker_patient_data)
plot(stroke_smoker_patient_data$smoker)

freq_table = xtabs( ~ smoker, data = stroke_smoker_patient_data)
chisq.test(freq_table)
# p-value = 0.3711 which reject H0
# so that there is correlation between smoke and stroke
#############
head(stroke_data_modified,3)
mytable <- xtabs( ~ smoker + Stroke, 
                 data = stroke_data_modified)
mytable
head(stroke_data_modified)
smoker_data <- xtabs( ~ smoker+stroke, data = stroke_patient_data)
chisq.test(smoker_data)
#chisq.test(table(ever_married, stroke))
##########################################
# H0 - age does not affect stroke status
# H1 - age affects stroke_status
plot(stroke, age, pch=9,col='lightblue', 
     main = "stroke status vs age")
# split the dichotomous variables into two group
library("lattice")
histogram(~age | stroke, data = stroke_data_modified,
          main = "distribution of stroke & age data",
          xlab = "age (degrees)",
          ylab = "stroke")
# it seem the patients not having got stroke is normally distributed
# the patients having got stroke are not normally distributed
# use Q-Q plot to check if age is normally distributed
qqnorm(age)
qqline(age, col = "red")
# check stroke_status
qqnorm(stroke)
qqline(stroke, col = "red")
# group
with(stroke_data_modified, { 
  qqnorm(age[stroke == 'Yes'], main = 'stroke data')  
  qqline(age[stroke == 'Yes'], col = "red")
})
# 这种图表适用于continuous 和 categorical 是否正态分布判断
with(male_stroke_data, { 
  qqnorm(age, main = 'bmi distribution of male stroke patients')  
  qqline(bmi, col = "red")
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
# 首尾不显著，中间正态分布
with(stroke_data_modified, { 
  qqnorm(age[stroke_status == 'No'], main = 'Stroke patients')  
  qqline(age[stroke_status == 'No']) 
})
# test 这个不能直接用于categorical variable
normality_test <- shapiro.test(stroke_data_modified$age)
normality_test$p.value
# p-value = 1.256842e-31
# run man-whinney test
wilcox.test(age ~ stroke)
#  p-value < 2.2e-16 so this indicates the Null hypothesis (H0) is rejected
# this indicates 
# 女性未中风患者正态分布
test_data <- stroke_data_modified[stroke_data_modified$stroke_status=="No", ]
histogram(~test_data$age | test_data$gender,
          data = test_data,
          main = "distribution of stroke & age data",
          xlab = "age (degrees)",
          ylab = "stroke")

# age 
boxplot(stroke_data_modified$age, col="skyblue2")
# below shows stroke patients has some outliers
boxplot(stroke_data_modified$age ~ stroke_data_modified$stroke_status,
        col="skyblue2", pch=20)
# use boxplot.stats to see the outlier of the stroke patients
boxplot.stats(
  stroke_data_modified$age[stroke_data_modified$stroke_status=="Yes"])$out
# check bmi distribution
boxplot(stroke_data_modified$bmi ~ stroke_data_modified$stroke_status,
        col="skyblue2", pch=20)
#check the mean outlier of bmi
bmi_outlier <- boxplot.stats(
  stroke_data_modified$bmi[stroke_data_modified$stroke_status=="Yes"])$out
mean(bmi_outlier)
#

# 首尾不显著，中间正态分布
attach(stroke_data_modified)
age_stroke_data <- stroke_data_modified[
  stroke_status == 'Yes', ]
detach(stroke_data_modified)

attach(age_stroke_data)
with(age_stroke_data, { 
  qqnorm(age, main = 'Stroke patients')  
  qqline(age) 
})
detach(age_stroke_data)

normality_test <- shapiro.test(age_stroke_data$age)
normality_test$p.value
###########################test 3 age & stroke###########
# Whether there is significant difference of patient in mean age
# H0 - 
# H1 - 
stroke_data <- stroke_data_modified[stroke_data_modified$stroke
                                    ]
library(FSA)
Summarize(age ~ stroke,
          data = stroke_data_modified)
histogram(~ age | stroke,
          data = stroke_data_modified,
          layout = c(1,2))
boxplot(age ~ stroke,
        data = stroke_data_modified,
        ylab="age",
        xlab="stroke")
# from the boxplot and histogram, there is a significant difference in the distribution of age
# among stroke patients and non-stroke patients

kruskal.test(age ~ stroke, data = stroke_data_modified)
# p-value is significant small, 
#  Only in cases where the distributions in each group are similar 
# can a significant Kruskal–Wallis test be interpreted 
# as a difference in medians.
# in this case, the distribution of age in both stroke and non-stroke groups are 
# has significant difference, so
