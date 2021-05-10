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
# from the summary, we can see gender variable has outlier 
# stroke_data_modified <- subset(stroke_data, select = -c(id))
stroke_data_modified <- subset(stroke_data, 
                               gender %in% c("Female", "Male"),  
                               select = -c(id))
levels(droplevels(stroke_data_modified$gender))
#stroke_data_modified[stroke_data_modified$smoking_status=='Unknown', ]
# 4908 rows 12 columns
dim(stroke_data_modified)
str(stroke_data_modified)
# drop unuseful level
stroke_data_modified$gender <- factor(as.character(stroke_data_modified$gender))
######################  data has been prepared##########
attach(stroke_data_modified)
# age, bmi, avg_glucose_level is continuous, do it later
# gender, smoking status, marital status are categorical, do chisq test
##################### hypothesis test  #################
# H0 - gender has no correlation with stroke
# H1 - gender has correlation with stroke
# summation of gender
table(gender)
# summation of stroke
table(gender)

chisq.test(table(gender, stroke))

chisq.test(table(smoking_status, stroke))
chisq.test(table(ever_married, stroke))


# H0 - gender does not affect stroke status
# H1 - gender affects stroke_status
plot(stroke_status, age, pch=9,col='lightblue', 
     main = "stroke status vs age")
# split the dichotomous variables into two group
library("lattice")
histogram(~heart_disease_status | stroke_status, data = stroke_data_modified,
          main = "distribution of stroke & age data",
          xlab = "age (degrees)",
          ylab = "stroke")
# it seem the patients not having got stroke is normally distributed
# the patients having got stroke are not normally distributed
# use Q-Q plot to check if age is normally distributed
qqnorm(age)
qqline(age, col = "red")
# check stroke_status
qqnorm(stroke_status)
qqline(stroke_status, col = "red")
# group
with(stroke_data_modified, { 
  qqnorm(age[stroke_status == 'Yes'], main = 'stroke data')  
  qqline(age[stroke_status == 'Yes'], col = "red")
})
names(stroke_data_modified)
# bmi of stroke patients seems to be normally distributed
# 应该进一步对bmi进行归类后再看
with(stroke_data_modified[age<70, ], { 
  qqnorm(bmi[stroke_status == 'Yes'], main = 'bmi stroke data')  
  qqline(bmi[stroke_status == 'Yes'], col = "red")
})
# 限定年龄组后再看bmi
# 男性人群中风患者基本呈正态分布
male_stroke_data <- stroke_data_modified[
  stroke_data_modified$gender == "Male" & 
  stroke_data_modified$stroke_status=="Yes", ]
dim(male_stroke_data)
# 这种图表适用于continuous 和 categorical 是否正态分布判断
with(male_stroke_data, { 
  qqnorm(bmi, main = 'bmi distribution of male stroke patients')  
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
# 用下面这种方式
with(stroke_data_modified, 
     tapply(bmi, stroke_status, shapiro.test))
names(stroke_data_modified)
# p-value = 1.256842e-31
# run man-whinney test
wilcox.test(age ~ stroke_status)
#  p-value < 2.2e-16 so this indicates the Null hypothesis (H0) is rejected
# this indicates 
detach(stroke_data_modified)
names(stroke_data_modified)
# 查看相关性， 根据生活方式和疾病史进行分类对照
# 相关性都较弱
stroke_data_sub1 <- stroke_data_modified[c("hypertension_status",
                                          "avg_glucose_level",
                                          "bmi",
                                          "heart_disease_status",
                                          "stroke_status")]
stroke_data_sub2 <- stroke_data_modified[c("work_type",
                                          "age",
                                          "gender",
                                          "ever_married",
                                          "Residence_type",
                                          "smoking_status",
                                          "stroke_status")]
pairs.panels(stroke_data_sub1,
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
pairs.panels(stroke_data_sub2,
             main = "life stype vs stroke",
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

# 正态分布
# 女性未中风患者正态分布
test_data <- stroke_data_modified[stroke_data_modified$stroke_status=="No", ]
histogram(~test_data$age | test_data$gender,
          data = test_data,
          main = "distribution of stroke & age data",
          xlab = "age (degrees)",
          ylab = "stroke")
# gender stroke status
table(stroke_data_modified$gender, stroke_data_modified$stroke_status)

with(stroke_data_modified, { 
  qqnorm(gender[stroke_status == 'No'], main = 'Stroke patients')  
  qqline(gender[stroke_status == 'No']) 
})

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

gender_stroke_data <- 
  subset(stroke_data_modified, select = c("gender", "stroke_status"))
chisq.test(table(gender_stroke_data))
                      