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
###############################data variables have been prepared################
attach(stroke_data_dummy)
# colnames(stroke_data_dummy)
# stepwise regression

# check correlation of variables
stroke_matrix <- cor (stroke_data_dummy[1:12])
# corrplot(stroke_matrix, method = "number") 
corrplot(stroke_matrix, type = "upper")
# initial model
fit <- lm(age ~ 
             hypertension + heart_disease +     
             ever_married +  
             bmi + stroke + diabetes,
           data = stroke_data_dummy)

summary(fit)
confint(fit)
attach(stroke_data_dummy)
# outliers
library("car")
# qqplot continous variables
qqPlot(stroke_data_dummy$bmi,
              main = "bmi")

plot(fit, pch = 10, cex = 2, main="Influential observations ") 
abline(h = 4 * mean(fit, na.rm=T), col="red")  # add cutoff line
text(x = 1:length(fit) + 1, y = fit, 
     labels=ifelse(fit > 4 * mean(fit, na.rm = T), 
                   names(lm),""),col="red")
# list outliers' value
stroke_data_dummy[c(4030,2020, 181), c("bmi") ]
# remove the clearly wrong collected data
# two rows are removed
stroke_data_dummy <- stroke_data_dummy[-c(2020, 4030), ]

# the model improved a little
# replace the diabetes with dummied variables
fit <- lm(age ~ 
            hypertension + heart_disease +     
            ever_married +  
            bmi + stroke + diabetes_1 + diabetes_2 + diabetes_3,
          data = stroke_data_dummy)
summary(fit)
# improved 1%
# remove diabetes_3, bmi 
fit <- lm(age ~ 
            hypertension + heart_disease +     
            ever_married + 
            work_type_1 + work_type_2 + work_type_3 + work_type_4 + 
            smoking_status_1 +  smoking_status_3 + 
            overweight + stroke + diabetes_1 + diabetes_2 ,
          data = stroke_data_dummy)
summary(fit)
# remove variables according to the correlation matrix
# remove - gender, residence type,
# add - work_type

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
qqPlot(fit, labels=row.names(stroke_data_dummy), 
       id.method="identify", 
       simulate=TRUE, 
       main="Q-Q Plot")



# training & testing dataset
set.seed(1)
no_rows_data <- nrow(stroke_data_dummy)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) 

training_data <- stroke_data_dummy[sample, ]
testing_data <- stroke_data_dummy[-sample, ]




library(psych)
# pairs.panels(stroke_data_dummy, 
#              smooth = TRUE, # If TRUE, draws loess smooths  
#              scale = FALSE, # If TRUE, scales the correlation text font  
#              density = TRUE, # If TRUE, adds density plots and histograms  
#              ellipses = TRUE, # If TRUE, draws ellipses   
#              method = "spearman",# Correlation method (also "pearson" or "kendall") 
#              pch = 21, # pch symbol   
#              lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit 
#              cor = TRUE, # If TRUE, reports correlations
#              jiggle = FALSE, # If TRUE, data points are jittered  
#              factor = 2, # Jittering factor  
#              hist.col = 4, # Histograms color   
#              stars = TRUE,
#              ci = TRUE) # If TRUE, adds confidence intervals 

# correlation table
res <- cor(stroke_data_dummy)
round(res, 2)
# correlation table2
install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(stroke_data_dummy))
res2
rcorr(stroke_data_dummy, type = c("pearson","spearman"))
res2$r
res2$P
# visualization the correlation
install.packages("corrplot")
library(corrplot)
corrplot(res2, 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45)

# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")
# 
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
# big & slow
# chart.Correlation(stroke_data_dummy, histogram=TRUE, pch=19)

boxplot(stroke_data_dummy[, c("age",  "bmi", "avg_glucose_level")])
# generate glucose value to diabetes status
# str(stroke_data_dummy)
# model
str(stroke_data_dummy)
model <- lm(age_level ~ gender + stroke + hypertension + 
              heart_disease + ever_married + 
              Residence_type + diabetes_0 + diabetes_1 + diabetes_2 + 
              bmi + work_type_1 + work_type_2 + 
              work_type_3 + work_type_4 + 
              smoking_status_1 + smoking_status_2 + smoking_status_3, 
            data = stroke_data_dummy)
summary(model)
# select variables
model <- lm(age_level ~ stroke + ever_married + 
              Residence_type + 
              bmi_level_0 + bmi_level_1 + bmi_level_2 + bmi_level_3, 
            data = stroke_data_dummy)
summary(model)
# train & test
# create training & testing data
set.seed(1)
no_rows_data <- nrow(stroke_data_dummy)
# use 70% and don't use it again
sample_data <- sample(1 : no_rows_data, size = round(0.7 * no_rows_data),
                      replace = FALSE)
training_data <- stroke_data_dummy[sample_data, ]
testing_data <- stroke_data_dummy[-sample_data, ]

fit <- lm(age_level ~ stroke + 
            bmi_level_0 + bmi_level_1 + bmi_level_2 + bmi_level_3, data=training_data)
summary(fit)
confint(fit)
# 
library(car)
qqPlot(fit, 
       labels=row.names(states), 
       id.method="identify", 
       simulate=TRUE, main="Q-Q Plot")
student_fit <- rstudent(fit)
hist(student_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

rug(jitter(student_fit), col="brown")
curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE, col="blue", lwd=2)
lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

outlierTest(fit)


# predict
prob <- predict(model, testing_data, type = "response")
# pred <- factor(prob > 0.5, levels = c(FALSE, TRUE))
lr.perf <- table(testing_data$age_level, prob, dnn = c("Actual", "Predicted"))
lr.perf
