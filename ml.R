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

summary(stroke_data)

# gender
# remove gender=='Other', just one row
stroke_data <- stroke_data[stroke_data$gender != 'Other', ]
# round these variables
stroke_data$age <- round(stroke_data$age,0)
stroke_data$avg_glucose_level <- round(stroke_data$avg_glucose_level,0)
stroke_data$bmi <- round(stroke_data$age,0)

# from summary, there are rows with smoking_status is unknown
# summary these data
summary(stroke_data[stroke_data$smoking_status=='Unknown',])
# 1544 rows with smoking_status rows
# if it's useful in the model, remove these outliers.
# if it's not, remove this variable



# processing missing values
#install.packages("VIM")
library(VIM) 
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
summary(missing_values)
# remove missing values (only bmi has null values from the above summary)
stroke_data <- na.omit(stroke_data)

str(stroke_data)

stroke_data_bk <- stroke_data
# stroke_data <- stroke_data_bk

# there are many factor columns in the dataset
# convert factor columns to number by using sapply
# factor level from 1 
# convert to numeric, set them start from 0
stroke_data[sapply(stroke_data, is.factor)] <- 
  data.matrix(stroke_data[sapply(stroke_data, is.factor)]) - 1

# remove unused variables
stroke_data <- subset(stroke_data, select = -c(id, Date))
str(stroke_data)

# dummy categorical columns
library(fastDummies)
stroke_data_dummy <- dummy_cols(stroke_data, 
           select_columns = c("work_type"), 
           remove_first_dummy = TRUE)
str(stroke_data_dummy)


library(psych)
pairs.panels(stroke_data_dummy, 
             smooth = TRUE, # If TRUE, draws loess smooths  
             scale = FALSE, # If TRUE, scales the correlation text font  
             density = TRUE, # If TRUE, adds density plots and histograms  
             ellipses = TRUE, # If TRUE, draws ellipses   
             method = "spearman",# Correlation method (also "pearson" or "kendall") 
             pch = 21, # pch symbol   
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit 
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered  
             factor = 2, # Jittering factor  
             hist.col = 4, # Histograms color   
             stars = TRUE,
             ci = TRUE) # If TRUE, adds confidence intervals 
attach(stroke_data_dummy)

cor(stroke_data_dummy)
round(res, 2)