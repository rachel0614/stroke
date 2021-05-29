# examples
# http://rstudio-pubs-static.s3.amazonaws.com/74431_8cbd662559f6451f9cd411545f28107f.html
# https://zhuanlan.zhihu.com/p/31642344
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
# library(VIM) 
# missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
# summary(missing_values)
# remove missing values (only bmi has null values from the above summary)
# stroke_data <- na.omit(stroke_data)

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
# diabetes_col <- cut(stroke_data$avg_glucose_level,
# breaks = c(0, 140, 199, Inf), 
# labels = c('normal', 'prediabetes', "diabetes"), 
# right = FALSE,
# order = TRUE)
# stroke_data$diabetes <- diabetes_col

# age level
# age_col <- cut(stroke_data$age,
#                breaks = c(0, 30, 40, 50, 60, 70,Inf), 
#                labels = c('under 30', '31-40', "41-50", "51-60","61-70","71 or older"), 
#                right = FALSE,
#                order = TRUE)

age_col <- cut(stroke_data$age,
               breaks = c(0,45, 70,Inf),
               labels = c('under 45', '45~70', "71 or older"),
               right = TRUE,
               order = TRUE)
stroke_data$age_level <- age_col

# create a new variable obesity with bmi>=30 is Yes else if No
stroke_data$obesity <- 
  factor(ifelse(stroke_data$bmi>=30, "Yes", "No"))

# stroke_data[stroke_data$smoking_status == "Unknown", ]
# according to the summary, remove outliers of gender & smoking_status
# and discard id & Date variables.
stroke_data <- subset(stroke_data, 
                      select = c(
                        obesity,
                        heart_disease,
                        ever_married,
                        age_level,
                        stroke))


summary(stroke_data)
str(stroke_data)

colnames(stroke_data)[colnames(stroke_data) == "age_level"] <- "label"

# 
library(psych)
pairs.panels(stroke_data, 
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

# Example 2: Selecting Variables of pairs Plot
# https://statisticsglobe.com/r-pairs-plot-example/
attach(stroke_data)
str(stroke_data)
pairs(~ obesity + heart_disease + ever_married +  label, data = data)

pairs(data[ , 1:5],
      col = "red",                                         # Change color
      pch = 18,                                            # Change shape of points
      labels = c("heart_disease", 
                 "ever_married", 
                 "obesity", 
                 "stroke",
                 "label"),                  # Change labels of diagonal
      main = "This is a nice pairs plot in R")             # Add a main title

# group
pairs(data[ , 1:5],
      col = c("red", "cornflowerblue", "purple", "blue")[age_level],   # Change color by group
      pch = c(8, 18, 1, 18, 1)[age_level],                            # Change points by group
      labels = c("heart_disease", 
                 "ever_married", 
                 "obesity", 
                 "stroke",
                 "label"),
      main = "This is an even nicer pairs plot in R")

# predict age level by diseases
library(ggpairs)
ggpairs(stroke_data)
# 

# install.packages("ggplot2")            # Packages need to be installed only once
# install.packages("GGally")

library("ggplot2")                     # Load ggplot2 package
library("GGally")                      # Load GGally package
ggpairs(stroke_data)      
