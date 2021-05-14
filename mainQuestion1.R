# make sure the time format works in my os (Chinese version)
Sys.setlocale("LC_TIME", "English")

# load data
stroke_data <- read.csv("data/stroke.csv", na = "N/A", 
                        stringsAsFactors = FALSE)
# have a look at the data
head(stroke_data, 2)
names(stroke_data)
str(stroke_data)
summary(stroke_data)

summary(stroke_data[stroke_data$stroke ==1, ])


attach(stroke_data)
table(age, stroke)
detach(stroke_data)
# keep analytical columns
attach(stroke_data)
stroke_data <- subset(stroke_data, select = c(age, stroke)) 
detach(stroke_data)
str(stroke_data)

# convert variable stroke from int to factor
stroke_col <- factor(stroke_data$stroke, labels = c("No", "Yes"))
stroke_data$stroke_status <- stroke_col
stroke_data <- subset(stroke_data, select = c(age, stroke_status)) 

# incomplete values
#install.packages("VIM")
library(VIM)
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
summary(stroke_data)
##########data has been prepared############################
# age - continuous variable
# stroke - categorical dichotomous
# H0 -  mean age is not affected by temperature
# check if the variables are normally distributed

str(stroke_data)
# the proportion of each category
p1 <- hist(stroke_data$age)
plot( p1, col = rgb(0,0,1,1/4), 
      xlim = c(0,100), 
      ylim = c(0,500), 
      xlab = "Age",
      ylab = "stroke amount",
      main="histograms")

stroke_data[stroke_data$age < 10, ]
