# Question5 - The distribution of stoke frequency in each month is equal
# make sure the time format works in my os (Chinese version)
Sys.setlocale("LC_TIME", "English")

# load data
stroke_data <- read.csv("data/stroke.csv", na = "N/A", 
                        stringsAsFactors = FALSE)
# have a look at the data
head(stroke_data, 2)
names(stroke_data)
str(stroke_data)
# 5110 rows  13 cols
dim(stroke_data)
# transform 
# convert the below columns type from char from to factor
categorical_col <- c("gender","hypertension","heart_disease",
                     "ever_married", "work_type", "Residence_type",
                     "smoking_status" )
stroke_data[categorical_col] <- lapply(stroke_data[categorical_col] , factor)
# convert date column (see strftime)
convert_date <- as.Date(stroke_data$Date,"%A %d %B %Y")
stroke_data$Date <- convert_date
str(stroke_data)
# extract month from Date variable and convert it to factor
# categorical ordinal variable transformation
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
month_col = factor(format(stroke_data$Date,"%b"),
            ordered = TRUE, 
            levels = month_levels)
stroke_data$month = month_col 

# convert variable stroke from int to factor
stroke_col <- factor(stroke_data$stroke, labels = c("No", "Yes"))
stroke_data$stroke_status <- stroke_col
# keep variables
stroke_data <- subset(stroke_data, select = c(month, stroke_status)) 
str(stroke_data)
# H0 - the distribution of stroke frequency in each month is equal
# H1 - the distribution of stroke frequency in each month is not equal
# prepared variables:
# month - categorical ordinal
# stroke - categorinal dichotomous
# Chi-squared test



# incomplete values
#install.packages("VIM")
library(VIM)
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
# bmi null value
# View(stroke_data[!complete.cases(stroke_data), ])
# stroke_data <- na.omit(stroke_data)
# str(stroke_data)
summary(stroke_data)
# gender 
# stroke_data[!stroke_data$gender %in% c("Female", "Male"), ]
# remove unmeaningful column, and rows without gender
# 4908 rows   12 columns left
# stroke_data <- subset(stroke_data, gender %in% c("Female", "Male"),
# select = -c(id))

##########data has been prepared############################
# table(stroke_data$gender, stroke_data$stroke)
# table(stroke_data$gender)
# table(stroke_data$stroke)

# binom.test(stroke_data$stroke, n, p = 0.5,
# alternative = c("two.sided", "less", "greater"),
# conf.level = 0.95)

# plot
# cor(stroke_data[categorical_col, ], 
# method = c("pearson", "kendall", "spearman"))

# question 1 - 中风平均年龄是否是50岁
# H0 = average stroke age is 50
# H1 = average stroke age is not 50
has_stroke_data <- stroke_data[stroke_data$stroke ==1, ]
summary(has_stroke_data)
t.test(has_stroke_data$age, mu=67.7)
# from this result, the p-value is quite low
# the interval of 95% confidence level is [66.02158, 69.40426]

# question2 - 男性和女性的中风平均年龄一样
# population A = male stroke patients
# population B = female difference patients
# H0 - no significant different between population A and population B 
# H1 - there is significant difference between both population
stroke_male <- subset(has_stroke_data, gender=="Male")
stroke_female <- subset(has_stroke_data,gender=="Female")

var.test(stroke_male$age, stroke_female$age, alternative = "two.sided")
# p-value is less than 0.05, failed to reject H0
t.test(stroke_male$age, stroke_female$age, var.equal=TRUE, paired=FALSE)
# in 0.05 significant level, p-value is large, we accept H0
# stroke patients has average age of 42
t.test(has_stroke_data$age, mu = 42)

# count the number of patients of ever gender 
observed <- table( stroke_data$gender )
observed
table(stroke_data[c("Residence_type", "gender" )])
table(stroke_data[c("gender", "Residence_type" )])

library(mice)
md.pattern(stroke_data)
stroke_data[!complete.cases(stroke_data), ]

# interest variables
library(gtsummary)
table(stroke_data[categorical_col])
# correlation
cor(stroke_data$age, stroke_data$stroke , 
    method = c("pearson", "kendall", "spearman"))
cor.test(stroke_data$age, stroke_data$stroke, method=c("pearson", "kendall", "spearman"))
# visualize

plot(stroke_data$age, stroke_data$stroke, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
t.test(stroke_data$age, stroke_data$stroke)
summary(stroke_data)
# 
shapiro.test(stroke_data$age) 
