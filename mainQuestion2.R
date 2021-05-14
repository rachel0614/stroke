# Question2 - whether man and woman differ in getting stroke
# make sure the time format works in my os (Chinese version)
Sys.setlocale("LC_TIME", "English")

# load data
stroke_data <- read.csv("data/stroke.csv", na = "N/A", 
                        stringsAsFactors = FALSE)
# have a look at the data
head(stroke_data, 2)
names(stroke_data)
str(stroke_data)

attach(stroke_data)
table(age, stroke)
detach(stroke_data)
# keep analytical columns
attach(stroke_data)
stroke_data <- subset(stroke_data, select = c(gender, stroke)) 
detach(stroke_data)
str(stroke_data)

# convert gender from chr to factor
stroke_data$gender <- as.factor(stroke_data$gender)
# convert variable stroke from int to factor
stroke_col <- factor(stroke_data$stroke, labels = c("No", "Yes"))
stroke_data$stroke_status <- stroke_col
stroke_data <- subset(stroke_data, select = c(gender, stroke_status)) 

# incomplete values
#install.packages("VIM")
library(VIM)
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
summary(stroke_data)
# summary
summary(stroke_data)
# remove outlier
stroke_data <- stroke_data[stroke_data$gender %in% c("Female", "Male"), ]
str(stroke_data) 
##########data has been prepared############################
# gender - categorical dichotomous [Female, Male]
# stroke - categorical dichotomous [Yes, No]
# H0 -  man and woman do not differ in getting stroke
# H1 -  man and woman differ in getting stroke
# proportion of each category
# table(stroke_data)
# margin.table(stroke_data,2) 
t <- table(stroke_data$gender, stroke_data$stroke_status)
# prop.table(t)
prop.table(t, 1)
prop.table(t, 2)
# margin.table(t, 1)
# margin.table(t, 2)
# plot by gender by stroke
barplot(t, beside=T,
        col="lightblue", 
        names.arg=c("stroke","not stroke"),
        legend("topright", 
               legend=c("Male", "Female"),fill=c("red", "blue"))
        )

       