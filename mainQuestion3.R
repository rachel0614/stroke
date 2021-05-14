# Whether smoking has correlation with stroke
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
# incomplete values
#install.packages("VIM")
library(VIM)
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
# bmi null value
# View(stroke_data[!complete.cases(stroke_data), ])
# stroke_data <- na.omit(stroke_data)
# str(stroke_data)
summary(stroke_data)
##########data has been prepared############################
# H0 - smoking does not affect stroke
# H1 - smoking affect stroke
str(stroke_data)
