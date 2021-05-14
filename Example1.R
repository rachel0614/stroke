race_data <- within(read.csv("data/hsb2.csv", na = "N/A", 
                        stringsAsFactors = FALSE),{
  race <- as.factor(race)
  schtyp <- as.factor(schtyp)
  prog <- as.factor(prog)     
  female <- factor(female, labels = c("No", "Yes"))
})
str(race_data)
str(race_data)
attach(race_data)
chisq.test(table(female, schtyp))
chisq.test(table(female, race))
## 	Chi-squared test for given probabilities
chisq.test(table(race), p = c(10, 10, 10, 70)/100)
summary(race_data)
str
