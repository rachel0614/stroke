# my os (windows Chinese version)
# the date format converts incorrectly without setting the locale
Sys.setlocale("LC_TIME", "English")
# predefine the factor 
zero_one_factor <- c("0","1")
# load and convert the below columns 
stroke_data <- within(read.csv("data/stroke.csv", na = "N/A", 
                               stringsAsFactors = FALSE),{
                                 gender <- as.numeric(factor(gender))
                                 hypertension <- as.numeric(factor(hypertension,labels = zero_one_factor))
                                 heart_disease <- as.numeric(factor(heart_disease, labels = zero_one_factor))     
                                 ever_married <- as.numeric(factor(ever_married))    
                                 work_type <- as.numeric(factor(work_type))
                                 Residence_type <- as.numeric(factor(Residence_type) )
                                 smoking_status <- factor(smoking_status)
                                 stroke <- as.numeric(factor(stroke,labels = zero_one_factor))
                                 Date <- as.Date(Date,"%A %d %B %Y")
                               })
# have a look at the data, to make sure the data type conversion has been done correctly.
head(stroke_data, 2)
str(stroke_data)
levels(stroke_data$smoking_status)
# age level
age_col <- 
  as.numeric(factor(ifelse(stroke_data$age>=65, "1", "0")))
stroke_data$age_level <- age_col

# str(stroke_data)
# smoking status 
smoke_col <- 
  factor(ifelse(stroke_data$smoking_status=="smokes", "1", "0"))
stroke_data$smoke_col <- as.numeric(smoke_col)

diabetes_col <- cut(stroke_data$avg_glucose_level,
                    breaks = c(0, 140, 199, Inf), 
                    labels = c('1', '2', "3"), 
                    right = FALSE,
                    order = TRUE)
stroke_data$diabetes <- as.numeric(diabetes_col)


# stroke_data$obesity <- 
#   as.num(ifelse(stroke_data$bmi>=30, "1", "0"))

str(stroke_data)

stroke_data <- subset(stroke_data, select = 
                        -c(Date, id,  
                           age, 
                           bmi, smoking_status,   
                           avg_glucose_level))

# dichotomous -> 0,1 encode
# ordinal -> ordinal number
# nominal -> dummy

# 数值变量预处理->连续型变量离散化
# age -> age_level


str(stroke_data)
attach(stroke_data)
# dichotomous
summary(stroke_data)
set.seed(1234)
total <- nrow(stroke_data)
train_indecies <- sample(total, 0.7 * total)
stroke_train <- stroke_data[train_indecies,]
stroke_test <- stroke_data[-train_indecies,]
table(stroke_train$smoke)
table(stroke_test$smoke)

lr_model <- glm(smoke ~., data = stroke_train, family = binomial())
summary(lr_model)

prob <- predict(lr_model, stroke_test, type = "response")
# pred <- factor(prob > 0.5, levels = c(FALSE, TRUE))
lr.perf <- table(stroke_test$smoke, prob, dnn = c("Actual", "Predicted"))
lr.perf

# decision tree
library(rpart)
set.seed(1234)
dt_model <- rpart(smoke ~ ., data = stroke_train,
                  method = "class",
                  parms = list(split = "information"))
dt_model$cptable
# 最优树交叉验证误差为0.3644±0.0036，所以选择第7个终端节点及六次分割，cp=0.01
stroke_data$smoke <- as.factor(stroke_data$smoke)
str(stroke_data)
pruned <- prune(dt_model, cp = 0.125)
library(rpart.plot)
prp(pruned, type = 2, extra = 104,
    fallen.leaves = TRUE,
    main = "Decision tree")
pred <- predict(pruned, stroke_test, type = "class")
dt.perf <- table(stroke_test$smoke, pred, dnn = c("Actual", "Predicted"))
dt.perf

# random forest
library(randomForest)
set.seed(1234)
fit.forest<-randomForest(smoke ~.,data=stroke_train,na.action= na.roughfix,
                         importance=TRUE)#生成森林
fit.forest
importance(fit.forest,type=2)
#给出变量重要性
forest.pred<-predict(fit.forest,stroke_test)
forest.perf<-table(stroke_test$smoke,
                   forest.pred,
                   dnn=c("Actual","Predicted"))
forest.perf
# svm
library(e1071)
set.seed(1234)
fit.svm<-svm(stroke_train$smoke ~., data = stroke_train)
fit.svm
svm.pred<-predict(fit.svm,na.omit(stroke_test))
svm.perf<-table(na.omit(stroke_test)$smoke, 
                svm.pred,
                dnn=c("Actual","Predicted"))
svm.perf

detach(stroke_data)
performance(forest.perf)
performance(dt.perf)
performance(svm.perf)



performance<-function(table,n=2){
  if(!all(dim(table)==c(2,2)))
    stop("MUST be a 2*2 table")
  tn = table[2,2]
  fp = table[2,1]
  fn = table[1,2]
  tp = table[1,1]
  sensitivity = tp / ( tp + fn )
  specificity = tn / ( tn + fp )
  positive = tp / (tp + fp)
  negative = tn / (tn + fn)
  hitrate = (tp + tn)/(tp + tn + fp + fn)
  result<- paste("Sensitivity = ",round(sensitivity,n),
                 "\nSpecificity = ",round(specificity,n),
                 "\nPositive predictive Value = ",round(positive,n),
                 "\nNegative predictive Value = ",round(negative,n),
                 "\nAccuracy = ",round(hitrate,n),
                 "\n",sep="")
  cat(result)
}
