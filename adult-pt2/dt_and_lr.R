library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)

set.seed(87)

# DATA LOAD AND PREPROCESSING
cnames <- c("age", "workclass", "fnlwgt", "education", "education.num", "marital.status", 
            "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", 
            "hours.per.week", "native.country", "income")

df <- read.csv("adult/adult.csv", col.names = cnames, header = FALSE)
test_set <- read.csv("adult/adult_test.csv", col.names = cnames, header = FALSE)
test_set$income <- sapply(test_set$income, as.character)
test_set$income[test_set$income == ' <=50K.'] <- ' <=50K'
test_set$income[test_set$income == ' >50K.'] <- ' >50K'
test_set$income = factor(test_set$income)

# columns with missing data: workclass, occupation, native country
df <- subset(df,!(df$workclass==" ?" | df$occupation==" ?" | df$native.country==" ?"))
# df now has length 30162 (original 32561)
test_set <- subset(test_set,!(test_set$workclass==" ?" | test_set$occupation==" ?" | test_set$native.country==" ?"))
# df now has length 15060 (original = 16281)

#prop.table(table(df$income))
#prop.table(table(test_set$income))
# pretty much same distribution between classes
# there's a bit of class imbalance

# duplicate column:
df$education <- NULL
test_set$education <- NULL

# factorize capital gain and loss (LR can't deal with them numerically)
df$capital.gain.greater.mean = factor(ifelse(df$capital.gain > mean(df$capital.gain), 1, 0))
df$capital.loss.greater.mean = factor(ifelse(df$capital.loss > mean(df$capital.loss), 1, 0))
df$capital.gain <- NULL
df$capital.loss <- NULL
test_set$capital.gain.greater.mean = factor(ifelse(test_set$capital.gain > mean(test_set$capital.gain), 1, 0))
test_set$capital.loss.greater.mean = factor(ifelse(test_set$capital.loss > mean(test_set$capital.loss), 1, 0))
test_set$capital.gain <- NULL
test_set$capital.loss <- NULL

# DECISION TREE
dt = rpart(formula = income~., data = df, method = "class",
                   control=rpart.control(cp=0.003))
y_pred_dt = predict(dt, newdata = test_set[-12], type = 'class')
#printcp(dt)
#plotcp(dt)
#summary(dt)
fancyRpartPlot(dt)
confusionMatrix(y_pred_dt, test_set$income)

# cp 0.0010 acc 0.839 terminal nodes 25
# cp 0.0020 acc 0.8355 terminal nodes 17
# cp 0.0030 acc 0.8331 terminal nodes 13
# cp 0.0035 acc 0.8331 terminal nodes 13
# cp 0.0050 acc 0.8313 terminal nodes 11

y_pred_dt = predict(dt, newdata = df[-12], type = 'class')
confusionMatrix(y_pred_dt, df$income)
# cp 0.0010 acc 0.8433 (diff .0043)
# cp 0.0020 acc 0.8393 (diff .0038)
# cp 0.0030 acc 0.8366 (diff .0035)
# cp 0.0050 acc 0.8343 (diff .003)

# LOGISTIC REGRESSION
lr = glm(formula = income ~ ., family = binomial, data = df)
prob_pred = predict(lr, newdata = test_set[-12], type = 'response')
y_pred_lr = factor(ifelse(prob_pred > 0.5, ' >50K', ' <=50K'))
confusionMatrix(y_pred_lr, test_set$income)
# 0.8403 with capital gain and loss as factors representing if greater than mean
# 0.8292 without capital gain and loss altogether

prob_pred = predict(lr, newdata = df[-12], type = 'response')
y_pred_lr = factor(ifelse(prob_pred > 0.5, ' >50K', ' <=50K'))
confusionMatrix(y_pred_lr, df$income)
# 0.8418 (diff 0.0015)

## CROSS VALIDATION

# DECISION TREE
cv_dt = train(income ~ ., df, method="rpart", 
                   trControl= trainControl(method="cv", number = 10, verboseIter = TRUE))
y_pred_cv_dt = predict(cv_dt, newdata = test_set[-12], type = 'raw')
fancyRpartPlot(cv_dt$finalModel)
confusionMatrix(y_pred_cv_dt, test_set$income)
# acc 0.8234 terminal nodes 6
# cp 0.00792488 acc  0.8227577 on classifier

y_pred_cv_dt = predict(cv_dt, newdata = df[-12], type = 'raw')
confusionMatrix(y_pred_cv_dt, df$income)
# 0.8239 (dif 0.0005 ---> variance diminuiu quando usou cross validation)

# LOGISTIC REGRESSION
cv_lr = train(income ~ ., method='glm', family = binomial, data = df, 
                   trControl= trainControl(method="cv", number = 10, verboseIter = TRUE))
y_pred_cv_lr = predict(cv_lr, newdata = test_set[-12], type = 'raw')
confusionMatrix(y_pred_cv_lr, test_set$income)
# 0.840926 on classifier
# 0.8402 on test set predictions

y_pred_cv_lr = predict(cv_lr, newdata = df[-12], type = 'raw')
confusionMatrix(y_pred_cv_lr, df$income)

# 0.8419 (diff 0.0017--> its weird that variance increased when using CV 
# (but maybe not, given that for LR it doesn't set any parameters from CV))

# T-TEST
results <- resamples(list(DT=cv_dt, LR=cv_lr))
#summary(results)
summary(diff(results))
#results$values
DT = results$values$`DT~Accuracy`
LR = results$values$`LR~Accuracy`

# H0 = DT's accuracy mean is equal to LR's accuracy mean
# H1 = DT's accuracy mean is less than LR's accuracy mean
t.test(DT,LR,paired=FALSE,alternative="less")
# if p-value > 0.05 we fail to reject the null hypothesis
# p-value = 0.00001232, which means we reject the null hypothesis
# rejecting null hypothesis means we accept H1
# therefore LR had a better accuracy than DT