cnames <- c("age", "workclass", "fnlwgt", "education", "education.num", "marital.status", 
            "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", 
            "hours.per.week", "native.country", "income")
df <- read.csv("adult/adult.csv", col.names = cnames, header = FALSE)

test_set <- read.csv("adult/adult_test.csv", col.names = cnames, header = FALSE)

# columns with missing data:
table(df$workclass)
table(df$occupation)
table(df$native.country)

summary(df)
summary(test_set)
count(df)

df <- subset(df,!(df$workclass==" ?" | df$occupation==" ?" | df$native.country==" ?"))
# df now has length 30162 (original 32561)
test_set <- subset(test_set,!(test_set$workclass==" ?" | test_set$occupation==" ?" | test_set$native.country==" ?"))
# df now has length 15060 (original = 16281)

table(df$age)
table(df$workclass)
table(df$fnlwgt)
table(df$education)
table(df$education.num)
table(df$marital.status)
table(df$occupation)
table(df$relationship)
table(df$race)
table(df$sex)
table(df$capital.gain)
table(df$capital.loss)
table(df$hours.per.week)
table(df$native.country)

df$education.num <- NULL
test_set$education.num <- NULL

df$workclass = factor(df$workclass)
test_set$workclass = factor(test_set$workclass)
df$education = factor(df$education)
test_set$education = factor(test_set$education)
df$marital.status = factor(df$marital.status)
test_set$marital.status = factor(test_set$marital.status)
df$occupation = factor(df$occupation)
test_set$occupation = factor(test_set$occupation)
df$relationship = factor(df$relationship)
test_set$relationship = factor(test_set$relationship)
df$race = factor(df$race)
test_set$race = factor(test_set$race)
df$sex = factor(df$sex)
test_set$sex = factor(test_set$sex)
df$native.country = factor(df$native.country)
test_set$native.country = factor(test_set$native.country)
df$income = factor(df$income)
test_set$income = factor(test_set$income)

df$over50k[df$income == " <=50K"] <- FALSE
df$over50k[df$income == " >50K"] <- TRUE
df$income <- NULL
test_set$over50k[test_set$income == " <=50K."] <- FALSE
test_set$over50k[test_set$income == " >50K."] <- TRUE
test_set$income <- NULL

df$over50k = factor(df$over50k, levels = c(0,1))
test_set$over50k = factor(test_set$over50k, levels = c(0,1))

library(rpart)
classifier = rpart(formula = income~., data = df
                   , method = "class",
                   control=rpart.control(minsplit=15, minbucket=5,cp=0.05))
y_pred = predict(classifier, newdata = test_set[-14], type = 'class')

cm = table(test_set[,14], y_pred)

TN = true_negatives = cm[1, 1]
FN = false_negatives = cm[2, 1]
FP = false_positives = cm[1, 2]
TP = true_positives = cm[2, 2]
total = TN + FN + FP + TP

accuracy = (TN + TP)/total # 
sensitivity = TP/(TP+FN) #
specificity = TN/(TN+FP) #
precision = TP/(TP+FP) # 

misclassification = (FN+FP)/total #

plot(classifier)
text(classifier)
# fit <- rpart(Price ~ Mileage + Type + Country, cu.summary)
#plot(classifier, compress = TRUE)
# text(classifier, use.n = TRUE)

printcp(classifier)
plotcp(classifier)
summary(classifier)
plot(classifier, uniform=TRUE, 
     main="Classification Tree for Adult Data Set")
text(classifier, use.n=TRUE, all=TRUE, cex=.8, pretty=FALSE)
text(t1, pretty=FALSE)
