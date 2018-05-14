library(bnlearn)

cnames <- c("age", "workclass", "fnlwgt", "education", "education.num", "marital.status", 
            "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", 
            "hours.per.week", "native.country", "income")
df <- read.csv("adult/adult.csv", col.names = cnames, header = FALSE)
test_set <- read.csv("adult/adult_test.csv", col.names = cnames, header = FALSE)

# columns with missing data: workclass, occupation, native country
df <- subset(df,!(df$workclass==" ?" | df$occupation==" ?" | df$native.country==" ?"))
# df now has length 30162 (original 32561)
test_set <- subset(test_set,!(test_set$workclass==" ?" | test_set$occupation==" ?" | test_set$native.country==" ?"))
# df now has length 15060 (original = 16281)

df$education.num <- NULL
test_set$education.num <- NULL

test_set$age[test_set$age <= 30] <- "Young"
test_set$age[test_set$age > 30 & test_set$age <= 40] <- "Middle"
test_set$age[test_set$age > 40 & test_set$age <= 65] <- "Senior"
test_set$age[test_set$age > 65 & test_set$age <= 90] <- "Old"
test_set$overtime[test_set$hours.per.week > 60] <- "too much"
test_set$overtime[test_set$hours.per.week > 40 & test_set$hours.per.week <= 60] <- "overtime"
test_set$overtime[test_set$hours.per.week > 25 & test_set$hours.per.week <= 40] <- "full time"
test_set$overtime[test_set$hours.per.week <= 25] <- "part time"
test_set$hours.per.week <- NULL

test_set$fnlwgt <- NULL
test_set$capital.gain <- NULL
test_set$capital.loss <- NULL
test_set$age = factor(test_set$age)
test_set$overtime = factor(test_set$overtime)

df$age[df$age <= 30] <- "Young"
df$age[df$age > 30 & df$age <= 40] <- "Middle"
df$age[df$age > 40 & df$age <= 65] <- "Senior"
df$age[df$age > 65 & df$age <= 90] <- "Old"
df$overtime[df$hours.per.week > 60] <- "too much"
df$overtime[df$hours.per.week > 40 & df$hours.per.week <= 60] <- "overtime"
df$overtime[df$hours.per.week > 25 & df$hours.per.week <= 40] <- "full time"
df$overtime[df$hours.per.week <= 25] <- "part time"
df$hours.per.week <- NULL

df$fnlwgt <- NULL
df$capital.gain <- NULL
df$capital.loss <- NULL
df$age = factor(df$age)
df$overtime = factor(df$overtime)

bl = data.frame(from = c("income", "income", "income", "income", 
                         "income", "income", "income", "income", 
                         "income", "income"), 
                to = c("overtime", "age", "workclass", "education", 
                       "marital.status", "occupation", "relationship",
                       "race", "sex", "native.country"))

pdag = iamb(df, blacklist = bl)
# pdag

pdag = set.arc(pdag, from = "native.country", to = "race")
pdag = set.arc(pdag, from = "occupation", to = "workclass")
pdag = set.arc(pdag, from = "education", to = "occupation")
plot(pdag)

automatic = bn.fit(pdag, df)
y_pred = predict(automatic, node='income', data=test_set[-10])

cm = table(test_set[,10], y_pred)

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

print(accuracy)
print(cm)

y_pred = predict(automatic, node='income', data=df[-10])

cm = table(df[,10], y_pred)

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

print(accuracy)
print(cm)
