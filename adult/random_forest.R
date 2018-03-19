library(rpart)

df <- read.csv("adult/df.csv")
test_set <- read.csv("adult/test_set.csv")
df['X'] <- NULL
test_set['X'] <- NULL

df$over50k = factor(df$over50k)
test_set$over50k = factor(test_set$over50k)

classifier = rpart(formula = over50k~., data = df, control=rpart.control(minsplit=15, minbucket=5,cp=0.005))
y_pred = predict(classifier, newdata = test_set[-6], type = 'class')

cm = table(test_set[,6], y_pred)
TN = true_negatives = cm[1, "0"]
FN = false_negatives = cm[2, "0"]
FP = false_positives = cm[1, "1"]
TP = true_positives = cm[2, "1"]
total = TN + FN + FP + TP

accuracy = (TN + TP)/total # 0.7629
sensitivity = TP/(TP+FN) # 0.6435
specificity = TN/(TN+FP) # 0.8008
precision = TP/(TP+FP) # 0.5060

misclassification = (FN+FP)/total # 0.2312

ROC = sensitivity * (1 - specificity) # 0.1282

library(Metrics)

# test_set[,6] = factor(test_set[,6])
# y_pred = factor(y_pred)

mse(actual = as.integer(test_set[,6]), predicted = as.integer(y_pred))
# 0.2371

plot(classifier)
text(classifier)
