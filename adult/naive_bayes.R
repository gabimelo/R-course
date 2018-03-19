library(e1071)

df <- read.csv("adult/df.csv")
test_set <- read.csv("adult/test_set.csv")
df['X'] <- NULL
test_set['X'] <- NULL

df$over50k = factor(df$over50k)
test_set$over50k = factor(test_set$over50k)

classifier = naiveBayes(x = df[-6], y = df$over50k)

y_pred = predict(classifier, newdata = test_set[-6])

cm = table(test_set[,6], y_pred)

TN = true_negatives = cm[1, "0"]
FN = false_negatives = cm[2, "0"]
FP = false_positives = cm[1, "1"]
TP = true_positives = cm[2, "1"]
total = TN + FN + FP + TP

accuracy = (TN + TP)/total # 0.7578
sensitivity = TP/(TP+FN) # 0.6646
specificity = TN/(TN+FP) # 0.7867
precision = TP/(TP+FP) # 0.4907

ROC = sensitivity * (1 - specificity) # 0.1417

library(Metrics)

mse(actual = as.integer(test_set[,6]), predicted = as.integer(y_pred))
# 0.2421