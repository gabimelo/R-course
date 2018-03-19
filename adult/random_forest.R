library(rpart)

df$over50k = factor(df$over50k, levels = c(0,1))
test_set$over50k = factor(test_set$over50k, levels = c(0,1))

classifier = rpart(formula = over50k ~ american + white + married + primeage + male, data = df)

y_pred = predict(classifier, newdata = test_set[-6], type = 'class')

cm = table(test_set[,6], y_pred)
cm
TN = true_negatives = cm[1, "0"]
FN = false_negatives = cm[2, "0"]
FP = false_positives = cm[1, "1"]
TP = true_positives = cm[2, "1"]
total = TN + FN + FP + TP

accuracy = (TN + TP)/total # 0.7629
sensitivity = TP/(TP+FN) # 0.6435
specificity = TN/(TN+FP) # 0.8008
precision = TP/(TP+FP) # 0.5060

ROC = sensitivity * (1 - specificity) # 0.1282

library(Metrics)

# test_set[,6] = factor(test_set[,6], levels = c(0,1))
# y_pred = factor(y_pred, levels = c(0,1))

mse(actual = as.integer(test_set[,6]), predicted = as.integer(y_pred))
# 0.2371

plot(classifier)
text(classifier)
