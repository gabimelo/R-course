# last column (index 58): 1 spam, 0 not

df = read.csv('spambase/spambase.data')

names <- colnames(df)[0:57]
for (name in names){
  mean <- mean(df[[name]])
  df[[name]][df[[name]] <= mean] <- 0
  df[[name]][df[[name]] > mean] <- 1
}

names(df)[names(df) == 'X1'] <- 'spam'
df$spam = factor(df$spam, levels = c(0,1))

library(caTools)

split = sample.split(df$spam, SplitRatio = 0.6)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

library(e1071)

classifier = naiveBayes(x = training_set[-58], y = training_set$spam)

y_pred = predict(classifier, newdata = test_set[-58])

cm = table(test_set[,58], y_pred)

TN = true_negatives = cm[1, "0"]
FN = false_negatives = cm[2, "0"]
FP = false_positives = cm[1, "1"]
TP = true_positives = cm[2, "1"]
total = TN + FN + FP + TP

# é muito ruim ter o número de false positives tão elevado

accuracy = (TN + TP)/total # 0.8054
sensitivity = TP/(TP+FN) # 0.96
specificity = TN/(TN+FP) # 0.7049
precision = TP/(TP+FP) # 0.6790

ROC = sensitivity * (1 - specificity) # 0.2833

library(Metrics)

test_set[,58] = factor(test_set[,58], levels = c(0,1))
y_pred = factor(y_pred, levels = c(0,1))

mse(actual = as.integer(test_set[,58]), predicted = as.integer(y_pred))
# 0.1945