library(bnlearn)

df <- read.csv("adult/df.csv")
test_set <- read.csv("adult/test_set.csv")
df['X'] <- NULL
test_set['X'] <- NULL

df[] <- lapply(df, factor)
test_set[] <- lapply(test_set, factor)

# manual graph, learned probabilities

dag = model2network("[american][white|american][male][married][primeage][over50k|white:male:married:primeage]")

manual_net_learned_probs <- bn.fit(dag, df)

plot(dag)

# manual probabilities and graph:

cptA = matrix(c(0.1, 0.9), ncol = 2, dimnames = list(NULL, c(0, 1)))
cptMR = matrix(c(0.55, 0.45), ncol = 2, dimnames = list(NULL, c(0, 1)))
cptML = matrix(c(0.33, 0.67), ncol = 2, dimnames = list(NULL, c(0, 1)))
cptP = matrix(c(0.36, 0.64), ncol = 2, dimnames = list(NULL, c(0, 1)))

cptW = c(0.35, 0.65, 0.12, 0.88)
dim(cptW) = c(2, 2)
dimnames(cptW) = list("white" = c(0, 1), "american" =  c(0, 1))

# newdata <- subset(df, !male & married & !primeage & white, select=c(over50k))
# table(newdata)
# sum = sum + count(newdata)

cptO = c(0.99, 0.01, 0.98, 0.02, 0.96, 0.04, 0.97, 0.03, 
         0.7, 0.3, 0.67, 0.33, 0.8, 0.2, 0.75, 0.25, 
         0.95, 0.05, 0.92, 0.08, 0.91, 0.09, 0.83, 0.17, 
         0.62, 0.38, 0.44, 0.56, 0.56, 0.44, 0.50, 0.50)
dim(cptO) = c(2, 2, 2, 2, 2)
dimnames(cptO) = list("over50k" = c(0, 1), "white" =  c(0, 1), "male" =  c(0, 1), "married" =  c(0, 1), "primeage" =  c(0, 1))

net = model2network("[american][white|american][male][married][primeage][over50k|white:male:married:primeage]")
manual = custom.fit(net, dist = list(american = cptA, white = cptW, male = cptML, married = cptMR, primeage = cptP, over50k = cptO))
manual
plot(net)

# learned probabilities and graph:

pdag = iamb(df)
pdag
plot(pdag)

# dag2 = set.arc(dag2, from = "male", to = "married")
# dag2 = set.arc(dag2, from = "male", to = "over50k")
pdag = set.arc(pdag, from = "married", to = "male")
pdag = set.arc(pdag, from = "primeage", to = "married")

plot(pdag)

automatic = bn.fit(pdag, df)

# scoring

score(dag, df)
score(net, df)
score(pdag, df)

# testing the predictions:

y_pred = predict(manual_net_learned_probs, node='over50k', data=test_set[-6])
y_pred = predict(manual, node='over50k', data=test_set[-6])
y_pred = predict(automatic, node='over50k', data=test_set[-6])

cm = table(test_set[,6], y_pred)
TN = true_negatives = cm[1, "0"]
FN = false_negatives = cm[2, "0"]
FP = false_positives = cm[1, "1"]
TP = true_positives = cm[2, "1"]
total = TN + FN + FP + TP

accuracy = (TN + TP)/total # 0.7661; 0.7676; 0.7688
sensitivity = TP/(TP+FN) # 0.0702; 0.4001; 0.6578
specificity = TN/(TN+FP) # 0.9814; 0.8812; 0.8031
precision = TP/(TP+FP) # 0.5389; 0.5103; 0.5082

ROC = sensitivity * (1 - specificity) # 0.0013; 0.0475; 0.1295

library(Metrics)

mse(actual = as.integer(test_set[,6]), predicted = as.integer(y_pred))
# 0.2338; 0.2324; 0.2312
