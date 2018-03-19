library(bnlearn)

# manual graph, learned probabilities

dag = model2network("[american][white|american][male][married][primeage][over50k|white:male:married:primeage]")

bn.fit(dag, df)

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
dfit = custom.fit(net, dist = list(american = cptA, white = cptW, male = cptML, married = cptMR, primeage = cptP, over50k = cptO))
dfit

# learned probabilities and graph:

pdag = iamb(df)
pdag
plot(pdag)

dag2 = set.arc(dag2, from = "male", to = "married")
dag2 = set.arc(dag2, from = "male", to = "over50k")
dag2 = set.arc(dag2, from = "married", to = "over50k")

plot(dag2)

fit = bn.fit(dag2, df)

# fit

# scoring

score(dag, df)
score(net, df)
score(dag2, df)
