df = read.csv("adult.csv")

printPercentages <- function(column){
  tab <- with(df, table(income, column))
  print(prop.table(tab, margin = 1)*100)
  
  tab <- with(df, table(column, income))
  print(prop.table(tab, margin = 1)*100)
}

df$fnlwgt <- NULL
df$occupation <- NULL
df$capital.gain <- NULL
df$capital.loss <- NULL

df$american[df$native.country!="United-States"] <- FALSE
df$american[df$native.country=="United-States"] <- TRUE
df$native.country <- NULL
printPercentages(df$american)

printPercentages(df$sex)

df$overtime[df$hours.per.week > 60] <- "too much"
df$overtime[df$hours.per.week > 40 & df$hours.per.week <= 60] <- "overtime"
df$overtime[df$hours.per.week > 25 & df$hours.per.week <= 40] <- "full time"
df$overtime[df$hours.per.week <= 25] <- "part time"
df$hours.per.week <- NULL
printPercentages(df$overtime)

df$white[df$race == "White"] <- TRUE
df$white[df$race != "White"] <- FALSE
df$race <- NULL
printPercentages(df$white)

df$married[df$relationship=="Husband" | df$relationship=="Wife"] <- TRUE
df$married[df$relationship!="Husband" & df$relationship!="Wife"] <- FALSE
df$relationship <- NULL
df$marital.status <- NULL
printPercentages(df$married)


df$education = gsub("^10th","Dropout",df$education)
df$education = gsub("^11th","Dropout",df$education)
df$education = gsub("^12th","Dropout",df$education)
df$education = gsub("^1st-4th","Dropout",df$education)
df$education = gsub("^5th-6th","Dropout",df$education)
df$education = gsub("^7th-8th","Dropout",df$education)
df$education = gsub("^9th","Dropout",df$education)
df$education = gsub("^Assoc-acdm","Associates",df$education)
df$education = gsub("^Assoc-voc","Associates",df$education)
df$education = gsub("^Bachelors","Bachelors",df$education)
df$education = gsub("^Doctorate","Doctorate",df$education)
df$education = gsub("^HS-grad","HS-Graduate",df$education)
df$education = gsub("^Masters","Masters",df$education)
df$education = gsub("^Preschool","Dropout",df$education)
df$education = gsub("^Prof-school","Prof-School",df$education)
df$education = gsub("^Some-college","HS-Graduate",df$education)
printPercentages(df$education)
df$education.num <- NULL


df$workclass = gsub("^Federal-gov","Govt",df$workclass)
df$workclass = gsub("^Local-gov","Govt",df$workclass)
df$workclass = gsub("^State-gov","Govt",df$workclass)
df$workclass = gsub("^Private","Private",df$workclass)
df$workclass = gsub("^Self-emp-inc","Self-Employed",df$workclass)
df$workclass = gsub("^Self-emp-not-inc","Self-Employed",df$workclass)
df$workclass = gsub("^Without-pay","Not-Working",df$workclass)
df$workclass = gsub("^Never-worked","Not-Working",df$workclass)
printPercentages(df$workclass)

df$age[df$age <= 30] <- "Young"
df$age[df$age > 30 & df$age <= 40] <- "Middle"
df$age[df$age > 40 & df$age <= 65] <- "Senior"
df$age[df$age > 65 & df$age <= 90] <- "Old"
printPercentages(df$age)

df$primeage[df$age == "Young" | df$age == "Old"] <- FALSE
df$primeage[df$age != "Young" & df$age != "Old"] <- TRUE

df$male[df$sex == "Male"] <- TRUE
df$male[df$sex == "Female"] <- FALSE

df$over50k[df$income == "<=50K"] <- FALSE
df$over50k[df$income == ">50K"] <- TRUE
df$income <- NULL

summary(df)

cor(df$married, df$over50k)
cor(df$primeage, df$over50k)
cor(df$male, df$over50k)
cor(df$white, df$over50k)
cor(df$american, df$over50k)

# selecionados: american (true), sex (male), white (true), married (true), age (30 < age <= 65)

library(bnlearn)

named_nodes = c("american", "white", "male", "married", "primeage", "over50k")

e = empty.graph(named_nodes)

adj = matrix(0L, ncol = 6, nrow = 6,
            dimnames = list(named_nodes, named_nodes))

adj["american", "white"] = 1L
adj["white", "over50k"] = 1L
adj["male", "over50k"] = 1L
adj["married", "over50k"] = 1L
adj["primeage", "over50k"] = 1L

amat(e) = adj

e


# df$age = NULL
# df$workclass = NULL
# df$education = NULL
# df$sex = NULL
# df$overtime = NULL
# 
# df$american[df$american == FALSE] <- 0
# df$american[df$american == TRUE] <- 1
# df$white[df$white == FALSE] <- 0
# df$white[df$white == TRUE] <- 1
# df$married[df$married == FALSE] <- 0
# df$married[df$married == TRUE] <- 1
# df$primeage[df$primeage == FALSE] <- 0
# df$primeage[df$primeage == TRUE] <- 1
# df$male[df$male == FALSE] <- 0
# df$male[df$male == TRUE] <- 1
# df$over50k[df$over50k == FALSE] <- 0
# df$over50k[df$over50k == TRUE] <- 1
