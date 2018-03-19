cnames <- c("age", "workclass", "fnlwgt", "education", "education.num", "marital.status", 
            "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", 
            "hours.per.week", "native.country", "income")
df <- read.csv("adult/adult.csv",col.names = cnames,header = FALSE)

test_set <- read.csv("adult/adult_test.csv", col.names = cnames,header = FALSE)

# columns with missing data:
table(df$workclass)
table(df$occupation)
table(df$native.country)

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

test_set$fnlwgt <- NULL
test_set$occupation <- NULL
test_set$capital.gain <- NULL
test_set$capital.loss <- NULL

df$american[df$native.country!=" United-States"] <- FALSE
df$american[df$native.country==" United-States"] <- TRUE
df$native.country <- NULL
printPercentages(df$american)

test_set$american[test_set$native.country!=" United-States"] <- FALSE
test_set$american[test_set$native.country==" United-States"] <- TRUE
test_set$native.country <- NULL

printPercentages(df$sex)

df$overtime[df$hours.per.week > 60] <- "too much"
df$overtime[df$hours.per.week > 40 & df$hours.per.week <= 60] <- "overtime"
df$overtime[df$hours.per.week > 25 & df$hours.per.week <= 40] <- "full time"
df$overtime[df$hours.per.week <= 25] <- "part time"
df$hours.per.week <- NULL
printPercentages(df$overtime)

test_set$hours.per.week <- NULL

df$white[df$race == " White"] <- TRUE
df$white[df$race != " White"] <- FALSE
df$race <- NULL
printPercentages(df$white)

test_set$white[test_set$race == " White"] <- TRUE
test_set$white[test_set$race != " White"] <- FALSE
test_set$race <- NULL

df$married[df$relationship==" Husband" | df$relationship==" Wife"] <- TRUE
df$married[df$relationship!=" Husband" & df$relationship!=" Wife"] <- FALSE
df$relationship <- NULL
df$marital.status <- NULL
printPercentages(df$married)

test_set$married[test_set$relationship==" Husband" | test_set$relationship==" Wife"] <- TRUE
test_set$married[test_set$relationship!=" Husband" & test_set$relationship!=" Wife"] <- FALSE
test_set$relationship <- NULL
test_set$marital.status <- NULL

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
test_set$education.num <- NULL


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

df$male[df$sex == " Male"] <- TRUE
df$male[df$sex == " Female"] <- FALSE

df$over50k[df$income == " <=50K"] <- FALSE
df$over50k[df$income == " >50K"] <- TRUE
df$income <- NULL

test_set$age[test_set$age <= 30] <- "Young"
test_set$age[test_set$age > 30 & test_set$age <= 40] <- "Middle"
test_set$age[test_set$age > 40 & test_set$age <= 65] <- "Senior"
test_set$age[test_set$age > 65 & test_set$age <= 90] <- "Old"

test_set$primeage[test_set$age == "Young" | test_set$age == "Old"] <- FALSE
test_set$primeage[test_set$age != "Young" & test_set$age != "Old"] <- TRUE

test_set$male[test_set$sex == " Male"] <- TRUE
test_set$male[test_set$sex == " Female"] <- FALSE

test_set$over50k[test_set$income == " <=50K."] <- FALSE
test_set$over50k[test_set$income == " >50K."] <- TRUE
test_set$income <- NULL

summary(df)

cor(df$married, df$over50k)
cor(df$primeage, df$over50k)
cor(df$male, df$over50k)
cor(df$white, df$over50k)
cor(df$american, df$over50k)

# selecionados: american (true), sex (male), white (true), married (true), age (30 < age <= 65)

df$age = NULL
df$workclass = NULL
df$education = NULL
df$sex = NULL
df$overtime = NULL

test_set$age = NULL
test_set$workclass = NULL
test_set$education = NULL
test_set$sex = NULL

write.csv(df, file = "adult/df.csv")
write.csv(test_set, file = "adult/test_set.csv")

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
# 
# test_set$american[test_set$american == FALSE] <- 0
# test_set$american[test_set$american == TRUE] <- 1
# test_set$white[test_set$white == FALSE] <- 0
# test_set$white[test_set$white == TRUE] <- 1
# test_set$married[test_set$married == FALSE] <- 0
# test_set$married[test_set$married == TRUE] <- 1
# test_set$primeage[test_set$primeage == FALSE] <- 0
# test_set$primeage[test_set$primeage == TRUE] <- 1
# test_set$male[test_set$male == FALSE] <- 0
# test_set$male[test_set$male == TRUE] <- 1
# test_set$over50k[test_set$over50k == FALSE] <- 0
# test_set$over50k[test_set$over50k == TRUE] <- 1

# df[] <- lapply(df, factor)
# test_set[] <- lapply(test_set, factor)

# df$over50k = factor(df$over50k, levels = c(0,1))
# df$american = factor(df$american, levels = c(0,1))
# df$white = factor(df$white, levels = c(0,1))
# df$male = factor(df$male, levels = c(0,1))
# df$married = factor(df$married, levels = c(0,1))
# df$primeage = factor(df$primeage, levels = c(0,1))
# test_set$over50k = factor(test_set$over50k, levels = c(0,1))
# test_set$american = factor(test_set$american, levels = c(0,1))
# test_set$white = factor(test_set$white, levels = c(0,1))
# test_set$male = factor(test_set$male, levels = c(0,1))
# test_set$married = factor(test_set$married, levels = c(0,1))
# test_set$primeage = factor(test_set$primeage, levels = c(0,1))
