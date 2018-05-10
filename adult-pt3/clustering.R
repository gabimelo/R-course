# DATA LOAD AND PREPROCESSING
cnames <- c("age", "workclass", "fnlwgt", "education", "education.num", "marital.status", 
            "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", 
            "hours.per.week", "native.country", "income")

full_df <- read.csv("adult/adult.csv", col.names = cnames, header = FALSE, nrows=600)

# df is the simple adult set:
df <- data.frame("age" = full_df$age, 
                 "fnlwgt" = full_df$fnlwgt, 
                 "education.num" = full_df$education.num,
                 "marital.status" = full_df$marital.status,
                 "sex" = full_df$sex,
                 "hours.per.week" = full_df$hours.per.week)

# z score normalization

z_score <- function(dataframe){
  continuous_names <- c("age", "fnlwgt", "education.num", "hours.per.week")
  for (name in continuous_names){
    mf <- mean(dataframe[[name]])
    sf <- sum(sapply(dataframe[[name]], function(x) abs(x - mf)))/600
    
    dataframe[[name]] <- sapply(dataframe[[name]], function(x) (x-mf)/sf)
  }
  
  return(dataframe)
}

df <- z_score(df)

# distance function
a_dist <- function(v1, v2){
  distances <- rep(0,length(v1))
  
  for (i in 1:length(v1)){
    if (suppressWarnings(!is.na(as.numeric(v1[i])))){
      distances[i] <- abs(as.numeric(v1[i]) - as.numeric(v2[i]))
    }
    else {
      distances[i] <- if (v1[i] != v2[i]) 1 else 0
    }
  }
  mean(distances)
}

# function dist_make of package usedist does this
# get structure of dist function for kccaFamily from distEuclidean
create_a_dist <- function(dataframe, dataframe2=dataframe, custom_dist=a_dist) {
  if (ncol(dataframe) != ncol(dataframe2)) 
    stop(sQuote("x"), " and ", sQuote("dataframe2"), " must have the same number of columns")

  mat <- matrix(0, nrow = nrow(dataframe), ncol = nrow(dataframe2))
  for (i in 1:nrow(dataframe)){
    for (j in 1:nrow(dataframe2)){
      mat[i,j] <- a_dist(dataframe[i,], dataframe2[j,])
    }
  }
  return(mat)
}

create_a_dist(head(df,10))

most_frequent <- function(col){
  values <- unique(col)
  values[which.max(tabulate(match(col,values)))]
}

custom_mean <- function(col){
  if (suppressWarnings(all(!is.na(as.numeric(col))))){
    mean(as.numeric(col))
  }
  else {
    most_frequent(col)
  }
}

a_means <- function(df){
  apply(df, 2, custom_mean)
}

# clustering
library(flexclust)
set.seed(42)

k = 2
model_2 <- kcca(df,k,family=kccaFamily(dist=create_a_dist, cent=a_means))
print(info(model_2, "size"))
print(Silhouette(model_2, df))

k = 4
model_4 <- kcca(df,k,family=kccaFamily(dist=create_a_dist, cent=a_means))
print(info(model_4, "size"))
print(Silhouette(model_4, df))

k = 6
model_6 <- kcca(df,k,family=kccaFamily(dist=create_a_dist, cent=a_means))
print(info(model_6, "size"))
print(Silhouette(model_6, df))

# using only continuous attributes and euclidian distance
# 1   2 
# 336 264 
# 1         2 
# 0.1533202 0.2291694 
# 1   2   3   4 
# 264  93 166  77 
# 1         2         3         4 
# 0.2975350 0.1591932 0.1718354 0.1801799 
# 1   2   3   4   5   6 
# 125  71 100 184  67  53 
# 1         2         3         4         5         6 
# 0.2742589 0.1242666 0.1677348 0.3544542 0.1646965 0.1963337 

# silhouttes_2 <- c(0.2908005,0.3571671)
# silhouttes_4 <- c(0.4033106,0.3729530,0.3053805,0.2725825)
# silhouttes_6 <- c(0.3935523,0.3859331,0.2898036,0.3521418,0.3681773,0.2676706)
# 

# 1   2 
# 357 243 
# 1         2 
# 0.1618181 0.2238968 
# 1   2   3   4 
# 63 231  74 232 
# 1         2         3         4 
# 0.1057056 0.2171990 0.1803694 0.1644212 
# 1   2   3   4   5   6 
# 62 205  59 170  29  75 
# 1          2          3          4          5          6 
# 0.09210878 0.16996839 0.21111160 0.22947173 0.18697583 0.19903790

# > k = 2
# > model_2 <- kcca(df,k,family=kccaFamily(dist=create_a_dist, cent=a_means))
# > print(info(model_2, "size"))
# 1   2 
# 362 238 
# > print(Silhouette(model_2, df))
# 1         2 
# 0.1876034 0.1797288 
# > 
#   > k = 4
# > model_4 <- kcca(df,k,family=kccaFamily(dist=create_a_dist, cent=a_means))
# > print(info(model_4, "size"))
# 1   2   3   4 
# 155  82 170 193 
# > print(Silhouette(model_4, df))
# 1         2         3         4 
# 0.1809695 0.1161434 0.1904446 0.2329581 
# > 
#   > k = 6
# > model_6 <- kcca(df,k,family=kccaFamily(dist=create_a_dist, cent=a_means))
# > print(info(model_6, "size"))
# 1   2   3   4   5   6 
# 114 205  38  72  90  81 
# > print(Silhouette(model_6, df))
# 1          2          3          4          5          6 
# 0.21621655 0.20680100 0.12541721 0.14916619 0.08496684 0.17585305 

silhouttes_2 <- c(0.1618181,0.2238968)
silhouttes_4 <- c(0.1057056,0.2171990,0.1803694,0.1644212)
silhouttes_6 <- c(0.09210878,0.16996839,0.21111160,0.22947173,0.18697583,0.19903790)

t.test(silhouttes_2, silhouttes_4)
t.test(silhouttes_2, silhouttes_6)
t.test(silhouttes_4, silhouttes_6)

# all p values are really big, so failed to reject H0
# so we cant say that the sillhouttes coefficient is better in any of them

# to see the clusters
#image(model)
# to see the final centroid positions 
#parameters(model)
# to see clusters to which original data were assigned to
#clusters(model)
