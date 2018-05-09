# DATA LOAD AND PREPROCESSING
cnames <- c("age", "workclass", "fnlwgt", "education", "education.num", "marital.status", 
            "occupation", "relationship", "race", "sex", "capital.gain", "capital.loss", 
            "hours.per.week", "native.country", "income")

full_df <- read.csv("adult/adult.csv", col.names = cnames, header = FALSE, nrows=600)

# df is the simple adult set:
df <- data.frame("age" = full_df$age, 
                 "fnlwgt" = full_df$fnlwgt, 
                 "education.num" = full_df$education.num,
                 # JUST FOR TESTING
                 #"marital.status" = full_df$marital.status,
                 #"sex" = full_df$sex,
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
  sqrt(sum((v1-v2)^2))
}

# function dist_make of package usedist does this
# get structure of dist function for kccaFamily from distEuclidean
create_a_dist <- function(dataframe, centers=dataframe, custom_dist=a_dist) {
  if (ncol(dataframe) != ncol(centers)) 
    stop(sQuote("x"), " and ", sQuote("centers"), " must have the same number of columns")
  
  # JUST FOR TESTING
  # mat <- matrix(0, nrow=10, ncol=10)
  # for (i in 1:10){
  #  for (j in 1:10){
  mat <- matrix(0, nrow = nrow(dataframe), ncol = nrow(centers))
  for (i in 1:nrow(dataframe)){
    for (j in 1:nrow(centers)){
      mat[i,j] <- a_dist(dataframe[i,], centers[j,])
    }
  }
  return(mat)
}

create_a_dist(df)[0:10,0:10]

# clustering
library(flexclust)
set.seed(42)

k_values = c(2,4,6)

for (k in k_values) {
  model <- kcca(df,k,family=kccaFamily(dist=create_a_dist, cent=colMeans))
  print(info(model, "size"))
  print(Silhouette(model, df))
}

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
# t.test(silhouttes_2, silhouttes_4)
# t.test(silhouttes_2, silhouttes_6)
# t.test(silhouttes_4, silhouttes_6)

# all p values are really big, so failed to reject H0
# so we cant say that the sillhouttes coefficient is better in any of them

# to see the clusters
#image(model)
# to see the final centroid positions 
#parameters(model)
# to see clusters to which original data were assigned to
#clusters(model)
