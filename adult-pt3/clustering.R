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

# JUST FOR TESTING
df <- data.frame("age" = full_df$age, 
                 "fnlwgt" = full_df$fnlwgt)

# z score normalization

z_score <- function(df){
  continuous_names <- c("age", "fnlwgt", "education.num", "hours.per.week")
  # JUST FOR TESTING
  continuous_names <- c("age", "fnlwgt")
  for (name in continuous_names){
    mf <- mean(df[[name]])
    sf <- sum(sapply(df[[name]], function(x) abs(x - mf)))/600
    
    df[[name]] <- sapply(df[[name]], function(x) (x-mf)/sf)
  }
  
  return(df)
}

df <- z_score(df)

# distance function
a_dist <- function(v1, v2){
  sqrt(sum((v1-v2)^2))
}

# function dist_make of package usedist does this
create_a_dist <- function (dataframe, custom_dist=a_dist) {
  # JUST FOR TESTING
  mat <- matrix(0, nrow=10, ncol=10)
  for (i in 1:10){
    for (j in 1:10){
  #mat <- matrix(0, nrow = nrow(dataframe), ncol = nrow(dataframe))
  #for (i in 1:nrow(dataframe)){
  #  for (j in 1:nrow(dataframe)){
      mat[i,j] <- custom_dist(dataframe[i,], dataframe[j,])
    }
  }
  return(mat)
}

create_a_dist(df)[0:10,0:10]

# clustering
library(flexclust)
set.seed(42)

# get structure of dist function for kccaFamily from distEuclidean
create_a_dist_for_centers <- function(dataframe, centers) {
  if (ncol(dataframe) != ncol(centers)) 
    stop(sQuote("x"), " and ", sQuote("centers"), " must have the same number of columns")
  
  mat <- matrix(0, nrow = nrow(dataframe), ncol = nrow(centers))
  for (i in 1:nrow(dataframe)){
    for (j in 1:nrow(centers)){
      mat[i,j] <- a_dist(dataframe[i,], centers[j,])
    }
  }
  return(mat)
}

k_values = c(2,4,6)

for (k in k_values) {
  model <- kcca(df,k,family=kccaFamily(dist=create_a_dist_for_centers, cent=colMeans))
  print('silhoutte for k:')
  print(k)
  print(Silhouette(model, df))
}

# to see the clusters
#image(model)
# to see the final centroid positions 
#parameters(model)
# to see clusters to which original data were assigned to
#clusters(model)