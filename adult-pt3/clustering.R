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

z_score <- function(df){
  continuous_names <- c("age", "fnlwgt", "education.num", "hours.per.week")
  for (name in continuous_names){
    mf <- mean(df[[name]])
    sf <- sum(sapply(df[[name]], function(x) abs(x - mf)))/600
    
    df[[name]] <- sapply(df[[name]], function(x) (x-mf)/sf)
  }
  
  return(df)
}

df <- z_score(df)

# distance function
a_dist <- function(df) {
  return(NULL)
}

create_a_dist <- function (dataframe, custom_dist=a_dist) {
  mat <- matrix(, nrow = nrow(dataframe), ncol = nrow(dataframe))
  for (i in 1:nrow(dataframe)){
    for (j in 1:nrow(dataframe)){
      mat[i,j] <- custom_dist(dataframe[i,], dataframe[j,])
    }
  }
  return(mat)
}

silhouette_coefficient <- function() {
  
  # s(i)!=!(b(i)!–!a(i))!/!max{a(i),b(i)}!
  # bad!!!Y1!≤!s(i)!≤!1!!good
  # where i is each datum, 
  # a(i) is the average distance between i and all other data within same cluster, 
  # and b(i) is lowest average distance of i to all points in any other cluster
  
  return(NULL)
}

# clustering
set.seed(42)

k = 2
k = 4
k = 6

# perhaps check out this package
# https://cran.r-project.org/web/packages/flexclust/index.html
custom_kmeans <- function() {
  
}

cluster <- custom_kmeans(df, k)
