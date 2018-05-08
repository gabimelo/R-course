df = data.frame(c(1,2,3,4), c(2,3,4,5), c(4,5,6,7))
names(df) <- c("c1","c2","c3")

custom_dist <- function(v1, v2){
  sqrt(sum((v1-v2)**2))
}

dist_make(df, custom_dist)

a_dist_make <- function(a_df, a_custom_dist) {
  mat <- matrix(, nrow = nrow(a_df), ncol = nrow(a_df))
  for (i in 1:nrow(a_df)){
    for (j in 1:nrow(a_df)){
      mat[i,j] <- a_custom_dist(df[i,], df[j,])
    }
  }
  return(mat)
}

a_dist_make(df, custom_dist)

dist(df)
