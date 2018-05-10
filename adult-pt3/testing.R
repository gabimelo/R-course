# df = data.frame(c(1,2,3,4), c(2,3,4,5), c(4,5,6,7))
# names(df) <- c("c1","c2","c3")
# df = data.frame(c(1,2,3,4), c(2,3,4,5))
# names(df) <- c("c1","c2")

df = data.frame(c(18,20,56), c("male","male","female"))
names(df) <- c("age","sex")

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

kcca(df,2,family=kccaFamily(dist=create_a_dist, cent=a_means))
