df = data.frame(c(1,2,3,4), c(2,3,4,5), c(4,5,6,7))
names(df) <- c("c1","c2","c3")
df = data.frame(c(1,2,3,4), c(2,3,4,5))
names(df) <- c("c1","c2")

df = data.frame(c(18,20,56), c("male","male","female"))
names(df) <- c("age","sex")

a_dist <- function(v1, v2){
  distances <- rep(0,length(v1))
  
  distances[1] <- abs(as.numeric(v1[1]) - as.numeric(v2[1]))
  #distances[2] <- abs(as.numeric(v1[2]) - as.numeric(v2[2]))
  #distances[3] <- abs(as.numeric(v1[3]) - as.numeric(v2[3]))
  #distances[4] <- if (v1[4] != v2[4]) 1 else 0
  #distances[5] <- if (v1[5] != v2[5]) 1 else 0
  #distances[6] <- abs(as.numeric(v1[6]) - as.numeric(v2[6]))
  distances[2] <- if (v1[2] != v2[2]) 1 else 0
  
  #sqrt(sum((v1-v2)^2))
  mean(distances)
}

most_frequent <- function(col){
  dd <- unique(col)
  dd[which.max(tabulate(match(col,dd)))]
}

a_means <- function(df){
  #new <- data.frame(0,0)
  #names(new) <- colnames(df)
  #rbind(new, c(mean(df[[1]]), most_frequent(df[[2]])))[2,]
  c(mean(df[[1]]), most_frequent(df[[2]]))
}

create_a_dist <- function(dataframe, centers=dataframe, custom_dist=a_dist) {
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

kcca(df,2,family=kccaFamily(dist=create_a_dist, cent=a_means))
