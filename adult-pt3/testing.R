old_vals <- c(1,2,1.3,2.3,1.6,2.6,6,7,6.3,7.3,6.6,7.6)
vals <- c(1,2,1.3,2.3,4.6,2.6,6,7,6.3,7.3,6.6,7.6)
df = data.frame(old_vals,vals)
names(df) <- c("c1","c2")
#plot(df)

custom_dist <- function(v1, v2){
  sqrt(sum((v1-v2)**2))
}

for (k in 1:nrow(centers)) {
  z[, k] <- sqrt(colSums((t(x) - centers[k, ])^2))
}

sqrt(colSums((t(x) - centers[k, ])^2))


a_dist_make <- function(dataframe, centers) {
  #print('no')
  #print(centers)
  #print(df)
  
  if (ncol(dataframe) != ncol(centers)) 
    stop(sQuote("x"), " and ", sQuote("centers"), " must have the same number of columns")
  
  mat <- matrix(0, nrow = nrow(dataframe), ncol = nrow(centers))
  for (i in 1:nrow(dataframe)){
    for (j in 1:nrow(centers)){
      mat[i,j] <- custom_dist(dataframe[i,], centers[j,])
    }
  }
  #print(mat)
  return(mat)
}


kcca(df,2,family=kccaFamily(dist=a_dist_make,cent="mean"))
# kcca(df,2,family=kccaFamily(dist=a_custom_dist,cent="mean"))

distEuclidean



# dont even know anymor
a_dist_make <- function(a_df, centers=NULL) {
  if(is.null(centers)){
    print('yes')
    
    mat <- matrix(, nrow = nrow(a_df), ncol = nrow(a_df))
    for (i in 1:nrow(a_df)){
      for (j in 1:nrow(a_df)){
        mat[i,j] <- custom_dist(a_df[i,], a_df[j,])
      }
    }
    #print(as.dist(mat))
    return(as.dist(mat))
    #return(mat)
  }
  else {
    #print('no')
    #print(centers)
    #print(df)
    
    if (ncol(a_df) != ncol(centers)) 
      stop(sQuote("x"), " and ", sQuote("centers"), " must have the same number of columns")
    
    mat <- matrix(0, nrow = nrow(a_df), ncol = nrow(centers))
    #for (i in 1:nrow(a_df)){
      #for (j in 1:nrow(centers)){
       # mat[i,j] <- a_custom_dist(df, centers[j,])
      #}
    #}
    for (k in 1:nrow(centers)) {
      mat[, k] <- sqrt(colSums((t(a_df) - centers[k, ])^2))
    }
    print(mat)
    return(mat)
  }
}

