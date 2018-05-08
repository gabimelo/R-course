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

#trying many different things
df = data.frame(c(1,2,3,4), c(2,3,4,5), c(4,5,6,7))
names(df) <- c("c1","c2","c3")

old_vals <- c(1,2,1.3,2.3,1.6,2.6,6,7,6.3,7.3,6.6,7.6)
vals <- c(1,2,1.3,2.3,4.6,2.6,6,7,6.3,7.3,6.6,7.6)
df = data.frame(vals,vals2)
names(df) <- c("c1","c2")

custom_dist <- function(v1, v2){
  sqrt(sum((v1-v2)**2))
}

#dist_make(df, custom_dist)

a_dist_make <- function(a_df, centers=NULL) {
  a_custom_dist <- function(v1, v2){
    #print(v1)
    sqrt(sum((v1-v2)**2))
  }
  if(is.null(centers)){
    print('yes')
    
    mat <- matrix(, nrow = nrow(a_df), ncol = nrow(a_df))
    for (i in 1:nrow(a_df)){
      for (j in 1:nrow(a_df)){
        mat[i,j] <- a_custom_dist(df[i,], df[j,])
      }
    }
    #print(as.dist(mat))
    return(as.dist(mat))
    #return(mat)
  }
  else {
    print('no')
    print(centers)
    print(df)
    
    if (ncol(a_df) != ncol(centers)) 
      stop(sQuote("x"), " and ", sQuote("centers"), " must have the same number of columns")
    
    mat <- matrix(, nrow = nrow(a_df), ncol = nrow(centers))
    for (i in 1:nrow(a_df)){
      for (j in 1:nrow(centers)){
        mat[i,j] <- a_custom_dist(df, centers[j,])
      }
    }
    print(mat)
    return(mat)
  }
}

a_dist_make(df)

dist(df)

image(kcca(df,2))
# to see the final centroid positions 
parameters(kcca(df,2))
# to see clusters to which original data were assigned to
clusters(kcca(df,2))

kcca(df,2,family=kccaFamily(dist=a_dist_make))
kcca(df,2,family=kccaFamily(dist=a_dist_make,cent="mean"))
# kcca(df,2,family=kccaFamily(dist=a_custom_dist,cent="mean"))

distEuclidean

