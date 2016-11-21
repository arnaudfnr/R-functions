euclidean <- function (x, M = 0) {
  return(sqrt(sum((x[1,]-x[2, ])^2)))
}

manhattan<- function(x, M = 0){ 
  return(sum(abs(x[1,]-x[2, ])))
}

canberra <- function(x, M=0){
  return(sum(abs(x[1,]-x[2,])/(abs(x[1,])+abs(x[2,]))))
}

chebyshev <- function(x, M = 0){
  return(min(abs(x[1,]-x[2, ])))
}


mahalonobis<-function(x, M){
  library(MASS)
  I = as.matrix(cov(M))
  diff = as.vector(x[1,] - x[2,])
  maha = as.numeric(diff) %*% ginv(I) %*% t(diff)
  return(sqrt(maha))
}


cosine <- function(x, M=0)
{
  x = as.matrix(x)
  Nx1 = sqrt(x[1,]%*%x[1,])
  Nx2 = sqrt(x[2,]%*%x[2,])
  Sum = sum(x[1,]*x[2,])
  return(1 - (Sum / (Nx1*Nx2)))
}