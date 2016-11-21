genEllipsArc <- function(ctr=c(0,0), a=1, b=1, sizeT = 250, nbout = 5, coeffL = 0.3)
{
  size <- sizeT - 30
  listX = mvrnorm(5*size, (ctr[1]+a*(coeffL-1)), abs(ctr[1]-ctr[1]/2), empirical = TRUE)
  listX = listX[listX>(ctr[1]-a)]
  listX = listX[listX<ctr[1]+coeffL*a]
  listX = listX[1:size]
  eps <- rnorm(3*size, 0, b/6)
  eps = eps[eps>0]
  eps = eps[1:(size-nbout)]
  out = rnorm(nbout, max(a,b)/2, 1)
  eps = c(eps, out)
  set = matrix(0, size, 2)
  for(i in 1:size)
  {
    x = listX[i]
    ymin = b * sqrt(1-((x-ctr[1])^2/a^2)) - eps[i]
    ymax = b * sqrt(1-((x-ctr[1])^2/a^2)) + eps[i]
    sign = 1
    if(x<=ctr[1]+a*coeffL)
      sign = sample(c(-1,1), 1)
    
    ymin = b * sign * sqrt(1-((x-ctr[1])^2/a^2)) - eps[i]
    ymax = b * sign * sqrt(1-((x-ctr[1])^2/a^2)) + eps[i]
    y = (runif(1, ymin, ymax) + ctr[2])
    set[i,] = c(x, y)
  }  
  # addition of points in the lefet corner of the ellipsoide
  x = rnorm(30, (ctr[1]-a), a/30)
  y = rnorm(30, ctr[2], b/4)
  set = as.data.frame(rbind(set, cbind(x,y)))
  
  return(set)
}