### DOESN'T WORK PROPERLY ###

source("distances.R")

expand<-function(Set, p, cluster, pNeighbors, isVisit, nC, dist, eps, minD)
{
  len <- dim(Set)[1]
  cluster[p] = nC
  for( n in pNeighbors )
  {
    if( isVisit[n] == FALSE )
    {
      isVisit[n] <- TRUE
      nNeighbors <- NULL
      for(x in 1:len)
        if( dist(rbind(Set[n,],Set[x,]), Set) < eps )
          nNeighbors <- c(nNeighbors, x)
      if( length(nNeighbors) >= minD )
      {
        pNeighbors <- union(pNeighbors, nNeighbors)
        cluster[pNeighbors] = nC
      }

    }
    if( cluster[n] == 0 )
      cluster[n] <- nC
  }
  return(cluster)
} 

DbScan<- function(Set, eps, minD, dist)
{
  size <- dim(Set)
  len <- size[1]
  dim <- size[2]
  nC <- 0
  cluster <- rep("0", len)
  isVisit <- rep(FALSE, len)
  for(p in 1:len)
  {
    if(isVisit[p] == FALSE)
    {
      isVisit[p] = TRUE
      neighbors <-  NULL
      for(n in 1:len)
      {
        d= dist(rbind(Set[n,], Set[p,]), Set)
        if(d<eps)
          neighbors = c(neighbors, n)
      }
      if(length(neighbors)<minD)
        cluster[p] <- "NOISE"
      else
      {
        nC <- nC + 1
        cluster <- expand(Set, p, cluster, neighbors, isVisit, nC, dist, eps, minD)
        
      }
    }
  }
  
  return(cluster)
}
