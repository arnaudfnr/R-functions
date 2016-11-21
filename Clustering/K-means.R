
source("C:/Users/Arnaud/Documents/Etudes/Semestre_Estonie_A16/Courses/Data_Mining_and_Network_Analysis/Practice/distances.R")

kMeans<-function(Set, k, dist=euclidean)
{
  # Obtention of numers of rows and columns using "dim" function.
  size = dim(Set)
  len = size[1]
  dim = size[2]
  CC1 <- rep(0, len) # Closest centroid for each point in the set
  CC2 <- rep(0, len) # Second closest centroid for each point in the set
  AN1 <- rep(0,k) # length Cluser / length Cluster-1
  AN2 <- rep(0,k) # length Cluster / length Cluster+1
  ITRAN <- rep(1, k)
  NCP <- rep(-1, k)
  sizeC <- rep(0, k) # number of points in each cluster
  Live <- rep(0, k) # Live Set
  D <- rep(0, len) # Distance between each point and the centroid of the cluster it belongs to.
  
  while(min(sizeC) == 0)
  {
    # Creation of centroids table containing a row for cluster number (maybe not useful 
    # because equal to index) and postion of the cluster in all dimensialities.
    centroids = as.data.frame(matrix(nrow = k, ncol = dim))
    
    # The position of the centroids in each dimensionality are initialized randomly.
    for(j in 1:dim)
      centroids[,j] = runif(k, min(Set[,j]), max(Set[,j])) 
    
    # We attach each set point to its closest centroid and second closest centroid
    dimnames(centroids)[[2]] = dimnames(Set)[[2]]
    for (i in 1:len)
    {
      distCtrd = rep(0, k)
      for(j in 1:k)
        distCtrd[j] = dist(rbind(Set[i,], centroids[j,]), Set)
      
      pos = which(distCtrd == min(distCtrd), arr.ind = TRUE)
      CC1[i] = pos[1]
      D[i] = distCtrd[pos]
      
      distCtrd2 =  distCtrd[distCtrd != min(distCtrd)]
      min = which(distCtrd == min(distCtrd2),arr.ind = TRUE)
      CC2[i] = min[1]
    }
    
    for(i in 1:len)
      centroids[CC1[i], ] = centroids[CC1[i],] + Set[i,]
    
    for(j in 1:k)
    {
      sizeC[j] = length(CC1[CC1 == j])
      AA = sizeC[j]
      centroids[j,] = centroids[j,] / AA
      AN2[j] = AA / (AA+1)
      AN1[j] = 10^5
      if(AA > 1) 
        AN1[j] = AA / (AA-1)
    }
  }
  
  Ind = 0
  for(ij in 1:15)
  {
    for(i in 1:k)
      if(ITRAN[i] == 1) 
        Live[i] = len + 1
      
    for(i in 1:len)
    {
      Ind = Ind + 1
      C1 = CC1[i]
      C2 = CC2[i]
      if(sizeC[C1] > 1)
      {
        if(NCP[C1] != 0)
          D[i]= dist(rbind(Set[i,], centroids[C1,]), rbind(Set[which(CC1 == C1),], Set[i,]))
        
        Rmin = AN2[C2]*dist(rbind(Set[i,], centroids[C2,]), rbind(Set[i,], Set[which(CC1 == C2),]))
        for(j in 1:k)
          if(i<Live[C1] || i<Live[j] && C1!=j && C2!=j )
          {
            RC = AN2[j]*dist(rbind(Set[i,], centroids[j, ]), rbind(Set[i,], Set[which(CC1 == j),]))
            if(RC < Rmin)
            {
              Rmin = RC
              C2 = j
            }
          }
        R = AN1[C1]*D[i]
        if(Rmin > R)
          CC2[i] = C2
        else
        {
          Ind = 0
          Live[C1] = len + i
          Live[C2] = len + i
          
          NCP[C1] = i
          NCP[C2] = i
          
          sizeC1 = sizeC[C1]
          size1 = sizeC1 - 1
          sizeC2 = sizeC[C2]
          size2 = sizeC2 + 1
          centroids[C1, ] = (centroids[C1, ] * sizeC1 - Set[i,]) / size1
          centroids[C2, ] = (centroids[C2, ] * sizeC2 + Set[i,]) / size2
          sizeC[C1] = size1
          sizeC[C2] = size2
          
          AN2[C1] = size1 / sizeC1
          AN1[C1] = 10^5
          if(size1>1)
            AN1[C1] = size1 / (size1-1)
          
          AN1[C2] = size2 / sizeC2
          AN2[C2] = size2 / (size2+1)
          
          CC1[i] = C2
          CC2[i] = C1
        }
        if(Ind < len)
          for(j in 1:k)
          {
            ITRAN[j] = 0
            Live[j] = Live[j] - len
          }  
        }
      }
      if(Ind != len)
      {
        count = 0
        step = 0
        for(i in 1:len)
        {
          count = count + 1
          C1 = CC1[i]
          C2 = CC2[i]
          
          if(sizeC[C1] > 1)
          {
            if(step <= sizeC[C1])
              D[i] = dist(rbind(Set[i, ], centroids[C1,]), rbind(Set[i,], Set[which(CC1 == C1, arr.ind =  TRUE),]))
            
            if(step < sizeC[C1] || step < sizeC[C2])
            {
              R2 = AN2[C2]*dist(rbind(Set[i,], centroids[C2,]), rbind(Set[i,], Set[which(CC1 == C2, arr.ind = TRUE),]))
              R = AN1[C1] * D[i]
              if(R2 < R)
              {
                count = 0
                Ind = 0
                
                ITRAN[C1] = 1
                ITRAN[C2] = 1
                
                NCP[C1] = i + len
                NCP[C2] = i + len
                
                sizeC1 = sizeC[C1]
                size1 = sizeC1 - 1
                sizeC2 = sizeC[C2]
                size2 = sizeC2 + 1
                centroids[C1, ] = (centroids[C1, ] * sizeC1 - Set[i,]) / size1
                centroids[C2, ] = (centroids[C2, ] * sizeC2 + Set[i,]) / size2
                sizeC[C1] = size1
                sizeC[C2] = size2
                
                AN2[C1] = size1 / sizeC1
                AN1[C1] = 10^5
                if(size1>1)
                  AN1[C1] = size1 / (size1-1)
                
                AN1[C2] = size2 / sizeC2
                AN2[C2] = size2 / (size2+1)
                
                CC1[i] = C2
                CC2[i] = C1
              }
            }
          }
        }
      }
      if(k>2)
        NCP = rep(0, k)
  }
  
  WSS = rep(0,k)
  centroids = as.data.frame(matrix(0, k, dim))
  for(i in 1:len)
    centroids[CC1[i], ] = centroids[CC1[i],] + Set[i,]
  
  for(j in 1:k)
    centroids[j,] = centroids[j,] / sizeC[j]
  
  dimnames(centroids)[[2]] = dimnames(Set)[[2]]
  for(i in 1:len)
    WSS[CC1[i]] = WSS[CC1[i]] + dist(rbind(Set[i,], centroids[CC1[i],]), Set[which(CC1 == CC1[i]),])

  return(CC1)
}

