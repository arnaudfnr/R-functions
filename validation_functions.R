source("distances.R")
InOutDistRatio <- function(Set, labelSet, k, dist){

  Sintra <- rep(0,k)
  Sinter <- rep(0,k)
  minInter <- rep(0,k)
  maxIntra <- rep(0,k)
  size = dim(Set)
  len = size[1]
  dim = size[2]
  lbldSet = cbind(Set, labelSet)
  
  labels = rep("", k)
  labels[1] = labelSet[1]
  filled = 1
  ind = 2
  while(filled < k)
  {
    if(length(which(labels == labelSet[ind])) == 0)
    {
      filled = filled + 1
      labels[filled] = labelSet[ind]
    }
    ind = ind + 1
    if(ind>len)
      warning("Le nombre de label donné en argument ne correspond pas à celui trouvé !", immediate. = TRUE)
  }
  
  
  for(i in 1:k)
  {
    count = 0
    cluster = lbldSet[which(labelSet==labels[i], arr.ind = TRUE), 1:dim]
    lenC = length(cluster[, 1])
    for (j in 1:(lenC-1))
      for(h in (j+1):lenC)
      {
        count = count +1
        Sintra[i] = Sintra[i] + dist(rbind(cluster[j,], cluster[h,]), cluster)
      }
    Sintra[i] = Sintra[i] / count
  }
  
  for(i in 1:k)
  {
    cluster = lbldSet[which(labelSet==labels[i], arr.ind = TRUE), 1:dim]
    otherClusters = lbldSet[which(labelSet!=labels[i], arr.ind = TRUE), 1:dim]
    lenC = length(cluster[,1])
    lenOC = length(otherClusters[,1])
    for(m in 1:lenC)
      for(n in 1:lenOC)
        Sinter[i] = Sinter[i] + dist(rbind(cluster[m,], otherClusters[n,]), Set)
    
    Sinter[i] = Sinter[i] / (lenC*lenOC) 
  }
  
  return(Sintra/Sinter)
}

DunnInd <- function(Set, labels, k, dist){
  minInter <- rep(0,k)
  maxIntra <- rep(0,k)
  size = dim(Set)
  len = size[1]
  dim = size[2]
  lblSet = cbind(Set, labels)
  
  for(i in 1:k)
  {
    cluster = lblSet[which(labels==labels[i],arr.ind = TRUE), 1:dim]
    lenC = length(cluster[, 1])
    max = 0
    for (j in 1:(lenC-1))
      for(h in (j+1):lenC)
      {
        d = dist(rbind(cluster[j,], cluster[h,]), cluster)
        if (d > max)
          max = d
      }
    maxIntra[i] = max
  }
  
  for(i in 1:k)
  {
    cluster = lblSet[which(labels==labels[i], arr.ind = TRUE), 1:dim]
    otherClusters = lblSet[which(labels!=labels[i], arr.ind = TRUE),]
    lenOC = length(otherClusters[,1])
    lenC = length(cluster[,1])
    min = Inf
    for(m in 1:lenC)
      for(n in 1:lenOC)
      {
        d = dist(rbind(cluster[m,], otherClusters[n,1:dim]), 
                 rbind(Set[m, ], Set[which(labels == otherClusters[n, dim+1]),]))
        if(d < min)
          min = d
      }
    
    minInter[i] = min 
  }
  
  return(minInter/maxIntra)
}

DBInd<- function(Set, labelSet, k, dist){
  db = 0
  size = dim(Set)
  len = size[1]
  dim = size[2]
  lblSet = cbind(Set, labelSet)
  centroids = as.data.frame(matrix(1, k, dim))
  names(centroids) = names(Set)
  sizeC = rep(0,k)
  avgDists = rep(0,k)
  labels = rep("", k)
  labels[1] = labelSet[1]
  filled = 1
  ind = 2
  while(filled < k)
  {
    if(length(which(labels == labelSet[ind])) == 0)
    {
      filled = filled + 1
      labels[filled] = labelSet[ind]
    }
    ind = ind + 1
    if(ind>len)
      warning("Le nombre de label donné en argument ne correspond pas à celui trouvé !", immediate. = TRUE)
  }
  
  for(i in 1:len)
    centroids[which(labels == labelSet[i]), ] = centroids[which(labels == labelSet[i]),] + Set[i,]
  
  for(j in 1:k)
  {
    cluster = lblSet[which(labelSet == labels[j], arr.ind = TRUE), 1:dim]
    sizeC[j] = length(cluster[,1])
    centroids[j,] = centroids[j,] / sizeC[j]
  }

  for(i in 1:len)
  {
    nC = which(labels == labelSet[i], arr.ind = TRUE)
    cluster = lblSet[which(labelSet == labels[nC], arr.ind = TRUE), 1:dim]
    d = dist(rbind(Set[i,], centroids[nC,]), cluster)
    avgDists[nC] = avgDists[nC] + d
  }
    
  for (j in 1:k)
    avgDists[j] = avgDists[j] / sizeC[j]
  
  coeffs = as.data.frame(matrix(0, nrow = k, ncol = k))
  for(i in 1:(k-1))
    for(j in (i+1):k)
    {
      if(i!=j)
      {
        d = dist(centroids[c(i,j),], centroids)
        coeffs[i,j] = (avgDists[i] + avgDists[j]) / d
        coeffs[j,i] = coeffs[i,j]
      }
    }
  
  for(j in 1:k)
    db = db + max(coeffs[j,])

  return(db/k)
}

Silhouette <- function(Set, labelSet, k, dist)
{
  size = dim(Set)
  len = size[1]
  dim = size[2]
  Silhouettes = rep(0,len)
  lblSet = cbind(Set, labelSet)
  sizeC<- rep(0,k)
  avgDists = data.frame(rep(0,len), rep(0,len))
  interMeanDist = as.data.frame(matrix(0, len, k))
  labels = rep("", k)
  labels[1] = labelSet[1]
  filled = 1
  ind = 2
  while(filled < k)
  {
    if(length(which(labels == labelSet[ind])) == 0)
    {
      filled = filled + 1
      labels[filled] = labelSet[ind]
    }
    ind = ind + 1
    if(ind>len)
      warning("Le nombre de label donné en argument ne correspond pas à celui trouvé !", immediate. = TRUE)
  }
  
  # calculation of all clusters size
  for(j in 1:k)
    sizeC[j] = length(lblSet[labelSet==labels[j], 1])
  
  #calculation of avg distance between each point and the other points in the same 
  #clusters (avg[i, 1]) and between each point and the points in each other clusters
  #(interMeanDist[i,]). At the end avg[i,2] contains avg distance of the point and 
  #the nearest cluster so we can calculate the silhouette of the point.
  for(i in 1:(len-1))
  {
    labelI = labelSet[i]
    nLabelI = which(labels == labelI, arr.ind = TRUE)
    for(j in (i+1):len)
    {
      labelJ = labelSet[j]
      nLabelJ = which(labels == labelJ, arr.ind = TRUE)
      if(labelI == labelJ)
      {
        d = dist(rbind(Set[i,], Set[j,]), rbind(Set[j,], lblSet[labelSet == labelI,1:dim]))
        avgDists[i,1] = avgDists[i,1] + d
        avgDists[j,1] = avgDists[j,1] + d
      }
      else
      {
        d = dist(rbind(Set[i,], Set[j,]), Set)
        interMeanDist[i, nLabelJ] = interMeanDist[i, nLabelJ] + d
        interMeanDist[j, nLabelI] = interMeanDist[j, nLabelI] + d
      }
    }
    for(c in 1:k)
      interMeanDist[i, c] = interMeanDist[i, c] / sizeC[c]
    
    avgDists[i,1] = avgDists[i,1] / sizeC[nLabelI]
    avgDists[i,2] = min(interMeanDist[i, interMeanDist[i,] != 0])
    
    Silhouettes[i] = (avgDists[i,2] - avgDists[i,1])/max(avgDists[i,1], avgDists[i,2])
  }
  # calculation of average for the last point because the for stops at len-1
  for(c in 1:k)
    interMeanDist[len, c] = interMeanDist[len, c] / sizeC[c]
  
  
  avgDists[len,1] = avgDists[len,1] / sizeC[which(labels == labelSet[len], arr.ind = TRUE)]
  avgDists[len,2] = min(interMeanDist[len, interMeanDist[len,] != 0])
  Silhouettes[len] = (avgDists[len,2] - avgDists[len,1])/max(avgDists[len,1], avgDists[len,2])
  
  return(Silhouettes)
}
