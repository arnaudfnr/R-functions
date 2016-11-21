# Import of the distance functions, weigthing function
source("distances.R")
source("weighting_functions.R")
source("validation_functions.R")


knn <- function(refSet, k=5, set, dist = euclidean, weighting = wknn){
  
  sizeSet = dim(set)
  len = sizeSet[1]
  dim = sizeSet[2]
  sizeRefSet = dim(refSet)
  lenRef = sizeRefSet[1]
  dimRef = sizeRefSet[2]
  if((dimRef-1) != dim)
  {
    warning("Invalid arguments !")
    warning("The dimensions of the set to be classified and the reference set are different !")
    break
  } 
  
  LBLtable = as.data.frame(matrix("", len, k), stringsAsFactors = FALSE)
  
  # NNtable contains values of the distances between each points and its k nearest 
  # neighbours
  NNtable = as.data.frame(matrix(Inf, len, k)) 
  

  #array with the final labels
  labels <- rep("", len) 
  # This loop will find the nearest neighbors of the set to be classified.
  
  if(k == 1)
    for(i in 1:len)
    {
      min = Inf
      label = ""
      for (j in 1:lenRef)
      {
        classJ = refSet[refSet[,dim+1] == refSet[j,dim+1],1:dim]
        d = dist(rbind(set[i,], refSet[j, 1:dim]), rbind(classJ, set[i,]))
        if(d < min)
        {
          min = d
          label = refSet[j, dim+1]
        }
        
      }
      NNtable[i, 1] = min
      LBLtable[i, 1] = label
    }
  else
    for(i in 1:len)
    {
      max = Inf
      for(j in 1:lenRef)
      {
        classJ = refSet[refSet[,dim+1] == refSet[j,dim+1],1:dim]
        d = dist(rbind(set[i,], refSet[j, 1:dim ]), rbind(classJ, set[i,]))
        if(d < max)
        {
          posMax = max(which(NNtable[i,] == max, arr.ind = TRUE))
          NNtable[i, posMax] = d
          LBLtable[i, posMax] = refSet[j, dim+1]
          max = max(NNtable[i,])
        } 
      }
    }

  if(k == 1)
    labels = LBLtable
  else
  {
    for(i in 1:len)
    {
      isUnique = TRUE
        for(j in 1:(k-1))
          for(l in (j+1):k)
            if(LBLtable[i,j] != LBLtable[i,l])
              isUnique = FALSE
            
      if(isUnique)
        labels[i] = LBLtable[i,1]
      else
        labels[i] = weighting((LBLtable[i,]),(NNtable[i,]))
    }
  }
    
  return(labels)
}


   