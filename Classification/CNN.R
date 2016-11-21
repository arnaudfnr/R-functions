
# Import of the distance functions, weigthing function
source("distances.R")
source("weighting_functions.R")
source("KNN.R")


detectOut <- function(data, k, dist = euclidean, weighting = counting){
  
  outliers <- NULL
  size = dim(data)
  len = size[1]
  dim = size[2]-1
  for(i in 1:len)
  {
    label = knn(data[-i,], k, data[i,1:dim], dist, weighting)
    if(label != data[1, 3])
      outliers=c(outliers, i)
  }
  return(outliers)
}

cnn <- function(x, k, dist = euclidean, weighting = counting)
{
  size = dim(x)
  len = size[1]
  dim = size[2]-1
  TS = x #trained Set
  labels <- unique(x[1, dim+1])
  nLabel <- length(unique(labels))
                   
  # Step 1 : eliminate outliers
  outliers <- detectOut(x[, 1:dim], labels, k, dist, weighting)
  TS = TS[-outliers[outliers != 0], ]
  len = length(TS[,1])
  
  # initialisation of prototypes : one random point of each label.
  prototypes = as.data.frame(matrix(0, nLabel, dim+1), optional = TRUE, stringsAsFactors = FALSE)
  for(i in 1:nLabel)
  {
    classe = x[ x[,dim+1] == labels[i] , ]
    randomPt = sample(1:length(classe),1)
    prototypes[i,] = classe[randomPt, ]
  }
  dimnames(prototypes)[[2]] = names(x)
  
  # Filling the prototype set
  testSet = TS
  lenT = len
  protofound = TRUE
  while(protofound == TRUE)
  {
    protofound = FALSE
    points2rm <- 0
    for(i in 1:lenT)
    {
      label = knn(prototypes, 1, testSet[i,1:dim], dist, weighting)
      if(label != testSet[i, dim+1])
      {
        protofound = TRUE
        newProto = as.vector(testSet[i,])
        prototypes = rbind(prototypes, newProto)
        points2rm = c(points2rm, i)
      }
    }
    testSet = testSet[-points2rm[points2rm != 0],]
    lenT = length(testSet)
  }
  
  
  return(prototypes)
  
}
