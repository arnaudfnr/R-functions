training<-function(Set, n, kValues, dist = euclidean, weight = dwknn){
  source("distances.R")
  source("KNN.R")
  source("weighting_functions.R")
  
  nK = length(kValues)
  errK = rep(0, nK)
  minErrK = rep(Inf, nK)
  posK = 0
  size = dim(Set)
  len = size[1]
  dim = size[2]-1
  t = as.data.frame(table(Set[,dim+1]))
  Labels = t[,1]
  nLabels = length(Labels)
  freqLabels = t[,2]
  testPoints = NULL
  
  # Creation of the n samples which will be used as the validation sets
  for(i in 1:n)
  {
    samp = NULL
    for(j in 1:nLabels)
      samp = c(samp, sample(which(Set[, dim+1] == Labels[j]), freqLabels[j]/4))
  
    testPoints = rbind(testPoints, samp)
  }
    
  testPtsNb = length(testPoints[1,])

  for(i in 1:nK)
  {
    k = kValues[i]
    minErr = Inf
    meanErr = 0
    for(j in 1:n)
    {
      print(paste("Test du ", j, "eme sample", sep=""))
      testPts = testPoints[j,]
      trainingSet = Set[-testPts, ]
      testingSet = Set[testPts,]
      print("debut knn...")
      label = knn(Set[-testPts, ], k, Set[testPts, 1:dim], dist, weight)
      print("fin knn.")
      l = 1
      err = 0
      for(p in testPts)
      {
        if(Set[p, dim+1] != label[l])
          err = err + 1
        l = l+1
      }
      err = err / testPtsNb
      if(err < minErr)
      {
        minErr = err
        posK = i
      }
      else if(err == minErr)
        posK = c(posK, i)
    
    meanErr = meanErr + err
    }
    errK[i] = meanErr/n
    minErrK[i] = minErr
  }
  results = data.frame(K = kValues, Min = minErrK, Mean = errK)
  print(results)
  return(results)
}