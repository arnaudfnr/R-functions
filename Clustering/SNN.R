source("C:/Users/Arnaud/Documents/Etudes/Semestre_Estonie_A16/Courses/Data_Mining_and_Network_Analysis/Practice/distances.R")

# Shared Nearest Neighbor Classification algorithm

genSimMatrix<- function(Set, len, dist)
{
  SM = as.data.frame(matrix(0, nrow = len, ncol = len), stringAsFactors = FALSE)
  for(i in 1:(len-1))
    for(j in (i+1):len)
      if(i != j)
      {
        d <- dist(rbind(Set[i,], Set[j,]), Set)
        SM[i,j] = d
        SM[j,i] = d
      }
  assign("SMRedSet1", SM, envir=.GlobalEnv)
  return(SM)
}

sparsify<-function(SM, len, k)
{
  spM <- as.data.frame(matrix(0, len, k))
  knn <- as.data.frame(matrix(0, len, k))
  dens <- rep(0, len)
  for(i in 1:len)
  {
    dists <- SM[i,]
    closests <- order(dists)
    closests <- dists[closests[2:(k+1)]] #Nearest is the point itself so it's not kept
    knn[i, ] <- which(dists %in% closests)
    spM[i,] <- closests
    dens[i] <- sum(closests)
  }
  return(list(sparsed = spM, nn = knn, dens = dens))
}

getSNN<-function(knn, len, k)
{
  SNN = as.data.frame(matrix(0, len, k))
  for(i in 1:len)
  {
    nni <- knn[i,]
    posP <- 0
    for(p in nni)
    {
      posP <- posP + 1
      nnp <- knn[p, ]
      shared <- length(intersect(nni, nnp))
      SNN[i,posP] <- shared
      SNN[p, which(nnp == i)] <- shared
    }
  }
  return(SNN)
}

classifyCores<-function(snn, knn, cores, clusters, eps, spM)
{
  len <- length(cores)
  nC <- 0
  isVisited <- rep(FALSE, len)
  for(p in 1:len)
  {
    if(isVisited[p]==FALSE)
    {
      isVisited[p] <- TRUE
      pos <- cores[p]
      nnshared <- snn[pos,]
      nnp <- which(nnshared >= eps)
      posnnp <- as.vector(knn[pos, nnp])
      clust <- unique(clusters[which(clusters[as.integer(posnnp),]$label != ""),1])
      if(!length(clust))
      {
        nC <- nC +1
        clusters[p,]$label <- nC
        clusters[as.integer(posnnp),]$label <- nC
        isVisited[as.integer(posnnp)] <- TRUE
      }  
      else if(length(clust) == 1)
        clusters[p,]$label <- clust
      else
      {
        nearests <- which(nnshared == max(nnshared))
        if(length(nearests)==1)
          clusters[p,]$label <- clusters[knn[pos, nearests],]$label
        else
        {
          similarities <- spM[pos, nearests]
          closest <- which(similarities==max(similarities))[1]
          clusters[p,]$label <- clusters[knn[pos, nearests[closest]],]$label
        }
      }
    }
  }
  return(clusters)
}

detectNoise <-function(cores, knn, snn, eps, clusters)
{
  radiusList <- NULL #Will contain all the point's indexes which are in the 
                     #radius of at least one core point.
  for(c in cores)
    radiusList <- rbind(radiusList, knn[which(snn[c,]>=eps),])
  
  radiusList <- unique(radiusList)
  print(radiusList)
  print(length(radiusList))
  if(!is.null(clusters[-(radiusList),]))
  {
    clusters[-(radiusList),]$label <- "Noise"
    clusters[-(radiusList),]$type <- "Noise"
  }
  
  return(clusters)
}

SNN<-function(Set, k, dist, minDens=0.7*k, eps=0.3*k, SM = NULL)
{
  size = dim(Set)
  len = size[1]
  dim = size[2]
  clusters <- data.frame(label = rep("",len), type = rep("",len), stringsAsFactors = FALSE)
  
  print(paste("Threshold Density:", eps))
  print(paste("Core Density:", minDens))
  #computation of similarity Matrix
  print("Computation of similarity matrix....")
    if(is.null(SM))
      simMatrix = genSimMatrix(Set, len, dist)
    else
      simMatrix <- SM
  print("Similarity matrix computed.")
  # Find K nearest neighbors with their similarity using Similarity matrix, 
  # as well a s the density, i.e, the sum of knn similarities, for each point.
  print("Sparsification of the SimMatrix.")
  sparList = sparsify(simMatrix, len , k)
  sparsedM <- sparList$sparsed
  knn <- sparList$nn

  print("Sparsification done.")
  print("K nearest neighbors completed.")
  
  # Calculation of the links between each point.
  print("Calculation of shared nn....")
  snn <- getSNN(knn, len,k)
  print("Shared NN computed.")
  
  # Calulate density of each point
  dens <- rep(0, len)
  for(i in 1:len)
    dens[i] <- length(which(snn[i,] >= eps))
  
  # Find core points, i.e having a density lower than threshold hyperparameter
  print("classifying Cores....")
  cores <- which(dens >= minDens)
  clusters[cores,]$type <- "Core Point"
  clusters <- as.data.frame(classifyCores(snn, knn, cores, clusters, eps, sparsedM), stringsAsFactors = FALSE)
  print("Cores classified")
  # Detection of noisy points
  print("Detection of Noise...")
  clusters <- as.data.frame(detectNoise(cores, knn, snn, eps, clusters), stringsAsFactors = FALSE)
  
  print("Clustering remaining points...")
  remainingPoints <- which(clusters$label == "")
  for(p in remainingPoints)
    cluster[p,]$label <- cluster[which(simMatrix[p,] == max(simMatrix[p,cores])),]$label
  
  print("Finished !")
  return(clusters) 
}