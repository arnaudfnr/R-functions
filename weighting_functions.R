wknn <- function(labels, nns)
{
  min = min(nns)
  max = max(nns)
  len = length(nns)
  w <- data.frame(rep(0, len))
  for(i in 1:len)
    w[i,1] = (max-nns[i])/(max-min)

  labels = as.character(labels)
  labelist <- labels[1]
  
  for(i in 2:len)
  {
    if(length(which(labelist == labels[i], arr.ind = TRUE)) == 0)
      labelist = cbind(labelist, labels[i])
  }
  nLabel = length(labelist)
  wlabels = data.frame(weight = w[,1], label = labels, stringsAsFactors = FALSE)
  
  wsum = rep(0,nLabel)
  for(i in 1:nLabel)
    wsum[i] = sum(wlabels[wlabels[,2]==labelist[i],1] )
  
  posLabel = which(wsum == max(wsum), arr.ind = TRUE)
  
  return(labelist[posLabel])
}

dwknn <- function(labels, nns)
{
  min = min(nns)
  max = max(nns)
  len = length(nns)
  w <- data.frame( w= rep(0, len))
  for(i in 1:len)
  {
    if(nns[i] != min)
      w[i,1] = ((max-nns[i])/(max-min)) * (max+min)/(max+nns[i])
    else
      w[i,1] = 1
  }
  
  labels = as.character(labels)
  labelist <- labels[1]
  
  for(i in 2:len)
  {
    if(length(which(labelist == labels[i], arr.ind = TRUE)) == 0)
      labelist = cbind(labelist, labels[i])
  }
  nLabel = length(labelist)
  wlabels = data.frame(weight = w[,1], label = labels, stringsAsFactors = FALSE)
  wsum = rep(0,nLabel)
  for(i in 1:nLabel)
    wsum[i] = sum(wlabels[wlabels[,2]==labelist[i],1] )
  
  posLabel = which(wsum == max(wsum), arr.ind = TRUE)
  return(labelist[posLabel])
}

expwknn <- function(labels, nns)
{
  len = length(nns)
  w <- data.frame(w = rep(0, len))
  for(i in 1:len)
    w[i,1] = exp(-nns[i])
  
  labels = as.character(labels)
  labelist <- labels[1]
  
  for(i in 2:len)
  {
    if(length(which(labelist == labels[i], arr.ind = TRUE)) == 0)
      labelist = cbind(labelist, labels[i])
  }
  nLabel = length(labelist)
  w = w / sum(w[,1])
  wlabels = data.frame(weight = w[,1], label = labels, stringsAsFactors = FALSE)
  wsum = rep(0,nLabel)
  for(i in 1:nLabel)
    wsum[i] = sum(wlabels[wlabels[,2]==labelist[i],1] )
  
  posLabel = which(wsum == max(wsum), arr.ind = TRUE)
  return(labelist[posLabel])
}

counting<-function(labels, nns)
{
  len = length(labels)
  countLabel = data.frame( m = rep("", len), n = rep(0, len), stringsAsFactors = FALSE)
  for(i in 1:length(labels))
  {
    pos = which(labels[i] == countLabel[,1], arr.ind = TRUE)
    if(length(pos) == 0)
      countLabel[which(countLabel$m == "")[1], ] = c(labels[i], 1)
    else
      countLabel[pos, 2] = as.integer(countLabel[pos, 2] + 1)
  }
  posMax = which(countLabel[,2] == max(as.integer(countLabel[, 2])))[1]
  return(countLabel[posMax, 1])
}
