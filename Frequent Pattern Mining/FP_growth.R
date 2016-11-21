createTree <- function(TrDB, indParent, nameNodes, nameFIHT, env = parent.frame())
{
  get(nameNodes, env) ->> Nodes
  get(nameFIHT, env) ->> headTab
  LevelNodes <- unique(TrDB[,1])
  nbNodes <- length(LevelNodes)
  i <- 0
  for(n in LevelNodes)
  {
    pos <- which(TrDB[,1] == n)
    count <- length(pos)
    # Add the new item to the item-tree.
    Nodes <- rbind.data.frame(Nodes, data.frame(itemID = n, support = count, posParent = indParent, posSucc = 0, aux = 0, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
    ind <- length(Nodes[,1])
    
    # Update header table and item-nodes
    header <- which(headTab$item == n)
    if(length(header) == 0)
      n = 0
    else if(headTab[header, ]$PosFirstFItemNode == 0)
      headTab[header,]$PosFirstFItemNode = ind
    else
    {
      nextI <- headTab[header,]$PosFirstFItemNode
      while(nextI)
      {
        parent <- nextI
        nextI <- Nodes[nextI,]$posSucc
      }
      
      Nodes[parent, ]$posSucc <- ind
    }
    
    # next layer of the (sub)section
    subTree <- data.frame(TrDB[pos,-1], stringsAsFactors = FALSE)
    print(subTree)
    subTree <- subTree[which(is.na(subTree[,1])==FALSE), ]
    
    assign(nameNodes, Nodes, env)
    assign(nameFIHT, headTab, env)
    
    # recursive call
    createTree(subTree, ind, nameNodes, nameFIHT, env)
    
    Nodes <- get(nameNodes, env)
    headTab <- get(nameFIHT, env)
    i <- i + 1
  }
  assign(nameNodes, Nodes, env)
  assign(nameFIHT, headTab, env)
}

mine<-function(tree, fHead, minSup)
{
  freqPatterns <- NULL
  len = dim(fHead)[1]
  if(len == 1)
  {
    if(fHead[1,2] > minSup)
      freqPatterns <- data.frame(support = fHead[1,2], pattern = fHead[1,1])
  }
  else if(len>1)
  {
    for(i in len:1)
    {
      ai <- fHead[i,]
      # Construct the Conditional Tree
      Nodes <- data.frame(itemID = ai$item, support = ai$support, posParent = 1, posSucc = 0, aux = ai$PosFirstFItemNode, stringsAsFactors = FALSE)
      FIHT <- data.frame(item = NULL, support = NULL, PosFirstFItemNode = NULL)
      nodeH <- tree[ai$PosFirstFItemNode,]
      crossed = NULL
      while(length(nodeH$posSucc) )
      {
        nodeV <- nodeH
        indHead <- 0
        support <- nodeV$support
        while(nodeV$posParent != 1)
        {
          posP <- nodeV$posParent
          nodeV <- tree[posP, ]
          if(posP %in% crossed)
          {
            ind <- intersect(which(Nodes$aux == posP),which(Nodes$itemID == nodeV$itemID))
            Nodes[ind,]$support = Nodes[ind,]$support + support
            FIHT[FIHT$item == nodeV$itemID,]$support = FIHT[FIHT$item == nodeV$itemID,]$support + 1
          }
          else
          {
            crossed <- c(crossed, posP)
            indHead <- indHead + 1
            
            # Add the new item to the item-tree.
            Nodes <- rbind.data.frame(Nodes, data.frame(itemID = nodeV$itemID, support = support, posParent = indHead, posSucc = 0, aux = posP, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
            ind <- length(Nodes$itemID)
            
            # Update header table
            header <- which(FIHT$item == nodeV$itemID)
            if(length(header)==0)
              FIHT <- rbind.data.frame(FIHT, data.frame(item = nodeV$itemID, support = support, PosFirstFItemNode = ind, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
            else
            {
              item <- FIHT[header,]
              item$support <- item$support + support
              FIHT[header,]$support <- item$support
              nextI <- item$PosFirstFItemNode
              while(nextI)
              {
                parent <- nextI
                nextI <- Nodes[nextI,]$posSucc
              }
              Nodes[parent, ]$posSucc <- ind
            }
          }
          
        }
        if(nodeH$posSucc != 0)
          nodeH <- tree[nodeH$posSucc,]
        else
          nodeH = NULL
      } 
      if(!(is.null(FIHT)))
      {
        freqP <- mine(Nodes, FIHT, minSup)
        if(!(is.null(freqP)))
        {
          freqP <- cbind(freqP, ai$item)
          freqP <- cbind.data.frame(support = freqP[,1], pattern = paste(freqP[,2], freqP[,3], sep=";"))
          freqPatterns <- rbind.data.frame(freqPatterns, freqP)
        }
      }
    }
  }
  return(freqPatterns)
}

fp_growth<-function(DB, minSup)
{
  freqPatterns <- NULL
  print(j)
  print("Beignning of Fp_growth...")
  
  #Table retrieves easily all different items and their frequency, here their support.
  tab = as.data.frame(table(DB[is.na(DB) == FALSE]), stringsAsFactors = FALSE)
  
  SItems = data.frame(item = tab[,1], support = tab[,2], stringsAsFactors = FALSE)
  
  FItems = SItems[SItems[,2]> minSup,]
  
  SortedFItems = FItems[order(FItems[,2], decreasing = TRUE),]
  
  # Order each transactions while removing unfrequent items in transactions
  lenDB = length(DB[,1])
  for (i in 1:lenDB)
  {
    Tr <- DB[i, is.na(DB[i,]) == FALSE]
    Tr <- SortedFItems[SortedFItems[,1] %in% Tr, 1]
    lenT<- length(Tr)
    if(lenT != 0)
    {
      DB[i, 1:lenT] = Tr
      DB[i, (lenT+1):74] = NA
    }
    else
      DB[i,] = NA
  }
  DB <- DB[is.na(DB[,1])==FALSE,]
  lenDB<- length(DB[,1])
  
  #We shuffle the database so the size of the tree doesn't depend on the initial order 
  shuffledInd <- sample(1:len,len)
  ShuffledDB <- DB[shuffledInd,]
  
  #Yhe next step is the initialization of the FP-Tree
  FIHT = data.frame(SortedFItems, PosFirstFItemNode = 0)
  NodesT = data.frame(itemID = "null", support = 0, posParent = 0, posSucc = 0, aux = 0, stringsAsFactors = FALSE)
  createTree(ShuffledDB, 1, "NodesT", "FIHT")
  print("Tree built.")
  print(date())
 
   #This step mines the FP-Tree in order to find all frequent patterns
  assign(paste("FIHT", j), FIHT, .GlobalEnv)
  assign(paste("Nodes", j), NodesT, .GlobalEnv)
  fpsamp <- mine(NodesT, FIHT, minSup)
  freqPatterns <- list(freqPatterns, fpsamp)
  
  return(freqPatterns)
}



