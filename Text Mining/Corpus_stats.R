stats_PF <- function(Corpus)
{
  ## Initialization
  len <- length(Corpus)
  
  ## Merge feature tables of all elements (one table by one table)
  PF <- Corpus[[1]]$Punctuation
  PF[,1] <- as.character(PF[,1]) # in case it is a factor
  for(i in 2:len)
  {
    # Retrieve table to merge
    temp <- Corpus[[i]]$Punctuation
    temp[,1] <- as.character(temp[,1]) # in case it is a factor
    # Equalize length of both table to merge
    len <- length(PF[,1])
    lenTmp <- length(temp[,1])
    if(len > lenTmp)
    {
      temp = data.frame(sapply(temp, function(x){
        c(x, rep(NA, len - lenTmp))
      }), stringsAsFactors = FALSE)
    }
    else
    {
      PF = data.frame(sapply(PF, function(x){
        c(x, rep(NA, lenTmp - len))
      }), stringsAsFactors = FALSE)
    }
    # Merge Tables using mapply
    PF <- data.frame(mapply(rbind, PF, temp, SYMPLIFY=FALSE), stringsAsFactors = FALSE)
    # Filter Residuals
    PF <- subset(PF, PF[,1] != "FALSE")
    PF <- subset(PF, is.na(PF[,1]) == "FALSE")
    # Cast attributes
    PF[,2] <- as.integer(PF[,2])
    PF[,3] <- as.numeric(PF[,3])
    PF[,4] <- as.numeric(PF[,4])
    PF[,5] <- as.numeric(PF[,5])
  }
  
  ## Global Feature Stats
  # Unifiy punctuation
  puncList <- unique(PF[,1])
  # Compute stats (sums, means,...)
  puncNb <- length(puncList)
  puncStats <- matrix(nrow = puncNb, ncol = 5)
  for(i in 1:puncNb)
  {
    s <- puncList[i]
    sumFreq <- sum(PF[which(PF[,1] == s),2])
    means <- colMeans(PF[which(PF[,1] == s), 3:5])
    puncStats[i,] <- c(s, sumFreq, means)
  }
  
  ## Corpus Attributes
  freq = as.integer(puncStats[,2])
  # Relative Frequence
  RelFreq = freq/sum(freq)
  stopSigns <- c(which(PF[,1]=="."),which(PF[,1]=="!"),which(PF[,1]=="?"))
  BySentFreq = freq/sum(PF[stopSigns, 2])
  # Frequences Z-Score 
  sdFreq = sd(freq)
  meanFreq = mean(freq)
  ZFreq = (freq-meanFreq) / sdFreq
  ## Create Corpus Feature
  puncStats <- data.frame( Sign = puncStats[,1], 
                           Freq_tot = freq,
                           Relative_tot = RelFreq,
                           Relative_avg = as.numeric(puncStats[,3]),
                           By_Sentence_tot = BySentFreq,
                           By_Sentence_avg = as.numeric(puncStats[,4]),
                           Z_Freq_tot = ZFreq,
                           Z_Freq_avg = as.numeric(puncStats[,5]),
                           stringsAsFactors = FALSE )
  # order by freq
  puncStats <- puncStats[order(puncStats[,2], decreasing = TRUE),]
  
  return(puncStats)
}

stats_Sent <- function(Corpus)
{
  ## Initialization
  len <- length(Corpus)
  
  ## Merge feature tables of all elements
  Sent <- Corpus[[1]]$Sentences
  Sent[,1] <- as.character(Sent[,1])
  for(i in 2:len)
  {
    # Retrieve table to merge
    temp <- Corpus[[i]]$Sentences
    temp[,1] <- as.character(temp[,1])
    # Equalize length of both table to merge
    len <- length(Sent[,1])
    lenTmp <- length(temp[,1])
    if(len > lenTmp)
    {
      temp = data.frame(sapply(temp, function(x){
        c(x, rep(NA, len - lenTmp))
      }), stringsAsFactors = FALSE)
    }
    else
    {
      Sent = data.frame(sapply(Sent, function(x){
        c(x, rep(NA, lenTmp - len))
      }), stringsAsFactors = FALSE)
    }
    
    # Merge Tables
    Sent <- data.frame(mapply(rbind, Sent, temp, SYMPLIFY=FALSE), stringsAsFactors = FALSE)
    # Filter
    Sent <- subset(Sent, Sent[,1] != "FALSE")
    Sent <- subset(Sent, is.na(Sent[,1]) == "FALSE")
    # Cast attributes
    Sent[,2] <- as.integer(Sent[,2])
    Sent[,3] <- as.numeric(Sent[,3])
    Sent[,4] <- as.numeric(Sent[,4])
    Sent[,5] <- as.integer(Sent[,5])
    Sent[,6] <- as.numeric(Sent[,6])
    Sent[,7] <- as.numeric(Sent[,7])
  }
  
  ##  Global Stats
  # Unify Sequences
  seqList <- unique(Sent[,1])
  seqNb <- length(seqList)
  
  # Compute stats
  SeqStats <- matrix(nrow = seqNb, ncol = 7)
  for(i in 1:seqNb)
  {
    s <- seqList[i]
    freqSum <- sum(Sent[which(Sent[,1] == s), 2])
    size <- Sent[which(Sent[,1] == s)[1], 5]
    means <- colMeans(Sent[which(Sent[,1] == s), c(3, 4, 6, 7)])
    SeqStats[i,] <- c(s, freqSum, size, means)
  }
  
  ## Corpus Attributes
  freq = as.integer(SeqStats[,2])
  # Relative Frequence
  RelFreq = freq/sum(freq)
  # Frequences Z-Score 
  sdFreq = sd(freq)
  meanFreq = mean(freq)
  ZFreq = (freq-meanFreq) / sdFreq
  # Relative Size
  Size <- as.integer(SeqStats[,3])
  RelSize <- Size/sum(Sent[,5])
  # Size Z-Score 
  sdSize <- sd(Size)
  meanSize <- mean(Size)
  ZSize <- (Size - meanSize) / sdSize
  
  ## Create Corpus Feature
  Sentences <- data.frame( Sequence = SeqStats[,1],
                           Freq_tot = freq,
                           Relative_tot = RelFreq,
                           Relative_avg = as.numeric(SeqStats[,4]),
                           Z_Freq_tot = ZFreq,
                           Z_Freq_avg = SeqStats[,5],
                           Size = Size,
                           Relative_Size_tot = RelSize,
                           Relative_Size_avg = as.numeric(SeqStats[,6]), 
                           Z_Size_tot = ZSize,
                           Z_Size_avg = as.numeric(SeqStats[,7]),
                           stringsAsFactors = FALSE )
  # order by Freq
  Sentences <- Sentences[order(Sentences[,2], decreasing = TRUE),]
  
  return(Sentences)
}

stats_Struct <- function(Corpus)
{
  ## Initialization
  len <- length(Corpus)
  
  ## Merge feature tables
  Struct <- Corpus[[1]]$Structure
  Struct[,1] <- as.character(Struct[,1])
  for(i in 2:len)
  {
    # Retrieve table to merge
    temp <- Corpus[[i]]$Structure
    temp[,1] <- as.character(temp[,1])
    # Equalize length of both table to merge
    len <- length(Struct[,1])
    lenTmp <- length(temp[,1])
    if(len > lenTmp)
    {
      temp = data.frame(sapply(temp, function(x){
        c(x, rep(NA, len - lenTmp))
      }), stringsAsFactors = FALSE)
    }
    else
    {
      Struct = data.frame(sapply(Struct, function(x){
        c(x, rep(NA, lenTmp - len))
      }), stringsAsFactors = FALSE)
    }
    # Merge Tables
    Struct <- data.frame(mapply(rbind, Struct, temp, SYMPLIFY=FALSE), stringsAsFactors = FALSE)
    # Filter
    Struct <- subset(Struct, Struct[,1] != "FALSE")
    Struct <- subset(Struct, is.na(Struct[,1]) == "FALSE")
    # Cast Attributes
    Struct[,2] <- as.integer(Struct[,2])
    Struct[,3] <- as.numeric(Struct[,3])
    Struct[,4] <- as.numeric(Struct[,4])
  }
  
  ## Global Feature Stats
  # Unify Structures
  structList <- unique(Struct[,1])
  structNb <- length(structList)
  # Compute Stats
  Structure <- matrix(nrow = structNb, ncol = 4)
  for(i in 1:structNb)
  {
    s <- structList[i]
    freqSum <- sum(Struct[which(Struct[,1] == s),2])
    means <- colMeans(Struct[which(Struct[,1] == s), 3:4])
    Structure[i,] <- c(s, freqSum, means)
  }

  ## Corpus Attributes
  freq = as.integer(Structure[,2])
  # Relative Frequence
  RelFreq = freq/sum(freq)
  # Frequences Z-Score 
  sdFreq = sd(freq)
  meanFreq = mean(freq)
  ZFreq = (freq-meanFreq) / sdFreq
  ##  Create Corpus Feature
  Structure <- data.frame( Structure = Structure[,1], 
                           Freq_tot = freq, 
                           Relative_tot = RelFreq,  
                           Relative_avg = as.numeric(Structure[,3]),
                           Z_Freq_tot = ZFreq,
                           Z_Freq_avg = as.numeric(Structure[,4]),
                           stringsAsFactors = FALSE )
  # order by freq
  Structure <- Structure[order(Structure[,2], decreasing = TRUE),]
  
  return(Structure)
}

# Main function
Corpus_stats <- function(Corpus)
{
  Punc_stats <- stats_PF(Corpus)
  Sent_stats <- stats_Sent(Corpus)
  Struct_stats <- stats_Struct(Corpus)
  
  return(list(Punc_stats, Sent_stats, Struct_stats))
}