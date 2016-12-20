library(MASS)
library(stats)

# Determine sentSignsributes of the text and return them. sentSignsributes are:
## Punctuation Set
## Main Punctuation Signs frequencies (abs and rel) ("." "," ";" ":")
## Punctuation Signs frequencies by sentence
## Sentence sizes (size = number of sentece part separated by a punc sign)
Txt_features <- function(text)
{
  ### TEXT FILTERING
  
  # Convert text into character vector
  text <- c(t(text)) 
  # Keep only punctuation in strings containing punctation
  punc <- gsub(".*([[:punct:]]+).*", "\\1", text) 
  # Remove full text strings, accents and filter punc signs
  punc <- gsub("[[:blank:][:cntrl:][:alpha:][:digit:]><×©àéèöüïëäâêôûîùço'\\-]*", "", punc) 
  # Remove empty strings
  punc <- punc[which(punc != "")] 
  
  
  ### INITIALIZATION : PUNCTUATION BY SENTENCE
  
  len <- length(punc)
  # Position of sentence stop signs
  ind <- c(which(punc == "."), which(punc == "!"), which(punc == "?"))
  ind = sort(ind)
  # Number of stops
  stopNb <- length(ind) 
  # creating list of punctuation signs in sentences
  sentSigns <- list() 
  # 1st sentence
  sentSigns[[1]] <- punc[1:(ind[1]-1)] 
  # Do the same for the ramining stops
  for(i in 2:stopNb)
  {
    inf <- ind[i-1] + 1
    sup <- ind[i] - 1
    # if punc between two stops, sup will be ge than inf
    if(sup >= inf) 
      sent = punc[inf:sup]
    else # No punctuation between two stops => 0 assigned
      sent = 0 
    
    sentSigns[[i]]= sent
  }
  # Retrieve last sentence if text does not end by a point
  lastStop <- ind[stopNb]
  if(lastStop<len)
  {
    sent = punc[(ind[stopNb]+1):len]
    sentSigns[[stopNb+1]] = sent
  }
  # Number of Sentences
  sentNb <- length(sentSigns)
  
  
  ### PUNCTUATION STATISTICS
  
  # Punctuation Frequences Table
  punc <- data.frame(Sign = punc, stringsAsFactors = FALSE)
  puncFT <- data.frame(table(punc), stringsAsFactors = FALSE)
  freq = puncFT[,2]
  # Relatives Frequences
  RelFreq <- freq/sum(freq)
  BySentFreq <- freq/stopNb                   
  # Frequence Z-Score 
  sdFreq <- sd(freq)
  meanFreq <- mean(freq)
  ZFreq <- (freq-meanFreq) / sdFreq
  #Feature Creation
  puncStats <- data.frame(Sign = puncFT[,1], 
                          Freq = freq, 
                          Relative = RelFreq, 
                          By_Sentence = BySentFreq, 
                          Z_Freq = ZFreq)
  # order by Z_Freq
  puncStats <- puncStats[order(puncStats[,2], decreasing = TRUE),]
  
  
  ### SENTENCE STATISTICS
  
  # Sequence statistics (sequences are sentence patterns)
  
  # Format sequences in one string
  formatSent <- rep("", sentNb)
  for(i in 1:sentNb)
  {
    sent <- sentSigns[[i]]
    for(s in 1:length(sent))
      formatSent[i] = paste(formatSent[i], sent[s])
    
  }
  # Sequences Frequency Table
  formatSent <- data.frame(Sent = formatSent, stringsAsFactors = FALSE)
  seqFT <- data.frame(table(formatSent), stringsAsFactors = FALSE )
  sequences <- seqFT[,1] 
  freq = seqFT[,2]
  # Relative Frequences
  RelFreq = freq/sum(freq)
  # Frequences Z-Score 
  sdFreq = sd(freq)
  meanFreq = mean(freq)
  ZFreq = (freq-meanFreq) / sdFreq
  # Size
  seqNb <- length(sequences)
  sequenceSize <- rep(0, seqNb)
  for(i in 1:seqNb)
    sequenceSize[i] = nchar(gsub("[[:blank:]]", "", sequences[i])) + 1
  # Relative Size
  RelSize <- sequenceSize/sum(sequenceSize*freq)
  # Size Z-Score 
  sdSize <- sd(sequenceSize)
  meanSize <- mean(sequenceSize)
  ZSize <- (sequenceSize - meanSize) / sdSize
  # Feature Creation
  sentStats <- data.frame( Sequence = seqFT[,1], 
                           Freq = freq, 
                           Relative = RelFreq, 
                           Z_Freq = ZFreq,
                           Size = sequenceSize, 
                           Relative_Size = RelSize, 
                           Z_Size = ZSize, 
                           stringsAsFactors = FALSE )
  # order by Z_Freq
  sentStats <- sentStats[order(sentStats[,2], decreasing = TRUE),] 
  
  
  ### SENTENCE STRUCTURES STATISTICS
  
  # Unify punctuation signs in sentences
  sentStruct <- lapply(sentSigns, unique)
  
  # Sort Structures
  sentStruct <- lapply(sentStruct, sort)
  
  # Format Structures in 1 string
  structs <- rep("", sentNb)
  for(i in 1:sentNb)
  {
    sent <- sentStruct[[i]]
    for(s in 1:length(sent))
      structs[i] = paste(structs[i], sent[s])
  }
  
  # Structures Frequency Table
  structs <- data.frame(Structure = structs, stringsAsFactors = FALSE)
  structFT <- data.frame( table(structs), stringsAsFactors = FALSE)
  freq = structFT[,2]
  # Relatives Frequence
  RelFreq = freq/sum(freq)
  # Frequence Z-Score 
  sdFreq = sd(freq)
  meanFreq = mean(freq)
  ZFreq = (freq-meanFreq) / sdFreq
  #Feature Creation
  structStats <- data.frame( Structure = structFT[,1],
                             Freq = freq, 
                             Relative = RelFreq,
                             Z_Freq = ZFreq )
  # order by Z_Freq
  structStats <- structStats[order(structStats[,2], decreasing = TRUE),] 
  
  return(list(Content = text, Punctuation = puncStats, Sentences = sentStats, Structure = structStats))
}

Corpus_features <- function(Corpus)
{
  len <- length(Corpus)
  Features <- list()
  for(j in 1:len)
    Features[[j]] <- Txt_features(Corpus[[j]]) 
  
  return(Features)
}