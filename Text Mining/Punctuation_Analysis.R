                      ###############################
                      ##### PUNCTUATION ANALYSIS ####
                      ###############################

###     INTRODUCTION      ###
# In this script you will find all the steps I have done in order to perform 
# punctuation analysis on 4 types of documents and classification.
# The four types of documents I used are :
#         - La Fontaine's Fables
#         - French Political Programs
#         - Research Papers
#         - Personal Production
# This script (and functions) have been coded to be as flexible as possible.
# Feel free to re-use it with your own documents !

##############################################################################

###    SUMMARY    ###
##  1) Initialization of libraries, source files and paths
##  2) Importation of all documents
##  3) Punctuation analysis of documents
##  4) Classification (modeling and application to my personal documents)
##  4) Storage of results


###     INITIALIZATION      ###

## libraries
library(xlsx)
library(tm)
library(plotly)
                
## Paths to main directories
workingDir <- "mywWorkingDir"
docDir <- paste(workingDir, "Documents/", sep="")
scriptDir <- paste(workingDir, "Scripts/", sep="")
resultDir <- paste(workingDir, "Results/Ex2/", sep="")
xpdfDir <- '"C:/Program Files (x86)/xpdf/bin64/pdftotext.exe"'

## Source functions
source(paste(scriptDir, "Corpus_features.R", sep=""))
source(paste(scriptDir, "Corpus_stats.R", sep=""))
source(paste(scriptDir, "training.R", sep="")) # in classification directory 
source(paste(scriptDir, "KNN.R", sep="")) # in classification directory

### IMPORTATION OF DOCUMENTS

## La Fontaine's Fables 
# Read text files in document directory
fables <- list()
for(i in 1:65)
  fables[[i]] = read.table(paste(docDir, "Fable_", i,".txt", sep=""), sep = "", row.names = NULL, fill = TRUE, quote = "", stringsAsFactors = FALSE )

## Research Papers
# conversion from PDF to TXT before Reading Data
pdfFiles <- list.files(path = docDir, pattern = "Paper_[0-9]*.pdf", full.names = TRUE, all.files = TRUE)  
lapply(pdfFiles, function(i) system(paste(xpdfDir, paste0('"', i, '"')), wait = FALSE) )
# read data
papers <- list()
for(i in 1:length(pdfFiles))
  papers[[i]] = read.table(paste(docDir, "Paper_", i,".txt", sep=""), sep = "", row.names = NULL, fill = TRUE, quote = "", stringsAsFactors = FALSE )

## Political Programs
firstPages <- c(2, 1, 18, 1, 10, 1, 8, 14, 11, 15, 1, 13)
lastPages <- c(9, 2, 26, 17, 17, 4, 11, 25, 15, 18, 3, 16)
programs <- list()
for(i in 1: 23)
{
  if(i<13)
  {
    uri <- paste(docDir, "Programme_", i,".pdf", sep="")
    Rpdf <- readPDF(engine = "xpdf", control = list(text = paste("-f", firstPages[i], "-l", lastPages[i])))
    programs[[i]] = content(Rpdf(elem = list(uri = uri), language = "fr", id = "id1"))
  }
  else
    programs[[i]] = read.table(paste(docDir, "Programme_", i,".txt", sep=""), sep = "", row.names = NULL, fill = TRUE, quote = "", stringsAsFactors = FALSE )
}

## Personal Files
Perso <- list()
for(i in 1:7)
  Perso[[i]] = read.table(paste(docDir, "Perso_", i,".txt", sep=""), sep = "", row.names = NULL, fill = TRUE, quote = "", stringsAsFactors = FALSE )

###   PUNCTUATION ANALYSIS   ###
# Feature extraction of documents beased on punctuation
FableCorpus <- Corpus_features(fables)
PaperCorpus <- Corpus_features(papers)
ProgramCorpus <- Corpus_features(programs)
PersoCorpus <- Corpus_features(Perso)

# Merge all corpuses in one
FinalCorpus <- do.call(c, list(FableCorpus, 
                               PaperCorpus, 
                               ProgramCorpus, 
                               PersoCorpus))

# Compact stats by type of documents
FableStats <- Corpus_stats(FableCorpus)
PaperStats <- Corpus_stats(PaperCorpus)
ProgramStats <- Corpus_stats(ProgramCorpus)
PersoStats <- Corpus_stats(PersoCorpus)

###   CLASSIFICATION   ###

# The goal here will be to classify my personal production into the 3 different types of documents
# First we will see the results of the trianing and then we will perform the classification
# I will use the Relative Frequence and Zfreq of the document structure feature
# I will take into account 6 structures : "0" "," ":" ",:" ",:" ),"
# Thus there is a total of 12 attributes for each observation point 

## Extraction of observation points

names <- c("Fable", "Paper", "Program", "Perso", "Final")
stats <- c("Punctuation", "Sentence", "Structure")
punc <- c(" 0", " ,", " :", " , :", " , ;", " ) ,")

labelSet <- NULL
FinalSet <- NULL
nameCols <- NULL # column names of the sets

for(i in 1:4)
{
  name <- names[i]
  corpus <- eval(parse(text = paste(name, "Corpus", sep="")))
  len <- length(corpus)
  set <- as.data.frame(matrix(0, nrow = len, ncol = 13))
  set[,13] <- name
  for(j in 1:len)
  {
    document <- corpus[[j]]
    structure <- document$Structure
    strucPunc <- as.character(structure$Structure)
    obsPt <- NULL
    for(p in punc)
    {
      val <- structure[which(strucPunc == p), 3:4]
      if(length(val[,1]))
        obsPt <- c(obsPt, val)
      else
        obsPt <- c(obsPt, 0, 0)
      
      # if first loop, creation of col names for the set
      if(j == 1 && i == 1)
        nameCols = c(nameCols, 
                     paste('Rel_Freq : "', p, '"', sep=""), 
                     paste('Z_Freq : "', p, '"', sep="") )
    }
    
    
    set[j, 1:12] = obsPt
  }
  names(set) <- c(nameCols, "Label")
  assign(paste(name, "Set", sep=""), set, .GlobalEnv)
  labelSet <- c(labelSet, rep(name, len))
  FinalSet <- rbind.data.frame(FinalSet, set[,1:12])
}

# Remove Perso documents from the modeling points.
# Indeed, After the training I am hoing to classiy my Perso docs into 1 of the three over types of document.
modelingPoints <- which(labelSet != "Perso")
modelSet <- FinalSet[modelingPoints,]
kValues <- c(3,5,9,11)

# Perform the training by using a classical euclidean function and the dwknn weighting function
results <- training(Set = modelSet, 
                testLabels = labelSet[modelingPoints], 
                n = 10, 
                kValues = kValues, 
                scriptDir = scriptDir )

# Take the k value that had the best results
k <- results[which(results[,3] == min(results[,3])), 1]

persoLabels <- knn(refSet = modelSet, 
                   labels = labelSet[modelingPoints], 
                   set = PersoSet[, 1:12], 
                   k = k, 
                   scriptDir = scriptDir )

###   CLUSTERING   ###
clustering <- kmeans(FinalSet, 4)
clusterLabels <- as.integer(clustering[[1]])
numLabels <- as.integer(rep(0, lgenth(labelSet)))
numLabels[1:65] = 1
numLabels[66:120] = 2
numLabels[121:143] = 3
numLabels[143:150] = 4

comp <- clusterLabels == labelSet
err <- length(which(comp == FALSE))

RF_norm <- sqrt(rowSums(clusterSet[2:151,clusterSet[1,]%%2 == 1]^2))
ZF_norm <- sqrt(rowSums(clusterSet[2:151,clusterSet[1,]%%2 == 0]^2))
RF_means <- rowMeans(clusterSet[2:151,clusterSet[1,]%%2 == 1])
ZF_means <- rowMeans(clusterSet[2:151,clusterSet[1,]%%2 == 0])                                
AvgDataClusters <- cbind.data.frame(RF_Mean = RF_means, ZF_Mean = ZF_means, cluster = clusterLabels)
NormedDataClusters <- cbind.data.frame(RF_Norm = RF_norm, ZF_Norm = ZF_norm, cluster = clusterLabels)
AvgDataSet <- cbind.data.frame(RF_Mean = RF_means, ZF_Mean = ZF_means, label = labelSet)
NormedDataSet <- cbind.data.frame(RF_Norm = RF_norm, ZF_Norm = ZF_norm, label = labelSet)


plot_ly(AvgDataClusters, x=~RF_Mean, y=~ZF_Mean, color=~cluster)
plot_ly(NormedDataClusters, x=~RF_Norm, y=~ZF_Norm, color=~cluster)

plot_ly(AvgDataSet, x=~RF_Mean, y=~ZF_Mean, color=~label)
plot_ly(NormedDataSet, x=~RF_Norm, y=~ZF_Norm, color=~label)

###   STORAGE OF RESULTS  ###

# Storage of statistics
for(i in 1:4)
{
  setName <- paste(names[i], "Stats", sep="")
  fileName <- paste(names[i], "Stats", sep="_")
  set <- eval(parse(text = setName))
  
  for(j in 1:3)
  {
    sheetName <- paste(stats[j], "Stats", sep="_")
    write.xlsx(set[[j]], file = paste(resultDir, fileName, ".xlsx", sep =""), sheetName=sheetName, append = TRUE, row.names= FALSE)
  }
}

# Storage of classification sets
for(i in 1:5)
{
  name = names[i]
  set <- eval(parse(text = paste(name, "Set", sep="")))
  write.xlsx(set, file = paste(resultDir, "classSets.xlsx", sep=""), sheetName = paste(name, "Set", sep=""), append=TRUE)
  
}

# Storage of classification results
write.xlsx2(results, file = paste(resultDir, "training_results.xlsx", sep=""), sheetName ="Results", row.names = FALSE)
class = cbind.data.frame(Doc = 1:7, Type = persoLabels) # make the results prettier by directly associating Doc and Labels
write.xlsx2(class, file = paste(resultDir, "classification_results.xlsx", sep=""), sheetName ="Class_Results", row.names = FALSE)
