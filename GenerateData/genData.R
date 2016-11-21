source("C:/Users/Arnaud/Documents/Etudes/Semestre_Estonie_A16/Courses/Data_Mining_and_Network_Analysis/Practice/GenerateData/genConcentric.R")
source("C:/Users/Arnaud/Documents/Etudes/Semestre_Estonie_A16/Courses/Data_Mining_and_Network_Analysis/Practice/GenerateData/genEllipsArc.R")
source("C:/Users/Arnaud/Documents/Etudes/Semestre_Estonie_A16/Courses/Data_Mining_and_Network_Analysis/Practice/Classification/K-means.R")


library(MASS)

library(xlsx)

dataSetH = read.xlsx("C:/Users/Arnaud/Documents/Etudes/Semestre_Estonie_A16/Courses/Data_Mining_and_Network_Analysis/Homeworks/Home Assignment 2/DataSetWithLabels.xlsx", sheetIndex = 1, as.data.frame = TRUE, stringAsFactors = FALSE)

x1 <- mvrnorm(75, 2.5, 0.34, empirical = TRUE)
y1 <- mvrnorm(75, 3, 1.12, empirical = TRUE)

x2 <- mvrnorm(75, 2.5, 1.13 , empirical = TRUE)
y2 <- mvrnorm(75, 3, 1.57, empirical = TRUE)

setT1 <- rbind(cbind(x1,y1), cbind(x1, y2), cbind(x2, y2), cbind(x2, y1))
concentrics = genConcentric(c(1, -0.5), 0.5, 1.25, 3, 150, 150, 150)


set1 <- as.data.frame(matrix(0, size, 2))
xe <- rexp(500, 0.3)
ye <- rexp(500, 0.3)
s = rep(c(1, -1), 1000)
signX <- sample(s, 20)
signY <- sample(s, 20)
xe = xe * signX
ye = ye * signY
xe = xe[xe>(-2)]
xe = xe[xe<5]
ye = ye[ye>(-2)]
ye = ye[ye<5]
xe = xe[1:20]
ye = ye[1:20]

write.xlsx("C:/Users/Arnaud/Documents/Etudes/Semestre_Estonie_A16/Courses/Data_Mining_and_Network_Analysis/Practice/GenerateData/LabeledDataSet.xlsx", col.names = FALSE, row.names = FALSE

setExp = data.frame(xe, ye)

test1 = genEllipsArc(c(0.5, 1), 2, 4, 250)