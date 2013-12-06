
dataPath <- "/Users/micha/GoogleDrive/WORKSPACE/data/production/privateProj/calTechML/"
inData <- read.table(paste0(dataPath,"in.dta"))
outData <- read.table(paste0(dataPath,"out.dta"))

getwd()

Xi <- as.matrix(inData[,c(1,2)])
yi <- inData[,c(3)]

w <- linReg2(Xi, yi)