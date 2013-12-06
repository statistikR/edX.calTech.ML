
library(ggplot2)
library(e1071)

dataPath <- "/Users/micha/GoogleDrive/WORKSPACE/data/production/private/calTechML"

inData <- read.table(paste0(dataPath,"in.dta"))
outData <- read.table(paste0(dataPath,"out.dta"))

getwd()

## no 1/2

no1In <- as.data.frame(inData)
names(no1In) <- c("x1", "x2", "y")

x1 <- no1In$x1
x2 <- no1In$x2
x0 <- 1
x3 <- no1In$x1^2
x4 <- no1In$x2^2
x5 <- no1In$x1 * no1In$x2
x6 <- abs(no1In$x1 - no1In$x2)
x7 <- abs(no1In$x1 + no1In$x2)

yIn <- no1In$y
XIn <- cbind(x0,x1,x2,x3,x4,x5,x6,x7)

head(XIn)
head(no1In)

no1Out <- as.data.frame(outData)
names(no1Out) <- c("x1", "x2", "y")

x1 <- no1Out$x1
x2 <- no1Out$x2
 
x0 <- 1
x3 <- no1Out$x1^2
x4 <- no1Out$x2^2
x5 <- no1Out$x1 * no1Out$x2
x6 <- abs(no1Out$x1 - no1Out$x2)
x7 <- abs(no1Out$x1 + no1Out$x2)
yOut <- no1Out$y
XOut <- cbind(x0,x1,x2,x3,x4,x5,x6,x7)




valError <- function(maxSub){
    maxSub <- maxSub +1
    XsubTrain <-XIn[c(1:25),c(1:maxSub)]
    XsubVal <-XIn[c(26:35),c(1:maxSub)]
    yTrain <- yIn[c(1:25)]
    ySubVal <- yIn[c(26:35)]
    
    w <-solve(t(XsubTrain) %*% XsubTrain) %*% t(XsubTrain) %*% yTrain 

    yPredVal <- XsubVal %*% w
    yPredVal[yPredVal>0] <- 1 
    yPredVal[yPredVal<=0] <- -1
    mse <- 1-mean(ySubVal == yPredVal)
    
    yPredOut <- XOut[,c(1:length(w))] %*% w
    yPredOut[yPredOut>0] <- 1 
    yPredOut[yPredOut<=0] <- -1
    mseOut <- 1-mean(yOut == yPredOut)
    cbind(yOut,yPredOut)

    return(list(mse,mseOut))
}


valError(3)
valError(4)
valError(5)
valError(6)
valError(7)


# no 3/4


valError2 <- function(maxSub){
    maxSub <- maxSub +1
    XsubVal <-XIn[c(1:25),c(1:maxSub)]
    XsubTrain <-XIn[c(26:35),c(1:maxSub)]
    ySubVal <- yIn[c(1:25)]
    yTrain <- yIn[c(26:35)]
    
    w <-solve(t(XsubTrain) %*% XsubTrain) %*% t(XsubTrain) %*% yTrain 

    yPredVal <- XsubVal %*% w
    yPredVal[yPredVal>0] <- 1 
    yPredVal[yPredVal<=0] <- -1
    mse <- 1-mean(ySubVal == yPredVal)
    
    yPredOut <- XOut[,c(1:length(w))] %*% w
    yPredOut[yPredOut>0] <- 1 
    yPredOut[yPredOut<=0] <- -1
    mseOut <- 1-mean(yOut == yPredOut)
    cbind(yOut,yPredOut)

    return(list(mse,mseOut))
}


valError2(3)
valError2(4)
valError2(5)
valError2(6)
valError2(7)








## no 6

    mat <- cbind(runif(1000),runif(1000))
    mat <- cbind(mat,apply(mat,1,min))
    apply(mat,2,mean)




## no 7/8/9/10

p_svmSuperior <- NA
nSV <- NA

for (i in 1:100){
    
    # create separable dataset and separate in training and test dataset
    results <- createSeparableData(1100, print=F)
    trainD <- results$D[c(1:100),]
    valD   <- results$D[c(101:dim(results$D)[1]),]
    trainY <- results$y[c(1:100)]
    valY   <- results$y[c(101:length(results$y))]
    
    # only run perceptron and svm if not all cases are -1 or 1
    if (mean(trainY)!= -1 | mean(trainY)!= 1){ 
    
        # run perceptron and save % errors
        wP <- perceptron2(trainD,10000,trainY)
        yPred <- ((valD %*% wP) >= 0)*1
        yPred[yPred==0] <- -1
        ePerceptron <- mean(valY!=yPred)

        #run svm and save % errors
        inputData <- as.data.frame(cbind(trainY,trainD[,c(2:3)]))
        svmModel <- try(svm(y = trainY, x = trainD[,c(2:3)], type ="C" , kernel = "linear",cost=1000 ))
        if("try-error" %in% class(svmModel)){
            yPred <- NA
        }else{
            yPred <- predict(svmModel, valD[,c(2:3)])
        }       
        eSvm <- mean(valY!=yPred,na.rm=T)

        
        # record results how often Svm is better than Perceptron
        p_svmSuperior[i] <-  (eSvm < ePerceptron)
        # record number of support vectors
        nSV[i] <- svmModel$tot.nSV
        
        #print progress of the loop
        if (i%%5==0){
            print(i)
        }
            
    }
}

# check results
mean(p_svmSuperior, na.rm = T)
mean(nSV,na.rm = T)


# graphing results returned by perceptron function

point1=c(1,-(g[1]+g[2])/g[3] )
point2=c(-1,-(g[1]-g[2])/g[3] )
points <- as.data.frame(rbind(point1,point2))
names(points) <- c("x1","x2")

data <- as.data.frame(results$D[c(1:10),c(2,3)])

ggplot() +
    geom_point(data=data, aes(x=x1, y=x2,color = as.factor(trainY)))  +
    geom_line(data=points,aes(x=x1,y=x2))



    
    
    