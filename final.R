library(MASS)
library(ggplot2)
library(e1071)

rm(list=ls())

dataPath <- "/Users/micha/GoogleDrive/WORKSPACE/data/production/privateProj/calTechML/"

feTrain <- read.table(paste0(dataPath,"features.train.txt"))
feTest <- read.table(paste0(dataPath,"features.test.txt"))

# prepare Train
names(feTrain) <- c("digit", "symm", "intense")
Xtrain <- feTrain[,c(2:3)]
yTrain <- feTrain[,c(1)]

# prepare Test

names(feTest) <- c("digit", "symm", "intense")
Xtest <- feTest[,c(2:3)]
yTest <- feTest[,c(1)]


## 7 regularization

outcome <- 7
lambd <- 1

ridgeReg <- function(outcome,lambd){
    dummyTrain <- (yTrain==outcome) *1
    dummyTrain[dummyTrain==0] <- -1
    reg <- lm.ridge(dummyTrain ~ Xtrain$symm + Xtrain$intense, lambda=lambd)  
    # Train predict
    y.pred.ridgeTrain = scale(Xtrain,center = T, scale = reg$scales)  %*% reg$coef + reg$ym
    pred.dummyTrain <- (y.pred.ridgeTrain>0)*1
    pred.dummyTrain[pred.dummyTrain==0] <- -1
    Ein.dummyTrain <- mean(pred.dummyTrain!=dummyTrain)
    # Test predict
    y.pred.ridgeTest = scale(Xtest,center = T, scale = reg$scales)  %*% reg$coef + reg$ym
    pred.dummyTest <- (y.pred.ridgeTest>0)*1
    pred.dummyTest[pred.dummyTest==0] <- -1
    dummyTest <- (yTest==outcome) *1
    dummyTest[dummyTest==0] <- -1
    Eout.dummyTest <- mean(pred.dummyTest!=dummyTest)
    
    return(c(Ein.dummyTrain ,Eout.dummyTest))
}

ridgeReg(0,1)
ridgeReg(1,1)
ridgeReg(2,1)
ridgeReg(3,1)
ridgeReg(4,1)
ridgeReg(5,1)
ridgeReg(6,1)
ridgeReg(7,1)
ridgeReg(8,1)
ridgeReg(9,1)

## 8
Xtrain$x1x2 <- Xtrain$symm * Xtrain$intense
Xtrain$x1_2 <- (Xtrain$symm)^2 
Xtrain$x2_2 <- (Xtrain$intense)^2 

Xtest$x1x2 <- Xtest$symm * Xtest$intense
Xtest$x1_2 <- (Xtest$symm)^2 
Xtest$x2_2 <- (Xtest$intense)^2 

outcome <- 5
lambd <- 1

ridgeReg2 <- function(outcome,lambd){
    dummyTrain <- (yTrain==outcome) *1
    dummyTrain[dummyTrain==0] <- -1
    reg <- lm.ridge(dummyTrain ~ Xtrain$symm + Xtrain$intense + Xtrain$x1x2 + Xtrain$x1_2 + Xtrain$x2_2, lambda=lambd)  
    # Train predict
    y.pred.ridgeTrain = scale(Xtrain,center = T, scale = reg$scales)  %*% reg$coef + reg$ym
    pred.dummyTrain <- (y.pred.ridgeTrain>0)*1
    pred.dummyTrain[pred.dummyTrain==0] <- -1
    Ein.dummyTrain <- mean(pred.dummyTrain!=dummyTrain)
    # Test predict
    y.pred.ridgeTest = scale(Xtest,center = T, scale = reg$scales)  %*% reg$coef + reg$ym
    pred.dummyTest <- (y.pred.ridgeTest>0)*1
    pred.dummyTest[pred.dummyTest==0] <- -1
    dummyTest <- (yTest==outcome) *1
    dummyTest[dummyTest==0] <- -1
    Eout.dummyTest <- mean(pred.dummyTest!=dummyTest)
    
    return(c(Ein.dummyTrain ,Eout.dummyTest))
}

ridgeReg2(0,1)
ridgeReg2(1,1)
ridgeReg2(2,1)
ridgeReg2(3,1)
ridgeReg2(4,1)
ridgeReg2(5,1)
ridgeReg2(6,1)
ridgeReg2(7,1)
ridgeReg2(8,1)
ridgeReg2(9,1)


#10: 1 against 5

Xtrain <- Xtrain[yTrain %in% c(1,5),]
Xtest <- Xtest[yTest %in% c(1,5),]
yTrain <- yTrain[yTrain %in% c(1,5)]
yTest  <- yTest[yTest %in% c(1,5)]

ridgeReg2(5,1)
ridgeReg2(5,0.01)

t <- (lm.ridge(dummyTrain ~ Xtrain$symm + Xtrain$intense + Xtrain$x1x2 + Xtrain$x1_2 + Xtrain$x2_2, lambda=0)) 
t
lm.ridge(dummyTrain ~ Xtrain$symm + Xtrain$intense + Xtrain$x1x2 + Xtrain$x1_2 + Xtrain$x2_2, lambda=1) 

#11: support vector machine

rm(list=ls())
data <-matrix(c(1,0,-1,0,1,-1,0,-1,-1,-1,0,1,0,2,1,0,-2,1,-2,0,1),nrow=7,byrow=T)
X <- data[,c(1,2)]
y <- data[,c(3)]

z1 <- X[,2]^2 - 2 * X[,1] - 1
z2 <- X[,1]^2 - 2 * X[,2] + 1

w1 <- c(-1,1)
w2 <- c(1,-1)
w3 <- c(1,0)
w4 <- c(0,1)



Z <- cbind(z1,z2)

(Z %*% w1) - 0.5
(Z %*% w2) - 0.5
(Z %*% w3) - 0.5
(Z %*% w4) - 0.5
y
## plotting

Xplot <- as.data.frame(cbind(y,X))
names(Xplot) <- c("y","x1","x2")
Zplot <- as.data.frame(cbind(y,Z))
names(Xplot) <- c("y","z1","z2")
library(ggplot2)
ggplot() + geom_point(data=Xplot, aes(x=x1, y=x2,color = as.factor(y), size=4))  
ggplot() + geom_point(data=Zplot, aes(x=z1, y=z2,color = as.factor(y), size=4))  

## kernel method

svmModel <- try(svm(y = y, x = Z, type ="C" , kernel = "linear",cost=1000 ))
summary(svmModel)