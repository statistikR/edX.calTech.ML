
# storage variables
percentIncorrect <- -100000
nIterationsToConverge <- -100000


for (i in 1:100){


## number of observations in dataset
nTrain = 10
nControl =10
N <- nTrain + nControl

## create x values for dataset D

x0 <- rep(1, times = N)
x1 <- runif(N,-1,1)
x2 <- runif(N,-1,1)
D <- cbind(x0,x1,x2)

## create target function based on 2 random points

p1 <- t(runif(2,-1,1))
p2 <- t(runif(2,-1,1))
A  <- rbind(p1,p2)

# solve random function with w0 chosen as 1

b <- c(-1,-1)
w1_2 <- solve(A,b)

# create target function w vector
trueW <- c(1,(w1_2))
trueY <- D %*% trueW 

y <- rep(1,N)
y[trueY < 0] <- -1




# plot target function line and dataset values

plot(D[,c(2,3)])
lines(A)

Dtrain   <- D[c(1:nTrain),]
Dcontrol <- D[c((nTrain+1):N),]
yTrain   <- y[1:nTrain]
yControl <- y[(nTrain+1):N]



# run perceptron

perceptron(dataX = Dtrain, nIterations = 10000, y = yTrain)

g
nIteration
recordIncorrectCases
# predict 1 and -1 based on current set of w's
        yControlEst           <- as.numeric(Dcontrol %*% g > 0)
        yControlEst[yControlEst==0] <- -1
        
        # record number of incorrect cases


        percentIncorrect[i]      <- 1-mean(yControlEst == yControl)
        nIterationsToConverge[i] <- nIteration

}
percentIncorrect
nIterationsToConverge
mean(percentIncorrect)
mean(nIterationsToConverge)
mean(nIterationsToConverge[nIterationsToConverge!=10000])



