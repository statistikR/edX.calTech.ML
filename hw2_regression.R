
# storage variables
E_in_vec <- -100000
E_out_vec <- -100000

system.time(
for (i in 1:1000){


## number of observations in dataset
nTrain = 100
nControl =1000
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

#plot(D[,c(2,3)])
#lines(A)

Dtrain   <- D[c(1:nTrain),]
Dcontrol <- D[c((nTrain+1):N),]
yTrain   <- y[1:nTrain]
yControl <- y[(nTrain+1):N]



# run regression

linReg(Dtrain, yTrain)

yPredTrain <- Dtrain %*% g
yPredTrain[yPredTrain <= 0] <- -1
yPredTrain[yPredTrain >  0] <-  1
E_in  <- 1-mean(yPredTrain==yTrain)

yPredControl <- Dcontrol %*% g
yPredControl[yPredControl <= 0] <- -1
yPredControl[yPredControl >  0] <-  1
E_out  <- 1-mean(yPredControl==yControl)

## record E_in and E_out

E_in_vec[i] <- E_in 
E_out_vec[i] <- E_out 

}
)

mean(E_in_vec)
mean(E_out_vec)


############ Exercise 7
############ use linear regression as starting weights for perceptron



nIterations <- -10000

i <- 1

system.time(
for (i in 1:1000){


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

#plot(D[,c(2,3)])
#lines(A)

Dtrain   <- D[c(1:nTrain),]
Dcontrol <- D[c((nTrain+1):N),]
yTrain   <- y[1:nTrain]
yControl <- y[(nTrain+1):N]



# run regression to get starting weights for perceptron

linReg(Dtrain, yTrain)

# convert starting weights to a vector
regWeights <- as.vector(t(g))

perceptron(Dtrain ,10000, yTrain, regWeights)



nIterations[i] <- nIteration

}
)

nIterations
mean(nIterations[nIterations != 10000])



############ Exercise 8,9,10
############ use linear regression as starting weights for perceptron

# storage variables
E_in_vec <- -100000
E_nonLin_out_vec <- -100000
z <- -10

nonLinW <- rbind(z,z,z,z,z,z)



system.time(
for (i in 1:10){


## number of observations in dataset
nTrain = 1000
nControl =1000
N <- nTrain + nControl

## create x values for dataset D

x0 <- rep(1, times = N)
x1 <- runif(N,-1,1)
x2 <- runif(N,-1,1)
D <- cbind(x0,x1,x2)
DnonLin <- cbind(x0,x1,x2,x1*x2,x1^2,x2^2)

## create target function give by professor

head(D)

trueY <- D[,2]^2 + D[,3]^2 -0.6

y <- rep(1,N)
y[trueY < 0] <- -1


# noise randomly flip sign of 10%

randomSignFlipper <- sample(   c(rep(1,(N-(N*0.1))), rep(-1,(N*0.1)) ))
yNoise <- y * randomSignFlipper



# plot target function line and dataset values

#plot(D[,c(2,3)])
#lines(A)

Dtrain   <- D[c(1:nTrain),]
Dcontrol <- D[c((nTrain+1):N),]
DnonLinTrain   <- DnonLin[c(1:nTrain),]
DnonLinControl <- DnonLin[c((nTrain+1):N),]
yTrain   <- y[1:nTrain]
yControl <- y[(nTrain+1):N]
yNoiseTrain   <- yNoise[1:nTrain]
yNoiseControl <- yNoise[(nTrain+1):N]



# run regression

linReg(Dtrain, yNoiseTrain)

yPredTrain <- Dtrain %*% g
yPredTrain[yPredTrain <= 0] <- -1
yPredTrain[yPredTrain >  0] <-  1
E_in  <- 1-mean(yPredTrain==yNoiseTrain)



## record E_in and E_out

E_in_vec[i] <- E_in 

##### Exercise 9: nonLinear solution

featureVector <- linReg(DnonLinTrain, yNoiseTrain)
nonLinW <- cbind(nonLinW,featureVector )

## Exercise 10: out of sample

yPredNonLinControl <- DnonLinControl %*% featureVector
yPredNonLinControl[yPredNonLinControl <= 0] <- -1
yPredNonLinControl[yPredNonLinControl >  0] <-  1
E_nonLin_out  <- 1-mean(yPredNonLinControl==yNoiseControl)
E_nonLin_out_vec[i] <- E_nonLin_out
}
)

# result exercise 8
mean(E_in_vec)
E_in_vec
#result exercise 9
rowMeans(nonLinW[,-1])
#result exercise 10
mean(E_nonLin_out_vec)



