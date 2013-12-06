growthFunc <- function(N,k){
  k <- 3
  i <- k-1
  exp <- seq(0,i)
  
  total <- 0
  for (i in exp){
    total <- total + choose(N,i)
    
  }

  return(total)
}





linReg <- function (dataX, y){
  
  # (X'X)^-1 X'y = b
  
  w <- (solve(t(dataX) %*% dataX)) %*% t(dataX) %*% y
  assign("g", w, envir = .GlobalEnv)
      
}

linReg2 <- function (dataX, y){
    
    intercept <- rep(1,dim(dataX)[1])
    dataX <- cbind(intercept,dataX)
  
  # (X'X)^-1 X'y = b
  
    w <- (solve(t(dataX) %*% dataX)) %*% t(dataX) %*% y
    return(w)
      
}




perceptron <- function (dataX, nIterations, y, wStart=0){
    
    Dtrain <- dataX
  
    # if no start values for w specified, then start with randomly chosen ws from H (1, 0, 0, 0, ...)
  
    if(length(wStart) == 1 && wStart == 0){
      wH <- c(1, (rep(0,(dim(Dtrain)[2] - 1))))
    }else{
      wH <- wStart
    }

    
    # setup loop variables
    
    nIncorrect           <- 2
    nIteration           <- 0
    recordIncorrectCases <- -1000
    recordIterations     <- -1000  
    
    for (i in 1:nIterations) {
        
        # iterations counter +1
        nIteration      <- nIteration + 1
        
        # predict 1 and -1 based on current set of w's
        yTemp           <- as.numeric(Dtrain %*% wH > 0)
        yTemp[yTemp==0] <- -1
        
        # check which predictions are wrong
        correct         <- (y == yTemp)
        sequence        <- seq(1,length(correct))
        predictions     <- as.data.frame(cbind(correct,sequence))
        incorrectCases  <- predictions[predictions$correct==0,c(2)]
        nIncorrectCases <- length(incorrectCases == 0)
        
        # store information about quality of prediction and iterations
        
        recordIncorrectCases[nIteration] <- nIncorrectCases
        recordIterations[nIteration]     <- nIteration
        
        if (nIncorrectCases <= 0) break
        
        # pick a incorrect prediction randomly
        
        incorrectCase <- sample(incorrectCases,1)
        
        
        
        # update w with information from incorrect prediction
        
        wH <- wH + y[incorrectCase]  * Dtrain[incorrectCase,]
        
        
        
    }
    
    assign("recordIncorrectCases", recordIncorrectCases, envir = .GlobalEnv)
    assign("nIteration", nIteration, envir = .GlobalEnv)
    assign("g", wH, envir = .GlobalEnv)
    
    #return(recordIterations,trueW,wH)
      
}

perceptron2 <- function (dataX, nIterations, y, wStart=0){
    
    Dtrain <- dataX
  
    # if no start values for w specified, then start with randomly chosen ws from H (1, 0, 0, 0, ...)
  
    if(length(wStart) == 1 && wStart == 0){
      wH <- c(1, (rep(0,(dim(Dtrain)[2] - 1))))
    }else{
      wH <- wStart
    }

    
    # setup loop variables
    
    nIncorrect           <- 2
    nIteration           <- 0
    recordIncorrectCases <- -1000
    recordIterations     <- -1000  
    
    for (i in 1:nIterations) {
        
        # iterations counter +1
        nIteration      <- nIteration + 1
        
        # predict 1 and -1 based on current set of w's
        yTemp           <- as.numeric(Dtrain %*% wH > 0)
        yTemp[yTemp==0] <- -1
        
        # check which predictions are wrong
        correct         <- (y == yTemp)
        sequence        <- seq(1,length(correct))
        predictions     <- as.data.frame(cbind(correct,sequence))
        incorrectCases  <- predictions[predictions$correct==0,c(2)]
        nIncorrectCases <- length(incorrectCases == 0)
        
        # store information about quality of prediction and iterations
        
        recordIncorrectCases[nIteration] <- nIncorrectCases
        recordIterations[nIteration]     <- nIteration
        
        if (nIncorrectCases <= 0) break
        
        # pick a incorrect prediction randomly
        
        incorrectCase <- sample(incorrectCases,1)
        
        
        
        # update w with information from incorrect prediction
        
        wH <- wH + y[incorrectCase]  * Dtrain[incorrectCase,]
        
        
        
    }
    
    return(wH)
    
    
      
}

## number of observations in dataset

createSeparableData <- function(n,print=T){

    N <- n
    
    ## create x values for dataset D
    
    x0 <- rep(1, times = N)
    x1 <- runif(N,-1,1)
    x2 <- runif(N,-1,1)
    D <- cbind(x0,x1,x2)
    
    ## separate points based on 2 random points
    
    p1 <- t(runif(2,-1,1))
    p2 <- t(runif(2,-1,1))
    A  <- rbind(p1,p2)
    
    
    X <- cbind(1,A[,c(1)]) # constant =1 plus first coordinate of both points
    y <- A[,c(2)]          # y
    
    intSlope <- solve(X,y) # calculate intercept and slope of the two points in the plane
    
    # create target function w vector
    
    predLine <- D[,c(1,2)] %*% intSlope # create separating line based on
    yCont <- D[,c(3)] -predLine         # calculate if point falls above (+) or below the line (-)
    y <- rep(1,N)
    y[yCont < 0] <- -1
    
    # conditional printing statement of the generated dataset
    if(print ==T){
        plot(D[,c(2,3)],pch=1,col="white") 
        points(D[y==-1,c(2,3)], pch ="_",col="red") 
        points(D[y==1,c(2,3)], pch ="+")
        lines(A)
    }
    
    results <- list(D,y)
    names(results) <- c("D","y")
    return(results)
    

}





