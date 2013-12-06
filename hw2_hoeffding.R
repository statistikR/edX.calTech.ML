

v1Vec        <- 9999
vminVec      <- 9999
vrandVec     <- 9999

i <- 1

system.time(
for( i in 1:100000){
    r <- 1000
    c <- 10
    m0 <- matrix(0, r, c)
    m <- apply(m0, c(1,2), function(x) sample(c(0,1),1))
    
    
    m <- cbind(m,rowSums(m,1))
    
    
    #c1   <- m[1,]
    #cmin <- m[ m[,(c+1)] ==  min(m[,(c+1)]) ,  ][1,]
    #crand<- m[sample(seq(1:r),1),]
    
    v1   <- ( m[1,11]) / c
    vmin <- ( m[ m[,(c+1)] ==  min(m[,(c+1)]) ,  ][11] )  / c
    vrand<- (m[sample(seq(1:r),1),11])/c

    v1Vec[i]    <- v1
    vminVec[i]  <- vmin 
    vrandVec[i] <- vrand 
}
)

mean(v1Vec)
mean(vminVec)
mean(vrandVec)

