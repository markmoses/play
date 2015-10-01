testCorPos <- function( rho, x = NULL, shape, rate, ...) {
    
    C <- matrix(rho, nrow = 2, ncol = 2)
    diag(C) <- 1
    C <- chol(C)
    
    
    
    # distribution correlated value
    #is drawn from
    y<- runif(length(x) , 0, shape)

    X <- cbind(x,y)
    
    # induce correlation (does not change X1)
    df <- X %*% C
    
    final <- as.numeric(df[,2]) 
    
    return(final)
    
} 


submitfun <- function(par){
    print(par)
    #print(par)
    set.seed(1)
    test2 <- testCorPos(0, test35, shape=par[1])
    
    test5 <- testCorPos(.5, test35, shape=par[1])
    test9 <- testCorPos(.9, test35, shape=par[1])
    
    check1 <- 0 -cor(test2, test35)
    
    check5 <- 0.5-cor(test5, test35)
    check9 <- .9-cor(test5, test35)

    check2 <- 15 - length(test2[test2<.35])/length(test2) *100 
    l <- -sum(dnorm(c(  check1, check2, check5, check9), 0, .00001, log=T))
  
    return(l)
}


ret1 <- optim(1,submitfun, method="Brent", lower=0, upper=10)




