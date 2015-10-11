rm(list=ls())

set.seed(2)
##read in data I sent you originally
df <- read.csv("example_data.csv")
test <- df[,5] ## look at everyone who test above 0.35 regardless of TB status

#define function
testCorPos <- function( rho, x = NULL, shape=shape_in, rate=rate_in,...) {
    
    C <- matrix(rho, nrow = 2, ncol = 2)
    diag(C) <- 1
    C <- chol(C)
    
    
    
    # distribution correlated value
    #is drawn from
    x<- pgamma(x , shape, rate)
    y <- runif(length(x))
  
    X <- cbind(x,y)
    
    # induce correlation (does not change X1)
    df <- X %*% C
    
    final <- as.numeric(df[,2]) 
#     final <- ifelse(final<=0, .001, final)
#     final <- ifelse(final>=1, .999, final)
#     
    final <- final - min(final) +.001
    final <- final/(max(final)+.001)
    final <- qgamma(final, shape, rate)
    

    return(final)
    
} 




## optomize function below based on correlation and a cutoff at a correlation of 0
submitfun <- function(par){
   
    #print(par)
    set.seed(1)
    
    test2 <- testCorPos(0, test, shape=par[1], rate=par[2])
    
    test5 <- testCorPos(.5, test, shape=par[1], par[2])
    test9 <- testCorPos(.9, test, shape=par[1], par[2])
    
    check0 <- 0 -cor(test2, test)
    
    check5 <- 0.5-cor(test5, test)
    check9 <- .9-cor(test9, test)

    check_revert <- 45 - length(test2[test2<1])/length(test2) *100 
    l <- -sum(dnorm(c(  check5, check0, check_revert, check9), 0, .001, log=T))
    if(is.na(l)){print(par);break}
    if(is.nan(l)){print(paste("nan")) ;print(par);break}
    return(l)
}
 
# s###tart values are sensitive and I was unable to find a good way to 
#suggest reasonable staring values so I'm gird searching with this function 
gridfun<- function(gridpar){
    print(gridpar)
    ret <- nlminb(objective=submitfun, start=gridpar, lower=.001)
    return(ret$objective)
} 


## this is the gird search DO NOT RUN unless you have a lot of time
# see below for the returned starting values.
## This grid search is probably overkill in terms of granularity and range

library(NMOF)
min_par <- gridSearch(gridfun, levels=list(c(seq(.01, 5, .1)),
                                           c(seq(.01, 5, .1))))

##Suggested starting values for optimization

#optimize to get shape and rate parameters
o <-  nlminb(objective=submitfun, start= min_par$minlevels, lower=.01)

#define variables in environment
shape_in <- o$par[1]
rate_in <- o$par[2]
print(shape_in)
print(rate_in)

## Compare the distirbution we were fitting before
# to the one we get now.
fitdistr(test, "gamma")
##they are different... 



## create test with rho of 0
test_rho0 <- testCorPos(0, test)

##check correlation of test and test_rho0
cor(test_rho0, test)

## look at the percent of reversions at a cutoff of 0.35
length(test_rho0[test_rho0<.35])/length(test_rho0) *100
# looks pretty good!

## now look at other cutoffs
length(test_rho0[test_rho0<.5])/length(test_rho0) *100
length(test_rho0[test_rho0<.75])/length(test_rho0) *100
length(test_rho0[test_rho0<1])/length(test_rho0) *100

##test out a corr5
test_rho5 <- testCorPos(.5, test)

##check correlation of test and test_rho5
cor(test_rho5, test)
#not bad?


## look at the percent of reversions..
length(test_rho5[test_rho5<.35])/length(test_rho5) *100





1.22
.76-.8


.76
.23

# cutoff at .35
# .1.01
# .48
