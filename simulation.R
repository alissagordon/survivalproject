library(extraDistr)
library(reda)

#Link to reda package info: https://cran.r-project.org/web/packages/reda/vignettes/reda-simulate.html

n <- 10

#This process only simulate the first recurrence, 
#we'll need to expand if we do more

gen_firstrecur <- function(n){
  #Generate baseline covariates
  #Made first two to model size/number of tumors, used MLE of full data for poisson
  W <- matrix(data = c(rpois(n,2.07), rpois(n,2.02)), nrow=n, ncol=2)
  W[W == 0] <- 1
  
  #Generate treatment, assume RCT
  A <- rdunif(n,0,2)
  
  #Generate outcome based on baselines and treatment
  lambdas <- 12 - 0.5*W[,1] + -1*W[,2] + 5*A
  
  lambdas[lambdas <= 0] <- 1e-4
  
  Y <- rpois(n, lambda = lambdas)
  
  #Generate censoring, assume CAR
  C <- rbinom(n, size = 1, 0.525)
  
  output <- as.data.frame(cbind(A, W, C, Y))
  
  colnames(output) <- c("treatment", "number", "size", "status", "time")
  
  return(output)
}

sim <- gen_firstrecur(n)
sim

#IGNORE THIS FOR NOW
gen_allrecur <- function(n){
  #Generate baseline covariates
  #Made first two to model size/number of tumors, used MLE of full data for poisson
  W <- matrix(data = c(rpois(n,2.07), rpois(n,2.02)), nrow=n, ncol=2)
  W[W == 0] <- 1
  
  #Generate treatment, assume RCT
  A <- rdunif(n,0,2)
  
  #Generate number of recurrences
  thetas <- 1 + rpois(n,1.70)
  
  #Generate outcome based on baselines and treatment
  lambdas <- 12 - 0.5*W[,1] + -1*W[,2] + 5*A
  lambdas[lambdas <= 0] <- 1e-4
  
  Y <- rep(NA,n*sum(thetas))
  C <- rep(0,n*sum(thetas))
    
  for(i in 1:n){
    for(j in 1:thetas[i]){
      print(i*thetas[i]+(j-1))
      Y[i*j+(j-1)] <- rpois(1,lambdas[i])
      C[i*j+(j-1)] <- rbinom(1,size = 1,0.9)
      
      if(C[i*j+(j-1)] == 0){
        next
      }
    }
  }
    
    
  
    return(as.data.frame(cbind(A, W, C, Y)))
  }
  
  #Generate outcome based on baselines and treatment
  lambdas <- 12 - 0.5*W[,1] + -1*W[,2] + 5*A
  
  lambdas[lambdas <= 0] <- 1e-4
  
  Y <- rpois(n, lambda = lambdas)
  
  #Generate censoring, assume CAR
  C <- rbinom(n, size = 1, 0.525)
  
  output <- as.data.frame(cbind(A, W, C, Y))
  
  colnames(output) <- c("treatment", "number", "size", "status", "time")
  
  return(output)
}

sim2 <- simEventData(n, z = c(0.2, 0.5), zCoef = c(1, - 0.5), rho = 0.5, endTime = 60)

