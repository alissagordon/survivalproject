library(extraDistr)


n <- 1000

#This process only simulate the first recurrence, 
#we'll need to expand if we do more

gen_firstrecur <- function(n){
  #Generate baseline covariates
  #Made first two to model size/number of tumors, used MLE of full data for poisson
  W <- matrix(data = c(rpois(n,2.07), rpois(n,2.02)), nrow=n, ncol=2)
  
  #Generate treatment, assume RCT
  A <- rdunif(n,0,2)
  
  #Generate outcome based on baselines and treatment
  Y <- rpois(n, lambda = 1.5*W[,1] + 2*W[,2] + 4*A)
  
  #Generate censoring, assume CAR
  C <- rbinom(n, size = 1, 0.525)
  
  return(as.data.frame(cbind(W, A, Y, C)))
}

sim <- gen_firstrecur(n)
sim

