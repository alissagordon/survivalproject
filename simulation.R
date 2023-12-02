library(extraDistr)
library(reda)

#Link to reda package info: https://cran.r-project.org/web/packages/reda/vignettes/reda-simulate.html

n <- 1e4

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
  lambdas <- 12 + 0.5*W[,1] + 1*W[,2] - 3*A
  
  lambdas[lambdas <= 0] <- 1e-4
  
  Y <- ceiling(rexp(n, rate = lambdas/(14.5/(log(2)/11))))
  
  #Y <- rpois(n, lambda = lambdas) + rdunif(n,-2,2)
  
  #Generate censoring, assume CAR
  C <- rbinom(n, size = 1, 0.525)
  
  output <- as.data.frame(cbind(A, W, C, Y))
  
  colnames(output) <- c("treatment", "number", "size", "status", "time")
  
  return(output)
}

sim <- gen_firstrecur(n)
sim

#write.csv(sim, "~/Desktop/survivalproject/sim_data_1e4.csv")

sim %>% group_by(treatment) %>% summarize(mean = mean(time))

library(ggplot2)

ggplot(data = sim, aes(x = time)) + geom_histogram() + 
  theme_minimal() + labs(x = "Time to 1st Recurrence", y = "Frequency",
                         title = "Simulated Data")

data <- bladder1
tau=24
data <- data %>% filter(enum == 1) %>%
  dplyr::select(-c(start,rtumor,rsize,enum,recur))

ggplot(data = data, aes(x = stop)) + geom_histogram(binwidth = 3) + 
  theme_minimal() + labs(x = "Time to 1st Recurrence", y = "Frequency",
                         title = "Real Data")

