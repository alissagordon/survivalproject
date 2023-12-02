
###Use this chunk if you are using simulated data
failure1_data <- readr::read_csv("~/Desktop/survivalproject/sim_data_1e4.csv")

###Use this chunk if you are using real data
failure1_data <- full_data

Ttilde <- failure1_data$time
Delta <- failure1_data$status == 1
n <- length(Ttilde)

# do a KM fit
## EDIT: Yi regressed against 1 instead of treatment
#km <- survfit(Surv(Ttilde, Delta, type = "right") ~ failure1_data$treatment, type = "kaplan-meier", 
#              conf.int = .95)

#this part will estimate overall survival
km <- survfit(Surv(Ttilde, Delta, type = "right") ~ 1, type = "kaplan-meier", 
              conf.int = .95)

# make step functions
km_step <- stepfun(km$time, c(1, km$surv))
km_lower <- stepfun(km$time, c(1, km$lower))
km_upper <- stepfun(km$time, c(1, km$upper))

range <- seq(0:max(failure1_data$time))

km_data <- data.frame(time = range, 
                      lower = km_lower(range),
                      KM = km_step(range),
                      upper = km_upper(range)
)

km_data_long <- data.frame(time = c(range,range,range),
                           estimate = c(km_data$lower,km_data$KM,km_data$upper),
                           label = c(rep("lower",length(range)),
                                     rep("KM",length(range)),
                                     rep("upper",length(range))))


ggplot(data = km_data_long, aes(x = time, y = estimate, color = label)) + 
  geom_point() + geom_line() + labs(y = "Estimated Probability", x = "Time (months)") + theme_minimal()


kmcurve=survfit(Surv(failure1_data$time,failure1_data$status)~failure1_data$treatment)
survdiff(Surv(failure1_data$time,failure1_data$status)~failure1_data$treatment)
survminer::ggsurvplot(kmcurve,data=failure1_data)

#STEP1 start
# order observed data according to increasing order Ttilde
T_ord <- Ttilde[order(Ttilde)]
D_ord <- Delta[order(Ttilde)]

# all known death times in order
T_death <- T_ord[D_ord]

# compute the hazard (lambda) at all known death times
nrisk <- sapply(T_death, function(x) sum(Ttilde >= x))
lambda <- 1/nrisk

# compute the prob of surviving up to or past each death time
Pbar <- sapply(T_death, function(x) mean(Ttilde >= x))

calculate_IC <- function(Ttilde, Delta, time, n) {
  sum((-(T_death <= time)*km_step(time)/(1 - lambda))*
        lambda^2*(1 - lambda)^2/((1 - lambda)^2/n + lambda^2*Pbar)*
        ((Ttilde == T_death & Delta == 1)/lambda - (Ttilde > T_death)/(1 - lambda)),na.rm=T)
}

# compute the IC matrix for times 40, 50, 60 where each column is an IC
times <- c(12,24,36)
IC_matrix <- sapply(times, function(t) {
  unlist(lapply(1:n, function(i) calculate_IC(Ttilde[i], Delta[i], t, n)))
})
#STEP1 end

#STEP2 start
# compute the correlation of the IC matrix
Sigma <- cor(IC_matrix)
# randomly draw from a multivariate Normal(0,Sigma) distribution
mean <- rep(0, length(times))
z <- rmvnorm(1e6, mean, Sigma)

# identify row-wise maximum of the absolute value of each MVN value in z
z_abs <- apply(z, 1, function(x) max(abs(x))) 
# identify 95th quantile of z_abs, which is the se to use for simultaneous CI
z_95 <- quantile(z_abs, .95)
# note how the se_simultaneous is larger when demanding simultaneous coverage
# of the truth, and it directly follows that simultaneous CIs are wider.

# check if all of the simultaneous CI's covered the truth
sd_IC1 <- sd(IC_matrix[,1])/sqrt(n)
sd_IC2 <- sd(IC_matrix[,2])/sqrt(n)
sd_IC3 <- sd(IC_matrix[,3])/sqrt(n)

ci1 <- c(km_step(times[1]) - z_95*sd_IC1, km_step(times[1]) + z_95*sd_IC1)

ci2 <- c(km_step(times[2]) - z_95*sd_IC2, km_step(times[2]) + z_95*sd_IC2)

ci3 <- c(km_step(times[3]) - z_95*sd_IC3, km_step(times[3]) + z_95*sd_IC3)

ci1
ci2
ci3
