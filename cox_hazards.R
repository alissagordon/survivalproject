
#######################################

# base work

library(survival)
library(ltmle)
library(ggplot2)
library(mvtnorm)
library(dplyr)
library(glmnet)
library(survminer)


sim.gate <- TRUE
data <- bladder1

data <- data %>% 
  dplyr::select(-c(rtumor,rsize,enum,recur))

#next change status = 3 to 0
#assume death is type of censoring
# for now change status = 2 to 0 as well...
data$status[data$status == 3 | data$status == 2] <- 0

#redefine treatment to numeric
#0 = placebo, 1 = pyridoxine, 2 = thiotepa
data$treatment <- as.numeric(data$treatment) - 1

#for now, only use the first recurrence for a patient
data <- data %>% filter(start == 0) %>%
  mutate(time = stop) %>%
  dplyr::select(-c(start,stop,id))

if (sim.gate) data <- read.table("sim_data_1e4.csv", sep= ",", header=TRUE)[,-1]

full_data <- data

#make 3 different datasets for 3 levels of comparison
data01 <- data %>% filter(treatment %in% c(0,1))
data02 <- data %>% filter(treatment %in% c(0,2))
data12 <- data %>% filter(treatment %in% c(1,2))

data0 <- data %>% filter(treatment == 0)
data1 <- data %>% filter(treatment == 1)
data2 <- data %>% filter(treatment == 2)

#data <- data01
#data <- data02
#data$treatment[data$treatment == 2] <- 1
data <- data1
data$treatment <- data$treatment - 1

data <- data[data$time != 0, ]

y <- survival::Surv(data$time, data$status)
x <- as.matrix(data[c(2,3)])


fit <- glmnet(x, y, family = "cox")

plot(survival::survfit(fit, s = 0.05, x = x, y = y), main = "T0 survival")


#######################################

# work now total


# Load necessary libraries
library(survival)
library(dplyr)
library(ggplot2)
library(ggfortify) # For autoplot function
library(glmnet)

# Load and prepare the dataset
data <- bladder1 %>% 
  dplyr::select(-c(rtumor, rsize, enum, recur)) %>% 
  mutate(status = as.numeric(status == 1), # Assuming 1 is the event, others are censored
         treatment = as.numeric(treatment) - 1)

# Use only first recurrence for each patient
data <- data %>%
  filter(start == 0) %>%
  mutate(time = stop) %>%
  dplyr::select(-c(start, stop, id))
if (sim.gate) data <- read.table("sim_data_1e4.csv", sep= ",", header=TRUE)[,-1]
# Fit the survival model (assuming Cox proportional hazards model)
y <- Surv(data$time, data$status)
x <- as.matrix(data[c("size", "number")]) # Assuming 'number' is a relevant variable

fit <- coxph(y ~ x, data = data)

# Plot the survival curve with confidence intervals total
autoplot(survfit(fit), conf.int = TRUE, title = "Survival Curve with Confidence Interval") + ggtitle("total survival") + xlim(0, 60)


# Split data by treatment
data_list <- split(data, data$treatment)

# Fit survival models and plot for each treatment group
plots <- lapply(data_list, function(d) {
  y <- Surv(d$time, d$status)
  x <- as.matrix(d[c("size", "number")])
  fit <- survfit(coxph(y ~ x, data = d))
  autoplot(fit, conf.int = TRUE) + 
    ggtitle(paste("Survival Curve for Treatment", unique(d$treatment))) + xlim(0, 60)
})

for (p in plots) {
  print(p)
}








