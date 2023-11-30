
#######################################

# base work

library(survival)
library(ltmle)
library(ggplot2)
library(mvtnorm)
library(dplyr)
library(glmnet)
library(survminer)

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

full_data <- data

#make 3 different datasets for 3 levels of comparison
data01 <- data %>% filter(treatment %in% c(0,1))
data02 <- data %>% filter(treatment %in% c(0,2))
data12 <- data %>% filter(treatment %in% c(1,2))

data0 <- data %>% filter(treatment == 0)

#data <- data01
#data <- data02
#data$treatment[data$treatment == 2] <- 1
data <- data0
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

# Fit the survival model (assuming Cox proportional hazards model)
y <- Surv(data$time, data$status)
x <- as.matrix(data[c("treatment", "number")]) # Assuming 'number' is a relevant variable

fit <- coxph(y ~ x, data = data)

# Plot the survival curve with confidence intervals
autoplot(survfit(fit), conf.int = TRUE, title = "Survival Curve with Confidence Interval")



# work now diff treat


# Load necessary libraries
library(survival)
library(dplyr)
library(ggplot2)
library(ggfortify) # For autoplot function

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

# Split data by treatment
data_list <- split(data, data$treatment)

# Fit survival models and plot for each treatment group
plots <- lapply(data_list, function(d) {
  fit <- survfit(Surv(time, status) ~ 1, data = d)
  autoplot(fit, conf.int = TRUE) + 
    ggtitle(paste("Survival Curve for Treatment", unique(d$treatment)))
})

# Display the plots (this depends on your environment, here's one way to do it)
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 1))


for(i in seq_along(data_list)) {
  fit <- survfit(Surv(time, status) ~ 1, data = data_list[[i]])
  plot <- autoplot(fit, conf.int = TRUE) + 
    ggtitle(paste("Survival Curve for Treatment", i - 1)) # Treatment group title
  
  # Save or display the plot
  print(plot) # Use ggsave() if you want to save the plot to a file
}



# Fit survival model for each treatment
fit <- survfit(Surv(time, status) ~ treatment, data = data)

print(summary(fit))

# Plot combined survival curves
ggsurvplot(fit, data = data, 
           conf.int = TRUE,
           palette = c("blue", "red", "green"), # Customize colors as needed
           ggtheme = theme_minimal(),
           xlab = "Time", 
           ylab = "Survival Probability",
           title = "Survival Curves by Treatment Group")
