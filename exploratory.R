library(survival)
library(tmle)
library(dplyr)
library(tidyr)
library(imputeMissings)
library(tidyverse)
library(ltmle)
library(survminer)

#exploratory data analysis
expdata <- bladder1
expdata<- expdata %>% filter(enum == 1) %>%
  dplyr::select(-c(start,rtumor,rsize,enum,recur))
#next change status = 3 to 0
#assume death is type of censoring
expdata$status[expdata$status == 3] <- 0
View(expdata)

expdata %>% filter(status==1) %>% pull(stop) %>% hist(main=paste("Time to First Recurrence"),xlab="Time in Months")
expdata %>% filter(status==0) %>% pull(stop) %>% hist(main=paste("Censoring Time"),xlab="Time in Months")


expdata %>% filter(treatment=="placebo") %>% filter(status==1) %>% pull(stop) %>% hist(main=paste("Placebo: Time to First Recurrence"),xlab="Time in Months",breaks=15,xlim=c(0,50),ylim=c(0,9))
expdata %>% filter(treatment=="placebo") %>% filter(status==0) %>% pull(stop) %>% hist(main=paste("Placebo: Censoring Time"),xlab="Time in Months",breaks=10)

expdata %>% filter(treatment=="pyridoxine") %>% filter(status==1) %>% pull(stop) %>% hist(main=paste("Pyridoxine: Time to First Recurrence"),xlab="Time in Months",breaks=15,xlim=c(0,50),ylim=c(0,9))
expdata %>% filter(treatment=="pyridoxine") %>% filter(status==0) %>% pull(stop) %>% hist(main=paste("Pyridoxine: Censoring Time"),xlab="Time in Months",breaks=10,ylim=c(0,4))

expdata %>% filter(treatment=="thiotepa") %>% filter(status==1) %>% pull(stop) %>% hist(main=paste("Thiotepa: Time to First Recurrence"),xlab="Time in Months",breaks=15,xlim=c(0,50),ylim=c(0,9))
expdata %>% filter(treatment=="thiotepa") %>% filter(status==0) %>% pull(stop) %>% hist(main=paste("Thiotepa: Censoring Time"),xlab="Time in Months",breaks=10)


