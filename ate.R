library(survival)
library(tmle)
library(dplyr)
library(tidyr)
library(imputeMissings)
library(tidyverse)
library(ltmle)


data <- bladder1
tau=24
data <- data %>% filter(enum == 1) %>%
  dplyr::select(-c(start,rtumor,rsize,enum,recur))

#next change status = 3 to 0
#assume death is type of censoring
data$status[data$status == 3] <- 0

#outcome for if they had recurrence in first 2 years (tau) or not
data$Y <- ifelse(data$stop <= tau & data$status==1, 1, 0)

#Censoring variable: I(C>tau)
data$censoring<-ifelse(data$stop>tau & data$status==0, 1,0)
data$delta=1-data$censoring
#redefine treatment to numeric
#0 = placebo, 1 = pyridoxine, 2 = thiotepa
data$treatment <- as.numeric(data$treatment) - 1

#remove stop variable since now outcome defined
data <- data %>% dplyr::select(-stop)

#binarize baseline covariates
#so that we have sufficient support in W
#no positivity violations
data$size <- ifelse(data$size > 1, 1, 0)
data$number <- ifelse(data$number > 1, 1, 0)

#make 3 different datasets for 3 levels of comparison
data01 <- data %>% filter(treatment %in% c(0,1))
data02 <- data %>% filter(treatment %in% c(0,2))
data12 <- data %>% filter(treatment %in% c(1,2))

tmp<-data01
tmle01_48=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")

data02$treatment[data02$treatment==2]<-1
tmp<-data02
tmle02_48=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")

tmp=data12 %>% mutate(treatment=as.numeric(treatment)-1)
tmle12_48=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")

tmp <- data01
tmle01_36=tmle(Y = tmp$Y,
     A = tmp$treatment,
     W = tmp %>% dplyr::select(c(size,number)),
     Delta = tmp$delta,
     family = "binomial")

data02$treatment[data02$treatment==2]<-1
tmp<-data02
tmle02_36=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")

tmp=data12 %>% mutate(treatment=as.numeric(treatment)-1)
tmle12_36=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")
tmp=data01
tmle01_24=tmle(Y = tmp$Y,
     A = tmp$treatment,
     W = tmp %>% dplyr::select(c(size,number)),
     Delta = tmp$delta,
     family = "binomial")

data02$treatment[data02$treatment==2]<-1
tmp<-data02
tmle02_24=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")

tmp=data12 %>% mutate(treatment=as.numeric(treatment)-1)
tmle12_24=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")

tmp=data01
tmle01_12=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")

data02$treatment[data02$treatment==2]<-1
tmp<-data02
tmle02_12=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")

tmp=data12 %>% mutate(treatment=as.numeric(treatment)-1)
tmle12_12=tmle(Y = tmp$Y,
               A = tmp$treatment,
               W = tmp %>% dplyr::select(c(size,number)),
               Delta = tmp$delta,
               family = "binomial")

#km curve
kmdata <- bladder1


kmdata <- kmdata %>% filter(enum == 1) %>%
  dplyr::select(-c(rtumor,rsize,enum,recur))
kmdata<-kmdata %>% mutate(time=stop-start)

## next change status = 3 to 1
## assume death is type of censoring
kmdata$status[kmdata$status %in% c(2,3)] <- 0
kmcurve=survfit(Surv(kmdata$time,kmdata$status)~kmdata$treatment)
survdiff(Surv(kmdata$time,kmdata$status)~kmdata$treatment)
ggsurvplot(kmcurve,data=kmdata)





