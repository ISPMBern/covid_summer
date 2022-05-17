
# Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021

#library
#library(eps)
library(MASS)
library(lubridate)
library(MCMCglmm)
library(RDS)
library(doParallel)
library(foreach)
registerDoParallel(cores=10)
# Set seed
set.seed(60321)

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
#z = as.numeric(commandArgs(trailingOnly=TRUE))
#setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/data")

# load data
cases_summer <- read.csv("cases_su.csv", row.names = 1, header=T, sep=",") # add file to Ubelix


# prep data
probs = c(.025,.5,.975)
#only first time:
Re_all <- numeric(1e5)
while(length(unique(Re_all)) !=1e5) {
Re_all <- as.vector(runif(1e5,0.5,1.5))
}
Re_all <- Re_all[order(Re_all)]
write.csv(Re_all, paste0("Re_all_",Sys.Date(),".csv"))
Re_all <- read.csv(paste0("Re_all_",Sys.Date(),".csv"))
Re_all <- Re_all[,2]

#only first time:
dispersion_parameters <- as.vector(rtnorm(1e5,mean=0.51,lower=0.49,upper=0.52))#on 2021-09-18#on 2021-05-20
dispersion_parameters <- dispersion_parameters[order(dispersion_parameters)]
#dispersion_parameters <- rep(0.1,10^5)#on 2021-05-25
#dispersion_parameters <- rep(1,10^5)#on 2021-05-26
write.csv(dispersion_parameters, paste0("dispersion_parameters_",Sys.Date(),".csv"))
dispersion_parameters <- read.csv(paste0("dispersion_parameters_",Sys.Date(),".csv"))
dispersion_parameters <- dispersion_parameters[,2]

# Initialize simulation
#####
generation_time <- mu <- 5.2
sigma <- 1.72
variance <- sigma^2
gamma_rate <- mu/variance #gamma_shape/generation_time
gamma_shape <- mu^2/variance


for (i in c("2020","2021")) {
#for (i in c("2021")) {
  if(i=="2020"){
    cases_su <- subset(cases_summer, year(date) %in% 2020)
    
  }
  else if(i=="2021"){
    cases_su <- subset(cases_summer, year(date) %in% 2021)
    
  }
  time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
  period <- c(time_window[1]:time_window[2])
  max_time <- length(period)
  
  cases_d_runs <- data.frame(array(0, dim = c(max_time,1)))
  cases_su$date <- as_date(cases_su$date)
  cases_su$weekend <- ifelse(weekdays(cases_su$date) == "Saturday" | weekdays(cases_su$date) == "Sunday", 1, 0)
  
seeds <- data.frame(date=as_date((time_window[1]-7):(time_window[1]-1)))
seeds$weekend <- ifelse(weekdays(seeds$date) == "Saturday" | weekdays(seeds$date) == "Sunday", 1, 0)
fit <- glm.nb(cases_date ~ date + weekend, data = cases_su)
est_interval_fun <- function(data, model){
  data[,c("mean", "ll", "ul")] <- predict(model, data, type = "link", se.fit=TRUE)
  data[,"ul"] <- exp(data[,"mean"] + 1.96 * data[, "ll"])
  data[, "ll"] <- exp(data[,"mean"] - 1.96 * data[, "ll"])
  data[,"mean"] <- exp(data[,"mean"])
  return(data)
}
seeds <- est_interval_fun(seeds,fit)
min_max_seeds <- round(c(min(seeds[,-c(1:2)]):max(seeds[,-c(1:2)])))

secondary <-c()
secondary_t <- c()

#####
model_outputs <- data.frame(array(as.numeric(0), dim = c(length(Re_all), 7+max_time)))
colnames(model_outputs) <- c("seeds","imports","Re","dispersion_parameter_noimports","dispersion_parameter_imports","cum_cases","final_incidence")

# Stochastic branching model without imports
#####
su2020_cases<- function(Re, dispersion){
  foreach(Ri=1:length(Re)) %dopar% {#lapply(Re,function(Ri) {
    R<-Re[Ri]
    cases_d_runs[,1] <- 0
      k <- sample(dispersion, size=1)
      seeds$seeds_d <- round(sample(min_max_seeds, length(seeds$date), replace = TRUE))
      t0  <- rep(as.numeric(seeds[,1])-as.numeric(time_window[1]-1), seeds$seeds_d)
      secondary_t <- t0
      while(length(secondary_t<max_time+0.5) >0 & sum(cases_d_runs[,1])<1e6) {
        secondary <- rnbinom(length(secondary_t), size = k, mu = R)
        secondary_t <- rep(secondary_t[secondary_t<(max_time+0.5)], secondary[secondary_t<(max_time+0.5)])
        secondary_t <- secondary_t + round(rgamma(length(secondary_t), shape = gamma_shape, rate = gamma_rate))
        cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])),rownames(cases_d_runs)),1] <-  cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])),rownames(cases_d_runs)),1] + table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])
      }
      model_outputs[Ri,]<- c(sum(seeds$seeds_d),0,R,k,NA,sum(cases_d_runs),cases_d_runs[max_time,1], cases_d_runs[,1])
      return(model_outputs[Ri,])}#)
}

model_outputs <- su2020_cases(Re_all, dispersion_parameters)
model_outputs <- as.data.frame(do.call(rbind, lapply(model_outputs, `length<-`, max(lengths(model_outputs)))))

if(i=="2020"){
  saveRDS(model_outputs, paste0("noimport_model_output2020_",Sys.Date(),".rds"))
}
else if(i=="2021"){
  saveRDS(model_outputs, paste0("noimport_model_output2021_",Sys.Date(),".rds"))
}
}
