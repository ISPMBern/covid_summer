
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
getDoParWorkers()
# Set seed
set.seed(60321)

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/data")

# load data
cases_su2020 <- read.csv("cases_su2020.csv", row.names = 1, header=T, sep=",")

# prep data
time_window <- c(as_date("2020-06-01"), as_date("2020-09-30"))
cases_su2020$date <- as_date(cases_su2020$date)
swiss_cases_su2020 <- subset(cases_su2020, date %in% seq(time_window[1],time_window[2],1))

swiss_cases_su2020$weekend <- ifelse(weekdays(swiss_cases_su2020$date) == "Saturday" | weekdays(swiss_cases_su2020$date) == "Sunday", 1, 0)

probs = c(.025,.5,.975)

# Initialize simulation
#####

generation_time <- mu <- 5.2
sigma <- 1.72
variance <- sigma^2
gamma_rate <- mu/variance #gamma_shape/generation_time
gamma_shape <- mu^2/variance

Re_all <- as.vector(runif(1e3,0.8,1.2))
runs <- 1e4
period <- c(time_window[1]:time_window[2])
max_time <- length(period)
dispersion_parameters <- as.vector(rtnorm(1e3,mean=0.51,lower=0.49,upper=0.52))

secondary <-c()
secondary_t <- c()
# imports:
imports_d <- swiss_cases_su2020[, c("date","cases_abroad")]
weighting <- 1+((sum(swiss_cases_su2020$cases_swiss)+sum(swiss_cases_su2020$cases_abroad))/sum(swiss_cases_su2020$cases_reported))

imports_d$abroad_weighted <- round(imports_d$cases_abroad*weighting)
imports_d$abroad2  <- round(swiss_cases_su2020$cases_abroad*2)
imports_d$abroad2_weighted <- round(swiss_cases_su2020$cases_abroad*2*weighting)
imports_d$abroad3  <- round(swiss_cases_su2020$cases_abroad*3)
imports_d$abroad3_weighted <- round(swiss_cases_su2020$cases_abroad*3*weighting)
import_num <- c(0,colSums(imports_d[,-1]))

cases_d_runs <- data.frame(array(0, dim = c(max_time,length(dispersion_parameters))))
#z = as.numeric(commandArgs(trailingOnly=TRUE))
ti <- c()
# Stochastic branching model of imports
su2020_imports <- function(Re, dispersion, imports){
  lapply(1:length(imports[1,]),function(I) {
    ti <- rep(c(15:max_time),imports[c(15:max_time),I])#ti <- rep(c(15:max_time),imports[15:max_time])
    foreach(Ri=1:length(Re)) %dopar% {#lapply(Re,function(R) {
      R <- Re[Ri]
      sapply(1:length(dispersion),function(i) {#for (i in 1:length(dispersion[1:2])) { #sapply(1:length(dispersion),function(i) {
        k <- dispersion[i]
        secondary_t <- ti
        while(length(secondary_t<max_time+0.5) >0 & sum(cases_d_runs[,i])<1e6) {
          secondary <- rnbinom(length(secondary_t), size = k, mu = R)
          secondary_t <- rep(secondary_t[secondary_t<(max_time+0.5)], secondary[secondary_t<(max_time+0.5)])
          secondary_t <- secondary_t + round(rgamma(length(secondary_t), shape = gamma_shape, rate = gamma_rate))
          cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])),rownames(cases_d_runs)),i] <-  cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])),rownames(cases_d_runs)),i] + table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])
        }
        return(cases_d_runs[,i])
      })
    }#)
  })
}
#imports_cases_d_runs_all <- su2020_imports(Re_all, dispersion_parameters, imports_d[,1+z])
imports_cases_d_runs_all <- su2020_imports(Re_all, dispersion_parameters, imports_d[,-1])
#saveRDS(imports_cases_d_runs_all, paste0("cases_d_imports_",Sys.Date(),".rds"))

#imports_cases_d_runs_all <- su2020_imports(Re_all[967], dispersion_parameters[1:4], imports_d[,c(2,3)])
#saveRDS(imports_cases_d_runs_all, paste0("cases_d_imports_",z,"_",Sys.Date(),".rds"))

