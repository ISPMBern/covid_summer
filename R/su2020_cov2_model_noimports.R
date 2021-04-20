
# Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021

#library
#library(eps)
library(MASS)
library(lubridate)
library(MCMCglmm)
library(RDS)

# Set seed
set.seed(60321)

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
#z = as.numeric(commandArgs(trailingOnly=TRUE))

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

period <- c(time_window[1]:time_window[2])
max_time <- length(period)
dispersion_parameters <- as.vector(rtnorm(1e3,mean=0.51,lower=0.49,upper=0.52))

secondary <-c()
secondary_t <- c()
cases_d_runs <- data.frame(array(0, dim = c(max_time,length(dispersion_parameters))))

seeds <- data.frame(date=as_date((time_window[1]-7):(time_window[1]-1)))
seeds$weekend <- ifelse(weekdays(seeds$date) == "Saturday" | weekdays(seeds$date) == "Sunday", 1, 0)
fit <- glm.nb(cases_reported ~ date + weekend, data = swiss_cases_su2020)
est_interval_fun <- function(data, model){
  data[,c("mean", "ll", "ul")] <- predict(model, data, type = "link", se.fit=TRUE)
  data[,"ul"] <- exp(data[,"mean"] + 1.96 * data[, "ll"])
  data[, "ll"] <- exp(data[,"mean"] - 1.96 * data[, "ll"])
  data[,"mean"] <- exp(data[,"mean"])
  return(data)
}
seeds <- est_interval_fun(seeds,fit)
min_max_seeds <- c(min(seeds[,-c(1:2)]):max(seeds[,-c(1:2)]))


#####


# Stochastic branching model without imports
#####
su2020_cases<- function(Re, dispersion){
  lapply(Re,function(R) {
    cat(R)
    sapply(1:length(dispersion),function(i) {
      k <- dispersion[i]
      seeds$seeds_d <- round(sample(min_max_seeds, length(seeds$date), replace = TRUE))
      t0  <- rep(as.numeric(seeds[,1])-as.numeric(time_window[1]-1), seeds$seeds_d)
      secondary_t <- t0
      while(length(secondary_t<max_time+0.5) >0 & sum(cases_d_runs[,i])<1e6) {
        secondary <- rnbinom(length(secondary_t), size = k, mu = R)
        secondary_t <- rep(secondary_t[secondary_t<(max_time+0.5)], secondary[secondary_t<(max_time+0.5)])
        secondary_t <- secondary_t + round(rgamma(length(secondary_t), shape = gamma_shape, rate = gamma_rate))
        cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])),rownames(cases_d_runs)),i] <-  cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])),rownames(cases_d_runs)),i] + table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])
      }
      return(cases_d_runs[,i])
    })
  })
}

cases_d_runs_all <- su2020_cases(Re_all, dispersion_parameters)
saveRDS(cases_d_runs_all, paste0("cases_d_noimports_",Sys.Date(),".rds"))

