
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
#registerDoParallel(cores=Sys.getenv("SLURM_CPUS_PER_TASK"))
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
cases_su2020 <- rm
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

# imports:
imports_d <- swiss_cases_su2020[, c("date","cases_abroad")]
weighting <- 1+((sum(swiss_cases_su2020$cases_swiss)+sum(swiss_cases_su2020$cases_abroad))/sum(swiss_cases_su2020$cases_reported))

imports_d$abroad_weighted <- round(imports_d$cases_abroad*weighting)
imports_d$abroad2  <- round(swiss_cases_su2020$cases_abroad*2)
imports_d$abroad2_weighted <- round(swiss_cases_su2020$cases_abroad*2*weighting)
imports_d$abroad3  <- round(swiss_cases_su2020$cases_abroad*3)
imports_d$abroad3_weighted <- round(swiss_cases_su2020$cases_abroad*3*weighting)
import_num <- c(0,colSums(imports_d[,-1]))

#####

# add imports and their dynamic to local dynamic (could also include different combinations)
all_cases_imports_infect <- list()
noimports <- list.files( pattern="cases_d_noimports.rds", full.names=TRUE, recursive=FALSE)
imports <- list.files( pattern="cases_d_imports_*", full.names=TRUE, recursive=FALSE)
imports <- imports[grepl("2021-04-16", imports)]
all_cases_imports_infect <- readRDS(noimports[1])
all_cases_imports_infect <- lapply(seq_len((1+length(imports))), function(X) all_cases_imports_infect)
for (I in 1:(1+length(imports))) {
  if(I!=1){
    imports_infect <- readRDS(imports[I-1])
    for (R in 1:length(Re_all)) {
      all_cases_imports_infect[[I]][[R]] <- imports_infect[[1]][[R]] + all_cases_imports_infect[[I]][[R]] 
    }
  }
}
imports_infect <- rm

#####

#####
model_outputs <- data.frame(array(as.numeric(0), dim = c((1+length(imports))*length(Re_all), 11)))
values <- data.frame(array(as.numeric(0), dim = c(3,10^3)))
# sum up growth rates, final size, cumulative cases
summary<-   sapply(1:(length(imports)+1),function(I) {
    foreach(Ri=1:length(Re_all), .combine=c) %dopar% {
      R <- as.numeric(Ri)
      m <- (I-1)*length(Re_all)+R
      model_outputs[m,1] <- import_num[I]
      model_outputs[m,2] <- Re_all[R]
      input <- all_cases_imports_infect[[I]][[R]]
      for (k in 1:length(dispersion_parameters)) {
        values[1,k] <- input[max_time,k]
        values[2,k] <- sum(input[,k])
        tryCatch(values[3,k] <- coef(glm.nb(input[,k]~swiss_cases_su2020$date,link = "log"))[2])
      }
      model_outputs[m,3:5] <- quantile(as.numeric(values[1,]),probs)
      model_outputs[m,6:8] <- quantile(as.numeric(values[2,]),probs)
      model_outputs[m,9:11] <- quantile(as.numeric(values[3,]),probs)
      unlist(model_outputs[m,])
      
    }#)
  })

sum_split <- split(as.vector(summary), as.integer(gl(length(as.vector(summary)), 11, length(as.vector(summary)))))
growth_cum_final_summary <- as.data.frame(do.call(rbind, lapply(sum_split, `length<-`, max(lengths(sum_split)))))
colnames(growth_cum_final_summary) <- c("imports","Re","final_size_ll","final_size_median","final_size_ul","cum_cases_ll","cum_cases_median","cum_cases_ul","growth_r_ll","growth_r_median","growth_r_ul")
saveRDS(growth_cum_final_summary, paste0("growth_cum_final_summary_",Sys.Date(),".rds"))


