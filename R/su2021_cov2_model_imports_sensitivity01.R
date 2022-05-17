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
#setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/data")

z = as.numeric(commandArgs(trailingOnly=TRUE))

# load data
cases_summer <- read.csv("cases_su.csv", row.names = 1, header=T, sep=",") # add file to Ubelix
cases_su <- subset(cases_summer, year(date) %in% 2021)

# prep data
Re_all <- read.csv("Re_all_2022-03-11.csv")
dispersion_parameters <- read.csv("dispersion_parameters_2022-03-11.csv")
noimport_model_output <- readRDS("sensitivity01_noimport_model_output2021_2022-03-17.rds")

# prep data
time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))

cases_su$date <- as_date(cases_su$date)
period <- c(time_window[1]:time_window[2])
max_time <- length(period)
Re_all <- Re_all[,2]
dispersion_parameters <- dispersion_parameters[,2]

# Initialize simulation
#####
generation_time <- mu <- 5.2
sigma <- 1.72
variance <- sigma^2
gamma_rate <- mu/variance #gamma_shape/generation_time
gamma_shape <- mu^2/variance

secondary <-c()
secondary_t <- c()
# imports:
imports_d <- cases_su[, c("date","cases_abroad")]
weighting <- sum(cases_su$cases_date)/(sum(cases_su$cases_swiss)+sum(cases_su$cases_abroad))
imports_d$abroad_weighted <- round(cases_su$cases_abroad*weighting)
imports_d$abroad_150_weighted <- round(cases_su$cases_abroad*1.5*weighting)
imports_d$abroad_50_weighted <- round(cases_su$cases_abroad*0.5*weighting)
import_num <- c(0,colSums(imports_d[,-1]))


model_outputs <- data.frame(array(as.numeric(0), dim = c(length(Re_all), 7+max_time)))
colnames(model_outputs) <- c("seeds","imports","Re","dispersion_parameter_noimports","dispersion_parameter_imports","cum_cases","final_incidence")
cases_d_runs <- data.frame(array(0, dim = c(max_time,1)))

ti <- c()

# Stochastic branching model of imports
su2020_imports <- function(Re, dispersion, imports){
  ti <- rep(c(1:max_time),imports)#ti <- rep(c(15:max_time),imports[15:max_time])
  foreach(Ri=1:length(Re)) %dopar% {#lapply(Re,function(R) {
    R <- Re[Ri]
    k <- sample(dispersion, size=1)#for (i in 1:length(dispersion[1:2])) { #sapply(1:length(dispersion),function(i) {
    secondary_t <- ti
    while(length(secondary_t<max_time+0.5) >0 & sum(cases_d_runs[,1])<1e6) {
      secondary <- rnbinom(length(secondary_t), size = k, mu = R)
      secondary_t <- rep(secondary_t[secondary_t<(max_time+0.5)], secondary[secondary_t<(max_time+0.5)])
      secondary_t <- secondary_t + round(rgamma(length(secondary_t), shape = gamma_shape, rate = gamma_rate))
      cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])),rownames(cases_d_runs)),1] <-  cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])),rownames(cases_d_runs)),1] + table(secondary_t[secondary_t>0&secondary_t<(max_time+0.5)])
    }
    model_outputs[Ri,]<- c(noimport_model_output[grep(R,noimport_model_output[,3]),1],import_num[(z+1)],R,noimport_model_output[grep(R,noimport_model_output[,3]),4],k,(sum(cases_d_runs[,1])+noimport_model_output[grep(R,noimport_model_output[,3]),6]),cases_d_runs[max_time,1]+noimport_model_output[grep(R,noimport_model_output[,3]),7], (cases_d_runs[,1]+noimport_model_output[grep(R,noimport_model_output[,3]),c(8:(max_time+7))]))
    return(model_outputs[Ri,])}
}
model_outputs <- su2020_imports(Re_all, dispersion_parameters, imports_d[,(z+1)])
model_outputs <- as.data.frame(do.call(rbind, lapply(model_outputs, `length<-`, max(lengths(model_outputs)))))
saveRDS(model_outputs, paste0("import_model_output2021_01_",z,"_",Sys.Date(),".rds"))
