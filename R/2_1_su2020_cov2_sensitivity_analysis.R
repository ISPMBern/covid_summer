# Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021

#library
library(MASS)
library(lubridate)


# Sensitivity analysis using k=0.1 and =1:

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/data/sensitivity")#https://github.com/owid/covid-19-data/tree/master/public/data

# load data
Re_all <- read.csv("../Re_all_2021-05-20.csv")
dispersion_parameters01 <- read.csv("dispersion_parameters_2021-05-25.csv")
dispersion_parameters1 <- read.csv("dispersion_parameters_2021-05-26.csv")

noimport_model_output01 <- readRDS("noimport_model_output_2021-05-25.rds")
noimport_model_output1 <- readRDS("noimport_model_output_2021-05-26.rds")

imports <- list.files( pattern="imports_model_outputs_*", full.names=TRUE, recursive=FALSE)
imports01 <- imports[grepl("2021-05-25", imports)]
import_models_output01 <- c()
for (i in 1:length(imports01)) {
  new <- readRDS(imports01[i])
  import_models_output01 <- rbind(import_models_output01,new)
}
models_output01 <- rbind(noimport_model_output01,import_models_output01)
models_output01 <- as.data.frame(models_output01)
models_output01$simulation_accepted  <- sapply(1:length(models_output01[,1]),FUN=accept_fun)
simulation_accept01 <- models_output[models_output01$simulation_accepted==1,]
length(simulation_accept01[,1])

for (i_name in names(table(import_num))) {
  print(length(simulation_accept01$Re[simulation_accept01$imports %in% i_name]))
}
for (i_name in names(table(import_num))) {
  print(quantile((simulation_accept01$Re[simulation_accept01$imports %in% i_name]),probs))
}

imports1 <- imports[grepl("2021-05-26", imports)]
import_models_output1<- c()
for (i in 1:length(imports1)) {
  new <- readRDS(imports1[i])
  import_models_output1 <- rbind(import_models_output1,new)
}
new <- rm
models_output1 <- rbind(noimport_model_output1,import_models_output1)
models_output1 <- as.data.frame(models_output1)
models_output1$simulation_accepted  <- sapply(1:length(models_output1[,1]),FUN=accept_fun)
simulation_accept1 <- models_output[models_output1$simulation_accepted==1,]
length(simulation_accept1[,1])

for (i_name in names(table(import_num))) {
  print(length(simulation_accept1$Re[simulation_accept1$imports %in% i_name]))
}

for (i_name in names(table(import_num))) {
  print(quantile((simulation_accept1$Re[simulation_accept1$imports %in% i_name]),probs))
}
