# Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021

#library
library(MASS)
library(lubridate)
# Set seed
set.seed(60321)


# Sensitivity analysis using k=0.1 and =1:

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/data/sensitivity")#https://github.com/owid/covid-19-data/tree/master/public/data

# load data
BAG_data_su<- read.csv("../BAG_data.csv")
cases_summer <- read.csv("../cases_su.csv")
Re_all <- read.csv("../Re_all_2022-03-11.csv")


imports <- list.files( pattern="import_model_*", full.names=TRUE, recursive=FALSE)
#noimports <- imports[grepl("sensitivity_*", imports)]
#imports <- imports[!grepl("sensitivity_*", imports)]
probs = c(.025,.5,.975)


for(i in c(2020, 2021)){
  import_all <- imports[grepl(i, imports)]
  if(i==2020){
    cases_su <- read.csv("../2020/cases_su2020.csv", row.names = 1)
  }
  if(i==2021){
    cases_su <- read.csv("../2021/cases_su2021.csv", row.names = 1)
  }
  
  cases_su$date <- as_date(cases_su$date)
  # Fit cases to generalized negative binomial model
  r_all <- c()
  fit <- glm.nb(cases_date ~ date , data = cases_su)
  r_all <- c(coef(summary(fit))[2, 1]- 1.96 * coef(summary(fit))[2, 2],coef(summary(fit))[2, 1],coef(summary(fit))[2, 1]+ 1.96 * coef(summary(fit))[2, 2])
  model_se <- summary(fit)$SE.theta
  
  # inputs to calculate Re
  generation_time <- mu <- 5.2
  sigma <- 1.72
  variance <- sigma^2
  gamma_rate <- mu/variance #gamma_shape/generation_time #1/scale #scale= variance/mu
  gamma_shape <- mu^2/variance #alpha
  #calculate Re
  repro <- function(growth) {
    (1 + growth/gamma_rate)^gamma_shape
  }
  observed_Re <- repro(r_all)
  
  # within expectation of number of cases (that were reported)
  n_sim <- 10^6
  cases <- numeric(n_sim)
  # potential final incidence
  cum <- cases_su$cases_date
  final <- tail(cum, 7)
  round(mean(final))
  sum(cum)
  cum_inc<- final_inc <- numeric(n_sim)
  final_inc <- numeric(n_sim)
  for(j in 1:n_sim) {
    cum_inc[j] <- sum(rnbinom(length(cum), size = model_se, mu = cum))
    final_inc[j] <- sum(rnbinom(length(final), size = model_se, mu = final))
  }
  cum_final_expected <- round(rbind(quantile(cum_inc, probs), quantile(final_inc, probs)/7))
  cum_expected_seq <- c(cum_final_expected[1,1]:cum_final_expected[1,3])
  final_expected_seq <- c(cum_final_expected[2,1]:cum_final_expected[2,3])
  
  imports_d <- cases_su[, c("date","cases_abroad")]
  weighting <- sum(cases_su$cases_date)/(sum(cases_su$cases_swiss)+sum(cases_su$cases_abroad))
  imports_d$abroad_a <- round(cases_su$cases_abroad*weighting)
  imports_d$abroad_b <- round(cases_su$cases_abroad*0.5*weighting)
  imports_d$abroad_c <- round(cases_su$cases_abroad*1.5*weighting)
  import_num <- c(0,colSums(imports_d[,-1]))
  imports_d$date <- as_date(imports_d$date)
  table4_all <- c()
  for (n in 1:2) {
    if(n==1){
      import <- import_all[grepl("01", import_all)]
    }
    if(n==2){
      import <- import_all[!grepl("01", import_all)]
    }
    models_output <- c()
  for (z in 1:length(import)) {
    new <- readRDS(import[z])
    models_output <- rbind(models_output,new)
}
  models_output$cum_cases <- as.numeric(models_output$cum_cases)
  models_output$final_incidence <- as.numeric(models_output$final_incidence)
  models_output$simulation_accepted <- 0
  models_output$simulation_accepted[models_output$cum_cases %in% cum_expected_seq & models_output$final_incidence %in% final_expected_seq] <-1
  simulation_accept <- models_output[models_output$simulation_accepted==1,]
  
  print(i)
  print(n)
  print(paste0("All ",length(simulation_accept[,1])))
  table4$Year <- i
  table4$`Overdispersion parameters` <- c("0.1","1")[n]
  table4$`Import scenario` <- c("Baseline scenario", "Confirmed cases exposed abroad","Scenario a)","Scenario b)","Scenario c)")
  
  for (i_name in unname(unlist(import_num))) {
    print(length(simulation_accept$Re[simulation_accept$imports %in% i_name]))
  }
  
  for (i_name in unname(unlist(import_num))) {
    table4$`Re (95%-CrI)`[grep(i_name, import_num)] <- paste0(round(quantile(simulation_accept$Re[simulation_accept$imports %in% i_name],probs),2)[2], " (", round(quantile(simulation_accept$Re[simulation_accept$imports %in% i_name],probs),2)[1],"-",round(quantile(simulation_accept$Re[simulation_accept$imports %in% i_name],probs),2)[3],")")  
    print(round(quantile((simulation_accept$Re[simulation_accept$imports %in% i_name]),probs),2))
  }
  table4_all<- rbind(table4_all,table4)

  }
    table_all <- rbind(table_all,table4_all)
}
table_all$Year <- as.character(table_all$Year)
table4 <- xtable(table_all)
table4 <- xtable(caption = "Results of simulated epidemic trajectories of the SARS-CoV-2 epidemic in Switzerland during summer 2020 and 2021 for all scenarios including sensitivity analyses (overdispersion parameter = 0.1 and 1).",
                 label = "t2", table4)
table4 <- print(table4, size = "footnotesize", include.rownames = FALSE, include.colnames = TRUE)
write(table4, file = "../table/table4.tex")


