
# Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021

#library
library(MASS)
library(lubridate)
library(xtable)
library(MCMCglmm)

# Set seed
set.seed(60321)

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/data")

# load (additional data) data
# get data from KOF (ETHZ): Stringency Index
KOF <- read.csv(paste0("https://datenservice.kof.ethz.ch/api/v1/public/sets/stringency_plus_web?mime=csv&df=Y-m-d.csv"))
BAG_data_su2020 <- read.csv("BAG_data_su2020.csv",row.names = 1, header=T, sep=",")
growth_cum_final_summary <- readRDS(paste0("growth_cum_final_summary_2021-04-19.rds"))
cases_su2020 <- read.csv("cases_su2020.csv", row.names = 1, header=T, sep=",")

# prep data
probs = c(.025,.5,.975)
time_window <- c(as_date("2020-06-01"), as_date("2020-09-30"))
period <- c(time_window[1]:time_window[2])
max_time <- length(period)
cases_su2020$date <- as_date(cases_su2020$date)
swiss_cases_su2020 <- subset(cases_su2020, date %in% seq(time_window[1],time_window[2],1))
cases_su2020 <- rm
swiss_cases_su2020$weekend <- ifelse(weekdays(swiss_cases_su2020$date) == "Saturday" | weekdays(swiss_cases_su2020$date) == "Sunday", 1, 0)
BAG_data_su2020$date <- as_date(BAG_data_su2020$date)
BAG_data_su2020 <- subset(BAG_data_su2020, date %in% seq(time_window[1],time_window[2],1))
KOF[,"date"] <- seq(as_date("2020-01-01"),(as_date("2020-01-01")+length(KOF[,"date"])-1),1)
KOF_su2020 <- subset(KOF, date %in% seq(time_window[1],time_window[2],1))

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



# Estimating incidence
swiss_cases_su2020$incidence <- swiss_cases_su2020$cases_reported/8697905*100000 #https://www.worldometers.info/world-population/switzerland-population/
before <- 3
after <- 3
incidence_fun <- function(x){
  set <- subset(cases_su2020, cases_su2020$date >= (as_date(x) - before) & cases_su2020$date <= (as_date(x) + after))
  incidence_weigthed<- mean(set$cases_reported)/8697905*100000
  return(incidence_weigthed)
}
swiss_cases_su2020$incidence_weigthed <- sapply(swiss_cases_su2020$date, incidence_fun)

#imports fraction
imports_d <- swiss_cases_su2020[, c("date","cases_abroad")]
weighting <- 1+((sum(swiss_cases_su2020$cases_swiss)+sum(swiss_cases_su2020$cases_abroad))/sum(swiss_cases_su2020$cases_reported))
imports_d$abroad_weighted <- round(imports_d$cases_abroad*weighting)
imports_d$abroad2  <- round(swiss_cases_su2020$cases_abroad*2)
imports_d$abroad2_weighted <- round(swiss_cases_su2020$cases_abroad*2*weighting)
imports_d$abroad3  <- round(swiss_cases_su2020$cases_abroad*3)
imports_d$abroad3_weighted <- round(swiss_cases_su2020$cases_abroad*3*weighting)
import_num <- c(0,colSums(imports_d[,-1]))
swiss_cases_su2020$imports_frac_all <- swiss_cases_su2020$cases_abroad/(swiss_cases_su2020$cases_reported)
swiss_cases_su2020$imports_frac_known <- swiss_cases_su2020$cases_abroad/(swiss_cases_su2020$cases_abroad+swiss_cases_su2020$cases_swiss)

#exposed countries
BAG_data_su2020$country <- factor(BAG_data_su2020$country, levels=unique(names(table(BAG_data_su2020$country))[order(table(BAG_data_su2020$country), decreasing = TRUE)]), ordered=TRUE)
BAG_data_su2020$country <- factor(BAG_data_su2020$country, levels=c(levels(BAG_data_su2020$country)[!levels(BAG_data_su2020$country) %in% c("Others","Unknown")],c("Others","Unknown")), ordered=TRUE)

## restriction, measures, quarantine
### school holidays (https://schulferien-ch.ch/schulferien-2020/) #if several dates widest range
su2020_schoolbreak <- data.frame(label= c("AG", "AR","AI", "VL", 
                                          "BS", "BE", "FR", "GE", 
                                          "GL", "GR",
                                          "JU", "LU","NE", "NW", 
                                          "OW", "SH", "SZ", "SO",
                                          "SG", "TI", "TG", "UR", 
                                          "VD", "VS", "ZG", "ZH"),
                                 start_date=c("2020-07-18", "2020-07-04","2020-07-04", "2020-06-27", 
                                              "2020-06-27","2020-06-27","2020-06-27","2020-06-27",
                                              "2020-06-27","2020-06-27",
                                              "2020-07-04", "2020-07-04","2020-07-04", "2020-07-04",
                                              "2020-07-04", "2020-07-04", "2020-07-04","2020-07-04",
                                              "2020-07-04", "2020-06-20", "2020-07-04", "2020-07-04",
                                              "2020-06-27","2020-06-20", "2020-07-04", "2020-07-11"),
                                 end_date=c("2020-08-09","2020-08-09","2020-08-16","2020-08-09",
                                            "2020-08-09","2020-08-16","2020-08-26", "2020-08-23",
                                            "2020-08-09", "2020-08-23",
                                            "2020-08-16", "2020-08-16","2020-08-16", "2020-08-23", 
                                            "2020-08-16", "2020-08-09", "2020-08-09", "2020-08-09",
                                            "2020-08-09","2020-08-30", "2020-08-09", "2020-08-16",
                                            "2020-08-23", "2020-08-16", "2020-08-16", "2020-08-16"),
                                 end_date_dot=c("2020-08-09","2020-08-09","2020-08-16","2020-08-09",
                                                "2020-08-09","2020-08-16","2020-08-26", "2020-08-23",
                                                "2020-08-09", "2020-08-23",
                                                "2020-08-16", "2020-08-16","2020-08-16", "2020-08-23", 
                                                "2020-08-16", "2020-08-09", "2020-08-09", "2020-08-09",
                                                "2020-08-09","2020-08-30", "2020-08-09", "2020-08-16",
                                                "2020-08-23", "2020-08-16", "2020-08-16", "2020-08-16"))
schoolbreak <- matrix(ncol = length(seq(time_window[1],time_window[2],1)), nrow = length(su2020_schoolbreak$label))
for (c in 1:length(su2020_schoolbreak$label)){
  schoolbreak[c,c(1:length(seq(as_date(su2020_schoolbreak$start_date[c]), as_date(su2020_schoolbreak$end_date[c]), 1)))]<- seq(as_date(su2020_schoolbreak$start_date[c]), as_date(su2020_schoolbreak$end_date[c]), 1)
}
school_date_min <- time_window[1]
school_date_max <- time_window[2]
for (c in 1:length(su2020_schoolbreak$label)){
  if(school_date_min < as_date(min(na.omit(schoolbreak[c,])))){
    school_date_min <- as_date(min(na.omit(schoolbreak[c,])))
  }
  if(school_date_max > as_date(max(na.omit(schoolbreak[c,])))){
    school_date_max <- as_date(max(na.omit(schoolbreak[c,])))
  }
}

### university holidays #https://www.swissuniversities.ch/service/studieren/studieren-in-der-schweiz/hochschulkalender
su2020_unibreak <- data.frame(label=c("all"),
                              start_date=c("2020-06-05"),
                              end_date=c("2020-09-13"),
                              end_date_dot=c("2020-09-13"))

### quarantine #https://www.fedlex.admin.ch/eli/cc/2020/496/en
## The number of new infections in the country or area concerned in the past 14 days is more than 60 per 100 000 persons, and this number is not due to spe-cific incidents or cases occurring in a narrowly defined geographical area.
# into operation on 2020-07-06: 10d quarantine if traveled from: Argentina, Armenia, Azerbaijan, Bahrain, Belarus, Bolivia, Brazil, Chile, Dominican Republic, Honduras, Iraq, Israel, Cap Verde, Qatar, Colombia, Kosovo, Kuwait, Moldova, North Macedonia, Oman, Panama, Peru, Russia, Saudi Arabia, Sweden, Serbia, RSA, Turks and Caicos Islands, USA
# into operation on 2020-07-23: 10d quarantine if traveled from: Argentina Armenia Azerbaijan Bahrain Bolivia Bosnia and Herzegovina Brazil Cape Verde Chile Colombia Costa Rica Dominican Republic Ecuador El Salvador Eswatini (Swaziland) Guatemala Honduras Iraq Israel Kazakhstan Kosovo Kuwait Kyrgyzstan Luxembourg Maldives Mexico Moldova Montenegro North Macedonia Occupied Palestinian Territory Oman Panama Peru Qatar Russia Saudi Arabia Serbia South Africa Suriname Turks and Caicos Islands United Arab Emirates United States of America  (including Puerto Rico and the United States Virgin Islands)
# into operation on 2020-08-08: 10d quarantine if traveled from: Argentina Armenia Bahamas Bahrain Bolivia Bosnia and Herzegovina Brazil Cape Verde Chile Colombia Costa Rica Dominican Republic Ecuador El Salvador Equatorial Guinea Eswatini (Swaziland) Guatemala Honduras Iraq Israel Kazakhstan Kosovo Kuwait Kyrgyzstan Luxembourg Maldives Mexico Moldova Montenegro North Macedonia Occupied Palestinian Territory Oman Panama Peru Qatar Romania Sao Tomé and Principe Saudi Arabia Serbia Singapore Sint Maarten South Africa Spain (with the exception of the Balearic and Canary Islands) Suriname Turks and Caicos Islands United States of America  (including Puerto Rico and the United States Virgin Islands)
# into operation on 2020-08-15: 10d quarantine if traveled from: Argentina Armenia Bahamas Bahrain Bolivia Bosnia and Herzegovina Brazil Cape Verde Chile Colombia Costa Rica Dominican Republic Ecuador El Salvador Equatorial Guinea Eswatini (Swaziland) Guatemala Honduras Iraq Israel Kazakhstan Kosovo Kuwait Kyrgyzstan Luxembourg Maldives Mexico Moldova Montenegro North Macedonia Occupied Palestinian Territory Oman Panama Peru Qatar Romania Sao Tomé and Principe Saudi Arabia Serbia Singapore Sint Maarten South Africa Spain (with the exception of the Balearic and Canary Islands) Suriname Turks and Caicos Islands United States of America  (including Puerto Rico and the United States Virgin Islands)
# into operation on 2020-08-20: 10d quarantine if traveled from: Albania Andorra Argentina Armenia Aruba Bahamas Bahrain Belgium Belize Bolivia Bosnia and Herzegovina Brazil Cape Verde Chile Colombia Costa Rica Dominican Republic Ecuador El Salvador Eswatini (Swaziland) Faroe Islands Gibraltar Guam Guatemala Honduras India Iraq Israel Kazakhstan Kosovo Kuwait Kyrgyzstan Luxembourg Maldives Malta Mexico Moldova Monaco Montenegro Namibia North Macedonia Occupied Palestinian Territory Oman Panama Peru Qatar Romania Sint Maarten South Africa Spain (with the exception of the Canary Islands) Suriname Turks and Caicos Islands United States of America  (including Puerto Rico and the United States Virgin Islands)
# into operation on 2020-09-07: 10d quarantine if traveled from: Albania Andorra Argentina Armenia Aruba Bahamas Bahrain Belize Bolivia Bosnia and Herzegovina Brazil Cape Verde Chile Colombia Costa Rica Croatia Dominican Republic Ecuador Faroe Islands French Polynesia Gibraltar Guam Guatemala Guyana Honduras India Iraq Israel Kosovo Kuwait Lebanon Libya Maldives Malta Moldova Monaco Montenegro Namibia North Macedonia Occupied Palestinian Territory Panama Paraguay Peru Qatar Romania San Marino Sint Maarten South Africa Spain (with the exception of the Canary Islands) Suriname Trinidad and Tobago Turks and Caicos Islands Ukraine United Arab Emirates United States of America  (including Puerto Rico and the United States Virgin Islands)
# into operation on 2020-09-14: 10d quarantine if traveled from: Albania Andorra Argentina Armenia Aruba Bahamas Bahrain Belize Bolivia Bosnia and Herzegovina Brazil British Virgin Islands Cape Verde Chile Colombia Costa Rica Croatia Czech Republic Dominican Republic Gibraltar Guyana Honduras India Iraq Israel Kosovo Kuwait Lebanon Libya Maldives Malta Moldova Monaco Montenegro Namibia North Macedonia Occupied Palestinian Territory Panama Paraguay Peru Qatar Romania San Marino Sint Maarten Spain Suriname Trinidad and Tobago Turks and Caicos Islands Ukraine United Arab Emirates United States of America  (including Puerto Rico, the United States Virgin Islands and Guam) + high risk regions in France and Austria
# into operation on 2020-09-28: 10d quarantine if traveled from: Albania Andorra Argentina Armenia Bahamas Bahrain Belgium Belize Bolivia Bosnia and Herzegovina Brazil Cape Verde Chile Colombia Costa Rica Croatia Czech Republic Denmark Dominican Republic Ecuador Guyana Honduras Hungary Iceland India Iraq Ireland Israel Jamaica Kuwait Lebanon Libya Luxembourg Maldives Malta Moldova Monaco Montenegro Morocco Namibia Nepal Netherlands North Macedonia Occupied Palestinian Territory Oman Panama Paraguay Peru Portugal Qatar Romania Slovenia Spain Suriname Trinidad and Tobago Ukraine United Arab Emirates United Kingdom United States of America + high risk regions in France, Austria, and Italy
su2020_quarantine <- data.frame(label= c("Greece", "France*","North Macedonia", "Netherlands", 
                                         "Italy", "Kosovo", "Bosnia and Herzegovina", "Portugal", 
                                         "Slovenia", "Albania", "UK", "Hungary",
                                         "Romania", "Germany", "Serbia", "Austria*", 
                                         "Croatia", "Poland","Turkey", "Spain*", 
                                         "Malta", "Czech Republic"),#* not whole country, only region
                                start_date=c(NA,"2020-09-14", "2020-07-06","2020-09-28",
                                             "2020-09-28","2020-07-06", "2020-07-23","2020-09-28",
                                             "2020-09-28","2020-08-20", "2020-09-28", "2020-09-28",
                                             "2020-07-23",NA, "2020-07-06", "2020-09-14",
                                             "2020-09-07",NA,NA,"2020-08-08",
                                             "2020-08-20", "2020-09-14"),
                                end_date=c(NA,"2020-09-30","2020-09-30","2020-09-30",
                                           "2020-09-30","2020-09-28","2020-09-30","2020-09-30",
                                           "2020-09-30","2020-09-30","2020-09-30","2020-09-30",
                                           "2020-09-30",NA, "2020-08-15", "2020-09-30",
                                           "2020-09-30",NA,NA,"2020-09-30",
                                           "2020-09-30","2020-09-30"),# max. date: "2020-09-30"
                                end_date_dot=c(NA,"**","**","**",
                                               "**","**","**","**",
                                               "**","**","**","**",
                                               "**",NA, "**", "**",
                                               "**",NA,NA,"**",
                                               "**","**"))


#restriction combined
restrictions <- data.frame(
  label = c("Open borders:","Mask mandatory in public transport","Swiss Covid App:", "University breaks*:", "Max. range of school breaks*:", "Overlap of all school breaks*:"),
  start_date = as_date(c("2020-06-15","2020-07-20","2020-06-25","2020-06-05", "2020-06-20", paste0(school_date_min))), 
  end_date = as_date(c("2020-09-30","2020-09-30","2020-09-30","2020-09-13", "2020-08-30", paste0(school_date_max))), # max. date: "2020-09-30"
  end_date_dot = as_date(c(format(Sys.time(), "%Y-%m-%d"),format(Sys.time(), "%Y-%m-%d"),format(Sys.time(), "%Y-%m-%d"),"2020-09-15", "2020-08-30", paste0(school_date_max))))
restrictions$label <- factor(restrictions$label, levels= restrictions$label, ordered=TRUE)


## statistics on age and most likely place of infection
stattest_imports<- data.frame(matrix(0, ncol =0, nrow = length(unique(BAG_data_su2020$country))-1))
stattest_imports$country <- levels(BAG_data_su2020$country)[-1]
for (i in unlist(unname(stattest_imports$country))) { # all countries but not Switzerland (national transmission)
  test_data<- BAG_data_su2020[BAG_data_su2020$country %in% c("Switzerland", i),]
  stattest_imports$ttest[stattest_imports$country==i] <- t.test(test_data$age~test_data$country,var.equal = FALSE)$p.value
  stattest_imports$wilcoxtest[stattest_imports$country==i] <- wilcox.test(test_data$age~test_data$country)$p.value#https://evol.bio.lmu.de/_statgen/StatBiol/11SS/zwei-stichproben-t-test_kompakt.pdf
  stattest_imports$ttest_less[stattest_imports$country==i] <- t.test(age ~ country, data = test_data, var.equal = TRUE, alternative = "less")$p.value
  stattest_imports$ttest_greater[stattest_imports$country==i] <- t.test(age ~ country, data = test_data, var.equal = TRUE, alternative = "greater")$p.value
  stattest_imports$wilcoxtest[stattest_imports$country==i] <- wilcox.test(age ~ country, data = test_data, var.equal = TRUE)$p.value
  stattest_imports$anova[stattest_imports$country==i] <-  anova(lm(test_data$age~test_data$country))$"Pr(>F)"[1]
}

boxplot(var(test_data$age[test_data$country=="Unknown"]),var(test_data$age[test_data$country=="Switzerland"]))
hist(test_data$age[test_data$country=="Unknown"])
hist(test_data$age[test_data$country=="Switzerland"])#https://statistics.berkeley.edu/computing/r-t-tests

FUN_sig <- function(x){
  if (is.na(x)){return(" ")}
  else if (x =="NA"){return(" ")}
  else if (x ==" "){return(" ")}
  else if (as.numeric(x) <= 0.001) {return("<.001")}
  else if (as.numeric(x) <= 0.01) {return(paste("~",round(as.numeric(x),3)))}
  else if (as.numeric(x) > 0.01) {return(paste("~",round(as.numeric(x),3)))}
}
stattest_imports$ttest_pvalue <- sapply(stattest_imports$ttest , FUN_sig)

#####

# Fit cases to generalized negative binomial model
## estimate growth rate and Re
#####
# Calculate (weighted) growth rate
r_all <- c()
r_all <- data.frame(intercept_cases = NA,
                    intercept_error_cases = NA,
                    rate_cases = NA,
                    error_cases = NA,
                    
                    intercept_hosp = NA,
                    intercept_error_hosp = NA,
                    rate_hosp = NA,
                    error_hosp = NA,
                    
                    intercept_dead = NA,
                    intercept_error_dead = NA,
                    rate_dead = NA,
                    error_dead = NA,
                    intercept_weigth = NA,
                    error_weigth = NA)

fit <- glm.nb(cases_hospitalized ~ date + weekend, data = swiss_cases_su2020)
r_all[1, 5:8] <- c(coef(summary(fit))[1, 1:2], coef(summary(fit))[2, 1:2])
fit <- glm.nb(cases_dead  ~ date + weekend, data = swiss_cases_su2020)
r_all[1, 9:12] <- c(coef(summary(fit))[1, 1:2], coef(summary(fit))[2, 1:2])
fit <- glm.nb(cases_reported ~ date + weekend, data = swiss_cases_su2020)
r_all[1, 1:4] <- c(coef(summary(fit))[1, 1:2], coef(summary(fit))[2, 1:2])
#r_all$intercept_weigth <- (r_all$intercept_cases/r_all$error_cases^2 + r_all$intercept_hosp/r_all$error_hosp^2 + r_all$intercept_dead/r_all$error_dead^2)/(1/r_all$error_cases^2 + 1/r_all$error_hosp^2 + 1/r_all$error_dead^2)
#r_all$intercept_error_weigth <- sqrt(1/(1/r_all$intercept_error_cases^2 + 1/r_all$intercept_error_hosp^2 + 1/r_all$intercept_error_dead^2))
r_all$rate_weigth <- (r_all$rate_cases/r_all$error_cases^2 + r_all$rate_hosp/r_all$error_hosp^2 + r_all$rate_dead/r_all$error_dead^2)/(1/r_all$error_cases^2 + 1/r_all$error_hosp^2 + 1/r_all$error_dead^2)
r_all$error_weigth <- sqrt(1/(1/r_all$error_cases^2 + 1/r_all$error_hosp^2 + 1/r_all$error_dead^2))

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
observed_Re <- repro(r_all$rate_weigth)
# within expectation of number of cases (that were reported)
#####

# potential Re if look at final size
Re_all <- as.vector(runif(1e3,0.8,1.2))
dispersion_parameters <- as.vector(rtnorm(1e3,mean=0.51,lower=0.49,upper=0.52))
runs <- 10^4
cases <- numeric(runs)
for(i in 1:runs) {
  x <- swiss_cases_su2020$cases_reported[max_time-1]
  cases[i] <- sum(rnbinom(x, size = median(dispersion_parameters), mu = observed_Re)) #size: target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution). Must be strictly positive, need not be integer.
}
final_size_95CI <- quantile(cases, probs) 

# potential Re if look at cumulative cases
x <- sum(swiss_cases_su2020$cases_reported)
cases <- rnorm(runs, sd = (observed_Re/(1+observed_Re/median(dispersion_parameters))), mean = x) 
cum_cases_95CI <- quantile(cases, probs) 

# potential Re if look at growth rate
growthrate_95CI <- c((r_all$rate_weigth- qnorm(0.975)* (r_all$error_weigth)),r_all$rate_weigth, (r_all$rate_weigth+ qnorm(0.975)* (r_all$error_weigth)))
names(growthrate_95CI) <- names(cum_cases_95CI)
#  look at Re
Re_95CI <- repro(growthrate_95CI)

#combine
growth_cum_final_expected<- as.data.frame(rbind(final_size_95CI,cum_cases_95CI,growthrate_95CI,Re_95CI ))
growth_cum_final_expected[(1:2),] <- round(growth_cum_final_expected[(1:2),])
growth_cum_final_expected[(3:4),] <- round(growth_cum_final_expected[(3:4),],3)

# which simulations are within 95% CI of expectation
growth_cum_final_summary[,12]  <- round(repro(growth_cum_final_summary$growth_r_ll),3)
growth_cum_final_summary[,13]<- round(repro(growth_cum_final_summary$growth_r_median),3)
growth_cum_final_summary[,14] <- round(repro(growth_cum_final_summary$growth_r_ul),3)
data <- growth_cum_final_summary[,c(1:2)]

growth_cum_final_summary[,c(9:11)] <- round(growth_cum_final_summary[,c(9:11)],3)
growth_cum_final_summary[,c(3:5)] <- round(growth_cum_final_summary[,c(3:5)])
growth_cum_final_summary[,c(6:8)] <- round(growth_cum_final_summary[,c(6:8)])

data$final_size <- 0
data$cumulative_cases<- 0
data$growth_rate<- 0
data$Re_fit <- 0

CI95_function <- function(found, expect){
  for (i in 1:length(found[,1])) {
    found_i <- found[i,]#growth_cum_final_summary[62,]
    #expect <- growth_cum_final_expected
    if(sum(seq.int(found_i[,3],found_i[,5],by=1) %in% seq.int(expect[1,1],expect[1,3],by=1))>0){
      data[i,3] <- 1
    }
     if(sum(seq.int(found_i[,6],found_i[,8],by=1) %in% seq.int(expect[2,1],expect[2,3],by=1))>0){
      data[i,4] <- 1
    }
      if(sum(seq.int(found_i[,9],found_i[,11],by=0.001) %in% seq.int(expect[3,1],expect[3,3],by=0.001))>0){
      data[i,5] <- 1
    }
     if(sum(seq.int(found_i[,12],found_i[,14],by=0.001) %in% seq.int(expect[4,1],expect[4,3],by=0.001))>0){
      data[i,6] <- 1
    }
    
  }
  return(data)
}
simulation_accept  <- CI95_function(growth_cum_final_summary,growth_cum_final_expected)

#final size 95%
min(Re_all)
max(Re_all)
round(quantile(simulation_accept$Re[na.omit(simulation_accept[,3])==1],probs),3)
round(quantile(simulation_accept$Re[simulation_accept$imports != 0 & na.omit(simulation_accept[,3])==1],probs),3)

for (i_name in names(table(growth_cum_final_summary$imports))) {
  print(i_name,print( round(quantile(simulation_accept$Re[simulation_accept$imports %in% i_name &na.omit(simulation_accept[,3])==1],probs),3)))
}


#cumulative cases range
quantile(simulation_accept$Re[na.omit(simulation_accept[,3])==1],probs)

#growth rate range
quantile(simulation_accept$Re[na.omit(simulation_accept[,3])==1],probs)

#Re range
quantile(simulation_accept$Re[na.omit(simulation_accept[,3])==1],probs)


#####

# for manuscript:
#Re
paste0("Re =",round(repro(r_all$rate_cases),3)," (95%-CI: ", round(repro(r_all$rate_cases- qnorm(0.975)* (r_all$error_cases)),3),"-", round(repro(r_all$rate_cases + qnorm(0.975)* (r_all$error_cases)),3),")")
paste0("growth rate =",round((r_all$rate_cases),3)," (95%-CI: ", round((r_all$rate_cases- qnorm(0.975)* (r_all$error_cases)),3),"-", round((r_all$rate_cases + qnorm(0.975)* (r_all$error_cases)),3),")")
paste0("Re weighted =",round(repro(r_all$rate_weigth),3)," (95%-CI: ", round(repro(r_all$rate_weigth- qnorm(0.975)* (r_all$error_weigth)),3),"-", round(repro(r_all$rate_weigth + qnorm(0.975)* (r_all$error_weigth)),3),")")
paste0("growth rate weighted =",round((r_all$rate_weigth),3)," (95%-CI: ", round((r_all$rate_weigth- qnorm(0.975)* (r_all$error_weigth)),3),"-", round((r_all$rate_weigth + qnorm(0.975)* (r_all$error_weigth)),3),")")

#Age distribution in study population:
paste0(round(quantile(BAG_data_su2020$age)[3]), " (IQR:",round(quantile(BAG_data_su2020$age)[2]),"-",round(quantile(BAG_data_su2020$age)[4]),"; range: ",round(quantile(BAG_data_su2020$age)[1]),"-",round(quantile(BAG_data_su2020$age)[5]),")")
round(mean(BAG_data_su2020$age))
paste0(round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[3]), " (",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[2]),"-",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[4]),"; range: ",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[1]),"-",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[5]),")")
paste0(round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[3]), " (",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[2]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[4]),"; range: ",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[1]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[5]),")")
paste0(round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[3]), " (",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[2]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[4]),"; range: ",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[1]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[5]),")")

#Age differences by country of exposure:
paste0("The age is significantly different (<.05) if not infected in Switzerland but in ",paste0(stattest_imports$country[stattest_imports$ttest<.05],collapse=", "))
paste0("Individuals were significantly younger (<0.05) if they were not infected in Switzerland but in ",paste0(stattest_imports$country[stattest_imports$ttest_greater<.05],collapse=", "))
paste0("Individuals were significantly older (<0.05) if they were not infected in Switzerland but in ",paste0(stattest_imports$country[stattest_imports$ttest_less<.05],collapse=", "))

#Gender distribution in study population:
round(table(BAG_data_su2020$sex)/length(BAG_data_su2020$sex)*100,2)
round(table(BAG_data_su2020$sex[BAG_data_su2020$country=="Switzerland"])/length(BAG_data_su2020$sex[BAG_data_su2020$country=="Switzerland"])*100,2)
round(table(BAG_data_su2020$sex[BAG_data_su2020$country=="Unknown"])/length(BAG_data_su2020$sex[BAG_data_su2020$country=="Unknown"])*100,2)
round(table(BAG_data_su2020$sex[!BAG_data_su2020$country %in%c("Switzerland","Unknown")])/length(BAG_data_su2020$sex[!BAG_data_su2020$country %in%c("Switzerland","Unknown")])*100,2)
chisq.test(BAG_data_su2020$country_cat[BAG_data_su2020$country_cat != "Unknown" &BAG_data_su2020$sex !="Unbekannt"], BAG_data_su2020$sex[BAG_data_su2020$country_cat != "Unknown" &BAG_data_su2020$sex !="Unbekannt"], correct=FALSE)$p.value
chisq.test(BAG_data_su2020$country_exposure_known[BAG_data_su2020$sex !="Unbekannt"], BAG_data_su2020$sex[BAG_data_su2020$sex !="Unbekannt"], correct=FALSE)$p.value

# Table 2: most likely place of infection and age
imports_country_age <- as.data.frame(matrix(ncol= 7, nrow= length(levels(BAG_data_su2020$country))+2))
colnames(imports_country_age) <- c("Country","No. cases","Known origin \n\n(in %)", "Imports \n\n(in %)", "Age in years,\n\nmedian (IQR)", "P-value*", "Mandatory \n\nquarantine")
imports_country_age[,1] <- c(c("All reported cases", "Imports"),levels(BAG_data_su2020$country))
for (c in c("All reported cases", "Imports")){
  imports_country_age[,2][imports_country_age$Country == c] <- if(c== "All reported cases"){length(BAG_data_su2020$country)} else{sum(!BAG_data_su2020$country %in% c("Unknown","Switzerland"))}
  imports_country_age[,3][imports_country_age$Country == c] <- if(c== "All reported cases"){" - "} else{format(round(sum(!BAG_data_su2020$country %in% c("Unknown","Switzerland"))/sum(!BAG_data_su2020$country %in% c("Unknown"))*100,2), nsmall = 2)}
  imports_country_age[,4][imports_country_age$Country == c] <- if(c== "All reported cases"){" - "} else{format(round(sum(!BAG_data_su2020$country %in% c("Unknown","Switzerland"))/sum(!BAG_data_su2020$country %in% c("Unknown","Switzerland"))*100,2), nsmall = 2)}
  imports_country_age[,5][imports_country_age$Country == c] <- if(c== "All reported cases"){paste0(round(quantile(BAG_data_su2020$age)[3]), " (",round(quantile(BAG_data_su2020$age)[2]),"-",round(quantile(BAG_data_su2020$age)[4]),")")} 
  else{paste0(round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Unknown", "Switzerland")])[3]), " (",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Unknown", "Switzerland")])[2]),"-",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Unknown", "Switzerland")])[4]),")")}
  imports_country_age[,6][imports_country_age$Country == c] <- " - "
  imports_country_age[,7][imports_country_age$Country == c] <- " - "
}
for (c in (levels(BAG_data_su2020$country))){
  imports_country_age[,2][imports_country_age$Country == c] <- sum(BAG_data_su2020$country == c)
  imports_country_age[,3][imports_country_age$Country == c] <- if(c== "Unknown"){" - "} else{format(round(sum(BAG_data_su2020$country == c)/sum(BAG_data_su2020$country != "Unknown")*100,2), nsmall = 2)}
  imports_country_age[,4][imports_country_age$Country == c] <- if(c %in% c("Unknown", "Switzerland")){" - "} else{format(round(sum(BAG_data_su2020$country == c)/sum(!BAG_data_su2020$country %in% c("Unknown", "Switzerland"))*100,2), nsmall = 2)}
  imports_country_age[,5][imports_country_age$Country == c] <- paste0(round(quantile(BAG_data_su2020$age[BAG_data_su2020$country == c])[3]), " (",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country == c])[2]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country == c])[4]),")")
  imports_country_age[,6][imports_country_age$Country == c] <- if(c== "Switzerland"){" - "} else{stattest_imports$ttest_pvalue[stattest_imports$country ==c]}
  imports_country_age[,7][imports_country_age$Country == c] <- if(c %in% c("Unknown", "Switzerland")){" - "} else if(c == "Others" || is.na(su2020_quarantine$start_date[gsub("\\*", "\\1", su2020_quarantine$label) ==c])){" - "} else{paste0(su2020_quarantine$start_date[gsub("\\*", "\\1", su2020_quarantine$label) ==c], " - ", su2020_quarantine$end_date_dot[gsub("\\*", "\\1", su2020_quarantine$label) ==c])}
}
table2 <- xtable(imports_country_age)
table2 <- xtable(table2,
                 caption = "Infections during summer 2020 regarding the age and most likely country of infection. *P-value of the two sided t-test; ** the mandataroy quarantine did not end on the $30^{th}$ of September 2020.",
                 label = "t2")
table2 <- print(table2, size = "footnotesize", include.rownames = FALSE, include.colnames = TRUE)

write(table2, file = "table2.tex")
