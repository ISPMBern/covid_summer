
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
cases_su2020 <- read.csv("cases_su2020.csv", row.names = 1, header=T, sep=",")
Re_all <- read.csv("Re_all_2021-04-29.csv")
dispersion_parameters <- read.csv("dispersion_parameters_2021-04-29.csv")
noimport_model_output <- readRDS("noimport_model_output_2021-04-29.rds")
imports <- list.files( pattern="imports_model_outputs_*", full.names=TRUE, recursive=FALSE)
imports <- imports[grepl("2021-04-29", imports)]
import_models_output<- c()
for (i in 1:length(imports)) {
  new <- readRDS(imports[i])
  import_models_output <- rbind(import_models_output,new)
}
new <- rm


# prep data
probs = c(.025,.5,.975)
time_window <- c(as_date("2020-06-01"), as_date("2020-09-30"))
period <- c(time_window[1]:time_window[2])
max_time <- length(period)
Re_all <- Re_all[,2]
dispersion_parameters <- dispersion_parameters[,2]
cases_su2020$date <- as_date(cases_su2020$date)
swiss_cases_su2020 <- subset(cases_su2020, date %in% seq(time_window[1],time_window[2],1))
swiss_cases_su2020$weekend <- ifelse(weekdays(swiss_cases_su2020$date) == "Saturday" | weekdays(swiss_cases_su2020$date) == "Sunday", 1, 0)
BAG_data_su2020$date <- as_date(BAG_data_su2020$date)
BAG_data_su2020 <- subset(BAG_data_su2020, date %in% seq(time_window[1],time_window[2],1))
KOF[,"date"] <- seq(as_date("2020-01-01"),(as_date("2020-01-01")+length(KOF[,"date"])-1),1)
KOF_su2020 <- subset(KOF, date %in% seq(time_window[1],time_window[2],1))
imports_d <- swiss_cases_su2020[, c("date","cases_abroad")]
weighting <- 1+((sum(swiss_cases_su2020$cases_swiss)+sum(swiss_cases_su2020$cases_abroad))/sum(swiss_cases_su2020$cases_reported))
imports_d$abroad_weighted <- round(imports_d$cases_abroad*weighting)
imports_d$abroad15_weighted <- round(swiss_cases_su2020$cases_abroad*1.5*weighting)
imports_d$abroad1_15_weighted <- round(swiss_cases_su2020$cases_abroad*(1/1.5)*weighting)
import_num <- c(0,colSums(imports_d[,-1]))
imports_d$date <- as_date(imports_d$date)
import_num <- import_num[order(import_num)]
swiss_cases_su2020$imports_propotion_all <- swiss_cases_su2020$cases_abroad/(swiss_cases_su2020$cases_reported)
swiss_cases_su2020$imports_propotion_known <- swiss_cases_su2020$cases_abroad/(swiss_cases_su2020$cases_abroad+swiss_cases_su2020$cases_swiss)
swiss_cases_su2020$imports_abroad_propotion_all <- imports_d$abroad_weighted/(swiss_cases_su2020$cases_reported)
swiss_cases_su2020$imports_abroad15_propotion_all <- imports_d$abroad15_weighted/(swiss_cases_su2020$cases_reported)
swiss_cases_su2020$imports_abroad1_15_propotion_all <- imports_d$abroad1_15_weighted/(swiss_cases_su2020$cases_reported)

models_output <- rbind(noimport_model_output,import_models_output)
models_output<- as.data.frame(models_output)
#colnames(models_output) <- c("seeds","imports","Re","cum_cases","final_incidence")
import_models_output <- rm
noimport_model_output <- rm
models_output$imports <- as.numeric(models_output$imports )
models_output$imports <- factor(models_output$imports, levels=unique(names(table(models_output$imports))[order(table(models_output$imports), decreasing = TRUE)]), ordered=TRUE)
models_output$Re_round3 <- round(models_output$Re,3)
models_output <- models_output[models_output$Re <=1.2 & models_output$Re >=0.8,]#should not be needed

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

#exposed countries
BAG_data_su2020$country <- factor(BAG_data_su2020$country, levels=unique(names(table(BAG_data_su2020$country))[order(table(BAG_data_su2020$country), decreasing = TRUE)]), ordered=TRUE)
BAG_data_su2020$country <- factor(BAG_data_su2020$country, levels=c(levels(BAG_data_su2020$country)[!levels(BAG_data_su2020$country) %in% c("Others","Abroad but unknown","Unknown")],c("Others","Abroad but unknown","Unknown")), ordered=TRUE)

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

#####

# Fit cases to generalized negative binomial model
## estimate growth rate and Re
#####
# Calculate  growth rate
r_all <- c()
fit <- glm.nb(cases_reported ~ date , data = swiss_cases_su2020) #made no difference glm.nb(cases_reported ~ date + weekend, data = swiss_cases_su2020)
r_all <- c(coef(summary(fit))[2, 1]- 1.96 * coef(summary(fit))[2, 2],coef(summary(fit))[2, 1],coef(summary(fit))[2, 1]+ 1.96 * coef(summary(fit))[2, 2])
size_expect <- summary(glm.nb(cases_reported ~ date, data = swiss_cases_su2020))$SE.theta
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
#####
# potential Re if look at final incidence
n_sim <- 10^4
cases <- numeric(n_sim)

cum <- swiss_cases_su2020$cases_reported
final <- tail(cum, 7)
round(mean(final))
cum_inc<- final_inc <- numeric(n_sim)
final_inc <- numeric(n_sim)
for(i in 1:n_sim) {
  cum_inc[i] <- sum(rnbinom(length(cum), size = model_se, mu = cum))
  final_inc[i] <- sum(rnbinom(length(final), size = model_se, mu = final))
}
cum_final_expected <- round(rbind(quantile(cum_inc, probs), quantile(final_inc, probs)/7))

cum_expected_seq <- c(cum_final_expected[1,1]:cum_final_expected[1,2])
final_expected_seq <- c(cum_final_expected[2,1]:cum_final_expected[2,2])

accept_fun <- function(i){
  found_i<- models_output[i,]
    if(found_i[,6] %in% cum_expected_seq & found_i[,7] %in% final_expected_seq)
    { return(1)}
  else return(0)
}
models_output$simulation_accepted  <- sapply(1:length(models_output[,1]),FUN=accept_fun)
simulation_accept <- models_output[models_output$simulation_accepted==1,]
length(simulation_accept[,1])

for (i_name in names(table(import_num))) {
  print(length(simulation_accept$Re[simulation_accept$imports %in% i_name]))
}

for (i_name in names(table(import_num))) {
  print(quantile((simulation_accept$Re[simulation_accept$imports %in% i_name]),probs))
}

# data prep 
Re_unique <- unique(models_output$Re_round3)
Re_length <- length(unique(models_output$Re_round3))
import_num <- import_num[-2]
imports_length <- length(import_num)
data <- as.data.frame(matrix(ncol=13,nrow = Re_length*imports_length))
colnames(data) <- c("imports","Re", "acceptance_score","cum_cases_ll", "cum_cases_median", "cum_cases_ul","cum_cases_min","cum_cases_max","final_incidence_ll", "final_incidence_median", "final_incidence_ul","final_incidence_min","final_incidence_max")
CI95_function <- function(inputs){
  for (n in 1:imports_length) {
    input <- models_output[inputs$imports %in% import_num[n],]
    for (i in 1:Re_length) {
      input_i <- input[input$Re_round3 %in% Re_unique[i],]
      m <- (i+((n-1)*Re_length))
      data[m,c(1:2)] <- c(as.character(import_num[n]), Re_unique[i])
      data[m,3] <- sum(input_i$simulation_accepted)/length(input_i$simulation_accepted)
      data[m,c(4:8)] <- round(quantile(input_i[,6], c(probs,0,1)))
      data[m,c(9:13)] <- round(quantile(input_i[,7], c(probs,0,1)))
      
    }
  }
  return(data)
}
models_output_summary  <- CI95_function(models_output)
data <- rm
colnames(models_output_summary) <- c("imports","Re", "acceptance_score","cum_cases_ll", "cum_cases_median", "cum_cases_ul","cum_cases_min","cum_cases_max","final_incidence_ll", "final_incidence_median", "final_incidence_ul","final_incidence_min","final_incidence_max")
models_output_summary$imports <- as.numeric(models_output_summary$imports )
models_output_summary$imports <- factor(models_output_summary$imports, levels=unique(names(table(models_output_summary$imports))[order(table(models_output_summary$imports), decreasing = TRUE)]), ordered=TRUE)
models_output_summary <- models_output_summary[models_output_summary$Re <=1.2 & models_output_summary$Re >=0.8,]#should not be needed


#####

# for manuscript:

#Age distribution in study population:
paste0(round(quantile(BAG_data_su2020$age)[3]), " (IQR:",round(quantile(BAG_data_su2020$age)[2]),"-",round(quantile(BAG_data_su2020$age)[4]),"; range: ",round(quantile(BAG_data_su2020$age)[1]),"-",round(quantile(BAG_data_su2020$age)[5]),")")
round(mean(BAG_data_su2020$age))
paste0(round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[3]), " (",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[2]),"-",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[4]),"; range: ",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[1]),"-",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Switzerland", "Unknown")])[5]),")")
paste0(round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[3]), " (",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[2]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[4]),"; range: ",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[1]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Switzerland")])[5]),")")
paste0(round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[3]), " (",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[2]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[4]),"; range: ",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[1]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country %in% c("Unknown")])[5]),")")

#Gender distribution in study population:
round(table(BAG_data_su2020$sex)/length(BAG_data_su2020$sex)*100,2)
round(table(BAG_data_su2020$sex[BAG_data_su2020$country=="Switzerland"])/length(BAG_data_su2020$sex[BAG_data_su2020$country=="Switzerland"])*100,2)
round(table(BAG_data_su2020$sex[BAG_data_su2020$country=="Unknown"])/length(BAG_data_su2020$sex[BAG_data_su2020$country=="Unknown"])*100,2)
round(table(BAG_data_su2020$sex[!BAG_data_su2020$country %in%c("Switzerland","Unknown")])/length(BAG_data_su2020$sex[!BAG_data_su2020$country %in%c("Switzerland","Unknown")])*100,2)
chisq.test(BAG_data_su2020$country_cat[BAG_data_su2020$country_cat != "Unknown" &BAG_data_su2020$sex !="Unbekannt"], BAG_data_su2020$sex[BAG_data_su2020$country_cat != "Unknown" &BAG_data_su2020$sex !="Unbekannt"], correct=FALSE)$p.value
chisq.test(BAG_data_su2020$country_exposure_known[BAG_data_su2020$sex !="Unbekannt"], BAG_data_su2020$sex[BAG_data_su2020$sex !="Unbekannt"], correct=FALSE)$p.value

# Table 2: most likely place of infection and age
imports_country_age <- as.data.frame(matrix(ncol= 6, nrow= length(levels(BAG_data_su2020$country))+2))
colnames(imports_country_age) <- c("Country","No. cases","Known exposure \n\n(in %)", "Exposure abroad \n\n(in %)", "Age in years,\n\nmedian (IQR)", "Mandatory \n\nquarantine")
imports_country_age[,1] <- c(c("All reported cases", "Imports"),levels(BAG_data_su2020$country))
for (c in c("All reported cases", "Imports")){
  imports_country_age[,2][imports_country_age$Country == c] <- if(c== "All reported cases"){length(BAG_data_su2020$country)} else{sum(!BAG_data_su2020$country %in% c("Unknown","Switzerland"))}
  imports_country_age[,3][imports_country_age$Country == c] <- if(c== "All reported cases"){" - "} else{format(round(sum(!BAG_data_su2020$country %in% c("Unknown","Switzerland"))/sum(!BAG_data_su2020$country %in% c("Unknown"))*100,2), nsmall = 2)}
  imports_country_age[,4][imports_country_age$Country == c] <- if(c== "All reported cases"){" - "} else{format(round(sum(!BAG_data_su2020$country %in% c("Unknown","Switzerland"))/sum(!BAG_data_su2020$country %in% c("Unknown","Switzerland"))*100,2), nsmall = 2)}
  imports_country_age[,5][imports_country_age$Country == c] <- if(c== "All reported cases"){paste0(round(quantile(BAG_data_su2020$age)[3]), " (",round(quantile(BAG_data_su2020$age)[2]),"-",round(quantile(BAG_data_su2020$age)[4]),")")} 
  else{paste0(round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Unknown", "Switzerland")])[3]), " (",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Unknown", "Switzerland")])[2]),"-",round(quantile(BAG_data_su2020$age[!BAG_data_su2020$country %in% c("Unknown", "Switzerland")])[4]),")")}
  imports_country_age[,6][imports_country_age$Country == c] <- " - "
}
for (c in (levels(BAG_data_su2020$country))){
  imports_country_age[,2][imports_country_age$Country == c] <- sum(BAG_data_su2020$country == c)
  imports_country_age[,3][imports_country_age$Country == c] <- if(c== "Unknown"){" - "} else{format(round(sum(BAG_data_su2020$country == c)/sum(BAG_data_su2020$country != "Unknown")*100,2), nsmall = 2)}
  imports_country_age[,4][imports_country_age$Country == c] <- if(c %in% c("Unknown", "Switzerland")){" - "} else{format(round(sum(BAG_data_su2020$country == c)/sum(!BAG_data_su2020$country %in% c("Unknown", "Switzerland"))*100,2), nsmall = 2)}
  imports_country_age[,5][imports_country_age$Country == c] <- paste0(round(quantile(BAG_data_su2020$age[BAG_data_su2020$country == c])[3]), " (",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country == c])[2]),"-",round(quantile(BAG_data_su2020$age[BAG_data_su2020$country == c])[4]),")")
  imports_country_age[,6][imports_country_age$Country == c] <- if(c %in% c("Unknown", "Switzerland")){" - "} else if(c%in%c("Others","Abroad but unknown") || is.na(su2020_quarantine$start_date[gsub("\\*", "\\1", su2020_quarantine$label) ==c])){" - "} else{paste0(su2020_quarantine$start_date[gsub("\\*", "\\1", su2020_quarantine$label) ==c], " - ", su2020_quarantine$end_date_dot[gsub("\\*", "\\1", su2020_quarantine$label) ==c])}
}
imports_country_age[2,1] <- "Cross-border-associated"
table2 <- xtable(imports_country_age)
table2 <- xtable(table2,
                 caption = "Reported SARS-CoV-2 cases during summer 2020 regarding age and most likely country of exposure.**Mandataroy quarantine did not end on the $30^{th}$ of September 2020. Abbreviation: IQR, interquartile range; No., number",
                 label = "t2")
table2 <- print(table2, size = "footnotesize", include.rownames = FALSE, include.colnames = TRUE)

write(table2, file = "table2.tex")
