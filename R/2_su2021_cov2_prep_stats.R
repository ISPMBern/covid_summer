
# Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021

#library
library(MASS)
library(MCMCglmm)
library(lubridate)
library(MCMCglmm)
library(xlsx)
library(httr)
library(downloader)

# Set seed
set.seed(60321)

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/data")#https://github.com/owid/covid-19-data/tree/master/public/data


## Swiss population information
url <- "https://www.bag.admin.ch/dam/bag/de/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-basisdaten-bevoelkerungszahlen.xlsx.download.xlsx/Population_Size_BFS.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
population_bag_ch <- read.xlsx(tf,sheetIndex = 2, startRow = 0)
population_bag_ch <- population_bag_ch[population_bag_ch$Kanton !="FL",]
colnames(population_bag_ch) <- c("canton", "gender", "age", "population_size")
pop_size <- 8544527

# get data from KOF (ETHZ): Stringency Index
KOF <- read.csv(paste0("https://datenservice.kof.ethz.ch/api/v1/public/sets/stringency_plus_web?mime=csv&df=Y-m-d.csv"))
KOF[,"date"] <- seq(as_date("2020-01-01"),(as_date("2020-01-01")+length(KOF[,"date"])-1),1)


probs = c(.025,.5,.975)

for (i in c("2020","2021")) {
  BAG_data_su<- read.csv("BAG_data.csv")
  cases_summer <- read.csv("cases_su.csv")
  Re_all <- read.csv("Re_all_2022-03-11.csv")
  dispersion_parameters <- read.csv("dispersion_parameters_2022-03-11.csv")
  if(i=="2020"){
    cases_su <- subset(cases_summer, year(date) %in% 2020)
    time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
    noimport_model_output <- readRDS("./2020/noimport_model_output2020_2022-03-11.rds")
  imports <- list.files( "./2020", pattern="import_model_output2020_", full.names=TRUE, recursive=FALSE)[-5]
  import_models_output<- c()
  for (l in 1:length(imports)) {
    new <- readRDS(imports[l])
    import_models_output <- rbind(import_models_output,new)
  }
  new <- rm
  
  
  
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
  ### quarantine list: https://www.fedlex.admin.ch/eli/cc/2020/496/en
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
                                           "Malta", "Czech Republic", "Cyprus" ),#* not whole country, only region
                                  country= c("Greece", "France","North Macedonia", "Netherlands", 
                                             "Italy", "Kosovo", "Bosnia and Herzegovina", "Portugal", 
                                             "Slovenia", "Albania", "UK", "Hungary",
                                             "Romania", "Germany", "Serbia", "Austria", 
                                             "Croatia", "Poland","Turkey", "Spain", 
                                             "Malta", "Czech Republic", "Cyprus"),
                                  start_date=c(NA,"2020-09-14", "2020-07-06","2020-09-28",
                                               "2020-09-28","2020-07-06", "2020-07-23","2020-09-28",
                                               "2020-09-28","2020-08-20", "2020-09-28", "2020-09-28",
                                               "2020-08-08",NA, "2020-07-06", "2020-09-14",
                                               "2020-09-07",NA,NA,"2020-08-08",
                                               "2020-08-20", "2020-09-14",NA),
                                  end_date=c(NA,"2020-09-30","2020-09-30","2020-09-30",
                                             "2020-09-30","2020-09-14","2020-09-30","2020-09-30",
                                             "2020-09-30","2020-09-30","2020-09-30","2020-09-30",
                                             "2020-09-30",NA, "2020-08-15", "2020-09-30",
                                             "2020-09-30",NA,NA,"2020-09-30",
                                             "2020-09-30","2020-09-30",NA),# max. date: "2020-09-30"
                                  start_date_dot=c(NA,NA,NA,NA,
                                                 NA,NA,NA,NA,
                                                 NA,NA,NA,NA,
                                                 NA,NA, NA, NA,
                                                 NA,NA,NA,NA,
                                                 NA,NA,NA),
                                  end_date_dot=c(NA,"*","*","*",
                                                 "*","2020-09-14","*","*",
                                                 "*","*","*","*",
                                                 "*",NA, "2020-08-15", "*",
                                                 "*",NA,NA,"*",
                                                 "*","*",NA))
  
  
  #restriction combined
  restrictions <- data.frame(
    label = c("Open borders*:","Mask mandatory in public transport:","Swiss Covid App:", "University breaks*:", "Max. range of school breaks*:", "Overlap of all school breaks*:"),
    start_date = as_date(c("2020-06-15","2020-07-20","2020-06-25","2020-06-05", "2020-06-20", paste0(school_date_min))), 
    end_date = as_date(c("2020-09-30","2020-09-30","2020-09-30","2020-09-13", "2020-08-30", paste0(school_date_max))), # max. date: "2020-09-30"
    end_date_dot = as_date(c(format(Sys.time(), "%Y-%m-%d"),format(Sys.time(), "%Y-%m-%d"),format(Sys.time(), "%Y-%m-%d"),"2020-09-15", "2020-08-30", paste0(school_date_max))))
  restrictions$label <- factor(restrictions$label, levels= restrictions$label, ordered=TRUE)

  
  }
  else if(i=="2021"){
    cases_su <- subset(cases_summer, year(date) %in% 2021)
    time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
    noimport_model_output <- readRDS("./2021/noimport_model_output2021_2022-03-11.rds")
    imports <- list.files("./2021", pattern="import_model_output2021_*", full.names=TRUE, recursive=FALSE)[-5]
    import_models_output<- c()
    for (l in 1:length(imports)) {
      new <- readRDS(imports[l])
      import_models_output <- rbind(import_models_output,new)
    }
    new <- rm
    #https://www.schulferien.org/schweiz/ferien/ (accessed 03/09/2021)
    #quarantine list for 2021
    # into operation on 2021-05-25: Andorra, Argentina, Bahrain, Belgium, Cape Verde, Chile, Colombia, Costa Rica, Egypt, Estonia, Georgia, Kuwait, Latvia, Lithuania, Maldives, Mexico, Mongolia, Netherlands, Paraguay, Seychelles, Slovenia, Sweden, Tanzania, Uruguay,Brazil, Canada, India, Nepal, South Africa, United Kingdom, Region in France, Regions in Germany, Regions in Italy
    # into operation on 2021-06-03: Andorra, Argentina, Bahrain, Belgium, Cape Verde, Chile, Colombia, Costa Rica, Egypt, Estonia, Georgia, Kuwait, Latvia, Lithuania, Maldives, Mexico, Mongolia, Netherlands, Paraguay, Seychelles, Slovenia, Sweden, Tanzania, Uruguay,Brazil, Canada, India, Nepal, South Africa, United Kingdom, Region in France
    # into operation on 2021-06-17: Andorra, Argentina, Bahrain, Belgium, Cape Verde, Chile, Colombia, Costa Rica, Egypt, Estonia, Georgia, Kuwait, Latvia, Lithuania, Maldives, Mexico, Mongolia, Netherlands, Paraguay, Seychelles, Slovenia, Sweden, Tanzania, Uruguay,Brazil, Canada, India, Nepal, South Africa, United Kingdom, Region in France
    # into operation on 2021-06-26: India, Nepal, United Kingdom
    # into operation on 2021-06-26: India, Nepal, United Kingdom
    # into operation on 2021-08-04: no country
    su2021_quarantine <- data.frame(label= c("North Macedonia","Kosovo", "Italy*", "Spain","France*","Turkey","Serbia","Croatia", "Albania", "Bosnia and Herzegovina"),#* not whole country, only region
                                    country= c("North Macedonia", "Kosovo", "Italy", "Spain","France","Turkey","Serbia","Croatia", "Albania", "Bosnia and Herzegovina"),
                                    start_date=c(NA,NA,"2021-06-01",NA,"2021-06-01",NA,NA,NA,NA,NA),# min date: "2021-06-01"
                                    end_date=c(NA,NA,"2021-06-03",NA,"2021-06-26",NA,NA,NA,NA,NA),# max. date: "2021-09-30"
                                    start_date_dot=c(NA,NA,"*",NA,"*",NA,NA,NA,NA,NA),
                                    end_date_dot=c(NA,NA,"2021-06-03",NA,"2021-06-26",NA,NA,NA,NA,NA))
    
    restrictions <- data.frame(
      label = c("Open borders*:","Mask mandatory in public transport:","Swiss Covid App:","University breaks*:", "Max. range of school breaks*:", "Overlap of all school breaks*:"),
      start_date = as_date(c("2021-06-01","2021-06-01","2021-06-01","2021-06-04", "2021-06-19", "2021-07-19")), 
      end_date = as_date(c("2021-09-30","2021-09-30","2021-09-30","2021-09-20", "2021-08-29", "2021-08-06")), 
      end_date_dot = as_date(c(format(Sys.time(), "%Y-%m-%d"),format(Sys.time(), "%Y-%m-%d"),format(Sys.time(), "%Y-%m-%d"),"2021-09-20", "2021-08-29", "2021-08-06")))
    restrictions$label <- factor(restrictions$label, levels= restrictions$label, ordered=TRUE)
    
  }
  Re_all <- Re_all[,2]
  dispersion_parameters <- dispersion_parameters[,2]
  
  models_output <- as.data.frame(rbind(noimport_model_output,import_models_output))
  models_output<- as.data.frame(models_output)
  models_output$imports <- as.numeric(models_output$imports )
  #cumulative incidence limit of model was 1e6 
  models_output <- models_output[models_output$cum_cases<=(1e6-1),]
  
  import_models_output <- rm
  noimport_model_output <- rm
  models_output$Re_round <- round(models_output$Re,3)

  
  # prep data
  period <- seq(time_window[1],time_window[2],1)
  max_time <- length(period)
  cases_su$date <- as_date(cases_su$date)
  #cases_su <- subset(cases_su, date %in% seq(time_window[1],time_window[2],1))
  cases_su$weekend <- ifelse(weekdays(cases_su$date) == "Saturday" | weekdays(cases_su$date) == "Sunday", 1, 0)
  #BAG_data_su$date <- as_date(BAG_data_su$date)
  #BAG_data_su <- subset(BAG_data_su, date %in% seq(time_window[1],time_window[2],1))
  
  imports_d <- cases_su[, c("date","cases_abroad")]
  weighting <- sum(cases_su$cases_date)/(sum(cases_su$cases_swiss)+sum(cases_su$cases_abroad))
  imports_d$abroad_a <- round(cases_su$cases_abroad*weighting)
  imports_d$abroad_b <- round(cases_su$cases_abroad*0.5*weighting)
  imports_d$abroad_c <- round(cases_su$cases_abroad*1.5*weighting)
  import_num <- c(0,colSums(imports_d[,-1]))
  imports_d$date <- as_date(imports_d$date)
  #import_num <- import_num[order(import_num)]
  #sum(imports_d$cases_abroad)
  
  # Estimating incidence
  #cases_su$incidence <- cases_su$cases_date/sum(population_bag_ch$population_size)*1e5 #https://www.worldometers.info/world-population/switzerland-population/
  cases_su$incidence <- cases_su$cases_date/pop_size*1e5 #https://www.worldometers.info/world-population/switzerland-population/
  before <- 3
  after <- 3
  incidence_fun <- function(x){
    set <- subset(cases_su, cases_su$date >= (as_date(x) - before) & cases_su$date <= (as_date(x) + after))
    incidence_weigthed<- mean(set$cases_date)/pop_size*1e5
    return(incidence_weigthed)
  }
  cases_su$incidence_weigthed <- sapply(cases_su$date, incidence_fun)
  
  
  
  
  #####
  
  # Fit cases to generalized negative binomial model
  r_all <- c()
  fit <- glm.nb(cases_date ~ date , data = cases_su) #made no difference glm.nb(cases_date ~ date + weekend, data = cases_su)
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
  
  models_output$cum_cases <- as.numeric(models_output$cum_cases)
  models_output$final_incidence <- as.numeric(models_output$final_incidence)
  models_output$simulation_accepted <- 0
  models_output$simulation_accepted[models_output$cum_cases %in% cum_expected_seq & models_output$final_incidence %in% final_expected_seq] <-1
  simulation_accept <- models_output[models_output$simulation_accepted%in%1,]
  
  print(i)
  print(paste0("All ",length(simulation_accept[,1])))
  for (i_name in unname(unlist(import_num))) {
    print(length(simulation_accept$Re[simulation_accept$imports %in% i_name]))
  }
  
  for (i_name in unname(unlist(import_num))) {
    print(quantile((simulation_accept$Re[simulation_accept$imports %in% i_name]),probs))
  }
  
  
  # data prep 
  Re_unique <- unique(models_output$Re_round)
  Re_length <- length(unique(models_output$Re_round))
  #import_num <- import_num[-1]
  imports_length <- length(import_num)
  data <- as.data.frame(matrix(ncol=13,nrow = Re_length*imports_length))
  colnames(data) <- c("imports","Re", "acceptance_score","cum_cases_ll", "cum_cases_median", "cum_cases_ul","cum_cases_min","cum_cases_max","final_incidence_ll", "final_incidence_median", "final_incidence_ul","final_incidence_min","final_incidence_max")
  CI95_function <- function(inputs){
    for (n in 1:imports_length) {
      input <- models_output[inputs$imports %in% import_num[n],]
      for (i in 1:Re_length) {
        input_i <- input[input$Re_round %in% Re_unique[i],]
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
  models_output_summary <-models_output_summary[!is.na(models_output_summary$cum_cases_median),]
  
  cum_final_expected <- as.data.frame(cum_final_expected)
  
  if(i=="2020"){
    write.csv(models_output, "./2020/models_output2020.csv")
    write.csv(cases_su, "./2020/cases_su2020.csv")
    write.csv(models_output_summary, "./2020/models_output2020_summary.csv")
    write.csv(restrictions, "./2020/restrictions2020_summary.csv")
    write.csv(cum_final_expected, "./2020/cum_final_2020expected.csv")
    write.csv(su2020_quarantine, "./2020/su2020_quarantine.csv")
  }
  else if(i=="2021"){
    write.csv(models_output, "./2021/models_output2021.csv")
    write.csv(cases_su, "./2021/cases_su2021.csv")
    write.csv(models_output_summary, "./2021/models_output2021_summary.csv")
    write.csv(restrictions, "./2021/restrictions2021_summary.csv")
    write.csv(cum_final_expected, "./2021/cum_final_2021expected.csv")
    write.csv(su2021_quarantine, "./2021/su2021_quarantine.csv")
  }
  
}



