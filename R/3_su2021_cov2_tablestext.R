# Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021
#library
library(lubridate)
library(xtable)
library(dplyr)
# load data
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/data")
# Set seed
set.seed(60321)

cases_summer <- read.csv("cases_su.csv", row.names = 1, header=T, sep=",") 
cases_summer$year <- year(cases_summer$date)
BAG_data_su <- read.csv("BAG_data.csv", row.names = 1, header=T, sep=",")
BAG_data_su$year <- year(BAG_data_su$date)

BAG_data_period <- subset(BAG_data_su, as_date(date) %in% as_date(cases_summer$date))

BAG_data_su %>% group_by(as.character(year))  %>% summarise(mean=mean(age, na.rm = TRUE),sd=sd(age, na.rm = TRUE),quantile_25 =quantile(age, na.rm = TRUE)[2],quantile_75 =quantile(age, na.rm = TRUE)[4])
BAG_data_su %>% group_by(as.character(year), country_cat)  %>% summarise(mean=mean(age, na.rm = TRUE),sd=sd(age, na.rm = TRUE),quantile_25 =quantile(age, na.rm = TRUE)[2],quantile_75 =quantile(age, na.rm = TRUE)[4])

probs = c(.025,.5,.975)

table_all <-c()
table4 <- as.data.frame(matrix(nrow=5, ncol=4))
colnames(table4) <- c("Year", "Overdispersion parameters", "Import scenario", "Re (95%-CrI)")

for (i in c(2020,2021) ) {
  if(i==2020){
    models_output <- read.csv("./2020/models_output2020.csv")
    cases_su <- read.csv("../data/2020/cases_su2020.csv", row.names = 1)
    
  }
  else if(i==2021){
    models_output <- read.csv("./2021/models_output2021.csv")
    cases_su <- read.csv("../data/2021/cases_su2021.csv", row.names = 1)
    
  }
  simulation_accept <- models_output[models_output$simulation_accepted==1,]
  print(i)
  print(paste0("All ",length(simulation_accept[,1])))
  
  imports_d <- cases_su[, c("date","cases_abroad")]
  weighting <- sum(cases_su$cases_date)/(sum(cases_su$cases_swiss)+sum(cases_su$cases_abroad))
  imports_d$abroad_a <- round(cases_su$cases_abroad*weighting)
  imports_d$abroad_b <- round(cases_su$cases_abroad*0.5*weighting)
  imports_d$abroad_c <- round(cases_su$cases_abroad*1.5*weighting)
  import_num <- c(0,colSums(imports_d[,-1]))
  imports_d$date <- as_date(imports_d$date)
  table4$Year <- i
  table4$`Overdispersion parameters` <- c("0.5 (range: 0.49-0.52)")
  table4$`Import scenario` <- c("Baseline scenario", "Confirmed cases exposed abroad","Scenario a)","Scenario b)","Scenario c)")
  
  
  for (i_name in unname(unlist(import_num))) {
    print(length(simulation_accept$Re[simulation_accept$imports %in% i_name]))
  }
  
  for (i_name in unname(unlist(import_num))) {
    print(quantile((simulation_accept$Re[simulation_accept$imports %in% i_name]),probs))
    table4$`Re (95%-CrI)`[grep(i_name, import_num)] <- paste0(round(quantile(simulation_accept$Re[simulation_accept$imports %in% i_name],probs),2)[2], " (", round(quantile(simulation_accept$Re[simulation_accept$imports %in% i_name],probs),2)[1],"-",round(quantile(simulation_accept$Re[simulation_accept$imports %in% i_name],probs),2)[3],")")  
    
  }
  table_all <- rbind(table_all, table4)
}


# global SARS-CoV-2 incidence data
incidence_data <- read.csv("owid-covid-data.csv", header=T, sep=",") # accessed 15/03/2022
incidence_data$location[incidence_data$location=="United Kingdom"] <- "UK"
incidence_data$location[incidence_data$location=="Czechia"] <- "Czech Republic"

# Table 2: Exposure comparison su2020 and su2021
table2 <- as.data.frame(matrix(ncol= 3, nrow= 7 ))
table2[,1] <- c("Total confirmed cases", "Confirmed cases with known exposure", "Confirmed cases exposed in Switzerland","Confirmed cases exposed abroad","Expected cases with exposure abroad\n\n scenario a)","Expected cases with exposure abroad\n\n scenario b)","Expected cases with exposure abroad\n\n scenario c)")

for (i in c("2020","2021") ) {
  if(i=="2020"){
    cases_su <- subset(cases_summer, year(date) %in% 2020)
    
    time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
    incidence <- subset(incidence_data, as_date(date) %in% as_date(seq(time_window[1], time_window[2],1)))

    su_quarantine <- read.csv("./2020/su2020_quarantine.csv")
    colnames(table2) <- c(" ","Summer 2020\n\n (Jun-Sep)","Summer 2021\n\n (Jun-Sep)")
    
}
else if(i=="2021"){
  cases_su <- subset(cases_summer, year(date) %in% 2021)
  
  time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
  incidence <- subset(incidence_data, as_date(date) %in% as_date(seq(time_window[1], time_window[2],1)))

  su_quarantine <- read.csv("./2021/su2021_quarantine.csv")
  colnames(table2) <- c(" ","Summer 2021\n\n (Jun-Sep)","Summer 2021\n\n (Jun-Sep)")
  
}

  
# Table 2: Exposure comparison su2020 and su2021
  i <- as.numeric(i)
table2[1,i-2018] <- sum(cases_su$cases_date)
table2[2,i-2018] <- paste0((sum(cases_su$cases_abroad)+sum(cases_su$cases_swiss))," (",round((sum(cases_su$cases_abroad)+sum(cases_su$cases_swiss))/(sum(cases_su$cases_date))*100),"%)")
table2[3,i-2018] <- paste0(sum(cases_su$cases_swiss)," (",round(sum(cases_su$cases_swiss)/(sum(cases_su$cases_abroad)+sum(cases_su$cases_swiss))*100),"%)")
table2[4,i-2018] <- paste0(sum(cases_su$cases_abroad)," (",round(sum(cases_su$cases_abroad)/(sum(cases_su$cases_abroad)+sum(cases_su$cases_swiss))*100),"%)")
table2[5,i-2018] <- round(sum(cases_su$cases_abroad)/(sum(cases_su$cases_abroad)+sum(cases_su$cases_swiss))*sum(cases_su$cases_date))
table2[6,i-2018] <- round(sum(cases_su$cases_abroad)/(sum(cases_su$cases_abroad)+sum(cases_su$cases_swiss))*sum(cases_su$cases_date)*0.5)
table2[7,i-2018] <- round(sum(cases_su$cases_abroad)/(sum(cases_su$cases_abroad)+sum(cases_su$cases_swiss))*sum(cases_su$cases_date)*1.5)


}
table2 <- xtable(table2)
table2 <- xtable(caption = "Confirmed SARS-CoV-2 cases during summer regarding the most likely country of exposure.",
                 label = "t1", table2)
table2 <- print(table2, size = "footnotesize", include.rownames = FALSE, include.colnames = TRUE)
write(table2, file = "./table/table2.tex")

# Table 3: most likely place of infection and age
#exposed countries
FUN_sig <- function(x){
  if (is.na(x)){return(" ")}
  else if (x =="NA"){return(" ")}
  else if (x ==" "){return(" ")}
  else if (x ==" - "){return(" - ")}
  else if (as.numeric(x) <= 0.001) {return("<.001")}
  else if (as.numeric(x) <= 0.01) {return(paste("~",round(as.numeric(x),3)))}
  else if (as.numeric(x) > 0.01) {return(paste("~",round(as.numeric(x),3)))}
}
FUN_sig1 <- function(x){
  if (is.na(x)){return(" ")}
  else if (x =="NA"){return(" ")}
  else if (x ==" "){return(" ")}
  else if (x ==" - "){return(" - ")}
  else if (as.numeric(x) <= 0.0001) {return("***")}
  else if (as.numeric(x) <= 0.001 & as.numeric(x) > 0.0001) {return("**")}
  else if (as.numeric(x) <= 0.05 & as.numeric(x) > 0.001) {return("*")}
  else {return("")}
}

for (i in c("2020","2021")){
  if(i=="2020"){
    cases_su <- subset(cases_summer, year(date) %in% 2020)
    time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
    BAG_data <- subset(BAG_data_su, as_date(date) %in% seq(time_window[1],time_window[2],1))
    su_quarantine <- read.csv("./2020/su2020_quarantine.csv")
    }
  else if(i=="2021"){
    cases_su <- subset(cases_summer, year(date) %in% 2021)
    time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
    BAG_data <- subset(BAG_data_su, as_date(date)  %in% seq(time_window[1],time_window[2],1))
    su_quarantine <- read.csv("./2021/su2021_quarantine.csv")
    }
  BAG_data <- BAG_data[!is.na(BAG_data$age),]
countries_least10 <- names(table(BAG_data$country)[table(BAG_data$country)>=10])
Fun_countries_least10 <- function(x){
  if( x %in% countries_least10 ){return(as.character(x))}
  else {return("Others")}
}
BAG_data$country <- sapply(BAG_data$country, Fun_countries_least10)
BAG_data$country <- factor(BAG_data$country, levels=unique(names(table(BAG_data$country))[order(table(BAG_data$country), decreasing = TRUE)]), ordered=TRUE)
BAG_data$country <- factor(BAG_data$country, levels=c(levels(BAG_data$country)[!levels(BAG_data$country) %in% c("Others","Unknown")],c("Others","Unknown")))

imports_country_age <- as.data.frame(matrix(ncol= 8, nrow= length(levels(BAG_data$country))+2))
colnames(imports_country_age) <- c("Country","Incidence per 10^6, median (range)", "Confirmed cases","Known exposure \n\n(in %)", "Exposure abroad \n\n(in %)", "Age in years,\n\nmedian (IQR)","T-test, (p-value)", "Mandatory \n\nquarantine")
imports_country_age[,1] <- c(c("All reported cases", "Imports"),levels(BAG_data$country))


for (c in c("All confirmed cases", "Imports")){
  imports_country_age[,2][imports_country_age$Country == c] <- " - "
  imports_country_age[,3][imports_country_age$Country == c] <- if(c== "All reported cases"){length(BAG_data$country)} else{sum(!BAG_data$country %in% c("Unknown","Switzerland"))}
  imports_country_age[,4][imports_country_age$Country == c] <- if(c== "All reported cases"){" - "} else{format(round(sum(!BAG_data$country %in% c("Unknown","Switzerland"))/sum(!BAG_data$country %in% c("Unknown"))*100,2), nsmall = 2)}
  imports_country_age[,5][imports_country_age$Country == c] <- if(c== "All reported cases"){" - "} else{format(round(sum(!BAG_data$country %in% c("Unknown","Switzerland"))/sum(!BAG_data$country %in% c("Unknown","Switzerland"))*100,2), nsmall = 2)}
  imports_country_age[,6][imports_country_age$Country == c] <- if(c== "All reported cases"){paste0(round(quantile(BAG_data$age)[3]), " (",round(quantile(BAG_data$age)[2]),"-",round(quantile(BAG_data$age)[4]),")")} 
  else{paste0(round(quantile(BAG_data$age[!BAG_data$country %in% c("Unknown", "Switzerland")])[3]), " (",round(quantile(BAG_data$age[!BAG_data$country %in% c("Unknown", "Switzerland")])[2]),"-",round(quantile(BAG_data$age[!BAG_data$country %in% c("Unknown", "Switzerland")])[4]),")")}
  imports_country_age[,7][imports_country_age$Country == c] <- " "
  imports_country_age[,8][imports_country_age$Country == c] <- " - "
}
for (c in (levels(BAG_data$country))){
  BAG_data_c <- BAG_data[BAG_data$country %in% c("Switzerland",c),]
  imports_country_age[,2][imports_country_age$Country == c] <- if(c %in% c("Unknown","Others", "Abroad but unknown")){" - "}  else{paste0(format(round(median(incidence$new_cases_smoothed_per_million[incidence$location == c]),1), nsmall = 1), " (", format(round(min(incidence$new_cases_smoothed_per_million[incidence$location == c]),1), nsmall = 1), " - ",format(round(max(incidence$new_cases_smoothed_per_million[incidence$location == c]),1), nsmall = 1), ")")}
  imports_country_age[,3][imports_country_age$Country == c] <- sum(BAG_data$country == c)
  imports_country_age[,4][imports_country_age$Country == c] <- if(c== "Unknown"){" - "} else{format(round(sum(BAG_data$country == c)/sum(BAG_data$country != "Unknown")*100,2), nsmall = 2)}
  imports_country_age[,5][imports_country_age$Country == c] <- if(c %in% c("Unknown", "Switzerland")){" - "} else{format(round(sum(BAG_data$country == c)/sum(!BAG_data$country %in% c("Unknown", "Switzerland"))*100,2), nsmall = 2)}
  imports_country_age[,6][imports_country_age$Country == c] <- paste0(round(quantile(BAG_data$age[BAG_data$country == c])[3]), " (",round(quantile(BAG_data$age[BAG_data$country == c])[2]),"-",round(quantile(BAG_data$age[BAG_data$country == c])[4]),")")
  imports_country_age[,7][imports_country_age$Country == c] <- if(c== "Switzerland"){" - "} else{as.numeric(  t.test(BAG_data_c[,"age"]~BAG_data_c[,"country"])$p.value)}
  imports_country_age[,8][imports_country_age$Country == c] <- if(c %in% c("Unknown", "Switzerland")){" - "} else if(c%in%c("Others","Abroad but unknown") || isTRUE(is.na(su_quarantine$start_date[gsub("\\*", "\\1", su_quarantine$label) ==c]))){" - "} else{paste0(su_quarantine$start_date[gsub("\\*", "\\1", su_quarantine$label) ==c], " - ", su_quarantine$end_date_dot[gsub("\\*", "\\1", su_quarantine$label) ==c])}
}
imports_country_age[2,1] <- "Cross-border-associated"
#imports_country_age <- imports_country_age[,-7]
imports_country_age$pvalue_stars <- sapply(imports_country_age$`T-test, (p-value)` , FUN_sig1)
imports_country_age$`T-test, (p-value)` <- sapply(imports_country_age$`T-test, (p-value)` , FUN_sig)

if(i==2020){
  table_su2020 <- imports_country_age[c(1,3,2,4:length(imports_country_age$Country)),]
  table_su2020$Year <- as.character("2020")
  table_su2020[ nrow(table_su2020) + 1 , ] <- ""
}
else if(i==2021){
  table_su2021 <- imports_country_age[c(1,3,2,4:length(imports_country_age$Country)),]
  table_su2021$Year <- as.character("2021")
}
}
table_su_age <- rbind(table_su2020,table_su2021)
table_su_age <- table_su_age[,c(grep("Year",colnames(table_su_age)), 1:9)]
table3 <- table_su_age[,-10]
colnames(table3) <- c("Year","Country","Incidence per 10^6, median (range)","Confirmed cases","Known exposure \n\n(in %)","Exposure abroad \n\n(in %)","Age in years,\n\nmedian (IQR)","T-test, (p-value)","Mandatory \n\nquarantine")
table3 <- xtable(table3)
table3 <- xtable(caption = "Confirmed cases of SARS-CoV-2 in Switzerland from 1 June to 30 September by country of exposure. Some countries had quarantine restrictions for high incidence regions, e.g., Austria, France, and Spain in 2020, and France and Italy in 2021. Abbreviation: IQR, interquartile range. Incidence is shown as 7-day moving
average.",  label = "t1", table3)
table3 <- print(table3, size = "footnotesize", include.rownames = FALSE, include.colnames = TRUE)

write(table3, file = paste0("./table/table3.tex"))



## text/inputs for manuscript
# global SARS-CoV-2 incidence data
incidence_data <- read.csv("owid-covid-data.csv", header=T, sep=",") # accessed 15/03/2022
incidence_data$location[incidence_data$location=="United Kingdom"] <- "UK"
incidence_data$location[incidence_data$location=="Czechia"] <- "Czech Republic"


for (i in c(2020,2021)) {
  if(i==2020){
    BAG_data <- BAG_data_su[BAG_data_su$year==i,]
    incidence <- incidence_data[incidence_data$date %in% BAG_data$date,]
    cases_su <- read.csv("./2020/cases_su2020.csv")
    #su_quarantine <- read.csv("./2020/su2020_quarantine.csv")
  }
  else if(i==2021){
    BAG_data <- BAG_data_su[BAG_data_su$year==i,]
    incidence <- incidence_data[incidence_data$date %in% BAG_data$date,]
    cases_su <- read.csv("./2021/cases_su2021.csv")
    #su_quarantine <- read.csv("./2021/su2021_quarantine.csv")

  }
  country_level <- table(BAG_data$country)[order(table(BAG_data$country),decreasing=TRUE)]
  #time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
  incidence_noswiss <- incidence
  incidence_noswiss$country <-  factor(incidence$location, levels = c( names(country_level)[!names(country_level) %in% c("Switzerland","Others","Unknown")]))
  incidence_noswiss<- incidence_noswiss[!is.na(incidence_noswiss$country),] 
  
  neighboring_country <- c("France", "Italy", "Germany","Austria")
  round(sum(BAG_data$country %in% neighboring_country)/sum(!BAG_data$country %in% c("Unknown","Switzerland"))*100,2)
  round(sum(!BAG_data$country %in% c(neighboring_country,"Unknown","Switzerland"))/sum(!BAG_data$country %in% c("Unknown","Switzerland"))*100,2)
  
  print(i)
  # for manuscript text
  
  # cases
  sum(cases_su$cases_abroad)
  sum(cases_su$cases_swiss)
  sum(cases_su$cases_date)
  (sum(cases_su$cases_abroad))+sum(cases_su$cases_swiss)
  
  (sum(cases_su$cases_swiss)+sum(cases_su$cases_abroad))/sum(cases_su$cases_date)
  (sum(cases_su$cases_abroad))/sum(cases_su$cases_date)
  (sum(cases_su$cases_abroad))/sum(cases_su$cases_swiss)
  (sum(cases_su$cases_abroad))/sum(cases_su$cases_swiss+cases_su$cases_abroad)
  
  subset(cases_su, cases_abroad %in% max(cases_abroad))
  subset(cases_su, cases_date %in% max(cases_date))
  
  #Age distribution in study population:
  #paste0(round(quantile(BAG_data$age)[3]), " (IQR:",round(quantile(BAG_data$age)[2]),"-",round(quantile(BAG_data$age)[4]),"; range: ",round(quantile(BAG_data$age)[1]),"-",round(quantile(BAG_data$age)[5]),")")
  #round(mean(BAG_data$age))
  #paste0(round(quantile(BAG_data$age[!BAG_data$country %in% c("Switzerland", "Unknown")])[3]), " (",round(quantile(BAG_data$age[!BAG_data$country %in% c("Switzerland", "Unknown")])[2]),"-",round(quantile(BAG_data$age[!BAG_data$country %in% c("Switzerland", "Unknown")])[4]),"; range: ",round(quantile(BAG_data$age[!BAG_data$country %in% c("Switzerland", "Unknown")])[1]),"-",round(quantile(BAG_data$age[!BAG_data$country %in% c("Switzerland", "Unknown")])[5]),")")
  #paste0(round(quantile(BAG_data$age[BAG_data$country %in% c("Switzerland")])[3]), " (",round(quantile(BAG_data$age[BAG_data$country %in% c("Switzerland")])[2]),"-",round(quantile(BAG_data$age[BAG_data$country %in% c("Switzerland")])[4]),"; range: ",round(quantile(BAG_data$age[BAG_data$country %in% c("Switzerland")])[1]),"-",round(quantile(BAG_data$age[BAG_data$country %in% c("Switzerland")])[5]),")")
  #paste0(round(quantile(BAG_data$age[BAG_data$country %in% c("Unknown")])[3]), " (",round(quantile(BAG_data$age[BAG_data$country %in% c("Unknown")])[2]),"-",round(quantile(BAG_data$age[BAG_data$country %in% c("Unknown")])[4]),"; range: ",round(quantile(BAG_data$age[BAG_data$country %in% c("Unknown")])[1]),"-",round(quantile(BAG_data$age[BAG_data$country %in% c("Unknown")])[5]),")")
  
  #Gender distribution in study population:
  table((BAG_data$sex))
  round(table(BAG_data$sex)/length(BAG_data$sex)*100,2)
  round(table(BAG_data$sex[BAG_data$country=="Switzerland"])/length(BAG_data$sex[BAG_data$country=="Switzerland"])*100,2)
  round(table(BAG_data$sex[BAG_data$country=="Unknown"])/length(BAG_data$sex[BAG_data$country=="Unknown"])*100,2)
  round(table(BAG_data$sex[!na.omit(BAG_data$country %in%c("Switzerland","Unknown"))])/length(BAG_data$sex[!BAG_data$country %in%c("Switzerland","Unknown")])*100,2)
  table(BAG_data$sex[!na.omit(BAG_data$country %in%c("Switzerland","Unknown"))])
  chisq.test(BAG_data$country_cat[BAG_data$country_cat != "Unknown" &BAG_data$sex !="Unbekannt"], BAG_data$sex[BAG_data$country_cat != "Unknown" &BAG_data$sex !="Unbekannt"], correct=FALSE)$p.value
  chisq.test(BAG_data$country_exposure_known[BAG_data$sex !="Unbekannt"], BAG_data$sex[BAG_data$sex !="Unbekannt"], correct=FALSE)$p.value
  
  # incidence data
  length(unique(incidence_noswiss$country))
  print(table(incidence_noswiss$country[incidence_noswiss$new_cases_smoothed_per_million> na.omit(incidence$new_cases_smoothed_per_million[incidence$location=="Switzerland"])]))
  mean(table(incidence_noswiss$country[incidence_noswiss$new_cases_smoothed_per_million> na.omit(incidence$new_cases_smoothed_per_million[incidence$location=="Switzerland"])]))
  quantile(table(incidence_noswiss$country[incidence_noswiss$new_cases_smoothed_per_million > na.omit(incidence$new_cases_smoothed_per_million[incidence$location=="Switzerland"])]))
  
  format(round(table(incidence_noswiss$country[incidence_noswiss$new_cases_smoothed_per_million> incidence$new_cases_smoothed_per_million[incidence$location=="Switzerland"]])/length(incidence$date)*100,1),1)[order(format(round(table(incidence_noswiss$country[incidence_noswiss$new_cases_smoothed_per_million> incidence$new_cases_smoothed_per_million[incidence$location=="Switzerland"]])/length(incidence$date)*100,1),1))]
  
}



# Age and most likely place of exposure
## statistics on age and most likely place of infection using t.test
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/data/table")
cases_summer <- read.csv("../cases_su.csv", row.names = 1, header=T, sep=",") # add file to Ubelix
cases_summer$year <- year(cases_summer$date)

for (i in c(2020,2021)){
  if(i %in% 2020){
    cases_su <- subset(cases_summer, year(date) %in% 2020)
    time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
    BAG_data <- BAG_data_su[BAG_data_su$year==i,]
  }
  else if(i %in% 2021){
    cases_su <- subset(cases_summer, year(date) %in% 2021)
    time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
    BAG_data <- BAG_data_su[BAG_data_su$year==i,]
  }
  age_summary<- as.data.frame(matrix(ncol= 0, nrow= length(unique(BAG_data$country[!grepl("Switzerland",BAG_data$country)]))))
  BAG_data <- BAG_data[!is.na(BAG_data$age),]
for (c in unique(BAG_data$country[!grepl("Switzerland",BAG_data$country)])) {
  BAG_data_c <- BAG_data[BAG_data$country %in% c("Switzerland",c),]
  #t.test(BAG_data_c[,"age"]~BAG_data_c[,"country"])$p.value
  age_summary$year[grepl(c,unique(BAG_data$country[!grepl("Switzerland",BAG_data$country)]))] <- i
  age_summary$country[grepl(c,unique(BAG_data$country[!grepl("Switzerland",BAG_data$country)]))] <- c
  age_summary$test_pvalue[grepl(c,unique(BAG_data$country[!grepl("Switzerland",BAG_data$country)]))] <- t.test(BAG_data_c[,"age"]~BAG_data_c[,"country"])$p.value
  age_summary$mean_age[grepl(c,unique(BAG_data$country[!grepl("Switzerland",BAG_data$country)]))] <- paste0(round(mean(BAG_data_c[BAG_data_c$country==c,"age"]))," compared to ", round(mean(BAG_data_c[BAG_data_c$country=="Switzerland","age"])))
  
}
  BAG_data_crossborder <- BAG_data[BAG_data$country_cat!="Unknown",]
  age_summary[length(age_summary$country)+1,] <- c("cross-border cases", i, paste0(t.test(BAG_data_crossborder[,"age"]~BAG_data_crossborder[,"country_cat"])$p.value),paste0(round(mean(BAG_data_crossborder[BAG_data_crossborder$country_cat=="Abroad","age"]))," compared to ", round(mean(BAG_data_crossborder[BAG_data_crossborder$country=="Switzerland","age"]))))
  if(i%in%2020){
    age_summary20 <- age_summary
  }
}
age_summary<- rbind(age_summary20, age_summary)


age_summary$test_pvalue1 <- sapply(age_summary$test_pvalue , FUN_sig)
age_summary$test_pvalue2 <- sapply(age_summary$test_pvalue , FUN_sig1)
paste0("Individals that were exposed to SARS-CoV-2 in ",paste0(rownames(age_summary)[age_summary$test_pvalue<.05],collapse=", ")," differed in their age significantly (<.05) compared to individuals that were only in Switzerland.")
paste0("Individuals were significantly younger (<.05) if they were exposed to SARS-CoV-2 in ",paste0(rownames(age_summary)[age_summary$test_pvalue<.05 & age_summary$Estimate < 0],collapse=", ")," compared to individuals that were only in Switzerland.")
paste0("Individuals were significantly older (<.05) if they were exposed to SARS-CoV-2 in ",paste0(rownames(age_summary)[age_summary$test_pvalue<.05 & age_summary$Estimate > 0],collapse=", ")," compared to individuals that were only in Switzerland.")

paste0("Individals that were exposed to SARS-CoV-2 in ",paste0(rownames(age_summary)[age_summary$test_pvalue<.001],collapse=", ")," differed in their age significantly (<.001) compared to individuals that were only in Switzerland.")
paste0("Individuals were significantly younger (<.001) if they were exposed to SARS-CoV-2 in ",paste0(rownames(age_summary)[age_summary$test_pvalue<.001 & age_summary$Estimate < 0],collapse=", ")," compared to individuals that were only in Switzerland.")
paste0("Individuals were significantly older (<.001) if they were exposed to SARS-CoV-2 in ",paste0(rownames(age_summary)[age_summary$test_pvalue<.001 & age_summary$Estimate > 0],collapse=", ")," compared to individuals that were only in Switzerland.")

cum_final_2021expected <- read.csv("2021/cum_final_2021expected.csv")
cum_final_2021expected
cum_final_2020expected <- read.csv("2020/cum_final_2020expected.csv")
cum_final_2020expected



# per day difference of trajectories to reported incidence (blue line):
#  square errors
for(i in c(2020, 2021)){
  labels_prop_ls = factor(c("Baseline assuming no imports","Reported imports",
                            "Scenario a) assuming reported imports were representative",
                            "Scenario c) ‘upper limit’ that assumed more imports among cases with missing information",
                          "Scenario b) ‘lower limit’ that assumed fewer imports among cases with missing information"),
                          levels=c(c("Baseline assuming no imports","Reported imports",
                                     "Scenario a) assuming reported imports were representative",
                                     "Scenario c) ‘upper limit’ that assumed more imports among cases with missing information",
                                     "Scenario b) ‘lower limit’ that assumed fewer imports among cases with missing information")))
  if(i==2020){
    cases_su <- read.csv("2020/cases_su2020.csv")
    models_output <- read.csv("2020/models_output2020.csv")
    #pop_size <- 8606033 #https://www.pxweb.bfs.admin.ch/pxweb/de/px-x-0102020000_103/px-x-0102020000_103/px-x-0102020000_103.px
  }
  if(i==2021){
    cases_su <- read.csv("2021/cases_su2021.csv")
    models_output <- read.csv("2021/models_output2021.csv")
    #pop_size <- 8670300 #https://www.pxweb.bfs.admin.ch/pxweb/de/px-x-0102020000_103/px-x-0102020000_103/px-x-0102020000_103.px
  }
  pop_size <- 8544527
  
  models_output <- models_output[models_output$simulation_accepted %in% 1,]
  
  models_output_ls<-NA
  models_output_ls <- data.frame(matrix(NA, ncol = length(unique(models_output$imports)), nrow = 10^5))
  colnames(models_output_ls)<- unique(models_output$imports)
  for (j in unique(models_output$imports)) {
    models_output_j <- models_output[models_output$imports==j,]
    models_output_j <- as.data.frame(t(models_output_j[9:130]))
    
    models_output_j <-as.data.frame(sapply(1:length(models_output_j[1,]), function(x) ((cases_su$incidence_weigthed*(pop_size/1e5) - as.numeric(models_output_j[,x]))^2)))
    models_output_ls[1:length(models_output_j[1,]),as.character(j)]<- sapply(1:length(models_output_j[1,]), function(x)   sum(models_output_j[,x]))
  }
  print(i)
  #print(colnames(models_output_ls))
 
  #print(sapply(1:5, function(x) min(na.omit(models_output_ls[,x]))))
  #print(min(sapply(1:5, function(x) min(na.omit(models_output_ls[,x])))))
  
  #print(sapply(1:5, function(x) length(na.omit(models_output_ls[,x]))))
  
  #print(sapply(1:5, function(x) sum(sample(na.omit(models_output_ls[,x]),1e3))))
  #print(min(sapply(1:5, function(x) min(sum(sample(na.omit(models_output_ls[,x]),1e3))))))
  
  #print(sapply(1:5, function(x) mean(sample(na.omit(models_output_ls[,x]),1e3))))
  #print(min(sapply(1:5, function(x) min(mean(sample(na.omit(models_output_ls[,x]),1e3))))))

  
  
  
  print(sapply(1:5, function(x) median(sample(na.omit(models_output_ls[,x]),1e3))))
  print(sapply(1:5, function(x) quantile(sample(na.omit(models_output_ls[,x]),1e3))))
  iqr_prob <- c(.25,.5,.75)
  models_output_ls_1e3 <- sapply(1:5, function(x)sample(na.omit(models_output_ls[,x]),1e3))
  colnames(models_output_ls_1e3) <- labels_prop_ls
  
  #print(min(sapply(1:5, function(x) min(median(sample(na.omit(models_output_ls[,x]),1e3))))))
  square_error <- sapply(1:5, function(x) as.numeric(sqrt(quantile(models_output_ls_1e3[,x],iqr_prob))))
  square_error <- rbind(square_error, sapply(1:5, function(x) as.numeric(sqrt(mean(models_output_ls_1e3[,x])))))
  
  square_error <- rbind(square_error, sapply(1:5, function(x) as.numeric(sd(sqrt(models_output_ls_1e3[,x])))))
  colnames(square_error) <- colnames(models_output_ls_1e3)
square_error <- rbind(square_error,paste0(formatC(round(as.numeric(square_error[2,])), format="d", big.mark=","), " (IQR: ",formatC(round(square_error[1,]), format="d", big.mark=","),"-",formatC(round(square_error[3,]), format="d", big.mark=","),")"))
square_error <- rbind(square_error,paste0(formatC(round(as.numeric(square_error[4,])), format="d", big.mark=","), " (sd: ",formatC(round(as.numeric(square_error[5,])), format="d", big.mark=","),")"))
square_error <- square_error[,order(as.numeric(square_error[4,]))]

  #print(square_error[4,])
  print(paste0("For ",i,", the RMSE was minimal to the daily incidence of confirmed cases (7-day moving average) for ",paste0(colnames(square_error)," ",square_error[7,],collapse = ", followed by ")))
  #print(sapply(1:5, function(x) quantile(sample(na.omit(models_output_ls[,x]),1e3),probs))[,order(sapply(1:5, function(x) mean(sample(na.omit(models_output_ls[,x]),1e3))))])
 
  
}


