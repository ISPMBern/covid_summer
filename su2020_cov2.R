
#Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021

#library
library(tiff)
library(eps)
library(ggplot2)
library(MASS)
library(ggpubr)
library(grid)
library(gridExtra)
library(wesanderson)
library(lubridate)
library(reshape2)
# Set seed
set.seed(60321)

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/StochasticModel_SarsCoV2/su2020_cov2")

# Prepare and load data
#####
# get data from FOPH (CONFIDENTIAL!) diagnosed COVID-19 cases per day:
BAG_data <- readRDS("/Users/mr19m223/Dropbox/INPUT_Martina/data/2021-01-04_08-12-19_MM820b_FOPH_COVID19_data_extract.rds")
date_fun <- function(x) as_date(x)
time_window <- c(as_date("2020-06-01"), as_date("2020-09-30"))
BAG_data[,c("fall_dt","hospdatin","pttoddat")]  <- data.frame(lapply(BAG_data[,c("fall_dt","hospdatin","pttoddat")] , date_fun))
BAG_data$reported <- BAG_data$fall_dt 
BAG_data$hospitalized <- BAG_data$hospdatin 
BAG_data$dead  <- BAG_data$pttoddat 

swiss_cases_su2020 <- data.frame(date= seq(time_window[1],time_window[2],1),
                           cases_reported=NA,
                           cases_hospitalized=NA,
                           cases_dead =NA,
                           cases_swiss=NA,
                           cases_abroad=NA)

swiss_cases_su2020$cases_reported <- hist(subset(BAG_data$reported,  BAG_data$reported %in% seq(time_window[1],time_window[2],1) & BAG_data$ktn != "FL"), breaks = seq(time_window[1]-1, time_window[2], 1), plot = FALSE)$counts
swiss_cases_su2020$cases_hospitalized <- hist(subset(BAG_data$hospitalized, BAG_data$hospitalized %in% seq(time_window[1],time_window[2],1) & BAG_data$reported %in% seq(time_window[1],time_window[2],1) & BAG_data$ktn != "FL"), breaks = seq(time_window[1]-1, time_window[2], 1), plot = FALSE)$counts
swiss_cases_su2020$cases_dead  <- hist(subset(BAG_data$dead ,  BAG_data$dead  %in% seq(time_window[1],time_window[2],1) & BAG_data$reported %in% seq(time_window[1],time_window[2],1) & BAG_data$ktn != "FL"), breaks = seq(time_window[1]-1, time_window[2], 1), plot = FALSE)$counts
swiss_cases_su2020$cases_swiss <- hist(subset(BAG_data$reported,   BAG_data$exp_ort %in% c(1) & BAG_data$reported %in% seq(time_window[1],time_window[2],1) & BAG_data$ktn != "FL"), breaks = seq(time_window[1]-1, time_window[2], 1), plot = FALSE)$counts
swiss_cases_su2020$cases_abroad <- hist(subset(BAG_data$fall_dt, BAG_data$exp_ort %in% c(2,3) & BAG_data$fall_dt %in% seq(time_window[1],time_window[2],1) & BAG_data$ktn != "FL"), breaks = seq(time_window[1]-1, time_window[2], 1), plot = FALSE)$counts

# Overview of data used, time window: 1st Jun to 30th Sep 2020
BAG_data_su2020 <- subset(BAG_data, fall_dt %in% seq(time_window[1],time_window[2],1) & ktn != "FL")
paste0("In total ", length(BAG_data_su2020$fall_dt), " cases were reported by the FOPH. For ",
       length(subset(BAG_data_su2020$fall_dt, BAG_data_su2020$exp_ort %in% c(1,2,3)))," (",
       round(length(subset(BAG_data_su2020$fall_dt, BAG_data_su2020$exp_ort %in% c(1,2,3)))/length(BAG_data_su2020$fall_dt)*100,2), "%)",
       " cases, it was stated if infection occured aboard or in Switzerland. The ratio of international and national transmission was ",
       length(subset(BAG_data_su2020$fall_dt, BAG_data_su2020$exp_ort %in% c(2,3))),"/", length(subset(BAG_data_su2020$fall_dt, BAG_data_su2020$exp_ort %in% c(1))),
       " (", round(length(subset(BAG_data_su2020$fall_dt, BAG_data_su2020$exp_ort %in% c(2,3)))/length(subset(BAG_data_su2020$fall_dt, BAG_data_su2020$exp_ort %in% c(1)))*100,2) ,"%).")

#####

# Fit cases to generalized negative binomial model
## estimate growth rate and Re
#####
# Calculate weighted growth rate
swiss_cases_su2020$weekend <- ifelse(weekdays(swiss_cases_su2020$date) == "Saturday" | weekdays(swiss_cases_su2020$date) == "Sunday", 1, 0)
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

set <- subset(swiss_cases_su2020, date %in% seq(time_window[1], time_window[2], 1))
fit <- glm.nb(cases_hospitalized ~ date + weekend, data = set)
r_all[1, 5:8] <- c(coef(summary(fit))[1, 1:2], coef(summary(fit))[2, 1:2])
fit <- glm.nb(cases_dead  ~ date + weekend, data = set)
r_all[1, 9:12] <- c(coef(summary(fit))[1, 1:2], coef(summary(fit))[2, 1:2])
fit <- glm.nb(cases_reported ~ date + weekend, data = set)
r_all[1, 1:4] <- c(coef(summary(fit))[1, 1:2], coef(summary(fit))[2, 1:2])

r_all$intercept_weigth <- (r_all$intercept_cases/r_all$error_cases^2 + r_all$intercept_hosp/r_all$error_hosp^2 + r_all$intercept_dead/r_all$error_dead^2)/(1/r_all$error_cases^2 + 1/r_all$error_hosp^2 + 1/r_all$error_dead^2)
r_all$intercept_error_weigth <- sqrt(1/(1/r_all$intercept_error_cases^2 + 1/r_all$intercept_error_hosp^2 + 1/r_all$intercept_error_dead^2))

r_all$rate_weigth <- (r_all$rate_cases/r_all$error_cases^2 + r_all$rate_hosp/r_all$error_hosp^2 + r_all$rate_dead/r_all$error_dead^2)/(1/r_all$error_cases^2 + 1/r_all$error_hosp^2 + 1/r_all$error_dead^2)
r_all$error_weigth <- sqrt(1/(1/r_all$error_cases^2 + 1/r_all$error_hosp^2 + 1/r_all$error_dead^2))

swiss_cases_su2020$predicted_cases <- round(exp(fit$coefficients[1]+ fit$coefficients[2]*as.numeric(swiss_cases_su2020$date)))
swiss_cases_su2020$predicted_cases2 <- swiss_cases_su2020$predicted_cases*2
swiss_cases_su2020$predicted_cases3 <- swiss_cases_su2020$predicted_cases*3

# calculate R_e
generation_time <- mu <- 5.2
sigma <- 2.8 
variance <- sigma^2
gamma_rate <- mu/variance#gamma_shape/generation_time
gamma_shape <- mu^2/variance

repro <- function(growth) {
  (1 + growth/rate)^gamma_shape
}
paste0("Re =",round(repro(r_all$rate_weigth),3)," (95%-CI: ", round(repro(r_all$rate_weigth- qnorm(0.975)* (r_all$error_weigth)),3),"-", round(repro(r_all$rate_weigth + qnorm(0.975)* (r_all$error_weigth)),3),")")
paste0("growth rate =",round((r_all$rate_weigth),3)," (95%-CI: ", round((r_all$rate_weigth- qnorm(0.975)* (r_all$error_weigth)),3),"-", round((r_all$rate_weigth + qnorm(0.975)* (r_all$error_weigth)),3),")")

#####


# Initialize simulation
#####

Re_all <- seq(0.60,1.2, by=0.1)
runs <- 1e3 
max_time <- c(time_window[1]:time_window[2])
dispersion_parameters <- c(Inf,1,0.5,0.1) #<- seq(0.4, 0.6, by=0.1) 

secondary <-c()
secondary_t <- c()
cases_d_runs <- data.frame(array(0, dim = c(length(max_time),runs)))

#####

# Stochastic branching model without imports
#####
# seeds:
seeds <- data.frame(date=as_date((time_window[1]-5):(time_window[1]-1)),
                    cases=NA)
seeds$cases <- round(exp(fit$coefficients[1]+ fit$coefficients[2]*as.numeric(seeds$date))*2)
t0  <- rep(as.numeric(seeds[,1])-as.numeric(time_window[1]-1), seeds[,2])

su2020_cases<- function(Re, dispersion){
  lapply(dispersion,function(k) {
    lapply(Re,function(R) {
      sapply(1:runs,function(i) { 
        secondary_t <- t0
        while(length(secondary_t<length(max_time)+0.5) >0 & sum(cases_d_runs[,i])<1e6) {
          secondary <- rnbinom(length(secondary_t), size = k, mu = R)
          secondary_t <- rep(secondary_t[secondary_t<(length(max_time)+0.5)], secondary[secondary_t<(length(max_time)+0.5)])
          secondary_t <- secondary_t + round(rgamma(length(secondary_t), shape = gamma_shape, rate = gamma_rate))
          cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(length(max_time)+0.5)])),rownames(cases_d_runs)),i] <-  cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(length(max_time)+0.5)])),rownames(cases_d_runs)),i] + table(secondary_t[secondary_t>0&secondary_t<(length(max_time)+0.5)])
        }
        return(cases_d_runs[,i])
      })
    })
  })
}

cases_d_runs_all <- su2020_cases(Re_all, dispersion_parameters)

#####

# Stochastic branching model of imports
#####
# imports:
imports_d <- swiss_cases_su2020[, c("date","cases_abroad")]
weighting <- 1+(1-(sum(swiss_cases_su2020$cases_swiss)+sum(swiss_cases_su2020$cases_abroad))/sum(swiss_cases_su2020$cases_reported))

imports_d$abroad_weighted <- round(imports_d$cases_abroad*weighting)
imports_d$abroad2  <- round(swiss_cases_su2020$cases_abroad*2)
imports_d$abroad2_weighted <- round(swiss_cases_su2020$cases_abroad*2*weighting)
imports_d$abroad3  <- round(swiss_cases_su2020$cases_abroad*3)
imports_d$abroad3_weighted <- round(swiss_cases_su2020$cases_abroad*3*weighting)

colSums(imports_d[,-1])

su2020_imports <- function(Re, dispersion, imports){
  lapply(1:(length(imports[1,])),function(I) {
    ti <- rep(c(15:length(max_time)),imports[15:length(max_time),I])
    lapply(dispersion,function(k) {
      lapply(Re,function(R) {
        sapply(1:runs,function(i) { 
          secondary_t <- ti
          while(length(secondary_t<length(max_time)+0.5) >0 & sum(cases_d_runs[,i])<1e6) {
            secondary <- rnbinom(length(secondary_t), size = k, mu = R)
            secondary_t <- rep(secondary_t[secondary_t<(length(max_time)+0.5)], secondary[secondary_t<(length(max_time)+0.5)])
            secondary_t <- secondary_t + round(rgamma(length(secondary_t), shape = gamma_shape, rate = gamma_rate))
            cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(length(max_time)+0.5)])),rownames(cases_d_runs)),i] <-  cases_d_runs[match(names(table(secondary_t[secondary_t>0&secondary_t<(length(max_time)+0.5)])),rownames(cases_d_runs)),i] + table(secondary_t[secondary_t>0&secondary_t<(length(max_time)+0.5)])
          }
          return(cases_d_runs[,i])
        })
      })
    })
  })
}

imports_cases_d_runs_all <- su2020_imports(Re_all, dispersion_parameters, imports_d[,-1]) 

# add imports and their dynamic to national dynamic:
## could also include different combinations e.g. imports other R values etc. if wished, so far same condition for imports as for national dynamic
all_cases_imports_infect <- lapply(seq_len(length(imports_cases_d_runs_all)+1), function(X) cases_d_runs_all)

# Imports infectious
for (I in 1:length(imports_d[,-1])) {
  for (k in 1:length(dispersion_parameters)) {
    for (R in 1:length(Re_all)) {
      all_cases_imports_infect[[I+1]][[k]][[R]] <-  all_cases_imports_infect[[I+1]][[k]][[R]] + imports_cases_d_runs_all[[I]][[k]][[R]]
    }
  }
}

# Imports non-infectious
all_cases_imports <- lapply(seq_len(length(imports_cases_d_runs_all)+1), function(X) cases_d_runs_all)

for (I in 1:length(imports_d[,-1])) {
  for (k in 1:length(dispersion_parameters)) {
    for (R in 1:length(Re_all)) {
      all_cases_imports[[I+1]][[k]][[R]] <-  all_cases_imports[[I+1]][[k]][[R]] + imports_d[[1+I]]
    }
  }
}
  
#####

# Estimating growth rate for simulations (including data prep for plots)
#####
growth_r <- as.data.frame(matrix(0,ncol=length(Re_all),nrow = runs))

growth_rate_fun <- function(imports, Re, dispersion, all_cases){
  lapply(1:length(imports),function(I) {
  lapply(1:length(dispersion),function(k) {
    sapply(1:length(Re),function(R) {
      sapply(1:runs,function(i) { 
        nb.model <- c()
        nb.model <- glm.nb(all_cases[[I]][[k]][[R]][,i]~c(1:length(all_cases[[I]][[k]][[R]][,i])),link = "log")
        growth_r[i,R] <-coef(nb.model)[2]
        return(growth_r[i,R])
      })
    })
    })
  })
}

growth_r_all_imports_infect <- growth_rate_fun(imports_d, Re_all, dispersion_parameters,all_cases_imports_infect)
growth_r_all_imports <- growth_rate_fun(imports_d, Re_all, dispersion_parameters,all_cases_imports)

# sum up "growth_r_all"
growth_r_all_summary <- as.data.frame(matrix(ncol=7,nrow = length(Re_all)*length(dispersion_parameters)* length(imports_d)))
colnames(growth_r_all_summary) <- c("Re", "median", "ll_IQR", "ul_IQR","imports", "imports_num", "dispersion_parameter")
growth_summary <- function(growth_r_all){
  growth_r_all_summary[,1] <- rep(Re_all,length(imports_d) * length(dispersion_parameters))
  growth_r_all_summary[,5] <- c(rep("No imports", length(Re_all)*length(dispersion_parameters)),rep("Imports", length(imports_d[,-1])*length(Re_all)*length(dispersion_parameters)))
  growth_r_all_summary[,6] <- rep(c(0,unname(colSums(imports_d[,-1]))), each = length(Re_all)*length(dispersion_parameters))
  growth_r_all_summary[,7] <- rep(dispersion_parameters,times=length(imports_d) , each = length(Re_all))
  
  for (I in 1:length(imports_d)) {
    for (k in 1:length(dispersion_parameters)) {
      for (R in 1:length(Re_all)) {
        m <- (I-1)*length(dispersion_parameters)*length(Re_all)+(k-1)*length(Re_all)+R-1+1
        growth_r_all_summary[m,2] <- quantile(growth_r_all[[I]][[k]][,R])[3]
        growth_r_all_summary[m,3] <- quantile(growth_r_all[[I]][[k]][,R])[2]
        growth_r_all_summary[m,4] <- quantile(growth_r_all[[I]][[k]][,R])[4]
      }
    }
  }
  
  growth_r_all_summary[,c(2:4,6)] <- lapply(growth_r_all_summary[,c(2:4,6)], as.numeric)
  growth_r_all_summary[,c(1,5,7)] <- lapply(growth_r_all_summary[,c(1,5,7)], as.character)
  return(growth_r_all_summary)
}

growth_imports_infect_summary<- growth_summary(growth_r_all_imports_infect)
growth_imports_summary <- growth_summary(growth_r_all_imports)
#####

# Cumulative cases for each simulations (including data prep for plots)
#####

cum_cases <- as.data.frame(matrix(0,nrow = length(Re_all)*length(dispersion_parameters)* length(imports_d), ncol = runs))
cum_cases_function <- function(all_cases){
  for (I in 1:length(imports_d)) {
  for (k in 1:length(dispersion_parameters)) {
    for (R in 1:length(Re_all)) {
      for (i in 1:runs) {
          m <- (I-1)*length(dispersion_parameters)*length(Re_all)+(k-1)*length(Re_all)+R-1+1
          cum_cases[m,i] <- as.numeric(sum(all_cases[[I]][[k]][[R]][,i]))
        }
      }
    }
  }

# legend for "cum_cases"
cum_cases[,c( "imports_num", "dispersion_parameter","Re")]<- NA
cum_cases <- cum_cases[,c("imports_num", "dispersion_parameter","Re", colnames(cum_cases[!colnames(cum_cases) %in% c("imports_num", "dispersion_parameter","Re")]))] 

cum_cases[,1] <- as.numeric(rep(c(0,unname(colSums(imports_d[,-1]))), each = length(Re_all)*length(dispersion_parameters)))
cum_cases[,2] <- as.numeric(rep(dispersion_parameters,times=length(imports_d) , each = length(Re_all)))
cum_cases[,3] <- as.character(rep(Re_all,length(imports_d) * length(dispersion_parameters)))
return(cum_cases)
}
cum_cases_imports_infect <- cum_cases_function(all_cases_imports_infect)
cum_cases_imports <- cum_cases_function(all_cases_imports)
#####

# Final size of different scenarios
#####
final_size <- as.data.frame(matrix(0,nrow = length(Re_all)*length(dispersion_parameters)* length(imports_d), ncol = runs))
final_cases_function <- function(all_cases){
  for (I in 1:length(imports_d)) {
    for (k in 1:length(dispersion_parameters)) {
      for (R in 1:length(Re_all)) {
        for (i in 1:runs) {
          m <- (I-1)*length(dispersion_parameters)*length(Re_all)+(k-1)*length(Re_all)+R-1+1
          final_size[m,i] <- all_cases[[I]][[k]][[R]][length(max_time),i]
        }
      }
    }
  }
  
  # legend for "final_size"
  final_size[,c( "imports_num", "dispersion_parameter","Re")]<- NA
  final_size <- final_size[,c("imports_num", "dispersion_parameter","Re", colnames(final_size[!colnames(final_size) %in% c("imports_num", "dispersion_parameter","Re")]))] 
  
  final_size[,1] <- as.numeric(rep(c(0,unname(colSums(imports_d[,-1]))), each = length(Re_all)*length(dispersion_parameters)))
  final_size[,2] <- as.numeric(rep(dispersion_parameters,times=length(imports_d) , each = length(Re_all)))
  final_size[,3] <- as.character(rep(Re_all,length(imports_d) * length(dispersion_parameters)))
  return(final_size)
}

final_cases_imports_infect <- final_cases_function(all_cases_imports_infect)
final_cases_imports <- final_cases_function(all_cases_imports)

#####

#Plots:
cols1 <- wes_palette("GrandBudapest2", length(Re_all)+2, type = c( "continuous"))
col <- wes_palette("Royal2", 12, type = c( "continuous"))

## Visualize probability of stochastic extinction, q
#####
generation_time * 3* sigma
P_extinction <- function(R,n,k){
  p_ext <- as.numeric(1/(R)^n)
  ifelse(p_ext < 1, p_ext, 1)
}
Rei <- seq(0.0001, 2, by=1e-5)
ni <- c(25, 50, 75)

p_ext <- mapply(P_extinction, list(Rei), ni, dispersion_parameters[2])

plot(Rei, p_ext[,2],xlim = c(0,2),col=cols[1], type="l", lty=1, lwd=2,las=1, xlab=bquote(italic("R"["e"])), ylab="Probability of stochastic extinction", frame=FALSE)
polygon(c(Rei, rev(Rei)), c(p_ext[,1], rev(p_ext[,3])),
        col=alpha(cols[1],0.5), border = NA)
#matplot(Rei, res, col=cols, type="l", lty=1, lwd=2, xlab="x", ylab="result")
#####


# Visualizing cases per day with and without Influx
#####
for (i in 1:3) {
  if(i==1){
    import_day <- imports_d 
    pdf(file=paste0("sim_cases_d_imports_infect_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = length(imports_d)*4, width =length(dispersion_parameters)*4)
    cases_all <- all_cases_imports_infect
  }
  if(i==2){
    import_day <- imports_d[1:3]
    pdf(file=paste0("sim3_cases_d_imports_infect_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = length(import_day)*4, width =length(dispersion_parameters)*4)
    cases_all <- all_cases_imports_infect
  }
  if(i==3){
    import_day <- imports_d 
    pdf(file=paste0("sim_cases_d_imports_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = length(import_day)*4, width =length(dispersion_parameters)*4)
    cases_all <- all_cases_imports
  }
  par(mar = c(5.1, 8.1, 4.1, 2.1),mfrow=c(length(import_day),length(dispersion_parameters)+1),bg=NA)
  for (I in 1:length(import_day)) {
    for (k in 1:length(dispersion_parameters)) {
      plot(NA,
           type = "n", xlim =c(time_window[1],time_window[2]), ylim = c(0, 2000),xaxt="n",
           xlab = NA, ylab = "", frame = FALSE, las=2)
      axis.Date(1, at=seq(min(time_window), max(time_window+1), by="months"), format="%d-%b")
      
      for (R in 1:length(Re_all)) {
        
        if(I==1){
          k_num <- dispersion_parameters[k]
          mtext(bquote(italic("k") == .(k_num)), side=3, line=2, cex.lab=1,las=1, col="black")}
        
        if(k ==1){
          if(I==1){
            import_num <- 0
          }
          else if(I>1){
            import_num <- sum(import_day[,I])
          }
          mtext(bquote(italic("I") == .(import_num)), side=2, line=3, cex.lab=1,las=3, col="black")}
        
        col_max <- matrix(0, ncol=2,nrow= 1)
        col_min <- matrix(1e10, ncol=2,nrow= 1)
        for (i in 1:runs) {
          if(sum(cases_all[[I]][[k]][[R]][,i])>col_max[,2]){
            col_max[,c(1:2)] <- c(i,sum(cases_all[[I]][[k]][[R]][,i])) 
          }
          if(sum(cases_all[[I]][[k]][[R]][,i])<col_min[,2]){
            col_min[,c(1:2)] <- c(i,sum(cases_all[[I]][[k]][[R]][,i])) 
          }
        }
        polygon(c(c(time_window[1]:time_window[2]), rev(c(c(time_window[1]:time_window[2])))), c(cases_all[[I]][[k]][[R]][,col_min[,1]], rev(cases_all[[I]][[k]][[R]][,col_max[,1]])),
                col=alpha(cols[R],0.4), border = NA)
        
      }
      points(c(time_window[1]:time_window[2]), swiss_cases_su2020$cases_reported, col=col[3], pch=20, cex=0.5)
      polygon(c(c(time_window[1]:time_window[2]), rev(c(c(time_window[1]:time_window[2])))), c(round(exp(fit$coefficients[1]+ fit$coefficients[2]*as.numeric(swiss_cases_su2020$date))), rev(exp(fit$coefficients[1]+ fit$coefficients[2]*as.numeric(swiss_cases_su2020$date))*3)),
              col=alpha(max(col[12]),0.3), border = NA)
      if(k== 4){
        plot(NA, type = "n", xlim =c(0,1), ylim = c(0,1),xaxt="n", yaxt="n", xlab = NA, ylab = "", frame = FALSE)
        if(I==1){
          legend(-0.1,1, legend=c("Reported cases","Interval of prediction",as.expression(bquote("Stochastic interval for " ~ italic("R"["e"]))),  paste("''",Re_all)),
                 inset=.02,fill=c(col[1], col[12],"transparent", cols[1:length(Re_all)]), cex=1.25,bty = "n", border = F)
          
        }
      }
    }
  }
  
  dev.off()
}
#####

# Visualizing growth rates (combining different dispersion and import values)
#####

for (i in 1:2) {
  ABC <- letters[seq(imports_d)]
  pp <-list()
  if(i==1){
    growth_r_all_summary <- growth_imports_infect_summary
  }
  if(i==2){
    growth_r_all_summary <- growth_imports_summary
  }
  
  for(I in 1:(length(imports_d[,-1]))){
    import_num <- sum(imports_d[,1+I])
    growth_r_all_summary1 <- growth_r_all_summary[growth_r_all_summary$imports_num %in% c(0, import_num),]
    
    pp[[I]] <- ggplot(growth_r_all_summary1, aes( x=Re,y=median,group=dispersion_parameter, color = dispersion_parameter)) +
      geom_point(aes(color=dispersion_parameter, shape=imports),position=position_dodge(width=0.5), fill = "transparent")+
      geom_errorbar(aes(ymin = ll_IQR, ymax =ul_IQR, color = dispersion_parameter),position=position_dodge(width=0.5), width=0.4) + 
      xlab(bquote("Effective reproduction number" ~ italic("R"["e"])))+
      ylab(bquote("Growth rate" ~ italic("r")))+
      theme_classic()+
      theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.box.background = element_rect(fill = "transparent"),# get rid of legend panel bg
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            axis.text.x = element_text(size = 12),#angle = -20),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            text = element_text(size =rel(3.5)),
            legend.text= element_text(size = 12),
            plot.title = element_text( size = 16, face = "bold", hjust = 0),
            plot.tag = element_text( size = 16, face = "bold", hjust = 0),
            plot.subtitle = element_text( size = 12, face = "bold", hjust = 0),
            legend.position = "none")+
      geom_hline(yintercept=r_all$rate_weigth - qnorm(0.975)*r_all$error_weigth, linetype="dashed", color = col[12])+
      geom_hline(yintercept=r_all$rate_weigth + qnorm(0.975)*r_all$error_weigth, linetype="dashed", color = col[12])+
      geom_hline(yintercept=0,  color =  "black")+
      scale_y_continuous(limits = c(-0.15, 0.06))+
      scale_color_manual(values =cols[1:length(dispersion_parameters)])+
      labs(subtitle = bquote(italic("I") == .(import_num)))
  }
  
  legend_plot <- ggplot(growth_r_all_summary1, aes( x=Re,y=median,group=dispersion_parameter, color = dispersion_parameter)) +
    geom_point(aes(color=dispersion_parameter, shape=imports),position=position_dodge(width=0.5), fill = "transparent")+
    scale_color_manual(values =cols[1:length(dispersion_parameters)])+
    theme(panel.background = element_rect(colour = "transparent", fill = "transparent"), # bg of the panel
          plot.background = element_rect(colour = "transparent", fill = "transparent"), # bg of the plot
          legend.background = element_rect(colour = NA, fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(colour = NA, fill = "transparent"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          text = element_text(size =rel(3.5)),
          legend.text= element_text(size = 12))+
    guides(color=guide_legend(title=bquote("Dispersion parameter" ~ italic("k"))))+
    scale_shape_discrete(name = bquote("Imports" ~ italic("I")))+
    theme_classic()
  
  g_legend <- function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]] 
    legend$grobs[[1]]$grobs[[1]] <-  editGrob(legend$grobs[[1]]$grobs[[1]], gp=gpar(fill ="transparent",col="transparent"))
    legend$grobs[[2]]$grobs[[1]] <-  editGrob(legend$grobs[[2]]$grobs[[1]], gp=gpar(fill ="transparent",col="transparent"))
    legend
  } 
  legend_plot <- g_legend(legend_plot)
  ppp <-pp
  
  for(I in 1:(length(imports_d[,-1]))){
    import_num <- sum(imports_d[,1+I])
    ppp[[I]] <- ppp[[I]]+ ylab("")+
      xlab("")+ labs(tag = paste(ABC[I], ")"))+
      scale_y_continuous(limits = c(-0.2, 0.06))
  }
  
  ppp[[round(length(imports_d[,-1])/2)]] <-grobTree(grid.arrange(ppp[[round(length(imports_d[,-1])/2)]]),textGrob(bquote("Growth rate" ~ italic("r")), x = 0.1, y = 0.5, rot=90))
  ppp[[length(imports_d[,-1])]] <- grobTree(grid.arrange(ppp[[length(imports_d[,-1])]]), textGrob(bquote("Effective reproduction number" ~ italic("R"["e"])), x=0.6, y = 0.05))
  ppp[[length(imports_d[,-c(1,2)])]] <- grobTree(grid.arrange(ppp[[length(imports_d[,-c(1,2)])]]), textGrob(bquote("Effective reproduction number" ~ italic("R"["e"])), x=0.6, y = 0.05))
  ppp[[length(imports_d)]] <- pp[[length(imports_d)]] <- grid.arrange(legend_plot)
  
  lay <- rbind(c(1,2,3),c(4,5,NA),c(6,7,NA))
  plot_large<- grid.arrange(ppp[[1]],ppp[[2]],ppp[[length(imports_d)]],ppp[[3]],ppp[[4]],ppp[[5]],ppp[[6]],layout_matrix = lay)
  lay <- rbind(c(1,2))
  plot_small<- grid.arrange(pp[[1]],pp[[length(imports_d)]],layout_matrix = lay)
  
  if(i==1){
    ggsave(plot_large, filename = paste0("growth_r_imports_infect_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 10, width = 12,  bg = "transparent")
    ggsave(plot_small, filename = paste0("growth_r_imports_infect_reported_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 4, width = 8,  bg = "transparent")
  }
  if(i==2){
    ggsave(plot_large, filename = paste0("growth_r_imports_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 10, width = 12,  bg = "transparent")
  }
}
#####

# Visualizing cumulative cases of infection per scenario
#####
cols <- rep(wes_palette("GrandBudapest2", length(dispersion_parameters), type = c( "continuous")),length(imports_d)*length(Re_all))

for (i in 1:2) {
  if(i==1){
    cum_cases <- cum_cases_imports_infect
    final_size <- final_cases_imports_infect
  }
  if(i==2){
    cum_cases <- cum_cases_imports
    final_size <- final_cases_imports
  }
  
  yscaling <- function(l) {
    l <- format(l, scientific = TRUE)
    l <- gsub("^(.*)e", "'\\1'e", l)
    l <- gsub("\\+", "", l)
    l <- gsub("e", "%*%10^", l)
    parse(text=l)
  }
  cum_cases$imports_dispersion <- as.character(paste0("I=",cum_cases$imports_num,"; k=", cum_cases$dispersion_parameter))
  cum_cases$imports_dispersion = factor(cum_cases$imports_dispersion, levels=unique(cum_cases$imports_dispersion[order(cum_cases$imports_num, cum_cases$dispersion_parameter, decreasing = FALSE)]), ordered=TRUE)
  
  cases_melt=melt(cum_cases,id.vars=c("imports_num", "dispersion_parameter","Re","imports_dispersion"))
  cases_melt$value <- as.numeric(cases_melt$value)
  cases_melt$imports_dispersion = factor(cases_melt$imports_dispersion, levels=unique(cases_melt$imports_dispersion[order(cases_melt$imports_num, cases_melt$dispersion_parameter, decreasing = FALSE)]), ordered=TRUE)
  
  cum_cases_plot <- ggplot(cases_melt)+ 
    geom_boxplot( aes(x=Re, y=value, color=imports_dispersion), position = position_dodge(width = 1),width = 0.9)+
    scale_y_continuous(labels=yscaling, trans = 'log10')+# scale_y_log10() +
    theme_classic()+ theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                           legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                           legend.box.background = element_rect(fill = "transparent"),# get rid of legend panel bg
                           panel.grid.major = element_blank(), # get rid of major grid
                           panel.grid.minor = element_blank(), # get rid of minor grid
                           axis.text.x = element_text(size = 12),#angle = -20),
                           axis.text.y = element_text(size = 12),
                           axis.title.y = element_text(size = 12),
                           text = element_text(size =rel(3.5)),
                           legend.text= element_text(size = 12),
                           plot.title = element_text( size = 16, face = "bold", hjust = 0),
                           plot.tag = element_text( size = 16, face = "bold", hjust = 0),
                           plot.subtitle = element_text( size = 12, face = "bold", hjust = 0),
                           legend.position = "none")+
    scale_color_manual(values = cols)+
    geom_hline(yintercept=sum(swiss_cases_su2020$cases_reported), color = col[1])+
    geom_hline(yintercept=sum(swiss_cases_su2020$predicted_cases), color = col[12])+
    geom_hline(yintercept=sum(swiss_cases_su2020$predicted_cases3), color = col[12])+
    labs(tag = paste(ABC[1], ")"))+ #labs(color= bquote("# imports" ~italic("I")~ "and dispersion parameter" ~ italic("k") ))+
    ylab("Cumulative cases (Jun to Sep 2020)") # + xlab(bquote("Effective reproduction number" ~ italic("R"["e"])))
  
  # Visualizing final size of infections per scenario
  
  final_size$imports_dispersion <- as.character(paste0("I=",final_size$imports_num,"; k=", final_size$dispersion_parameter))
  final_size$imports_dispersion = factor(final_size$imports_dispersion, levels=unique(final_size$imports_dispersion[order(final_size$imports_num, final_size$dispersion_parameter, decreasing = FALSE)]), ordered=TRUE)
  
  cases_melt=melt(final_size,id.vars=c("imports_num", "dispersion_parameter","Re","imports_dispersion"))
  cases_melt$value <- as.numeric(cases_melt$value)
  cases_melt$imports_dispersion = factor(cases_melt$imports_dispersion, levels=unique(cases_melt$imports_dispersion[order(cases_melt$imports_num, cases_melt$dispersion_parameter, decreasing = FALSE)]), ordered=TRUE)
  
  
  final_size_plot <- ggplot(cases_melt)+ 
    geom_boxplot(aes(x=Re, y=value, color=imports_dispersion), position = position_dodge(width = 1),width = 0.9)+
    theme_classic()+ theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                           legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                           legend.box.background = element_rect(fill = "transparent"),# get rid of legend panel bg
                           panel.grid.major = element_blank(), # get rid of major grid
                           panel.grid.minor = element_blank(), # get rid of minor grid
                           axis.text.x = element_text(size = 12),#angle = -20),
                           axis.text.y = element_text(size = 12),
                           axis.title.y = element_text(size = 12),
                           text = element_text(size =rel(3.5)),
                           legend.text= element_text(size = 12),
                           plot.title = element_text( size = 16, face = "bold", hjust = 0),
                           plot.tag = element_text( size = 16, face = "bold", hjust = 0),
                           plot.subtitle = element_text( size = 12, face = "bold", hjust = 0),
                           legend.position = "none")+
    scale_color_manual(values = cols)+
    geom_hline(yintercept=swiss_cases_su2020$predicted_cases[122], color = col[12])+
    geom_hline(yintercept=swiss_cases_su2020$predicted_cases3[122], color = col[12])+
    ylab(bquote("Cases on 30"^"th"~" Sep 2020")) + xlab(bquote("Effective reproduction number" ~ italic("R"["e"])))+ 
    labs(tag = paste(ABC[2], ")"), color= bquote("# imports" ~italic("I") ~ "and dispersion paramter" ~ italic("k") ))
  
  legend_scenarios_plot <- ggplot(cases_melt)+ 
    geom_boxplot( aes(x=Re, y=value, color=imports_dispersion))+
    theme_classic()+
    theme(panel.background = element_rect(colour = "transparent", fill = "transparent"), # bg of the panel
          plot.background = element_rect(colour = "transparent", fill = "transparent"), # bg of the plot
          legend.background = element_rect(colour = NA, fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(colour = NA, fill = "transparent"),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          text = element_text(size =rel(3.5)),
          legend.text= element_text(size = 12))+
    labs(color= bquote("# imports" ~italic("I")~ "and dispersion parameter" ~ italic("k") ))+
    scale_color_manual(values = cols)
  
  g_legend <- function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]] 
    legend$grobs[[1]]$grobs[[1]] <-  editGrob(legend$grobs[[1]]$grobs[[1]], gp=gpar(fill ="transparent",col="transparent"))
    legend
  } 
  legend_scenarios_plot <- g_legend(legend_scenarios_plot)
  
  lay <- rbind(c(1,1,1,1,2),c(3,3,3,3,NA))
  plot<- grid.arrange(cum_cases_plot,legend_scenarios_plot,final_size_plot,layout_matrix = lay)
  
  if(i==1){
    ggsave(plot, filename = paste0("size_scenarios_imports_infect_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 10, width = 15,  bg = "transparent")
  }
  if(i==2){
    ggsave(plot, filename = paste0("size_scenarios_imports_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 10, width = 15,  bg = "transparent")
  }
}

#####


#save.image("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/StochasticModel_SarsCoV2/su2020_cov2/su2020_cov2.RData")




