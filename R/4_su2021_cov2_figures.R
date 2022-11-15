# Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021

#library
library(lubridate)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(reshape2)
library(scales)
library(plyr)

# Set seed
set.seed(60321)

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/Figures")
Re_all <- read.csv("../data/Re_all_2022-03-11.csv")
Re_all <- Re_all[,2]
cases_summer <- read.csv("../data/cases_su.csv", row.names = 1, header=T, sep=",") # add file to Ubelix
cases_summer$year <- year(cases_summer$date)

#colors for figures
col_9 <- (brewer.pal(9,"Set1"))

# basic plot for all figures
basic_figplot <- ggplot(data=as.data.frame(NA))+
  theme(plot.margin = margin(8, 10, 2, 10, "mm"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", color = NA),# get rid of legend panel bg
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        axis.text.x = element_text(size = 10),#angle = -20),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        title = element_text(size = 10),
        plot.tag = element_text( size = 16,  hjust = 0),
        legend.text= element_text(size = 10),
        legend.title= element_text(size = 10))+
  theme_minimal()+
  theme(title = element_text(size = 12))
        

g_legend <- function(a.gplot,num){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]] 
  if(num==1){
    legend$grobs[[1]]$grobs[[1]] <-  editGrob(legend$grobs[[1]]$grobs[[1]], gp=gpar(fill ="transparent",col="transparent"))
  }
  if(num==2){
    legend$grobs[[1]]$grobs[[1]] <-  editGrob(legend$grobs[[1]]$grobs[[1]], gp=gpar(fill ="transparent",shape="transparent"))
    legend$grobs[[2]]$grobs[[1]] <-  editGrob(legend$grobs[[2]]$grobs[[1]], gp=gpar(fill ="transparent",shape="transparent"))
  }
  legend
} 

yscaling_log <- function(l) {
  l <- format(l, scientific = TRUE)
  l <- gsub("^(.*)e", "'\\1'e", l)
  l <- gsub("\\+", "", l)
  l <- gsub("e", "%*%10^", l)
  parse(text=l)
}

cases_summer <- read.csv("../data/cases_su.csv", row.names = 1, header=T, sep=",")
cases_summer$date <-as_date(cases_summer$date)
cases_summer$year <- year(cases_summer$date)

imports <- c()
import <- c()
for(i in c(2020,2021)){
  imports_d <-c()
  cases_su <- cases_summer[cases_summer$year==i,]
  weighting <- sum(cases_su$cases_date)/(sum(cases_su$cases_swiss)+sum(cases_su$cases_abroad))
  imports_d$cases_abroad <- cases_su$cases_abroad
  imports_d$abroad_a <- round(cases_su$cases_abroad*weighting)
  imports_d$abroad_b <- round(cases_su$cases_abroad*0.5*weighting)
  imports_d$abroad_c <- round(cases_su$cases_abroad*1.5*weighting)
  imports_d <- as.data.frame(imports_d)
  import_num <- c(0,colSums(imports_d))
  import_num <- as.data.frame(import_num)
  imports_d$year <- i
  imports_d$date <- cases_su$date
  import_num$year <- i
  imports <- rbind(imports,imports_d)
  import <- rbind(import,import_num)
}
import_num <- import
imports_d <- imports

## Figure 1: visualize reported imports
#####
p_cases <- basic_figplot+
  geom_bar(data= cases_summer, aes(x=date,y=cases_date, fill=col_9[2]),  width=1, position="identity", stat = "identity", alpha = 0.8)+
  geom_bar(data= cases_summer,aes(x=date,y=cases_abroad, fill=col_9[1]), width=1, stat = "identity", position = "identity", alpha = 0.8)+
  #theme(plot.margin = margin(8, 0, 2, 40, "mm"))+
  scale_fill_manual(name="",
    values = c(col_9[2],col_9[1]),
    labels = c("Reported cases", "Reported imports"))+
  facet_wrap( ~ year,  scales = "free")+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")+
  theme(legend.position="none",plot.subtitle=element_text(size=10),
        strip.text.x = element_text(hjust = -0.01))+
  labs(tag="A",x = "", y =bquote("Confirmed cases"))
#p_cases_imports_legend <- grid.arrange(grobs = list(basic_figplot,p_cases_imports_legend),layout_matrix =  rbind(1,2))

#Baseline assuming no imports
#Scenario a) assuming reported imports were representative
#Scenario b) 'lower limit' that assumed \nfewer imports among cases with missing information
#Scenario c) 'upper limit' that assumed \nmore imports among cases with missing information
# b) ('lower limit' that assumed fewer imports among cases with missing information) 
#c) ('upper limit' that assumed more imports among cases with missing information

labels_prop = factor(c("Scenario a) assuming reported imports were representative",
                       "Scenario b) 'lower limit' that assumed \nfewer imports among cases with missing information",
                       "Scenario c) 'upper limit' that assumed \nmore imports among cases with missing information",
                       "Reported imports"), levels=c("Scenario a) assuming reported imports were representative",
                                                     "Scenario b) 'lower limit' that assumed \nfewer imports among cases with missing information",
                                                     "Scenario c) 'upper limit' that assumed \nmore imports among cases with missing information",
                                                     "Reported imports"))
cols <- col_9[c(8,5,4,1)]

imports_plot <- melt(imports_d, id.vars=c("date","year"))
imports_plot$date <- as_date(imports_plot$date)
imports_plot$year <- year(imports_plot$date)
imports_plot$variable <- factor(imports_plot$variable, levels = c("abroad_a","abroad_b","abroad_c","cases_abroad"))

p_cases_imports <- basic_figplot+
  geom_line(data= imports_plot, aes(x=date, y=value,color=variable),alpha = 0.8)+
  scale_color_manual(values=cols,label= labels_prop, name="") +
  #guides(color = guide_legend(override.aes = list(size=10))) +
  theme(legend.position="none",plot.subtitle=element_text(size=10),
        strip.text.x = element_text(hjust = -0.01))+
  facet_wrap( ~ year, scales = "free")+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")+
  labs(tag="B", x = "", y =bquote("Number of imports"))

cols <- col_9[c(2,1,8,5,4)]
legend_d<- data.frame(x=1:25, y=1:25)
legend_d$g <- as.character(rep(1:5,5))
legend_d<- basic_figplot +
  geom_bar(data= legend_d, aes(x=x, group=g, fill=g),size=2)+
  guides(fill=guide_legend(nrow=3,byrow=TRUE))+
  theme(legend.text=element_text(size=10),legend.spacing.y = unit(1, 'cm'),legend.position = 'top')+
  scale_fill_manual(values=cols, name="", labels=c("Reported cases\n","Reported imports\n",as.character(labels_prop[-4])))
legend_d <- g_legend(legend_d,1)



plot_fig1 <- grid.arrange(rbind(ggplotGrob(p_cases),
                          ggplotGrob(p_cases_imports),size = "last"))

plot_fig1 <- grid.arrange(grobs = list(plot_fig1,legend_d),layout_matrix =  rbind(cbind(1,1),cbind(1,1),cbind(2,2)))

ggsave(plot_fig1, filename = paste0("Figure1_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 8, width = 10,  bg = "transparent")
ggsave(plot_fig1, filename = paste0("Figure1_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 8, width = 10,  bg = "transparent")

grid.newpage()

plot_fig1 <- rm
p_cases <- rm
p_cases_imports <- rm

# Figure 3

#c) prior posterior distribution:
cols <- c(col_9[c(9,7)],col_9[c(8,5,4,1)])

for (i in c(2020,2021)) {
  if(i==2020){
    models_output <- read.csv("../data/2020/models_output2020.csv", row.names = 1)
    
  }
  else if(i==2021){
    models_output <- read.csv("../data/2021/models_output2021.csv", row.names = 1)
    
  }
  prior <-as.data.frame(matrix(ncol = 1,nrow=length(Re_all)))
  prior$Re <- as.numeric(Re_all)
  prior$class <- "prior"
 prior$imports <- as.character("Prior distribution")
  prior <- prior[,-c(1)]
  
  simulation_accept <- models_output[models_output$simulation_accepted==1,]
  simulation_accept$class <- "posterior"
  posterior<- simulation_accept[,c("Re","class","imports")]
  posterior$imports <- factor(posterior$imports, levels=unique(posterior$imports)[order(unique(posterior$imports))])
  
  #Baseline assuming no imports
  #Scenario a) assuming reported imports were representative
  #Scenario b) 'lower limit' that assumed \nfewer imports among cases with missing information
  #Scenario c) 'upper limit' that assumed \nmore imports among cases with missing information
  
  posterior$imports_name[posterior$imports==levels(posterior$imports)[1]] <- "Baseline assuming no imports"
  posterior$imports_name[posterior$imports==levels(posterior$imports)[2]] <- "Reported imports"
  posterior$imports_name[posterior$imports==levels(posterior$imports)[3+1]] <- "Scenario a) assuming reported imports were representative"
  posterior$imports_name[posterior$imports==levels(posterior$imports)[2+1]] <- "Scenario b) 'lower limit' that assumed \nfewer imports among cases with missing information"
  posterior$imports_name[posterior$imports==levels(posterior$imports)[4+1]] <- "Scenario c) 'upper limit' that assumed \nmore imports among cases with missing information"
  posterior <- posterior[!grepl("Reported imports",posterior$imports_name),]
  posterior$imports <- posterior$imports_name
  posterior$imports_name <- NULL
  prior_posterior <- rbind(prior,posterior)
  prior_posterior$imports <- factor(prior_posterior$imports, levels=c("Prior distribution","Baseline assuming no imports","Scenario a) assuming reported imports were representative","Scenario b) 'lower limit' that assumed \nfewer imports among cases with missing information","Scenario c) 'upper limit' that assumed \nmore imports among cases with missing information"))
  prior_posterior <- as.data.frame(prior_posterior)
  
  
  prior_posterior_plot <- basic_figplot+
    geom_density(data=prior_posterior, aes(x=as.numeric(Re),fill=imports),color="transparent",alpha=0.5)+
    scale_fill_manual(values=cols,name="")+
    theme_minimal()+
    scale_x_continuous(breaks = seq(from = 0.5, to = 1.5, by = 0.1))+
    coord_cartesian(xlim = c(0.5,1.5)) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))+
    theme(legend.text=element_text(size=10),legend.spacing.y = unit(1, 'cm'),legend.position = 'top')+
    labs(tag=bquote(.("")),subtitle ="", x = bquote(italic("R"["e"])), y =bquote("Density"))# bquote(italic("Imports") == .("5050"))
  prior_posterior_legend <- g_legend(prior_posterior_plot,1)
  
  if(i==2020){
    prior_posterior_2020_plot <- prior_posterior_plot+theme(legend.position="none")+labs(subtitle ="2020", x = "", y =bquote("Density"))

  }
  if(i==2021){
    prior_posterior_2021_plot <- prior_posterior_plot+theme(legend.position="none")+labs(subtitle ="2021")
  }
}
plot_fig2 <- grid.arrange(rbind(ggplotGrob(prior_posterior_2020_plot), ggplotGrob(prior_posterior_2021_plot),size = "last"))
plot_fig2 <- grid.arrange(grobs = list(plot_fig2,prior_posterior_legend),layout_matrix =  rbind(cbind(1,1),cbind(1,1),cbind(2,2)))

ggsave(plot_fig2, filename = paste0("Figure3_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 8, width = 10,  bg = "transparent")
#ggsave(plot_fig2, filename = paste0("Figure3_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 8, width = 10,  bg = "transparent")
grid.newpage()


plot_fig2 <- rm 
fig2_legend <- rm
fig22 <- rm
fig2_legend <- rm 
f2_sim_cases <- rm
legend_sim_cases <- rm
legend_final_cum <- rm


#####

#Figure 3)
plots_simulated <- list()
#labels_prop = factor(c("Baseline scenario","Plausible scenario a)","Lower limit scenario b)","Upper limit scenario c)"), levels = c("Baseline scenario","Plausible scenario a)","Lower limit scenario b)","Upper limit scenario c)"))
for(i in c(2020,2021)){
  if(i==2020){
    models_output <- read.csv("../data/2020/models_output2020.csv", row.names = 1)
    cases_su <- read.csv("../data/2020/cases_su2020.csv", row.names = 1)
    x_name <- "Daily incidence"
     }
  if(i==2021){
    models_output <- read.csv("../data/2021/models_output2021.csv", row.names = 1)
    cases_su <- read.csv("../data/2021/cases_su2021.csv", row.names = 1)
    x_name <- ""
  }
  imports_d <- cases_su[, c("date","cases_abroad")]
  weighting <- sum(cases_su$cases_date)/(sum(cases_su$cases_swiss)+sum(cases_su$cases_abroad))
  imports_d$abroad_a <- round(cases_su$cases_abroad*weighting)
  imports_d$abroad_b <- round(cases_su$cases_abroad*0.5*weighting)
  imports_d$abroad_c <- round(cases_su$cases_abroad*1.5*weighting)
  import_num <- c(0,colSums(imports_d[,-1]))
  import_num <- import_num[c(1,3,4,5,2)]
for (I in c(1,2)) {
  #Baseline assuming no imports
  #Scenario a) assuming reported imports were representative
  if(I %in% c(1)){
    tags <- paste0("Baseline assuming no imports for ",i)
  }
  else{
    tags <- paste0("Scenario a) assuming reported imports were representative for ",i)
  }
  models_output1 <- models_output[models_output$imports ==import_num[I],]
  max_x <- round(max(models_output1$value))
  models_output1$Re <- as.character(models_output1$Re)
  models_output1_accept <- c()
  if(sum(models_output1$simulation_accepted ==1)!=0){#simulation_accepted_final
    models_output1_accept <- models_output1[models_output1$simulation_accepted ==1,]#simulation_accepted_final
    models_output1_accept <- as.data.frame(t(models_output1_accept[,c(8:(length(models_output)-2))]))#8
    models_output1_accept$date <- cases_su$date
    models_output1_accept<- melt(models_output1_accept, id.vars="date")
  }
  cols <- col_9[c(7,8,4,5,1)]
  cols <- cols[c(I)]
  

  plots <- basic_figplot +
    geom_line(data=models_output1_accept, aes( x=as_date(date), y=value ,group= variable,color="Accepted trajectories"), alpha=0.8)+
    stat_summary(data=models_output1_accept,aes(x=as_date(date), y=value), fun=median, geom="line", color=col_9[6],size=2,alpha=0.8, linetype="dotted")+
    #stat_summary(data=models_output1_accept,aes(x=as_date(date), y=value), fun=mean, geom="line", color=col_9[9],size=2,alpha=0.8)+
    geom_line(data=cases_su, aes(x=as_date(date), y = incidence_weigthed* 8544527/1e5),color=col_9[2],size=2,alpha=0.8)+
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b")+
    coord_cartesian(xlim = c(0,max_x)) +
    scale_shape_discrete(name="", label="Reported cases")+
    scale_color_manual(values=cols[1], name="", labels=c("Accepted trajectories"))+
    guides(shape = guide_legend(override.aes = list(size=3)),color = guide_legend(override.aes = list(size=3))) +
    theme(legend.position="none",plot.subtitle=element_text(size=10))+
    labs(subtitle = tags, x = "", y =bquote(.(x_name)))
  if(i==2020){
    n <-I
  }
  if(i==2021){
    n <-I+2
  }
  plots_simulated[[n]]<- plots
}
}

# legend:
legend_d<- data.frame(x=1:20, y=1:20)
legend_d$g <- as.character(rep(1:2,5))
legend_d<- basic_figplot +
  geom_bar(data= legend_d, aes(x=x, group=g, fill=g),size=2)+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  theme(legend.text=element_text(size=10),legend.spacing.y = unit(1, 'cm'),legend.position = 'top')+
  scale_fill_manual(values=col_9[c(2,6)], name="", labels=c("Reported incidence of confirmed cases (7-day moving average)","Median of simulated trajectories"))
legend_d <- g_legend(legend_d,1)
f3_sim_cases <- grid.arrange(grobs = plots_simulated,layout_matrix =  matrix(1:4,2,2))

plot_fig_2 <- grid.arrange(grobs = list(f3_sim_cases,legend_d),layout_matrix =  rbind(cbind(1,1),cbind(1,1),cbind(2,2)))

ggsave(plot_fig_2, filename = paste0("Figure2_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 8, width = 10,  bg = "transparent")
ggsave(plot_fig_2, filename = paste0("Figure2_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 8, width = 10,  bg = "transparent")
grid.newpage()





#####
#Supplement
BAG_data_su <- read.csv("../data/BAG_data.csv", row.names = 1, header=T, sep=",")
BAG_data_su$year <- year(BAG_data_su$date)

#exposed countries
BAG_data_su$country <- factor(BAG_data_su$country, levels=unique(names(table(BAG_data_su$country))[order(table(BAG_data_su$country), decreasing = TRUE)]), ordered=TRUE)
BAG_data_su$country <- factor(BAG_data_su$country, levels=c(levels(BAG_data_su$country)[!levels(BAG_data_su$country) %in% c("Others","Abroad but unknown","Unknown")],c("Others","Abroad but unknown","Unknown")), ordered=TRUE)

#####
restrictions2021<- read.csv("../data/2021/restrictions2021_summary.csv")
restrictions2020<- read.csv("../data/2020/restrictions2020_summary.csv")
restrictions <- rbind(restrictions2020,restrictions2021)

restrictions$start_date <- as_date(restrictions$start_date)
restrictions$end_date <- as_date(restrictions$end_date)
#restrictions$end_date_dot <- as_date(restrictions$end_date_dot)
restrictions$year <- year(restrictions$start_date)
p_regulation <- basic_figplot+
  geom_segment(data=restrictions, aes(x=start_date, xend=end_date, y=label, yend=label),col= col_9[2], linetype=1, size=1) +
  geom_point(data=restrictions,aes(x=start_date,y=label),col= col_9[2], size=2)+
  geom_point(data=restrictions,aes(x=end_date, y=label),col= col_9[2], size=2)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")+
  facet_grid( ~ year, scales = "free")+
  theme(plot.margin = margin(8, 0, 2, 40, "mm"),
        axis.ticks.y = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.text.x = element_text(color="transparent"),
        axis.text.y = element_text(size = 8))+
  labs(tag="A",x = "", y =bquote(""))
KOF <- read.csv(paste0("https://datenservice.kof.ethz.ch/api/v1/public/sets/stringency_plus_web?mime=csv&df=Y-m-d.csv"))
KOF[,"date"] <- seq(as_date("2020-01-01"),(as_date("2020-01-01")+length(KOF[,"date"])-1),1)
KOF$date <- as_date(KOF)
KOF_su <- subset(KOF, date %in%  as_date(cases_summer$date))
KOF_su$year <- as_date(KOF_su$date)
KOF_su$year <- year(KOF_su$date)
p_KOF_ch <- basic_figplot+
  geom_line(data= KOF_su,aes(x= date, y=ch.kof.stringency.ch.stringency_plus), col= col_9[2], size = 2) +
  ylim(0, 100)+
  theme(plot.margin = margin(8, 0, 2, 40, "mm"))+
  facet_grid( ~ year, scales = "free")+
  labs(tag="B",x = "", y =bquote("KOF Stringency-Plus Index"))
supplot_fig1 <- grid.arrange(rbind(ggplotGrob(p_regulation),
                                ggplotGrob(p_KOF_ch),size = "last"))
ggsave(supplot_fig1, filename = paste0("SF1_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 8, width = 10,  bg = "transparent")


# Supplementary Figure 1: incidence per country
country_level <- table(BAG_data_su$country)[order(table(BAG_data_su$country),decreasing=TRUE)]
BAG_data_su$country <-  factor(BAG_data_su$country, levels = c("Switzerland", names(country_level)[!names(country_level) %in% c("Switzerland","Others","Abroad but unknown","Unknown")],"Others","Abroad but unknown","Unknown"))
BAG_data_su$age_cat<- factor(BAG_data_su$age_cat, levels=c("<21","21-64",">64","Unknown"))

# global SARS-CoV-2 incidence data
incidence_data <- read.csv("../data/owid-covid-data.csv", header=T, sep=",") # accessed 15/03/2022
incidence_data$location[incidence_data$location=="United Kingdom"] <- "UK"
incidence_data$location[incidence_data$location=="Czechia"] <- "Czech Republic"

for (i in 1:2) {
  if(i==1){
    BAG_data <- BAG_data_su[BAG_data_su$year==2020,]
    incidence <- incidence_data[incidence_data$date %in% BAG_data$date,]
    cases_su <- read.csv("../data/2020/cases_su2020.csv")
    su_quarantine <- read.csv("../data/2020/su2020_quarantine.csv")
    tags <- "For 2020"
    max_num <- 300
  }
  else if(i==2){
    BAG_data <- BAG_data_su[BAG_data_su$year==2021,]
    incidence <- incidence_data[incidence_data$date %in% BAG_data$date,]
    cases_su <- read.csv("../data/2021/cases_su2021.csv")
    su_quarantine <- read.csv("../data/2021/su2021_quarantine.csv")
    tags <- "For 2021"
    max_num <- 1200
  }
  country_level <- names(table(BAG_data$country)[table(BAG_data$country)>=10])
  #country_level <- table(BAG_data$country)[order(table(BAG_data$country),decreasing=TRUE)]
  time_window <- as_date(c(min(cases_su$date), max(cases_su$date)))
 incidence_noswiss <-  incidence[incidence$location!="Switzerland",]
  incidence_noswiss$country <-  factor(incidence_noswiss$location, levels = c(country_level[!country_level %in% c("Switzerland","Others","Abroad but unknown","Unknown")]))
  su_quarantine$country <- factor(su_quarantine$country, levels = c(country_level[!country_level %in% c("Switzerland","Others","Abroad but unknown","Unknown")]))
  incidence_noswiss<- incidence_noswiss[!is.na(incidence_noswiss$country),] 
  su_quarantine<- su_quarantine[!is.na(su_quarantine$country),] 

incidence_plot <- ggplot()+
  geom_line(data=incidence_noswiss, aes(x=as_date(date), y=new_cases_smoothed_per_million))+
  geom_rect(data= su_quarantine, aes(xmin = as_date(start_date), ymin = 0, xmax = as_date(end_date), ymax =max_num), fill= col_9[6], colour= "transparent", alpha=0.4)+
  facet_wrap(.~country,ncol=3)+
  geom_line(data=cases_su,aes(x=as_date(date), y=incidence_weigthed*10),color= col_9[2])+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b", limits = c(time_window[1], time_window[2]))+
  theme_minimal()+  
  coord_cartesian(ylim = c(0, max_num-10))+
  labs(tag=tags,x = "", y =bquote("Daily incidence per million"))

if(i==1){
  incidence2020_plot <- incidence_plot
}
else if(i==2){
  incidence2021_plot <- incidence_plot
}
}
incidence_plot1 <- grid.arrange(grobs = list(incidence2020_plot,incidence2021_plot),layout_matrix =  rbind(1,1,1,2,2))
ggsave(incidence_plot1, filename = paste0("SF2_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 20, width = 7,  bg = "transparent")



  
# Supplementary Figure 2:
#plot age difference by most likely place of infection
give.n <- function(x){
  return(c(y = -5, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}
for (i in 1:2) {
  if(i==1){
    BAG_data <- BAG_data_su[BAG_data_su$year==2020,]
   tags <- "For 2020"
   table_su_age1<- table_su_age[table_su_age$Year=="2020",]
   table_su_age1$country   <- table_su_age1$Country
  }
  else if(i==2){
    BAG_data <- BAG_data_su[BAG_data_su$year==2021,]
    tags <- "For 2021"
    table_su_age1<- table_su_age[table_su_age$Year=="2021",]
  }
  BAG_data$country <- factor(BAG_data$country, levels=unique(names(table(BAG_data$country))[order(table(BAG_data$country), decreasing = TRUE)]), ordered=TRUE)
  BAG_data$country <- factor(BAG_data$country, levels=c(levels(BAG_data$country)[!levels(BAG_data$country) %in% c("Others","Abroad but unknown","Unknown")],c("Others","Abroad but unknown","Unknown")))
  table_su_age1$country <-  table_su_age1$Country
  table_su_age1 <- table_su_age1[table_su_age1$country %in% levels(BAG_data$country),]
age_percountry<- ggplot(BAG_data, aes(x=country, y=age)) +
  geom_boxplot(aes(y=age),fill="transparent",color= col_9[9],size=0.5)+
  geom_hline(yintercept=mean(BAG_data$age[BAG_data$country=="Switzerland"]),color=col_9[2])+
  stat_summary(fun=mean, geom="point", shape=3, size=3, color="black") +
  stat_summary(fun.data = give.n, geom = "text", size=2, fun.y = median)+
  geom_text(data= table_su_age1,aes(x=country,y=102, label=pvalue_stars), col="black", size=2)+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust=0.95,vjust = 0.1, size=8, angle=90),#5
        plot.margin = margin(8, 2, 2, 2, "mm"),
        plot.background = element_rect(fill = "transparent", color =  "transparent"), # bg of the plot
        axis.text.y = element_text(size = 8),#5
        axis.title.y = element_text(size = 10),#5
        text = element_text(size =rel(3.5)),
        plot.tag = element_text( size = 16,  hjust = 0),
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.position = "none")+
  labs( x = "", y = "Age (in years)",tag = tags)
if(i==1){
  age_percountry2020_plot <- age_percountry
}
else if(i==2){
  age_percountry2021_plot <- age_percountry
}
}
plot_countries_age <- grid.arrange(grobs = list(age_percountry2020_plot,age_percountry2021_plot),layout_matrix =  rbind(1,2))
ggsave(plot_countries_age, filename = paste0("SF3_",i,format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 8, width = 7,  bg = "transparent")
grid.newpage()

#Suplement Figure 4

#Figure 3)
plots_simulated <- list()
labels_sf =factor(c("Baseline scenario",levels(labels_prop)),levels = c("Baseline scenario",levels(labels_prop)))
for(i in c(2020,2021)){
  if(i==2020){
    models_output <- read.csv("../data/2020/models_output2020.csv", row.names = 1)
    cases_su <- read.csv("../data/2020/cases_su2020.csv", row.names = 1)
    x_name <- "Daily incidence"
  }
  if(i==2021){
    models_output <- read.csv("../data/2021/models_output2021.csv", row.names = 1)
    cases_su <- read.csv("../data/2021/cases_su2021.csv", row.names = 1)
    x_name <- ""
  }
  imports_d <- cases_su[, c("date","cases_abroad")]
  weighting <- sum(cases_su$cases_date)/(sum(cases_su$cases_swiss)+sum(cases_su$cases_abroad))
  imports_d$abroad_a <- round(cases_su$cases_abroad*weighting)
  imports_d$abroad_b <- round(cases_su$cases_abroad*0.5*weighting)
  imports_d$abroad_c <- round(cases_su$cases_abroad*1.5*weighting)
  import_num <- c(0,colSums(imports_d[,-1]))
  import_num <- import_num[c(1,3,4,5,2)]
  for (I in 1:length(import_num)) {
 tags <- paste0(labels_sf[I]," ",i)
    models_output1 <- models_output[models_output$imports ==import_num[I],]
    models_output1$Re <- as.character(models_output1$Re)
    models_output1_accept <- c()
    if(sum(models_output1$simulation_accepted ==1)!=0){#simulation_accepted_final
      models_output1_accept <- models_output1[models_output1$simulation_accepted ==1,]#simulation_accepted_final
      models_output1_accept <- as.data.frame(t(models_output1_accept[,c(8:(length(models_output)-2))]))#8
      models_output1_accept$date <- cases_su$date
      models_output1_accept<- melt(models_output1_accept, id.vars="date")
    }
    cols <- col_9[c(7,8,5,4,1)]
    cols <- cols[c(I)]
    
    plots <- basic_figplot +
      geom_line(data=models_output1_accept, aes( x=as_date(date), y=value ,group= variable,color="Accepted trajectories"), alpha=0.8)+
      stat_summary(data=models_output1_accept,aes(x=as_date(date), y=value), fun=median, geom="line", color=col_9[6],size=2,alpha=0.8, linetype="dotted")+
      #stat_summary(data=models_output1_accept,aes(x=as_date(date), y=value), fun=mean, geom="line", color=col_9[9],size=2,alpha=0.8)+
      geom_line(data=cases_su, aes(x=as_date(date), y = incidence_weigthed* 8544527/1e5),color=col_9[2],size=2,alpha=0.8)+
      scale_x_date(date_breaks = "1 month", 
                   date_labels = "%b")+
      scale_shape_discrete(name="", label="Reported cases")+
      scale_color_manual(values=cols[1], name="", labels=c("Accepted trajectories"))+
      guides(shape = guide_legend(override.aes = list(size=3)),color = guide_legend(override.aes = list(size=3))) +
      theme(legend.position="none",)+
      labs(subtitle = tags, x = "", y =bquote(.(x_name)))

    if(i==2020){
      n <-I
    }
    if(i==2021){
      n <-I+length(import_num)
    }
    plots_simulated[[n]]<- plots
  }
}
sf4_sim_cases <- grid.arrange(grobs = plots_simulated,layout_matrix =  t(rbind(1:5,6:10)))
plot_fig_ls <- grid.arrange(grobs = list(sf4_sim_cases,legend_d),layout_matrix =  rbind(cbind(1,1),cbind(1,1),cbind(1,1),cbind(1,1),cbind(1,1),
                                                                                        cbind(1,1),cbind(1,1),cbind(1,1),cbind(1,1),cbind(1,1), cbind(NA,2)))

ggsave(sf4_sim_cases, filename = paste0("SF4_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 24, width = 10,  bg = "transparent")
ggsave(sf4_sim_cases, filename = paste0("SF4_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 24, width = 10,  bg = "transparent")
grid.newpage()


# per day difference of trajectories to reported incidence (blue line):
#  square error
for(i in c(2020, 2021)){
  labels_prop_ls = factor(c("Baseline assuming no imports\n",
                            "Scenario a) assuming reported imports were \nrepresentative",
                            "Scenario b) ‘lower limit’ that assumed \nfewer imports among cases with missing information",
                            "Scenario c) ‘upper limit’ that assumed \nmore imports among cases with missing information",
                            "Reported imports\n"),
                          levels=c(c("Baseline assuming no imports\n",
                                     "Scenario a) assuming reported imports were \nrepresentative",
                                     "Scenario b) ‘lower limit’ that assumed \nfewer imports among cases with missing information",
                                     "Scenario c) ‘upper limit’ that assumed \nmore imports among cases with missing information",
                                     "Reported imports\n")))
  
  
  if(i==2020){
    cases_su <- read.csv("../data/2020/cases_su2020.csv")
    models_output <- read.csv("../data/2020/models_output2020.csv")
    #pop_size <- 8606033 #https://www.pxweb.bfs.admin.ch/pxweb/de/px-x-0102020000_103/px-x-0102020000_103/px-x-0102020000_103.px
    max_y <-1e6
    }
  if(i==2021){
    cases_su <- read.csv("../data/2021/cases_su2021.csv")
    models_output <- read.csv("../data/2021/models_output2021.csv")
    #pop_size <- 8670300 #https://www.pxweb.bfs.admin.ch/pxweb/de/px-x-0102020000_103/px-x-0102020000_103/px-x-0102020000_103.px
    max_y <- 1e8
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
 
  models_output_ls_1e3<-as.data.frame(matrix(ncol=5,nrow=1e3))
  colnames(models_output_ls_1e3) <- labels_prop_ls
  models_output_ls <- models_output_ls[,c(1,3,5,4,2)]
  for(x in 1:5){
    models_output_ls_1e3[,x] <- sample(na.omit(models_output_ls[,x]),1e3)
  }
  
  models_output_ls_1e3<- melt(models_output_ls_1e3)
  cols <- c(col_9[c(7)],col_9[c(8,5,4,1)])
  ls_plot<- basic_figplot+
    geom_density(data=models_output_ls_1e3, aes(x=value, fill=variable),color="transparent",alpha=0.5)+
    scale_fill_manual(values=cols,label= levels(labels_prop_ls), name="") +
    theme_minimal()+
    scale_x_continuous(labels = yscaling_log,limits=c(1,max_y))+
    scale_y_continuous(labels = yscaling_log)+
    guides(fill=guide_legend(nrow=3,byrow=TRUE))+
    theme(legend.text=element_text(size=10),legend.spacing.y = unit(1, 'cm'),legend.position = 'top')+
    labs(tag=bquote(.("")),subtitle =i, x = bquote(italic("Sum of squared residuals (SSR)")), y =bquote("Density"))# bquote(italic("Imports") == .("5050"))
  ls_legend <- g_legend(ls_plot,1)
  if(i==2020){
    ls_plot_2020 <- ls_plot +theme(legend.position="none")
  }
  if(i==2021){
    ls_plot_2021 <- ls_plot+theme(legend.position="none")
  }
  
}


plot_fig_ls <- grid.arrange(rbind(ggplotGrob(ls_plot_2020), ggplotGrob(ls_plot_2021)))
plot_fig_ls <- grid.arrange(grobs = list(plot_fig_ls,ls_legend),layout_matrix =  rbind(cbind(1,1),cbind(1,1),cbind(2,2)))
ggsave(plot_fig_ls, filename = paste0("SF5_LS_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 8, width = 10,  bg = "transparent")
ggsave(plot_fig_ls, filename = paste0("SF5_LS_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 8, width = 10,  bg = "transparent")




#####


