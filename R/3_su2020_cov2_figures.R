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

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/Figures")


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
        axis.text.x = element_text(size = 6),#angle = -20),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        title = element_text(size = 10),
        plot.tag = element_text( size = 16,  hjust = 0),
        legend.text= element_text(size = 10),
        legend.title= element_text(size = 10))+
  theme_minimal()+
  theme(title = element_text(size = 10))
        

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

yscaling <- function(l) {
  l <- format(l, scientific = TRUE)
  l <- gsub("^(.*)e", "'\\1'e", l)
  l <- gsub("\\+", "", l)
  l <- gsub("e", "%*%10^", l)
  parse(text=l)
}


## Figure 1: visualize reported imports
#####
p_regulation <- basic_figplot+
  geom_segment(data=restrictions, aes(x=start_date, xend=end_date, y=label, yend=label),col= col_9[2], linetype=1, size=1) +
  geom_point(data=restrictions,aes(x=start_date,y=label),col= col_9[2], size=2)+
  geom_point(data=restrictions,aes(x=end_date_dot, y=label),col= col_9[2], size=2)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               limits = as_date(c(time_window[1]-1,time_window[2]+1)))+
  theme(plot.margin = margin(8, 0, 2, 40, "mm"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color="transparent"),
        axis.text.y = element_text(size = 8))+
  labs(tag="a)",x = "", y =bquote(""))

p_KOF_ch <- basic_figplot+
  theme(axis.text.x = element_text(color="transparent"))+
  geom_line(data= KOF_su2020,aes(x= date, y=ch.kof.stringency.ch.stringency_plus), col= col_9[2], size = 2) +
  ylim(0, 100)+
  labs(tag="b)",x = "", y =bquote("KOF Stringency-Plus Index"))

p_cases <- basic_figplot+
  geom_bar(data= swiss_cases_su2020, aes(x=date,y=cases_reported), width=1, position="identity", stat = "identity", fill=col_9[2], alpha = 0.6)+
  geom_bar(data= swiss_cases_su2020, aes(x=date,y=cases_abroad), width=1, stat = "identity", position = "identity", fill=col_9[8], alpha = 0.6)+
  ylim(0, 600)+
  theme(plot.margin = margin(8, 0, 2, 40, "mm"))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               limits = as_date(c(time_window[1]-1,time_window[2]+1)))+
  labs(tag="c)",x = "", y =bquote("Confirmed cases"))


labels_prop = factor(c("Reported cross-border-associated cases","Plausible scenario a)","Lower limit scenario b)","Upper limit scenario c)"), levels = c("Reported cross-border-associated cases","Plausible scenario a)","Lower limit scenario b)","Upper limit scenario c)"))
cols <- col_9[c(8,3:5)]

imports_plot <- melt(imports_d, id.vars="date")
p_cases_frac <- basic_figplot+
  geom_line(data= imports_plot,aes(x=date, y=value,color=variable),alpha = 0.5)+
  ylim(0, 250)+
  scale_color_manual(values=cols, label= labels_prop,name="") +
  guides(color = guide_legend(override.aes = list(size=6))) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               limits = as_date(c(time_window[1]-2,time_window[2]+2)))+
  labs(tag="", x = "", y =bquote("Incidence of potential\ncross-border-associated cases"))
ggsave(p_cases_frac, filename = paste0("..presentations/Figures/abroad_frac_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 3, width = 6,  bg = "transparent")

p_cases_frac_legend <- p_cases_frac
p_cases_frac <- p_cases_frac + theme(legend.position = "none")+labs(tag="d)")
p_cases_frac_legend  <- g_legend(p_cases_frac_legend,1)

p_cases_frac_legend <- grid.arrange(grobs = list(basic_figplot,p_cases_frac_legend),layout_matrix =  rbind(1,2))



plot_fig1 <- grid.arrange(cbind(rbind(ggplotGrob(p_regulation), ggplotGrob(p_cases),size = "last"),
                          rbind(ggplotGrob(p_KOF_ch), ggplotGrob(p_cases_frac),size = "last")))

plot_fig1 <- grid.arrange(grobs = list(plot_fig1,p_cases_frac_legend),layout_matrix =  cbind(1,1,2))

ggsave(plot_fig1, filename = paste0("Figure1_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 5, width = 10,  bg = "transparent")
ggsave(plot_fig1, filename = paste0("Figure1_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 5, width = 10,  bg = "transparent")

grid.newpage()

plot_fig1 <- rm
p_regulation <- rm
p_cases <- rm
p_KOF_ch <- rm
p_cases_frac <- rm

# Figure 2
## a) Cumulative incidence

models_output_summary$Re <- as.numeric(models_output_summary$Re)
#models_output_summary$imports <- factor(as.numeric(models_output_summary$imports),levels = unique(as.numeric(models_output_summary$imports)))
models_output_summary$imports <- as.character(models_output_summary$imports)
models_output_summary$imports <- factor(models_output_summary$imports,levels = c("0", "5050","3359", "7573"))

labels_prop = factor(c("Baseline scenario","Plausible scenario a)","Lower limit scenario b)","Upper limit scenario c)"), levels =c("Baseline scenario","Plausible scenario a)","Lower limit scenario b)","Upper limit scenario c)"))
cols <- col_9[c(7,3:5)]
f2_cum<- basic_figplot +
  geom_line(data=models_output_summary, aes(x=Re, y=cum_cases_median, group=imports, color = col_9[9]), alpha=0.6)+
  geom_ribbon(data=models_output_summary, aes(x=Re, ymin = cum_cases_min,ymax = cum_cases_max,fill=imports),alpha=0.4)+
  geom_hline(yintercept=cum_final_expected[1,1], linetype="dashed", color = col_9[9]) +
  geom_hline(yintercept=cum_final_expected[1,3], linetype="dashed", color = col_9[9]) +
  scale_color_manual(values=col_9[9], label="",name="") +#95% prediction interval
  scale_fill_manual(values=cols,name="", label= labels_prop) +
  scale_y_continuous(labels=yscaling, trans = 'log10') +
  scale_x_continuous(breaks = seq(from = 0.5, to = 1.5, by = 0.1))+
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)))+
  coord_cartesian(ylim = c(10^4, 10^5)) +
  guides(color = guide_legend(override.aes = list(size=6))) +
  labs(tag=bquote(.("")), x = bquote(italic("R"["e"])), y =bquote("Cumulative incidence"))

## b) final incidence
models_output_summary1 <- models_output_summary
models_output_summary1[models_output_summary1$Re >1.3,c("final_incidence_ll","final_incidence_median","final_incidence_ul","final_incidence_min","final_incidence_max")] <- 10^6
f2_final <- basic_figplot +
  geom_line(data=models_output_summary1, aes(x=as.numeric(Re), y= final_incidence_median, group=imports, color = col_9[9]),alpha=0.6)+
  geom_ribbon(data=models_output_summary1, aes( x=as.numeric(Re), ymin = final_incidence_min, ymax = final_incidence_max,fill=imports),alpha=0.4)+
  geom_hline(yintercept=cum_final_expected[2,1], linetype="dashed", color = col_9[9]) +
  geom_hline(yintercept=cum_final_expected[2,3], linetype="dashed", color = col_9[9]) +
  scale_color_manual(values=col_9[9], label="",name="") +#95% prediction interval
  scale_fill_manual(values=cols,name="", label= labels_prop) +
  scale_x_continuous(breaks = seq(from = 0.5, to = 1.5, by = 0.1))+
  coord_cartesian(ylim = c(0, 10^3)) +
  #guides(color = guide_legend(override.aes = list(size=6))) +
  labs(tag=bquote(.("")), x = bquote(italic("R"["e"])), y =bquote("Final incidence"))

#legend
legend_final_cum <- basic_figplot +
  geom_ribbon(data=models_output_summary1, aes( x=as.numeric(Re), ymin = final_incidence_min, ymax = final_incidence_max,fill=imports),alpha=0.4)+
  scale_fill_manual(values=cols,name="", label= labels_prop) +
  guides(color = guide_legend(override.aes = list(size=6)))
models_output_summary1 <- rm
legend_final_cum  <- g_legend(legend_final_cum,1)
f2_final<- f2_final + theme(legend.position = "none")
f2_cum_final <- grid.arrange(grobs = list(f2_cum,f2_final,legend_final_cum),layout_matrix =  cbind(1,2,3))
ggsave(f2_cum_final, filename = paste0("../presentations/Figures/F2_cum_final_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 3, width = 10,  bg = "transparent")

f2_final<-  f2_final+ labs(tag=bquote(.("b)")))
f2_cum <- f2_cum+ labs(tag=bquote(.("a)")))

#c) prior posterior distribution:
prior <-as.data.frame(matrix(ncol = 1,nrow=length(Re_all)))
prior$Re <- as.numeric(Re_all)
prior$class <- "prior"
prior$imports <- prior$imports1 <- as.character("prior distribution")
prior <- prior[,-c(1)]
simulation_accept$class <- "posterior"
posterior<- simulation_accept[,c("Re","class","imports")]
posterior$imports <- factor(posterior$imports, levels=unique(posterior$imports)[order(unique(posterior$imports))])

posterior$imports_name[posterior$imports==levels(posterior$imports)[1]] <- "baseline scenario"
posterior$imports_name[posterior$imports==levels(posterior$imports)[3]] <- "a)"
posterior$imports_name[posterior$imports==levels(posterior$imports)[2]] <- "b)"
posterior$imports_name[posterior$imports==levels(posterior$imports)[4]] <- "c)"

posterior$imports1 <- paste0("posterior distribution for ",posterior$imports_name)
posterior$imports_name <- NULL
posterior$imports1 <- factor(posterior$imports1, levels=unique(posterior$imports1)[order(unique(posterior$imports))])

prior_posterior <- rbind(prior,posterior)
prior_posterior$imports <- factor(prior_posterior$imports1, levels=c("prior distribution","posterior distribution for baseline scenario","posterior distribution for a)","posterior distribution for b)","posterior distribution for c)"))
prior_posterior <- as.data.frame(prior_posterior)
cols <- col_9[c(9,7,3:5)]

prior_posterior_plot <- basic_figplot+
  geom_density(data=prior_posterior, aes(x=as.numeric(Re),fill=imports),color="transparent",alpha=0.5)+
  #scale_color_manual(values=cols,name="")+
  scale_fill_manual(values=cols,name="")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(from = 0.5, to = 1.5, by = 0.1))+
  coord_cartesian(xlim = c(0.5,1.5)) +
  labs(tag=bquote(.("")),subtitle ="", x = bquote(italic("R"["e"])), y =bquote("Density"))# bquote(italic("Imports") == .("5050"))
ggsave(prior_posterior_plot, filename = paste0("../presentations/Figures/prior_posterior_plot_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 4, width = 6,  bg = "transparent")

prior_posterior_plot <- prior_posterior_plot+theme(legend.position="none")+  labs(tag=bquote(.("c)")),subtitle ="", x = bquote(italic("R"["e"])), y =bquote("Density"))# bquote(italic("Imports") == .("5050"))


plot_fig2 <- grid.arrange(rbind(ggplotGrob(f2_cum), ggplotGrob(prior_posterior_plot),size = "last"))
f2_final <- ggplotGrob(f2_final)
plot_fig2 <- grid.arrange(grobs = list(plot_fig2,f2_final,legend_final_cum),layout_matrix =  rbind(c(1,2),c(1,3)))

ggsave(plot_fig2, filename = paste0("Figure2_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 5, width = 10,  bg = "transparent")
ggsave(plot_fig2, filename = paste0("Figure2_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 5, width = 10,  bg = "transparent")
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
plots <- list()
labels_prop = factor(c("Baseline scenario","Plausible scenario a)","Lower limit scenario b)","Upper limit scenario c)"), levels = c("Baseline scenario","Plausible scenario a)","Lower limit scenario b)","Upper limit scenario c)"))
for (I in 1:imports_length) {
  if(I %in% c(1,3)){
    tags <- ""
    x_name <- "Daily incidence"
  }
  else{
    tags <- ""
    x_name <- ""
  }
  models_output1 <- models_output[models_output$imports ==import_num[I],]
  models_output1$Re <- as.character(models_output1$Re)
  models_output1_notaccept <- c()
  models_output1_accept <- c()
  if(sum(models_output1$simulation_accepted ==1)!=0){
    models_output1_accept <- models_output1[models_output1$simulation_accepted ==1,]
    models_output1_accept <- as.data.frame(t(models_output1_accept[,c(8:129)]))#8
    models_output1_accept$date <- swiss_cases_su2020$date
    models_output1_accept<- melt(models_output1_accept, id.vars="date")
  }
  if(sum(models_output1$simulation_accepted ==0)!=0){
    models_output1_notaccept <- models_output1[models_output1$simulation_accepted ==0,]
    models_output1_notaccept[1,]<-NA
    models_output1_notaccept<-as.data.frame(t(models_output1_notaccept[,c(8:129)]))#8
    models_output1_notaccept$date <- swiss_cases_su2020$date
    models_output1_notaccept<- melt(models_output1_notaccept, id.vars="date")
  }
  cols <- col_9[c(7,3:5,9)]
  cols <- cols[c(I,5)]
  plots[[I]] <- basic_figplot +
    geom_line(data=models_output1_notaccept, aes( x=date, y=value ,group= variable,color="Rejected trajectories"), alpha=0.4)+
    geom_line(data=models_output1_accept, aes( x=date, y=value ,group= variable,color="Accepted trajectories"), alpha=0.8)+
    geom_point(data=swiss_cases_su2020, aes(x=date, y = cases_reported,shape=col_9[2]),color=col_9[2],size=0.8,alpha=0.7)+
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b",
                 limits = as_date(c(time_window[1],time_window[2])))+
    scale_shape_discrete(name="", label="Reported cases")+
    scale_color_manual(values=cols, name="", labels=c("Accepted trajectories","Rejected trajectories"))+
    coord_cartesian(ylim = c(0, 10^3)) +
    guides(shape = guide_legend(override.aes = list(size=3)),color = guide_legend(override.aes = list(size=3))) +
    theme(legend.position="none")+
    labs(tag=bquote(.(tags)),subtitle = labels_prop[I], x = "", y =bquote(.(x_name)))
}
f3_sim_cases <- grid.arrange(grobs = plots[c(1:(I))],layout_matrix =  t(matrix(1:4,2,2)))
grid.newpage()
ggsave(f3_sim_cases, filename = paste0("Figure3_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 5, width = 10,  bg = "transparent")
grid.newpage()
plots <- rm

simulation_0_accepted <- models_output[models_output$simulation_accepted ==1 & models_output$imports =="0",]
simulation_0_notaccepted <- models_output[models_output$simulation_accepted ==0& models_output$imports =="0",]
simulation_0_accepted <- as.data.frame(t(simulation_0_accepted[,c(8:129)]))
simulation_0_notaccepted<-as.data.frame(t(simulation_0_notaccepted[,c(8:129)]))
simulation_0_accepted$date <- simulation_0_notaccepted$date <- swiss_cases_su2020$date
simulation_0_accepted<- melt(simulation_0_accepted, id.vars="date")
simulation_0_notaccepted<- melt(simulation_0_notaccepted, id.vars="date")

f3baseline_sim_cases <- basic_figplot +
  geom_line(data=simulation_0_notaccepted, aes( x=date, y=value ,group= variable,color="Rejected trajectories"))+
  geom_line(data=simulation_0_accepted, aes( x=date, y=value ,group= variable,color="Accepted trajectories"))+
  geom_point(data=swiss_cases_su2020, aes(x=date, y = cases_reported,shape=col_9[2]),color=col_9[2],size=0.8,alpha=0.5)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               limits = as_date(c(time_window[1],time_window[2])))+
  scale_shape_discrete(name="", label="Reported cases")+
  scale_color_manual(values=c(col_9[c(7,9)]), name="", labels=c("Accepted trajectories","Rejected trajectories"))+
  coord_cartesian(ylim = c(0, 10^3)) +
  guides(shape = guide_legend(override.aes = list(size=3)),color = guide_legend(override.aes = list(size=6))) +
  labs(tag=bquote(.("")),subtitle = bquote("Baseline scenario"), x = "", y =bquote(.("Incidence")))
#legend
ggsave(f3baseline_sim_cases, filename = paste0("../presentations/Figures/f3baseline_sim_cases",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 2, width = 5,  bg = "transparent")

## d) simulations with 5050 imports
simulation_5050_accepted <- models_output[models_output$simulation_accepted ==1 & models_output$imports =="5050",]
simulation_5050_notaccepted <- models_output[models_output$simulation_accepted ==0& models_output$imports =="5050",]
simulation_5050_accepted <- as.data.frame(t(simulation_5050_accepted[,c(8:129)]))
simulation_5050_notaccepted<-as.data.frame(t(simulation_5050_notaccepted[,c(8:129)]))
simulation_5050_accepted$date <- simulation_5050_notaccepted$date <- swiss_cases_su2020$date
simulation_5050_accepted<- melt(simulation_5050_accepted, id.vars="date")
simulation_5050_notaccepted<- melt(simulation_5050_notaccepted, id.vars="date")

f2a_sim_cases <- basic_figplot +
  geom_line(data=simulation_5050_notaccepted, aes( x=date, y=value ,group= variable,color="Rejected trajectories"))+
  geom_line(data=simulation_5050_accepted, aes( x=date, y=value ,group= variable,color="Accepted trajectories"))+
  geom_point(data=swiss_cases_su2020, aes(x=date, y = cases_reported,shape=col_9[2]),color=col_9[2],size=0.8,alpha=0.5)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               limits = as_date(c(time_window[1],time_window[2])))+
  scale_shape_discrete(name="", label="Reported cases")+
  scale_color_manual(values=c(col_9[c(3,9)]), name="", labels=c("Accepted trajectories","Rejected trajectories"))+
  coord_cartesian(ylim = c(0, 10^3)) +
  guides(shape = guide_legend(override.aes = list(size=3)),color = guide_legend(override.aes = list(size=6))) +
  labs(tag=bquote(.("")),subtitle = bquote("Plausible scenario a)"), x = "", y =bquote(.("Incidence")))
#legend
ggsave(f2a_sim_cases, filename = paste0("../presentations/Figures/f2a_sim_cases",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 2, width = 5,  bg = "transparent")

#####
#Supplement

#####


# Supplementary Figure 1: incidence per country
country_level <- table(BAG_data_su2020$country)[order(table(BAG_data_su2020$country),decreasing=TRUE)]
BAG_data_su2020$country <-  factor(BAG_data_su2020$country, levels = c("Switzerland", names(country_level)[!names(country_level) %in% c("Switzerland","Others","Abroad but unknown","Unknown")],"Others","Abroad but unknown","Unknown"))
BAG_data_su2020$age_cat<- factor(BAG_data_su2020$age_cat, levels=c("<21","21-64",">64","Unknown"))

incidence_su2020_swiss <- incidence_su2020[incidence_su2020$location=="Switzerland",]
incidence_su2020_swiss$swiss <- incidence_su2020_swiss$location
incidence_su2020_swiss$country <- NULL
incidence_su2020_noswiss <-  incidence_su2020[incidence_su2020$location!="Switzerland",]
incidence_su2020_noswiss$country <-  factor(incidence_su2020_noswiss$location, levels = c( names(country_level)[!names(country_level) %in% c("Switzerland","Others","Abroad but unknown","Unknown")]))
#incidence_su2020$abroad[incidence_su2020$country=="Switzerland"] <-"Switzerland"
#incidence_su2020$abroad[incidence_su2020$country!="Switzerland"] <-"abroad"
su2020_quarantine$country <- factor(su2020_quarantine$country, levels = c( names(country_level)[!names(country_level) %in% c("Switzerland","Others","Abroad but unknown","Unknown")]))


incidence_plot <- ggplot()+
  geom_line(data=incidence_su2020_noswiss, aes(x=as_date(date), y=new_cases_smoothed_per_million))+
  facet_wrap(.~country,ncol=3)+
  geom_line(data=incidence_su2020_swiss,aes(x=as_date(date), y=new_cases_smoothed_per_million),color= col_9[2])+
  geom_rect(data= su2020_quarantine, aes(xmin = as_date(start_date), ymin = 0, xmax = as_date(end_date), ymax = 250), fill= col_9[6], colour= "transparent", alpha=0.4)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               limits = as_date(c(time_window[1],time_window[2])))+
  theme_minimal()+  
  labs(tag="",x = "", y =bquote("Daily incidence per million"))
ggsave(incidence_plot, filename = paste0("SF1_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 10, width = 7,  bg = "transparent")


incidence_plot1 <- ggplot()+
  facet_wrap(~country,ncol=3, scales = "free")+
  geom_line(data=incidence_su2020[incidence_su2020$country!="Switzerland",], aes(x=as_date(date), y=new_cases_smoothed_per_million))+
  geom_line(data=incidence_su2020_swiss,aes(x=as_date(date), y=new_cases_smoothed_per_million),color= col_9[2])+
  geom_rect(data= su2020_quarantine, aes(xmin = as_date(start_date), ymin = 0, xmax = as_date(end_date), ymax = 250), fill= col_9[6], colour= "transparent", alpha=0.4)+
  geom_histogram(data=BAG_data_su2020[!BAG_data_su2020$country%in%c("Switzerland","Unknown","Abroad but unknown", "Others"),], 
                 aes(y=..count..*25/3,x=as_date(date),fill= age_cat), 
                 position = pos_fill,binwidth = 1, alpha = 0.6)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               limits = as_date(c(time_window[1],time_window[2])))+
  theme_minimal()+  
  scale_y_continuous(sec.axis = sec_axis(~./25*3, name = "Daily incidence"))+
  scale_fill_manual(values =col_9[3:5], name="")+
  labs(tag="",x = "", y =bquote("Daily incidence per million"))
ggsave(incidence_plot1, filename = paste0("SF12_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 10, width = 5,  bg = "transparent")


  
# Supplementary Figure 2:
#plot reported cases including regarding their most likely place of infection and their age category
for(i in 1:2){
  p_import <-list()
  for(c in unname(levels(BAG_data_su2020$country)[-25])){
    if(i==1){
      #max_y <-1
      pos_fill <- "fill"
    }
    if(i==2){
      if(c %in% c("Switzerland", "Unknown")){
        # max_y <- 350
      }
      if(!c %in% c("Switzerland", "Unknown")){
        # max_y <- 50
      } 
      pos_fill <- "stack"
    }
    countries <- BAG_data_su2020[BAG_data_su2020$country %in% c,]
    p_import[[c]] <- ggplot(countries[,c("date", "country","age_cat")], aes(x=date)) +
      geom_histogram(aes(fill= age_cat), 
                     position = pos_fill,binwidth = 1, alpha = 0.6)+
      scale_x_date(date_labels = "%b",date_breaks = "1 month", limits = c(time_window[1], time_window[2]))+
      scale_fill_manual(values =col_9[3:5])+
      labs(x = "", y ="",subtitle =c)+
      
      theme(plot.margin = margin(8, 2, 2, 2, "mm"),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = "transparent"), # bg of the plot
            axis.text.x = element_text(size = 5),#angle = -20),
            axis.text.y = element_text(size = 5),
            plot.subtitle  = element_text(size = 6),
            axis.title.y = element_text(size = 5),
            text = element_text(size =rel(3.5)),
            rect = element_rect(fill = "transparent"),
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none")
  }
  if(i==1){
    p_import_proportion <- p_import
  }
}

legend <- ggplot(countries[,c("date", "country","age_cat")], aes(x=date)) +
  geom_histogram(aes(fill= age_cat),position = pos_fill,binwidth = 1, alpha = 0.6)+
  theme(plot.margin = margin(8, 10, 2, 10, "mm"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"))+
  scale_fill_manual(values =col_9[3:5], name="Age categories")
legend_sup2  <- g_legend(legend,1)


p_import[[11]] <- p_import[[11]]+labs(y = "Daily incidence",subtitle =levels(BAG_data_su2020$country)[11])
p_import_proportion[[11]] <- p_import_proportion[[11]]+labs(y = "Proportion of age categories by daily incidence",subtitle =levels(BAG_data_su2020$country)[11])

plot_countries <- grid.arrange(grobs = p_import,layout_matrix =  t(matrix(1:25,5,5)))
plot_countries_proportion <- grid.arrange(grobs = p_import_proportion,layout_matrix =  t(matrix(1:25,5,5)))

plot_countries <- grobTree(plot_countries,textGrob(bquote("a)"), x = 0.02, y = 0.98))
plot_countries_proportion <-grobTree(plot_countries_proportion,textGrob(bquote("b)"), x = 0.02, y = 0.98))
plot_countries <- grid.arrange(plot_countries,plot_countries_proportion,layout_matrix = rbind(1,2))

#plot age difference by most likely place of infection
age_percountry<- ggplot(BAG_data_su2020, aes(x=country, y=age)) +
  geom_boxplot(aes(y=age),fill="transparent",color= col_9[9],size=0.5)+
  geom_hline(yintercept=mean(BAG_data_su2020$age[BAG_data_su2020$country=="Switzerland"]),color=col_9[2])+
  stat_summary(fun=mean, geom="point", shape=3, size=3, color="black") +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust=0.95,vjust = 0.1, size=8, angle=90),#5
        plot.margin = margin(8, 2, 2, 2, "mm"),
        plot.background = element_rect(fill = "transparent", color =  "transparent"), # bg of the plot
        axis.text.y = element_text(size = 8),#5
        axis.title.y = element_text(size = 10),#5
        text = element_text(size =rel(3.5)),
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.position = "none")+
  labs( x = "", y = "Age (in years)")
ggsave(age_percountry, filename = paste0("../presentations/Figures/plot_countries_age_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 4, width = 6,  bg = "transparent")

age_percountry <- grid.arrange(age_percountry, ncol=1)
age_percountry <-grobTree(age_percountry,textGrob(bquote("c)"), x = 0.02, y = 0.98))

plot_countries_age <- grid.arrange(plot_countries,legend_sup2,age_percountry,layout_matrix =  rbind(c(1,1,1,1),
                                                                                                    c(1,1,1,1),
                                                                                                    c(1,1,1,1),
                                                                                                    c(1,1,1,1),
                                                                                                    c(3,3,3,2)))
ggsave(plot_countries_age, filename = paste0("SF2_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 15, width = 6,  bg = "transparent")
grid.newpage()
plot_countries_age <- rm
plot_countries <- rm
#####

# Supplementary Figure 3: Regional differences in cross-border associated cases

cantons <- basic_figplot+
  geom_histogram(data=BAG_data_su2020, aes(canton, fill=country_cat), stat="count",position = "stack") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_9) +
  labs(tag="",subtitle = "Cantons", x =bquote(""), y =bquote(""))
cantons1 <-basic_figplot+
  geom_histogram(data=BAG_data_su2020, aes(canton, fill=country_cat), stat="count",position = "fill") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_9) +
  labs(tag="",subtitle = "", x =bquote(""), y =bquote(""),fill="")
regions <-basic_figplot+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust=1,vjust = 0.2,angle=90))+
  geom_histogram(data=BAG_data_su2020, aes(regions, fill=country_cat), stat="count",position = "stack") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_9) +
  labs(tag="",subtitle = "Regions of CH", x =bquote(""), y =bquote(""),fill="")
regions1 <-basic_figplot+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust=1,vjust = 0.2,angle=90))+
  geom_histogram(data=BAG_data_su2020, aes(regions, fill=country_cat), stat="count",position = "fill") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_9) +
  labs(tag="",subtitle = "", x =bquote(""), y =bquote(""),fill="")
airport <-basic_figplot+
  theme_minimal()+
  geom_histogram(data=BAG_data_su2020, aes(canton_inter_airport, fill=country_cat), stat="count",position = "stack") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_9) +
  labs(tag="",subtitle = "International airports in CH", x =bquote(""), y =bquote(""),fill="")
airport1 <- basic_figplot+
  theme_minimal()+
  geom_histogram(data=BAG_data_su2020, aes(canton_inter_airport, fill=country_cat), stat="count",position = "fill") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_9) +
  labs(tag="",subtitle  = "",x =bquote(""), y =bquote(""),fill="")
legend <- basic_figplot+
  geom_histogram(data=BAG_data_su2020, aes(canton_inter_airport, fill=country_cat), stat="count",position = "fill") +#scale_fill_discrete(name="")+
  scale_fill_manual(name="Exposure",values=col_9)
legend  <- g_legend(legend,1)

regions_exposure <- grid.arrange(cantons, cantons1, regions, regions1, airport,airport1,legend,layout_matrix =  rbind(c(1,1,1,2,2,2,7),c(3,3,NA,4,4,NA,NA), c(5,NA, NA,6,NA,NA,NA)))
ggsave(regions_exposure, filename = paste0("SF3_",format(Sys.time(), "%Y-%m-%d"), ".png"), height = 10, width = 15,  bg = "transparent")




