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
library(lubridate)
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
        plot.tag = element_text( size = 16,  hjust = 0),
        legend.text= element_text(size = 10),
        legend.title= element_text(size = 10))+
theme_minimal()
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
  labs(tag="c)",x = "", y =bquote("Reported cases"))


labels_prop = factor(c("3304 by cases with reported exposure place","3359 by all reported cases","5050 by all reported cases","7573 by all reported cases"), levels = c("3304 by cases with reported exposure place","3359 by all reported cases","5050 by all reported cases","7573 by all reported cases"))
p_cases_frac <- basic_figplot+
  geom_line(data= swiss_cases_su2020,aes(x=date, y=imports_propotion_known,color=labels_prop[1]),alpha = 0.5)+
  geom_line(data= swiss_cases_su2020,aes(x=date, y=imports_abroad1_15_propotion_all,color=labels_prop[2]),alpha = 0.5)+
  geom_line(data= swiss_cases_su2020,aes(x=date, y=imports_abroad_propotion_all, color=labels_prop[3]),  alpha = 0.5)+
  geom_line(data= swiss_cases_su2020,aes(x=date, y=imports_abroad15_propotion_all, color=labels_prop[4]),alpha = 0.5)+
  guides(color = guide_legend(override.aes = list(size=6))) +
  ylim(0, 1)+
  scale_color_manual(values=col_9[1:4], label= labels_prop,name="Potential import scenarios") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               limits = as_date(c(time_window[1]-1,time_window[2]+1)))+
  #guides(color = guide_legend(override.aes = list(size=3))) +
  labs(tag="d)", x = "", y =bquote("Proportion of imports"))


p_cases_frac_legend <- p_cases_frac
p_cases_frac <- p_cases_frac + theme(legend.position = "none")
p_cases_frac_legend  <- g_legend(p_cases_frac_legend,1)

p_cases_frac_legend <- grid.arrange(grobs = list(basic_figplot,p_cases_frac_legend),layout_matrix =  rbind(1,2))



plot_fig1 <- grid.arrange(cbind(rbind(ggplotGrob(p_regulation), ggplotGrob(p_cases),size = "last"),
                          rbind(ggplotGrob(p_KOF_ch), ggplotGrob(p_cases_frac),size = "last")))

plot_fig1 <- grid.arrange(grobs = list(plot_fig1,p_cases_frac_legend),layout_matrix =  cbind(1,1,2))

ggsave(plot_fig1, filename = paste0("Figure1_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 5, width = 10,  bg = "transparent")
grid.newpage()

plot_fig1 <- rm
p_regulation <- rm
p_cases <- rm
p_KOF_ch <- rm
p_cases_frac <- rm

# Figure 2
## a) Cumulative incidence

models_output_summary$Re <- as.numeric(models_output_summary$Re)
models_output_summary$imports <- as.character(models_output_summary$imports)
f2_cum<- basic_figplot +
  geom_line(data=models_output_summary, aes(x=Re, y=cum_cases_median, group=imports, color = "Expected cases"), alpha=0.6)+
  geom_ribbon(data=models_output_summary, aes(x=Re, ymin = cum_cases_ll,ymax = cum_cases_ul,fill=imports),alpha=0.4)+
  geom_hline(yintercept=cum_final_expected[1,1], linetype="dashed", color = col_9[9]) +
  geom_hline(yintercept=cum_final_expected[1,3], linetype="dashed", color = col_9[9]) +
  scale_y_continuous(labels=yscaling, trans = 'log10') +
  scale_color_manual(values=col_9[9], name="",label="95% CI incidence") +
  scale_fill_manual(values=col_9[2:5],name="No. cross-border-associated cases", label= as.character(import_num)) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1.2, by = 0.1))+
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)))+
  coord_cartesian(ylim = c(10^4, 10^5)) +
  guides(color = guide_legend(override.aes = list(size=6))) +
  labs(tag=bquote(.("a)")), x = bquote(italic("R"["e"])), y =bquote("Cumulative incidence"))

## b) final incidence
f2_final <- basic_figplot +
  geom_line(data=models_output_summary, aes(x=Re, y= final_incidence_median, group=imports, color = col_9[9]),alpha=0.6)+
  geom_ribbon(data=models_output_summary, aes( x=Re, ymin = final_incidence_ll, ymax = final_incidence_ul,fill=imports),alpha=0.4)+
  geom_hline(yintercept=cum_final_expected[2,1], linetype="dashed", color = col_9[9]) +
  geom_hline(yintercept=cum_final_expected[2,3], linetype="dashed", color = col_9[9]) +
  scale_color_manual(values=col_9[9], label="95% CI incidence",name="") +
  scale_fill_manual(values=col_9[2:5],name="No. cross-border-associated cases", label= as.character(import_num)) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1.2, by = 0.1))+
  coord_cartesian(ylim = c(100, 10^3)) +
  guides(color = guide_legend(override.aes = list(size=6))) +
  labs(tag=bquote(.("b)")), x = bquote(italic("R"["e"])), y =bquote("Final incidence"))
#legend
legend_final_cum <- f2_final
legend_final_cum  <- g_legend(legend_final_cum,2)
f2_final<- f2_final + theme(legend.position = "none")

#c) prior posterior distripution:
all_simulation_5050 <- models_output[models_output$imports %in% "5050",]
simulation_accept_5050 <- simulation_accept[simulation_accept$imports %in% "5050",]
all_simulation_5050$class <- "prior"
simulation_accept_5050$class <- "posterior"
simulations_5050<- rbind(simulation_accept_5050, all_simulation_5050)
simulations_5050$Re <- as.numeric(as.character(simulations_5050$Re))
simulations_5050$class <- factor(simulations_5050$class, levels = c("prior","posterior"))
prior_posterior_plot <- ggplot(simulations_5050, aes(x=Re, color=class, fill=class))+
  geom_density( alpha=0.5)+
  scale_color_manual(values=col_9[c(7,2)],name="", labels=c("prior ","posterior distribution"))+
  scale_fill_manual(values=col_9[c(7,2)],name="", labels=c("prior ","posterior distribution"))+
  theme_minimal()+
  scale_x_continuous()+
  theme(legend.position="none")+
  labs(tag=bquote(.("c)")),subtitle = bquote(italic("Imports") == .("5050")), x = bquote(italic("R"["e"])), y =bquote("Density"))

## d) simulations with 5050 imports
simulation_5050_accepted <- all_simulation_5050[all_simulation_5050$simulation_accepted ==1,]
simulation_5050_notaccepted <- all_simulation_5050[all_simulation_5050$simulation_accepted ==0,]

simulation_5050_accepted <- as.data.frame(t(simulation_5050_accepted[,c(8:129)]))
simulation_5050_notaccepted<-as.data.frame(t(simulation_5050_notaccepted[,c(8:129)]))

simulation_5050_accepted$date <- simulation_5050_notaccepted$date <- swiss_cases_su2020$date

simulation_5050_accepted<- melt(simulation_5050_accepted, id.vars="date")
simulation_5050_notaccepted<- melt(simulation_5050_notaccepted, id.vars="date")

f2_sim_cases <- basic_figplot +
  geom_line(data=simulation_5050_notaccepted, aes( x=date, y=value ,group= variable,color="Not accepted scenarios"))+
  geom_line(data=simulation_5050_accepted, aes( x=date, y=value ,group= variable,color="Accepted scenarios"))+
  geom_point(data=swiss_cases_su2020, aes(x=date, y = cases_reported,shape=col_9[1]),color=col_9[1],size=0.8,alpha=0.5)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               limits = as_date(c(time_window[1],time_window[2])))+
  scale_shape_discrete(name="", label="Reported cases")+
  scale_color_manual(values=c(col_9[c(2,9)]), name="", labels=c("Accepted scenarios","Not accepted scenarios"))+
  coord_cartesian(ylim = c(0, 10^3)) +
  guides(shape = guide_legend(override.aes = list(size=3)),color = guide_legend(override.aes = list(size=6))) +
  labs(tag=bquote(.("d)")),subtitle = bquote(italic("Imports") == .("5050")), x = "", y =bquote(.("No. cases")))
#legend
legend <- f2_sim_cases
legend_sim_cases  <- g_legend(legend,2)
f2_sim_cases <- f2_sim_cases + theme(legend.position = "none")

fig2_legend <- grid.arrange(rbind(legend_final_cum, legend_sim_cases,size = "last"))

plot_fig2 <- grid.arrange(rbind(ggplotGrob(f2_cum), ggplotGrob(prior_posterior_plot),size = "last"))
fig22 <- grid.arrange(rbind(ggplotGrob(f2_final),ggplotGrob(f2_sim_cases),size = "last"))
plot_fig2 <- grid.arrange(grobs = list(plot_fig2,fig22),layout_matrix =  cbind(1,2))

plot_fig2 <- grid.arrange(grobs = list(plot_fig2,fig2_legend),layout_matrix =  cbind(1,1,2))
ggsave(plot_fig2, filename = paste0("Figure2_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 5, width = 10,  bg = "transparent")
grid.newpage()

plot_fig2 <- rm 
fig2_legend <- rm
fig22 <- rm
fig2_legend <- rm 
f2_sim_cases <- rm
legend_sim_cases <- rm
legend_final_cum <- rm

#####

#Supplement

#####

# Supplementary Figure 1: Visualizing cases per day with and without Influx
#####


plots <- list()
for (I in 1:imports_length) {
  if(I %in% c(1,3)){
    tags <- ""
    x_name <- "No. cases"
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
  if(sum(models_output1$simulation_accepted ==1)==0){
    models_output1_accept <- models_output1[models_output1$simulation_accepted ==1,]
    models_output1_accept <- as.data.frame(t(models_output1_accept[,c(8:129)]))#8
    models_output1_notaccept[1,]<-NA
    models_output1_accept$date <- swiss_cases_su2020$date
    models_output1_accept<- melt(models_output1_accept, id.vars="date")
    models_output1_accept$variable<- -1
    models_output1_accept$value<- -1
  }
  
  if(sum(models_output1$simulation_accepted ==0)!=0){
    models_output1_notaccept <- models_output1[models_output1$simulation_accepted ==0,]
    models_output1_notaccept<-as.data.frame(t(models_output1_notaccept[,c(8:129)]))#8
    models_output1_notaccept$date <- swiss_cases_su2020$date
    models_output1_notaccept<- melt(models_output1_notaccept, id.vars="date")
  }
  if(sum(models_output1$simulation_accepted ==0)==0){
    models_output1_notaccept <- models_output1[models_output1$simulation_accepted ==0,]
    models_output1_notaccept[1,]<-NA
    models_output1_notaccept<-as.data.frame(t(models_output1_notaccept[,c(8:129)]))#8
    models_output1_notaccept$date <- swiss_cases_su2020$date
    models_output1_notaccept<- melt(models_output1_notaccept, id.vars="date")
    models_output1_notaccept$variable<- -1
    models_output1_notaccept$value<- -1
  }
  
  plots[[I]] <- basic_figplot +
    geom_line(data=models_output1_notaccept, aes( x=date, y=value ,group= variable,color="Not accepted scenarios"), alpha=0.4)+
    geom_line(data=models_output1_accept, aes( x=date, y=value ,group= variable,color="Accepted scenarios"), alpha=0.8)+
    geom_point(data=swiss_cases_su2020, aes(x=date, y = cases_reported,shape=col_9[1]),color=col_9[1],size=0.8,alpha=0.7)+
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b",
                 limits = as_date(c(time_window[1],time_window[2])))+
    scale_shape_discrete(name="", label="Reported cases")+
    scale_color_manual(values=c(col_9[c(2,9)]), name="", labels=c("Accepted scenarios","Not accepted scenarios"))+
    coord_cartesian(ylim = c(0, 10^3)) +
    guides(shape = guide_legend(override.aes = list(size=3)),color = guide_legend(override.aes = list(size=3))) +
    labs(tag=bquote(.(tags)),subtitle = bquote(italic("Imports") == .(import_num[I])), x = "", y =bquote(.(x_name)))
}
plots[[I+1]] <-  plots[[I]] 
for (I in 1:imports_length){# import_num[c(1:3)]
  plots[[I]] <-   plots[[I]] + theme(legend.position = "none")
}
plots[[I+1]]  <- g_legend(plots[[I+1]],2)
sf1_sim_cases <- grid.arrange(grobs = plots[c(1:(I+1))],layout_matrix =  rbind(c(1,2,5),c(3,4,NA)))
plots <- rm
grid.newpage()
ggsave(sf1_sim_cases, filename = paste0("SF1_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 5, width = 10,  bg = "transparent")
grid.newpage()

#####

# Supplementary Figure 2:
#plot reported cases including regarding their most likely place of infection and their age category

country_level <- table(BAG_data_su2020$country)[order(table(BAG_data_su2020$country),decreasing=TRUE)]
BAG_data_su2020$country <-  factor(BAG_data_su2020$country, levels = c("Switzerland", names(country_level)[!names(country_level) %in% c("Switzerland","Others","Abroad but unknown","Unknown")],"Others","Abroad but unknown","Unknown"))
BAG_data_su2020$age_cat<- factor(BAG_data_su2020$age_cat, levels=c("<21","21-64",">64","Unknown"))

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


p_import[[3]] <- p_import[[3]]+labs(y = "Number of reported cases",subtitle =levels(BAG_data_su2020$country)[3])
p_import_proportion[[3]] <- p_import_proportion[[3]]+labs(y = "Proportion of age categories of reported cases per day",subtitle =levels(BAG_data_su2020$country)[3])

plot_countries <- grid.arrange(grobs = p_import,layout_matrix =  matrix(1:25,5,5))
plot_countries_proportion <- grid.arrange(grobs = p_import_proportion,layout_matrix =  matrix(1:25,5,5))

plot_countries <- grobTree(plot_countries,textGrob(bquote("a)"), x = 0.02, y = 0.98))
plot_countries_proportion <-grobTree(plot_countries_proportion,textGrob(bquote("b)"), x = 0.02, y = 0.98))
plot_countries <- grid.arrange(plot_countries,plot_countries_proportion,layout_matrix = matrix(1:2,2,1))
#plot age difference by most likely place of infection
age_percountry<- ggplot(BAG_data_su2020, aes(x=country, y=age)) +
  geom_boxplot(aes(y=age),fill="transparent",color= col_9[9],size=0.5)+
  geom_hline(yintercept=mean(BAG_data_su2020$age[BAG_data_su2020$country=="Switzerland"]),color=col_9[5])+
  stat_summary(fun=mean, geom="point", shape=3, size=3, color=col_9[3]) +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust=0.95,vjust = 0.1, size=5, angle=90),
        plot.margin = margin(8, 2, 2, 2, "mm"),
        plot.background = element_rect(fill = "transparent", color =  "transparent"), # bg of the plot
        axis.text.y = element_text(size = 5),
        axis.title.y = element_text(size = 5),
        text = element_text(size =rel(3.5)),
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.position = "none")+
  labs( x = "", y = "Age (in years)")

age_percountry <- grid.arrange(age_percountry, ncol=1)
age_percountry <-grobTree(age_percountry,textGrob(bquote("c)"), x = 0.02, y = 0.98))
plot_countries_age <- grid.arrange(plot_countries,legend_sup2,age_percountry,layout_matrix =  rbind(c(1,1,1,1),
                                                                                                    c(1,1,1,1),
                                                                                                    c(1,1,1,1),
                                                                                                    c(1,1,1,1),
                                                                                                    c(3,3,3,2)))
ggsave(plot_countries_age, filename = paste0("SF2_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 15, width = 6,  bg = "transparent")
grid.newpage()
plot_countries_age <- rm
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
ggsave(regions_exposure, filename = paste0("SF3_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 10, width = 15,  bg = "transparent")



