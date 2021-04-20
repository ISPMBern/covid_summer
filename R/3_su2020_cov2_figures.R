# Stochastic branching model
## SARS-CoV2 in Switzerland during Summer 2020
### M Reichmuth, February 2021

#library
library(lubridate)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(wesanderson)
library(lubridate)
library(reshape2)

# COVID-19 cases observed in Summer (Jun-Sep) 2020 in Switzerland
setwd("/Users/mr19m223/Documents/COVID_projects/Epidemic_Su2020/covid_summer/Figures")


#colors for figures
col_4 <- wes_palette("GrandBudapest2",4,  type = c( "discrete"))
col_3  <- wes_palette("Royal2",3, type = c( "continuous"))


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
        axis.title.y = element_text(size = 12),
        plot.tag = element_text( size = 16,  hjust = 0),
        legend.text= element_text(size = 12))+
theme_minimal()


## Figure 1: visualize reported imports
#####
p_regulation <- basic_figplot+
  theme_void()+
  geom_segment(data=restrictions, aes(x=start_date, xend=end_date, y=label, yend=label),col= c("#536475"), linetype=1, size=1) +
  geom_point(data=restrictions,aes(x=start_date,y=label),col= c("#536475"), size=2)+
  geom_point(data=restrictions,aes(x=end_date_dot, y=label),col= c("#536475"), size=2)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d-%b",
               limits = as_date(c(time_window[1],time_window[2])))+
  theme(plot.margin = margin(8, 10, 2, 30, "mm"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 8))+
  labs(tag="a)",x = "", y =bquote(""))

p_KOF_ch <- basic_figplot+
  theme(plot.margin = margin(8, 30, 2, 10, "mm"),
        axis.text.x = element_text(color="transparent"))+
  geom_line(data= KOF_su2020,aes(x= date, y=ch.kof.stringency.ch.stringency_plus), col= c("#536475"), size = 2) +
  ylim(0, 100)+
  labs(tag="b)",x = "", y =bquote("KOF Stringency-Plus Index"))

p_cases <- basic_figplot+
  geom_bar(data= swiss_cases_su2020, aes(x=date,y=cases_reported), width=1, position="identity", stat = "identity", fill=col_4[1], alpha = 0.3)+
  geom_bar(data= swiss_cases_su2020, aes(x=date,y=cases_abroad), width=1, stat = "identity", position = "identity", fill=col_4[2], alpha = 0.9)+
  theme(plot.margin = margin(8, 10, 2, 30, "mm"))+
  ylim(0, 600)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d-%b",
               limits = as_date(c(time_window[1],time_window[2])))+
  labs(tag="c)",x = "", y =bquote("Reported cases"))

p_cases_frac <- basic_figplot+
  geom_bar(data= swiss_cases_su2020, aes(x=date, y=imports_frac_known), width=1, position="identity", stat = "identity", fill=col_4[2], alpha = 0.5)+
  geom_bar(data= swiss_cases_su2020, aes(x=date, y=imports_frac_all),width=1, position="identity", stat = "identity", fill=col_4[2], alpha = 1)+
  theme(plot.margin = margin(8, 30, 2, 10, "mm"))+
  ylim(0, 1)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d-%b",
               limits = as_date(c(time_window[1],time_window[2])))+
  labs(tag="d)", x = "", y =bquote("Fraction of imports"))

plot_fig1 <- grid.arrange(cbind(rbind(ggplotGrob(p_regulation), ggplotGrob(p_cases),size = "last")),
                          rbind(ggplotGrob(p_KOF_ch), ggplotGrob(p_cases_frac),size = "last"),
                          layout_matrix = cbind(1,2))
ggsave(plot_fig1, filename = paste0("Figure1_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 5, width = 10,  bg = "transparent")
grid.newpage()


# Figure 2
#####
cases_summary_all <- data.frame(date=NA, ll=NA, median=NA,ul=NA, Imports=NA, Re=NA)
cases_summary_all <- cases_summary_all[-1,]
Re_fig2 <- round(quantile(simulation_accept$Re[simulation_accept$imports == c("5050") & na.omit(simulation_accept[,3])==1],probs),2)
I_fig2 <-import_num[c(1:3)]
cases_I_summary_all <- c()
for (I in 1:length(I_fig2)) {
for (R in 1:length(Re_fig2)) {
  cases_summary <- matrix(0, ncol=6,nrow= max_time)
  Ri <- grep(Re_fig2[R], round(Re_all,2))
  all_cases_r <-c()
  for (r in Ri) {
    all_cases_r <- rbind(all_cases_r,all_cases_imports_infect[[I]][[r]])
  }
  for (d in 1:max_time) {
    cases_summary[d,c(4:6)] <- quantile(as.numeric(all_cases_r[d,]),probs) 
    cases_summary[d,1] <- period[d] 
    cases_summary[d,c(2:3)] <- c(import_num[I],Re_fig2[R])
  }
  colnames(cases_summary) <- c("date", "imports_num", "Re", "ll", "median", "ul")
  cases_summary <- data.frame(cases_summary)
  cases_summary_all <- rbind(cases_summary,cases_summary_all)
}
  cases_I_summary_all <- rbind(cases_summary_all,cases_I_summary_all)
}
cases_I_summary_all$date <- as_date(cases_I_summary_all$date)

growth_cum_final_fig2<- growth_cum_final_summary[growth_cum_final_summary$imports   %in%  I_fig2,]

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


# plotting
## a)
plots <- list()
for (I in 1:length(I_fig2)) {
  if(I==1){
    tags <- "a)"
    x_name <- "Reported cases"
  }
  else{
    tags <- ""
    x_name <- ""
  }
  import_num_i <- import_num[I]
  cases_summary_all1 <- cases_I_summary_all[cases_I_summary_all$imports_num ==import_num_i,]
  cases_summary_all1$date <- as_date(cases_summary_all1$date)
  cases_summary_all1$Re <- as.character(cases_summary_all1$Re)
  #cases_summary_all1[,c(4:6)] <- as.numeric(cases_summary_all1[,c(4:6)])
  #cases_summary_all1 <- cases_summary_all1[cases_summary_all1$Re==0.92,]
  plots[[I]] <- basic_figplot +
    geom_ribbon(data=cases_summary_all1, aes( x=date, ymin = ll, ymax = ul,fill=Re), alpha=0.5)+
    geom_point(data=swiss_cases_su2020, aes(x=date, y = cases_reported,shape=col_4[4]),color=col_4[4],size=0.8,alpha=0.7)+
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%d-%b",
                 limits = as_date(c(time_window[1],time_window[2])))+
    scale_color_manual(values=col_4[4]) +
    scale_fill_manual(values=col_3, label= round(Re_fig2,2)) +
    scale_shape_discrete(name="", label="Reported cases")+
    coord_cartesian(ylim = c(0, 10^3)) +
    guides(shape = guide_legend(override.aes = list(size=3))) +
    labs(tag=bquote(.(tags)),subtitle = bquote(italic("I") == .(import_num_i)~italic("; k ~") ~.(0.5)), x = "", y =bquote(.(x_name)))
    }
plots[[I+1]]<- basic_figplot +
  geom_ribbon(data=cases_summary_all1, aes( x=date, ymin = ll, ymax = ul,fill=Re), alpha=0.5)+
  geom_point(data=swiss_cases_su2020, aes(x=date, y = cases_reported,shape=col_4[4]),color=col_4[4],size=4,alpha=0.7)+
 scale_color_manual(values=col_4[4]) +
  scale_fill_manual(values=col_3, label= round(Re_fig2,2),name= bquote(italic("R"["e"]))) +
  scale_shape_discrete(name="", label="Reported cases")

for (I in 1:length(c(1:3))){# import_num[c(1:3)]
  plots[[I]] <-   plots[[I]] + theme(legend.position = "none")
}
plots[[I+1]]  <- g_legend(plots[[I+1]],2)
f2_sim_cases <- grid.arrange(grobs = plots[c(1:(I+1))],layout_matrix =  matrix(1:4,1,4))

## b) 
plots <- list()

plots[[1]] <- basic_figplot +
  geom_line(data=growth_cum_final_fig2, aes(x=as.numeric(Re), y= cum_cases_median,group=as.character(imports)),color=col_4[4], alpha=0.5)+
  geom_ribbon(data=growth_cum_final_fig2, aes( x=as.numeric(Re), ymin = cum_cases_ll, ymax = cum_cases_ul,fill=as.character(imports),group=as.character(imports)), alpha=0.5)+
  geom_hline(yintercept=cum_cases_95CI[1], linetype="dashed", color = col_3[3]) +
  geom_hline(yintercept=cum_cases_95CI[3], linetype="dashed", color = col_3[3]) +
  scale_y_continuous(labels=yscaling, trans = 'log10') +
  scale_color_manual(values=col_3[3], name="Expected cases") +
  scale_fill_manual(values=col_4[1:3],label= I_fig2) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1.2, by = 0.1))+
  theme(legend.position = "none")+
  labs(tag=bquote(.("b)")), x = bquote(italic("R"["e"])), y =bquote("Cumulative cases"))
f2_cum <- grid.arrange(grobs = plots,layout_matrix =  matrix(1,1))

## c) 
plots <- list()
plots[[1]] <- basic_figplot +
  geom_line(data=growth_cum_final_fig2, aes(x=as.numeric(Re), y= final_size_median,group=as.character(imports),color=col_4[4]), alpha=0.5)+
  geom_ribbon(data=growth_cum_final_fig2, aes( x=as.numeric(Re), ymin = final_size_ll, ymax = final_size_ul,fill=as.character(imports),group=as.character(imports)), alpha=0.5)+
  geom_hline(yintercept=final_size_95CI[1], linetype="dashed", color = col_3[3]) +
  geom_hline(yintercept=final_size_95CI[3], linetype="dashed", color = col_3[3]) +
  scale_color_manual(values=col_3[3], name="Expected cases") +
  scale_fill_manual(values=col_4[1:3],label= I_fig2) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1.2, by = 0.1))+
  theme(legend.position = "none")+
  labs(tag=bquote(.("c)")), x = bquote(italic("R"["e"])), y =bquote("Final size"))
f2_final <- grid.arrange(grobs = plots,layout_matrix =  matrix(1,1))

## d) 
plots <- list()
plots[[1]] <- basic_figplot +
  geom_line(data=growth_cum_final_fig2, aes(x=as.numeric(Re), y= growth_r_median,group=as.character(imports),color=col_4[4]), alpha=0.5)+
  geom_ribbon(data=growth_cum_final_fig2, aes(x=as.numeric(Re), ymin = growth_r_ll, ymax = growth_r_ul,fill=as.character(imports),group=as.character(imports)), alpha=0.5)+
  geom_hline(yintercept=r_all$rate_weigth - qnorm(0.975)*r_all$error_weigth, linetype="dashed", color = col_4[4]) +
  geom_hline(yintercept=r_all$rate_weigth + qnorm(0.975)*r_all$error_weigth, linetype="dashed", color = col_4[4]) +
  geom_hline(yintercept=0,  color =  "grey") +
  scale_y_continuous(limits = c(-0.1, 0.1))+
  #scale_color_manual(values=col_3[3], name="Expected cases") +
  scale_fill_manual(values=col_4[1:3],label= I_fig2) +
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1.2, by = 0.1))+
  labs(tag=bquote(.("d)")),x = bquote(italic("R"["e"])), y =bquote("Growth rate" ~ italic("r")))

f2_growthrate <- grid.arrange(grobs = plots,layout_matrix =  matrix(1,1))

#legend
legend <- basic_figplot +
  geom_line(data=growth_cum_final_fig2, aes(x=as.numeric(Re), y= growth_r_median,group=as.character(imports),color=col_4[4]), alpha=0.5)+
  geom_ribbon(data=growth_cum_final_fig2, aes(x=as.numeric(Re), ymin = growth_r_ll, ymax = growth_r_ul,fill=as.character(imports),group=as.character(imports)), alpha=0.5)+
  geom_hline(yintercept=r_all$rate_weigth - qnorm(0.975)*r_all$error_weigth, linetype="dashed", color = col_4[4]) +
  scale_color_manual(values=col_3[3], label="Expected cases",name="") +
  scale_fill_manual(values=col_4[1:3],label= I_fig2, name="No. imports") 
legend  <- g_legend(legend,2)

plot_fig2 <- grid.arrange(
  grobs = list(f2_sim_cases,f2_cum, f2_final,f2_growthrate,legend),
  layout_matrix = rbind(c(1,1,1,1),
                        c(2,3,4,5)))
ggsave(plot_fig2, filename = paste0("Figure2_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 10, width = 20,  bg = "transparent")
grid.newpage()

#####

#Supplement

#####

# Supplementary Figure 1: Visualizing cases per day with and without Influx
#####

Re_supfig1 <- unique(round(Re_all,1))
Re_supfig1 <- Re_supfig1[order(Re_supfig1)]
cases_summary_all <- data.frame(date=NA, ll=NA, median=NA,ul=NA, Imports=NA, Re=NA)
cases_summary_all <- cases_summary_all[-1,]
I_supfig1 <-import_num[1:length(all_cases_imports_infect)]
cases_I_summary_all <- c()
for (I in 1:length(I_supfig1)) {
  for (R in 1:length(Re_supfig1)) {
    cases_summary <- matrix(0, ncol=6,nrow= max_time)
    Ri <- grep(Re_supfig1[R], round(Re_all,2))
    all_cases_r <-c()
    for (r in Ri) {
      all_cases_r <- rbind(all_cases_r,all_cases_imports_infect[[I]][[r]])
    }
    for (d in 1:max_time) {
      cases_summary[d,c(4:6)] <- quantile(as.numeric(all_cases_r[d,]),probs) 
      cases_summary[d,1] <- period[d] 
      cases_summary[d,c(2:3)] <- c(import_num[I],Re_supfig1[R])
    }
    colnames(cases_summary) <- c("date", "imports_num", "Re", "ll", "median", "ul")
    cases_summary <- data.frame(cases_summary)
    cases_summary_all <- rbind(cases_summary,cases_summary_all)
  }
  cases_I_summary_all <- rbind(cases_summary_all,cases_I_summary_all)
}
cases_I_summary_all$date <- as_date(cases_I_summary_all$date)
#saveRDS(cases_I_summary_all, paste0("../data/cases_I_summary_all_",Sys.Date(),".rds"))
plots <- list()
for (I in 1:length(I_supfig1)) {
  if(I==1){
    tags <- ""
    x_name <- "Reported cases"
  }
  else{
    tags <- ""
    x_name <- ""
  }
  import_num_i <- import_num[I]
  cases_summary_all1 <- cases_I_summary_all[cases_I_summary_all$imports_num ==import_num_i,]
  cases_summary_all1$date <- as_date(cases_summary_all1$date)
  cases_summary_all1$Re <- as.character(cases_summary_all1$Re)
  plots[[I]] <- basic_figplot +
    geom_ribbon(data=cases_summary_all1, aes( x=date, ymin = ll, ymax = ul,fill=Re), alpha=0.5)+
    geom_point(data=swiss_cases_su2020, aes(x=date, y = cases_reported,shape=col_4[4]),color=col_4[4],size=0.8,alpha=0.7)+
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%d-%b",
                 limits = as_date(c(time_window[1],time_window[2])))+
    scale_color_manual(values=col_4[4]) +
    scale_fill_manual(values=c(col_3,col_4[1:2]), label= round(Re_supfig1,2)) +
    scale_shape_discrete(name="", label="Reported cases")+
    coord_cartesian(ylim = c(0, 10^3)) +
    guides(shape = guide_legend(override.aes = list(size=3))) +
    labs(tag=bquote(.(tags)),subtitle = bquote(italic("I") == .(import_num_i)~italic("; k ~") ~.(0.5)), x = "", y =bquote(.(x_name)))
}
plots[[I+1]]<- basic_figplot +
  geom_ribbon(data=cases_summary_all1, aes( x=date, ymin = ll, ymax = ul,fill=Re), alpha=0.5)+
  geom_point(data=swiss_cases_su2020, aes(x=date, y = cases_reported,shape=col_4[4]),color=col_4[4],size=4,alpha=0.7)+
  scale_color_manual(values=col_4[4]) +
  scale_fill_manual(values=c(col_3,col_4[1:2]), label= round(Re_supfig1,2),name= bquote(italic("R"["e"]))) +
  scale_shape_discrete(name="", label="Reported cases")

for (I in 1:length(all_cases_imports_infect)){# import_num[c(1:3)]
  plots[[I]] <-   plots[[I]] + theme(legend.position = "none")
}
plots[[I+1]]  <- g_legend(plots[[I+1]],2)
sf1_sim_cases <- grid.arrange(grobs = plots[c(1:(I+1))],layout_matrix =  rbind(cbind(1,2),cbind(3,4),cbind(NA,5)))
ggsave(sf1_sim_cases, filename = paste0("SF1_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 10, width = 6,  bg = "transparent")
grid.newpage()


# Supplementary Figure 2: Visualizing cumulative cases of infection per scenario
#####

## a) 
sf2_cum <- basic_figplot +
  #geom_line(data=growth_cum_final_summary, aes(x=as.numeric(Re), y= cum_cases_median,group=as.character(imports)),color=col_4[4], alpha=0.5)+
  geom_ribbon(data=growth_cum_final_summary, aes( x=as.numeric(Re), ymin = cum_cases_ll, ymax = cum_cases_ul,group=as.character(imports)), alpha=0.5)+
  geom_hline(yintercept=cum_cases_95CI[1], linetype="dashed", color = col_3[3]) +
  geom_hline(yintercept=cum_cases_95CI[3], linetype="dashed", color = col_3[3]) +
  scale_y_continuous(labels=yscaling, trans = 'log10') +
  scale_color_manual(values=col_3[3], name="Expected cases") +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1.2, by = 0.1))+
  theme(legend.position = "none")+
  labs(tag=bquote(.("a)")), x = bquote(italic("R"["e"])), y =bquote("Cumulative cases"))+
  facet_wrap(~ as.character(imports))

## b) 

sf2_final<- basic_figplot +
  #geom_line(data=growth_cum_final_summary, aes(x=as.numeric(Re), y= final_size_median,group=as.character(imports),color=col_4[4]), alpha=0.5)+
  geom_ribbon(data=growth_cum_final_summary, aes( x=as.numeric(Re), ymin = final_size_ll, ymax = final_size_ul,group=as.character(imports)), alpha=0.5)+
  geom_hline(yintercept=final_size_95CI[1], linetype="dashed", color = col_3[3]) +
  geom_hline(yintercept=final_size_95CI[3], linetype="dashed", color = col_3[3]) +
  scale_color_manual(values=col_3[3], name="Expected cases") +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1.2, by = 0.1))+
  theme(legend.position = "none")+
  labs(tag=bquote(.("b)")), x = bquote(italic("R"["e"])), y =bquote("Final size"))+
  facet_wrap(~ as.character(imports))

## c) 
sf2_growthrate <- basic_figplot +
  #geom_line(data=growth_cum_final_summary, aes(x=as.numeric(Re), y= growth_r_median,group=as.character(imports),color=col_4[4]), alpha=0.5)+
  geom_ribbon(data=growth_cum_final_summary, aes(x=as.numeric(Re), ymin = growth_r_ll, ymax = growth_r_ul,group=as.character(imports)), alpha=0.5)+
  geom_hline(yintercept=r_all$rate_weigth - qnorm(0.975)*r_all$error_weigth, linetype="dashed", color = col_4[4]) +
  geom_hline(yintercept=r_all$rate_weigth + qnorm(0.975)*r_all$error_weigth, linetype="dashed", color = col_4[4]) +
  geom_hline(yintercept=0,  color =  "grey") +
  scale_y_continuous(limits = c(-0.1, 0.1))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1.2, by = 0.1))+
  labs(tag=bquote(.("c)")),x = bquote(italic("R"["e"])), y =bquote("Growth rate" ~ italic("r")))+
  facet_wrap(~ as.character(imports))


plot_sf2 <- grid.arrange(
  grobs = list(sf2_cum, sf2_final,sf2_growthrate),
  layout_matrix = rbind(1,2,3))

ggsave(plot_sf2, filename = paste0("SF2_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 10, width = 4,  bg = "transparent")
grid.newpage()

#####

# Supplementary Figure 3:
#plot reported cases including regarding their most likely place of infection and their age category
grid.newpage()
country_level <- table(BAG_data_su2020$country)[order(table(BAG_data_su2020$country),decreasing=TRUE)]
BAG_data_su2020$country <-  factor(BAG_data_su2020$country, levels = c("Switzerland", names(country_level)[!names(country_level) %in% c("Switzerland","Others","Unknown")],"Others","Unknown"))

for(i in 1:2){
  p_import <-list()
  for(c in unname(levels(BAG_data_su2020$country))){
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
      scale_fill_manual(values =col_3)+
      labs(x = "", y ="",subtitle =c)+
      
      theme(plot.margin = margin(8, 2, 2, 2, "mm"),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = "transparent"), # bg of the plot
            axis.text.x = element_text(size = 5),#angle = -20),
            axis.text.y = element_text(size = 5),
            plot.subtitle  = element_text(size = 8),
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
  scale_fill_manual(values =col_3, name="Age categories")
legend_sup3  <- g_legend(legend,1)


p_import[[3]] <- p_import[[3]]+labs(y = "Number of reported cases",subtitle =levels(BAG_data_su2020$country)[3])
p_import_proportion[[3]] <- p_import_proportion[[3]]+labs(y = "Fraction age categories of reported cases per day",subtitle =levels(BAG_data_su2020$country)[3])

plot_countries <- grid.arrange(grobs = p_import,layout_matrix =  matrix(1:25,5,5))
plot_countries_proportion <- grid.arrange(grobs = p_import_proportion,layout_matrix =  matrix(1:25,5,5))

plot_countries <- grobTree(plot_countries,textGrob(bquote("a)"), x = 0.02, y = 0.98))
plot_countries_proportion <-grobTree(plot_countries_proportion,textGrob(bquote("b)"), x = 0.02, y = 0.98))
plot_countries <- grid.arrange(plot_countries,plot_countries_proportion,layout_matrix = matrix(1:2,2,1))
#plot age difference by most likely place of infection
age_percountry<- ggplot(BAG_data_su2020, aes(x=country, y=age)) +
  geom_boxplot(aes(y=age),fill="transparent",color= col_4[1],size=0.5)+
  geom_text(data=stattest_imports, aes(x=country, y=-4, label=ttest_pvalue), col='black', size=1)+
  geom_hline(yintercept=mean(BAG_data_su2020$age[BAG_data_su2020$country=="Switzerland"]),color=col_4[2])+
  stat_summary(fun=mean, geom="point", shape=3, size=3, color=col_4[3]) +
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
age_percountry <-grobTree(age_percountry,textGrob(bquote("c)"), x = 0.02, y = 0.98))
plot_countries_age <- grid.arrange(plot_countries,legend_sup3,age_percountry,layout_matrix =  rbind(c(1,1,1,1),
                                                                                                    c(1,1,1,1),
                                                                                                    c(1,1,1,1),
                                                                                                    c(1,1,1,1),
                                                                                                    c(3,3,3,2)))
ggsave(plot_countries_age, filename = paste0("SF4_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 15, width = 6,  bg = "transparent")
#####

# Supplementary Figure 4: Regional differences in cross-border associated cases

cantons <- basic_figplot+
  geom_histogram(data=BAG_data_su2020, aes(canton, fill=country_cat), stat="count",position = "stack") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_3) +
  labs(tag="",subtitle = "Cantons", x =bquote(""), y =bquote(""))
cantons1 <-basic_figplot+
  geom_histogram(data=BAG_data_su2020, aes(canton, fill=country_cat), stat="count",position = "fill") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_3) +
  labs(tag="",subtitle = "", x =bquote(""), y =bquote(""),fill="")
regions <-basic_figplot+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust=1,vjust = 0.2,angle=90))+
  geom_histogram(data=BAG_data_su2020, aes(regions, fill=country_cat), stat="count",position = "stack") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_3) +
  labs(tag="",subtitle = "Regions of CH", x =bquote(""), y =bquote(""),fill="")
regions1 <-basic_figplot+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust=1,vjust = 0.2,angle=90))+
  geom_histogram(data=BAG_data_su2020, aes(regions, fill=country_cat), stat="count",position = "fill") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_3) +
  labs(tag="",subtitle = "", x =bquote(""), y =bquote(""),fill="")
airport <-basic_figplot+
  theme_minimal()+
  geom_histogram(data=BAG_data_su2020, aes(canton_inter_airport, fill=country_cat), stat="count",position = "stack") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_3) +
  labs(tag="",subtitle = "International airports in CH", x =bquote(""), y =bquote(""),fill="")
airport1 <- basic_figplot+
  theme_minimal()+
  geom_histogram(data=BAG_data_su2020, aes(canton_inter_airport, fill=country_cat), stat="count",position = "fill") +#scale_fill_discrete(name="")+
  theme(legend.position = "none")+
  scale_fill_manual(values=col_3) +
  labs(tag="",subtitle  = "",x =bquote(""), y =bquote(""),fill="")
legend <- basic_figplot+
  geom_histogram(data=BAG_data_su2020, aes(canton_inter_airport, fill=country_cat), stat="count",position = "fill") +#scale_fill_discrete(name="")+
  scale_fill_manual(name="Exposure",values=col_3)
legend  <- g_legend(legend,1)

regions_exposure <- grid.arrange(cantons, cantons1, regions, regions1, airport,airport1,legend,layout_matrix =  rbind(c(1,1,1,2,2,2,7),c(3,3,NA,4,4,NA,NA), c(5,NA, NA,6,NA,NA,NA)))
ggsave(regions_exposure, filename = paste0("SF4_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), height = 10, width = 15,  bg = "transparent")





