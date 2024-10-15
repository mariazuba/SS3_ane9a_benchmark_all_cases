## Prepare plots and tables for report

## Before:
## After:

# Script information ------------------------------------------------------
# This script automates the preparation of plots and tables for a report based 
# on the results of SS3 model runs. For each model scenario, the script loads 
# input and output data, generates a series of plots including temporal data 
# coverage, growth curves, catches, age compositions, residuals, and more. 
# Additionally, the script creates summary tables using `flextable` for estimated 
# parameters, time series, and other key diagnostics. The generated plots and 
# tables are saved in the `report/run` directory corresponding to each scenario.

rm(list=ls())
run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 

# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ss3diags)
library(flextable)
library(reshape2)

# working directory
wd <- getwd()
# Load data ---------------------------------------------------------------

# input data
data<-"data/run/"
output<-"output/run/"
report<-"report/run/"

list.files(output)

run_data<-paste0(data,esc)
run_out<-paste0(output,esc)
path_rep<-paste0(report,esc)

mkdir(path_rep)
#dir.create("nueva_carpeta")

load(paste0(run_out,"/output.RData"))
load(paste0(run_data,"/inputData.RData")) 



# Figures --------------------------------------------
## Temporal coverage of input data ----
  png(file.path(paste0(path_rep,"/fig_input_data.png")),width=9,height=6,res=300,units='in')
  sspar(mfrow = c(1, 1), plot.cex = 0.8)
  SSplotData(output, subplots = 2,cex.main = 0.8,cex = 1,margins = c(2.1, 2.1, 1.1, 8.1))
  dev.off()


## wt at age ----
  west[west==0]<-NA
  watage_mid<-west  %>% melt(id.vars=c("year","seas"))
  
  fig7<-watage_mid %>% ggplot(aes(x=year,y=value,colour=variable)) +
    geom_point() + geom_line()+
    facet_wrap(.~seas,ncol=2,as.table = TRUE, strip.position = "top",
               labeller = labeller(seas = c("1" = "Q1", 
                                            "2" = "Q2",
                                            "3" = "Q3", 
                                            "4" = "Q4")))+
    labs(x="Year",y="Weight mean (Kg)")+
    scale_color_discrete(name  ="Age")+
    theme(panel.background = element_rect(fill ="gray80")) +
    theme(panel.grid=element_line(color=NA)) +
    ggtitle('')+
    theme(plot.title = element_text(size =5),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          strip.text = element_text(size = 6),
          panel.background = element_rect(colour="gray",fill = "gray99"),
          strip.background = element_rect(colour = "gray", fill = "gray99"),
          legend.title = element_text(size = 6, face = "bold"), 
          legend.text = element_text(size = 6)) + 
    theme(legend.position = 'top') 
  ggsave(file.path(paste0(path_rep,"/fig_weight_by_quarters.png")), fig7,  width=5, height=5)

  
## Catches by quarters
fig1b<- ggplot(catch, aes(x = year, y = catch,fill=factor(seas))) +
  geom_bar(stat = "identity") +
  labs(x = "Year",y = "Catches (ton)",title = "",fill = "Quarters" ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "top")

ggsave(file.path(paste0(path_rep,"/fig_catches.png")), fig1b,  width=8, height=5)

  
## Fit data: Abundance indices ----
  png(file.path(paste0(path_rep,"/fig_indices_fit.png")),width=6,height=6,res=300,units='in')
  sspar(mfrow = c(4, 2), plot.cex = 0.6)
  SSplotIndices(output, subplots = c(2,3),mainTitle = T)
  dev.off()

 CPUEs<- dat$dat$CPUE
fig_xa<-ggplot(CPUEs, aes(x = year, y = obs,color=factor(index))) +
    geom_point() + geom_line()+
    labs(x = "Year",y = "Index (tonnes)",title = "",color = "Indices" ) + 
    scale_color_discrete(name = "Surveys",
                         labels = c("PELAGO","ECOCADIZ","BOCADEVA","ECOCADIZ-RECLUTAS"))+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),legend.position = "right")+
    theme(plot.title = element_text(size =5),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          strip.text = element_text(size = 8),
          panel.background = element_rect(colour="gray",fill = "gray99"),
          strip.background = element_rect(colour = "gray", fill = "gray99"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(size = 10, face = "bold"), 
          legend.text = element_text(size = 8))+
  scale_x_continuous(breaks = seq(min(CPUEs$year), max(CPUEs$year), by = 2)) 
  ggsave(file.path(paste0(path_rep,"/fig_indiceBiomass.png")), fig_xa,  width=8, height=5)
  
## age composition Seine ----
catageQ1[catageQ1==0]<-NA
catageQ2[catageQ2==0]<-NA
catageQ3[catageQ3==0]<-NA
catageQ4[catageQ4==0]<-NA

catage<-rbind(catageQ1,catageQ2,catageQ3,catageQ4)

agecomp<-  catage%>% 
           melt(id.vars=c("year","month")) %>% 
           mutate(variable = factor(variable, levels = c("a0","a1", "a2", "a3"),
                           labels = c("0","1", "2", "3")))

figxx<- agecomp %>% ggplot(aes(x=year,y=value,fill=variable)) +
  geom_bar(stat = "identity") + 
    facet_wrap(.~month,ncol=2,as.table = TRUE, strip.position = "top",
               labeller = labeller(month = c("3" = "Q1", 
                                            "6" = "Q2",
                                            "9" = "Q3", 
                                            "12" = "Q4")))+
    labs(x="Year",y="Proportion",fill="Age")+
    scale_color_discrete(name  ="Age")+
    theme(panel.background = element_rect(fill ="gray80")) +
    theme(panel.grid=element_line(color=NA)) +
    ggtitle('')+
    theme(plot.title = element_text(size =12),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          strip.text = element_text(size = 6),
          panel.background = element_rect(colour="gray",fill = "gray99"),
          strip.background = element_rect(colour = "gray", fill = "gray99")) + 
    theme(legend.position = 'top') 
ggsave(file.path(paste0(path_rep,"/fig_agecomp_by_quartersSeine.png")), figxx,  width=7, height=5)
  
# Age composition surveys ----

agecompSurvey <-  dat$dat$agecomp  %>% filter(fleet>=5) %>% 
  select(c(year,month,fleet,`a0`,`a1`,`a2`,`a3`)) %>% 
  melt(id.vars=c("year","month","fleet")) %>% 
  mutate(variable = factor(variable, 
                           levels = c("a0","a1", "a2", "a3"),
                           labels = c("0","1", "2", "3")))

figx1<- agecompSurvey %>% ggplot(aes(x=year,y=value,fill=variable)) +
  geom_bar(stat = "identity") + 
  facet_wrap(.~fleet,ncol=1,as.table = TRUE, strip.position = "top",
             labeller = labeller(fleet = c("5" = "PELAGO", 
                                          "6" = "ECOCADIZ",
                                          "8" = "ECOCADIZ-RECLUTAS"))) +
  labs(x="Year",y="Proportion",fill="Age")+
  scale_color_discrete(name  ="Age")+
  theme(panel.background = element_rect(fill ="gray80")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('')+
  theme(plot.title = element_text(size =12),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(colour="gray",fill = "gray99"),
        strip.background = element_rect(colour = "gray", fill = "gray99")) + 
  theme(legend.position = 'top') 
ggsave(file.path(paste0(path_rep,"/fig_agecomp_by_quartersSurveys.png")), figx1,  width=5, height=5)

  
## Fit data: Age composition (aggregated) ----
  png(file.path(paste0(path_rep,"/fig_age_fit_agg.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(21),kind = "AGE",maxrows = 3,maxcols = 3,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()


## Fit data: Age composition by source data ----
### *FLEET by quarters* ----
  png(file.path(paste0(path_rep,"/fig_age_fit_SeineQ1.png")),width=10,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 1,maxrows = 6,maxcols =6,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  png(file.path(paste0(path_rep,"/fig_age_fit_SeineQ2.png")),width=10,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 2,maxrows = 6,maxcols =6,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  png(file.path(paste0(path_rep,"/fig_age_fit_SeineQ3.png")),width=10,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 3,maxrows = 6,maxcols =6,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  png(file.path(paste0(path_rep,"/fig_age_fit_SeineQ4.png")),width=10,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 4,maxrows = 6,maxcols =6,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
### historical mean length
  png(file.path(paste0(path_rep,"/fig_meanage_fit_SeineQ1.png")),width=6,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(9),kind = "AGE",fleets = 1,maxrows = 6,maxcols =6,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  png(file.path(paste0(path_rep,"/fig_meanage_fit_SeineQ2.png")),width=6,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(9),kind = "AGE",fleets = 2,maxrows = 6,maxcols =6,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  png(file.path(paste0(path_rep,"/fig_meanage_fit_SeineQ3.png")),width=6,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(9),kind = "AGE",fleets = 3,maxrows = 6,maxcols =6,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  png(file.path(paste0(path_rep,"/fig_meanage_fit_SeineQ4.png")),width=6,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(9),kind = "AGE",fleets = 4,maxrows = 6,maxcols =6,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  
  
file.copy(from=paste0(run_out,"/plots/comp_agefit_data_weighting_TA1-8_SEINE.png"),
            to=paste0(path_rep,"/fig_comp_agefit_SEINE.png"), 
            overwrite=T)

### *PELAGO spring survey* ----
  png(file.path(paste0(path_rep,"/fig_age_fit_Pelago.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 5,maxrows = 4,maxcols = 3,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()

  
### historical mean length
  png(file.path(paste0(path_rep,"/fig_meanage_fit_Pelago.png")),width=6,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(9),kind = "AGE",fleets = 5,maxrows = 6,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  
  file.copy(from=paste0(run_out,"/plots/comp_agefit_data_weighting_TA1-8_PELAGO.png"),
            to=paste0(path_rep,"/fig_comp_agefit_PELAGO.png"), 
            overwrite=T)
  
### *ECOCADIZ summer survey* ----
  png(file.path(paste0(path_rep,"/fig_age_fit_Ecocadiz.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 6,maxrows = 4,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()

  
### historical mean length
  png(file.path(paste0(path_rep,"/fig_meanage_fit_Ecocadiz.png")),width=6,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(9),kind = "AGE",fleets = 6,maxrows = 4,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  file.copy(from=paste0(run_out,"/plots/comp_agefit_data_weighting_TA1-8_ECOCADIZ.png"),
            to=paste0(path_rep,"/fig_comp_agefit_ECOCADIZ.png"), 
            overwrite=T)
  
### *ECOCADIZ-RECLUTAS fall survey* ----

  png(file.path(paste0(path_rep,"/fig_age_fit_EcocadizRecl.png")),width=8,height=9,res=300,units='in')
  SSplotComps(output, subplots = c(1),kind = "AGE",fleets = 8,maxrows = 3,maxcols = 3,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()

### historical mean length
  
  png(file.path(paste0(path_rep,"/fig_meanage_fit_EcocadizRecl.png")),width=6,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(9),kind = "AGE",fleets = 8,maxrows = 4,maxcols = 4,
              showsampsize = F,showeffN = F,mainTitle = T)
  dev.off()
  
  
  file.copy(from=paste0(run_out,"/plots/comp_agefit_data_weighting_TA1-8_ECORECLUTAS.png"),
            to=paste0(path_rep,"/fig_comp_agefit_ECORECLUTAS.png"), 
            overwrite=T)
  
  
## Residuals length composition by source data

### *FLEET by quarters* ----
  png(file.path(paste0(path_rep,"/fig_age_residuals_SeineQ1.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 1,maxrows = 12,maxcols = 5,
              cexZ1 = 1.5,yupper=5,
              cohortlines=T, showsampsize = F,showeffN = F)
  dev.off()

  png(file.path(paste0(path_rep,"/fig_age_residuals_SeineQ2.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 2,maxrows = 12,maxcols = 5,
              cexZ1 = 1.5,yupper=5,
              cohortlines=T, showsampsize = F,showeffN = F)
  dev.off()
  
  png(file.path(paste0(path_rep,"/fig_age_residuals_SeineQ3.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 3,maxrows = 12,maxcols = 5,
              cexZ1 = 1.5,yupper=5,
              cohortlines=T, showsampsize = F,showeffN = F)
  dev.off()
  
  png(file.path(paste0(path_rep,"/fig_age_residuals_SeineQ4.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 4,maxrows = 12,maxcols = 5,
              cexZ1 = 1.5,yupper=5,
              cohortlines=T, showsampsize = F,showeffN = F)
  dev.off()
### *PELAGO spring survey* ----
  png(file.path(paste0(path_rep,"/fig_age_residuals_Pelago.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets =5,maxrows = 12,maxcols = 5,cexZ1 = 1.5,
              showsampsize = F,showeffN = F)
  dev.off()

### *ECOCADIZ summer survey* ----
  png(file.path(paste0(path_rep,"/fig_age_residuals_Ecocadiz.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 6,maxrows = 12,maxcols = 5,cexZ1 = 1.5,
              showsampsize = F,showeffN = F)
  dev.off()

### *ECOCADIZ-RECLUTAS fall survey* ----
  png(file.path(paste0(path_rep,"/fig_age_residuals_EcocadizRecl.png")),width=7,height=3,res=300,units='in')
  SSplotComps(output, subplots = c(24),kind = "AGE",fleets = 8,maxrows = 12,maxcols = 5,cexZ1 = 1.5,
              showsampsize = F,showeffN = F)
  dev.off()

  # file.copy(from=paste0(run_out,"/plots/comp_agefit__multi-fleet_comparison.png"),
  #           to=paste0(path_rep,"/fig_comp_agefit_multi-fleet_comparison.png"), 
  #           overwrite=T)

## Run test indices ----
  png(file.path(paste0(path_rep,"/fig_runtest_residuals_indices.png")),width=7,height=7,res=300,units='in')
  sspar(mfrow = c(2, 2), plot.cex = 0.8)
  SSplotRunstest(output,subplots = "cpue", add = TRUE, legendcex = 0.8,verbose = F)
  dev.off()

  
  SSplotJABBAres(output,subplots = "cpue", add = TRUE, legendcex = 0.8,verbose = F)
  
## Run test length ----
  png(file.path(paste0(path_rep,"/fig_runtest_residuals_age.png")),width=7,height=7,res=300,units='in')
  sspar(mfrow = c(4, 2), plot.cex = 0.8)
  SSplotRunstest(output,subplots = "age", add = TRUE, legendcex = 0.8,verbose = F)
  dev.off()

  SSplotJABBAres(output,subplots = "age", add = TRUE, legendcex = 0.8,verbose = F)
  
  
## Selectivity ----
  # if (esc !="S1") {
  # png(file.path(paste0(path_rep,"/fig_age_selectivity_var.png")),width=4,height=4,res=300,units='in')
  #  SSplotSelex(output,subplots=11)
  #  dev.off()
  # }
  
  png(file.path(paste0(path_rep,"/fig_age_selectivity.png")),width=4,height=4,res=300,units='in')
  sspar(mfrow = c(1, 1), plot.cex = 0.8)
  SSplotSelex(output,subplots=2,mainTitle = FALSE)
  dev.off()
  
  sel<-subset(output$ageselex[output$ageselex$Factor=="Asel2" & output$ageselex$Yr %in% c(output$startyr:(output$endyr)),c("Yr","Fleet","Seas","0","1","2","3") ])
  sel<-sel %>% reshape::melt(id.vars=c('Yr','Fleet','Seas'))
  selfleet<-sel%>% filter(Fleet<=4)
  
  fig_self<-selfleet %>% ggplot(aes(x=as.numeric(variable),y=value,group=Fleet)) +
    geom_point() + geom_line()+
    facet_wrap(.~Fleet,ncol=2,as.table = TRUE, strip.position = "top",
               labeller = labeller(Fleet = c("1" = "SEINE_Q1", 
                                            "2" = "SEINE_Q2",
                                            "3" = "SEINE_Q3", 
                                            "4" = "SEINE_Q4")))+
    labs(x="Year",y="Selectivity")+
    scale_color_discrete(name  ="Age")+
    theme(panel.background = element_rect(fill ="gray80")) +
    theme(panel.grid=element_line(color=NA)) +
    ggtitle('Commercial fleet')+
    theme(plot.title = element_text(size =9),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          strip.text = element_text(size = 6),
          panel.background = element_rect(colour="gray",fill = "gray99"),
          strip.background = element_rect(colour = "gray", fill = "gray99"),
          legend.title = element_text(size = 6, face = "bold"), 
          legend.text = element_text(size = 6)) + 
    theme(legend.position = 'top') 
  ggsave(file.path(paste0(path_rep,"/fig_Sel_commercial_fleet.png")),   fig_self,  width=5, height=5)
  
  
  selsurvey<-sel%>% filter(Fleet==c(5,6,8))
  fig_selS<-selsurvey %>% ggplot(aes(x=as.numeric(variable),y=value,group=Fleet)) +
    geom_point() + geom_line()+
    facet_wrap(.~Fleet,ncol=2,as.table = TRUE, strip.position = "top",
               labeller = labeller(Fleet = c("5" = "PELAGO", 
                                             "6" = "ECOCADIZ",
                                             "8" = "ECOCADIZ-RECLUTAS")))+
    labs(x="Year",y="Selectivity")+
    scale_color_discrete(name  ="Age")+
    theme(panel.background = element_rect(fill ="gray80")) +
    theme(panel.grid=element_line(color=NA)) +
    ggtitle('Acoustic surveys')+
    theme(plot.title = element_text(size =9),
          axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          strip.text = element_text(size = 6),
          panel.background = element_rect(colour="gray",fill = "gray99"),
          strip.background = element_rect(colour = "gray", fill = "gray99"),
          legend.title = element_text(size = 6, face = "bold"), 
          legend.text = element_text(size = 6)) + 
    theme(legend.position = 'top') 
  ggsave(file.path(paste0(path_rep,"/fig_Sel_surveys.png")),   fig_selS,  width=5, height=5)
  
  
   # sspar(mfrow = c(6, 3), plot.cex = 0.8)
  # SSplotPars(output)
  # dev.off()
## Stock-Recluta ----
  png(file.path(paste0(path_rep,"/fig_stock-recluta.png")),width=4,height=4,res=300,units='in')
  sspar(mfrow = c(1, 1), plot.cex = 0.6)
  SSplotSpawnrecruit(output,subplots =2,pwidth = 4,pheight = 4,legendloc ="bottomright")
  dev.off()

  png(file.path(paste0(path_rep,"/fig_stock-recluta_0.png")),width=3,height=3,res=300,units='in')
  sspar(mfrow = c(1, 1), plot.cex = 0.6)
  SSplotSpawnrecruit(output,subplots =2,pwidth = 4,pheight = 4,
                     legendloc ="bottomright",bias_adjusted = F,legend = F,
                     virg = F,
                     init = F,
                     expected = F,
                     estimated = T)
  dev.off()
  
##  Recruitment devs ----
  png(file.path(paste0(path_rep,"/fig_Recdevs.png")),width=5,height=5,res=300,units='in')
  sspar(mfrow = c(1, 1), plot.cex = 0.8)
  SSplotRecdevs(output,subplots = 2,pwidth = 5,pheight = 5)
  dev.off()

   file.copy(from=paste0(run_out,"/plots/recdevs3_varcheck.png"),
             to=paste0(path_rep,"/fig_recdevs2_varcheck.png"), 
            overwrite=T)
  
   file.copy(from=paste0(run_out,"/plots/recruit_fit_bias_adjust.png"),
             to=paste0(path_rep,"/fig_recruit_fit_bias_adjust.png"), 
             overwrite=T)
   
## Catchability ----
  
  Calc_Q<-output$cpue %>% select(c("Yr","Fleet_name","Vuln_bio","Obs","Exp","Calc_Q"))
  Calc_Q<-Calc_Q %>% 
    mutate(Fleet_name=fct_reorder(Fleet_name,Calc_Q))
  
  fig_q<-Calc_Q %>% ggplot(aes(x=Fleet_name,y=Calc_Q))+
    geom_point()+
    labs(x="Surveys",y="Catchability",title="")+
    theme_bw()+
    theme(plot.title=element_text(hjust=0.5),legend.position="top")
  ggsave(file.path(paste0(path_rep,"/fig_catchability.png")), fig_q,  width=4, height=4)
  
  # natural mortality
  head(output$Natural_Mortality)
  
  #SSplotSPR(output)
  #SSplotCatch(output)
  #SSplotNumbers(output)
  
  ## time series ----
  stdreptlist<-data.frame(output$derived_quants[,1:3])
  head(stdreptlist)
  head(summary)
  
  # Define the range of years to include
  start_year <- 1989
  end_year <- 2023
  
  # Define a function to process data
  process_data <- function(data, pattern, value_col, stddev_col = NULL) {
    # Ensure the required column exists
    if (!"Label" %in% colnames(data)) {
      stop("The 'Label' column is missing in the data frame.")
    }
    
    filtered_data <- data %>%
      filter(grepl(pattern, Label)) %>%
      mutate(year = as.numeric(sub(paste0(pattern, "_"), "", Label))) %>%
      filter(!is.na(year) & year >= start_year & year <= end_year) %>%
      select(year, Value, StdDev)
    
    # Remove StdDev column if not needed
    if (is.null(stddev_col)) {
      filtered_data <- filtered_data %>% select(-StdDev)
    }
    
    return(filtered_data)
  }
  
  # Process 'summary' data
  process_summary_data <- function(data, pattern, value_col) {
    if (!"V1" %in% colnames(data)) {
      stop("The 'V1' column is missing in the summary data frame.")
    }
    
    filtered_data <- data %>%
      filter(grepl(pattern, V1)) %>%
      mutate(year = as.numeric(sub(paste0(pattern, "_"), "", V1))) %>%
      filter(!is.na(year) & year >= start_year & year <= end_year) %>%
      select(year, value_col)
    
    return(filtered_data)
  }
  
  # Apply the function to each dataset
  ssb <- process_data(stdreptlist, "SSB", "Value", "StdDev")
  ssb$type<-"SSB"
  recr <- process_data(stdreptlist, "Recr", "Value", "StdDev")
  recr$type<-"Rt"
  ft <- process_data(stdreptlist, "F", "Value", "StdDev")
  ft$type<-"Ft"
  bt <- process_summary_data(summary, "TotBio", "V2") %>% mutate(StdDev=NA)
  colnames(bt)<-c("year","Value","StdDev")
  bt$type<-"Bt"
  
  catch <- process_summary_data(summary, "TotCatch", "V2") %>% mutate(StdDev=NA)
  colnames(catch)<-c("year","Value","StdDev")
  catch$type<-"Catch"
  
  data<-rbind(ssb,recr,ft,bt,catch)
  data <- data %>%
    mutate(
      Value = as.numeric(Value),
      StdDev = as.numeric(StdDev)) %>%
    mutate(
      lower = case_when(
        is.na(StdDev) ~ 0,
        TRUE ~ Value - 1.96 * StdDev),
      upper = case_when(
        is.na(StdDev) ~ 0,
        TRUE ~ Value + 1.96 * StdDev),
      CV=case_when(
        is.na(StdDev) ~ 0,
        TRUE ~ StdDev/ Value))
  
  # Calcular la media, el valor máximo y mínimo de 'Value' por 'type'
  agg_Value <- aggregate(Value ~ type, data, function(x) c(mean = mean(x), max = max(x), min = min(x)))
  
  agg_Value2 <- agg_Value %>%
    mutate(
      year.max = sapply(type, function(t) data$year[data$type == t][which.max(data$Value[data$type == t])]),
      year.min = sapply(type, function(t) data$year[data$type == t][which.min(data$Value[data$type == t])])
    )
  
  # Convertir a un formato de data frame manejable
  agg_Value2 <- do.call(data.frame, agg_Value2)
  
  
  
  agg_CV <- aggregate(CV ~ type, data, function(x) c(mean = mean(x), max = max(x), min = min(x)))
  # Convertir a un formato de data frame manejable
  agg_CV <- do.call(data.frame, agg_CV)
  
  # Renombrar las columnas correctamente
  names(agg_CV) <- c("type", "Value.mean", "Value.max", "Value.min")
  agg_CV$year_max <- sapply(agg_CV$type, function(t) data$year[data$type == t][which.max(data$Value[data$type == t])])
  agg_CV$year_min <- sapply(agg_CV$type, function(t) data$year[data$type == t][which.min(data$Value[data$type == t])])
  agg_CV[, c("Value.mean", "Value.max", "Value.min")] <- lapply(agg_CV[, c("Value.mean", "Value.max", "Value.min")], round, 2)

  
 
  
    
fig1a<- ggplot(data, aes(x = year, y = Value)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
        facet_wrap(.~type,scales = "free",ncol=2,strip.position = "top",
                   labeller = labeller(type = c("SSB" = "SSB", 
                                                "Rt" = "Recruits",
                                                "Ft" = "F apical", 
                                                "Catch" = "Catch",
                                                "Bt" = "Biomass")))+
        labs(x = "",y = "",title = "") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),legend.position = "top")
ggsave(file.path(paste0(path_rep,"/fig_time_series.png")), fig1a,  width=8, height=7)
  
# Tamaño de muestra
namesfleet<-c("SEINE_Q1","SEINE_Q2","SEINE_Q3","SEINE_Q4","PELAGO","ECOCADIZ")#output$Age_Comp_Fit_Summary$Fleet
inputnm<-output$Age_Comp_Fit_Summary$mean_Nsamp_in
francisnm<-output$Age_Comp_Fit_Summary$mean_Nsamp_adj

nm_fleet<-data.frame(namesfleet,inputnm,francisnm) %>% melt(id.vars='namesfleet')


ggplot(nm_fleet,aes(x=namesfleet,y=value,color=variable,group=variable))+
  geom_point()+geom_line()+labs(x = "",y = "",title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "top")
#ggsave(file.path(paste0(path_rep,"/fig_time_series.png")), fig1a,  width=8, height=7)


  
# tablas ----
tb_catch <- dat$dat$catch%>% filter(year>-999) %>% 
            select(c(year,seas,catch)) %>% 
            pivot_wider(
            names_from = "seas", 
            values_from = c("catch"))

indices <- dat$dat$CPUE%>% 
           pivot_wider(
           names_from = "index",  
           values_from = c("obs", "se_log","month"))

nsamp <- dat$dat$agecomp %>%
         filter(fleet >= 2) %>% 
         select(year, fleet, Nsamp) %>% 
         pivot_wider(
           names_from = fleet, 
           values_from = Nsamp) %>% 
         rename_with(~c("year", "nm_2", "nm_3", "nm_5"),
                     .cols = c(year, `2`, `3`, `5`)) %>%
         mutate(nm_4=NA)

combined_df <- left_join(indices, nsamp, by = "year")



params <- output$estimated_non_dev_parameters %>%
           rownames_to_column(var = "Parameter")

params_est <- params %>% 
              select(c(Parameter,Value,Phase,Min,Max,Init,Status,Parm_StDev,Gradient,Afterbound))


# 
# run_cpue<-SSplotRunstest(output,subplots = "cpue")
# jaba_cpue<-SSplotJABBAres(output,subplots = "cpue")
# run_age<-SSplotRunstest(output,subplots = "age")
# jaba_age<-SSplotJABBAres(output,subplots = "age")

#diagnostico
#Convergencia Likehood RMSE_indices RMSE_tallas Rho ForcastRho

diags<-data.frame(convergency=convergency,
                  Totallike=like$values[1],
                  RMSE_index=jaba_cpue$RMSE.perc[5],
                  RMSE_age=jaba_age$RMSE.perc[5])


timeseries<-  data %>% select(c(year,Value,CV,type)) %>% 
pivot_wider(
  names_from = "type",  # Esta columna (index) se convertirá en nombres de columnas
  values_from = c("Value","CV")  # Estas columnas llenarán las nuevas columnas
)
timeseries
# Eliminar columnas donde todos los valores de CV sean 0
timeseries_filtered <- timeseries %>% 
  select(-matches("^CV_")) %>% 
  bind_cols(timeseries %>% select(starts_with("CV_")) %>% select_if(~any(. != 0)))

# Reorganizar las columnas para que Value_xx esté al lado de CV_xx
timeseries_final <- timeseries_filtered[, c("year", "Value_SSB", "CV_SSB", 
                                            "Value_Rt", "CV_Rt",
                                            "Value_Ft", "CV_Ft",
                                            "Value_Bt", "Value_Catch")]


#'*=================================================================*
# Flextables ----
## table index by surveys 
indices<-indices %>%
  select(year,obs_5,obs_6,obs_7,obs_8) %>%
  arrange(year) %>%  # Ordenar por la columna 'year'
  mutate(across(c(obs_5,obs_6,obs_7,obs_8), \(x) round(x, 0))) 

ft1<-indices%>%  # Redondear todas las columnas seleccionadas
  flextable()  # Crear la flextable

ft1<-set_header_labels(ft1, 
                         year="year",
                         obs_5="PELAGO",
                         obs_6="ECOCADIZ",
                         obs_7="BOCADEVA",
                         obs_8="ECOCADIZ-RECLUTAS")

ft1 <- add_header_row(ft1, 
                      values = c("", "Acoustic Biomass (ton) by surveys"),
                      colwidths = c(1, 4))

ft1 <- colformat_double(ft1, digits=1, na_str = "")
ft1 <- colformat_num(ft1,big.mark = "", na_str = "")
ft1 <- align(ft1,part = "header", align = "center") 
ft1 <- fontsize(ft1, size = 9, part = "body")
ft1 <- autofit(ft1)
ft1


#'*CV, NM and timing survey*
# ft2<-combined_df %>%
#   select(year,
#          month_5,se_log_5,nm_2,
#          month_6,se_log_6,nm_3,
#          month_7,se_log_7,nm_7,
#          month_8,se_log_8,nm_8) %>%
#   arrange(year)%>%
#   mutate(across(where(is.numeric), ~round(.x, 2))) %>%  # Redondear solo columnas numéricas
#   flextable()

# 
# ft2<-set_header_labels(ft2,
#                        year="year",
#                        seas_2="month",
#                        se_log_2="cv",
#                        nm_2="nm",
#                        seas_3="month",
#                        se_log_3="cv",
#                        nm_3="nm",
#                        seas_4="month",
#                        se_log_4="cv",
#                        nm_4="nm",
#                        seas_5="month",
#                        se_log_5="cv",
#                        nm_5="nm")
# 
# ft2 <- add_header_row(ft2, 
#                       values = c("", "PELAGO","ECOCADIZ","BOCADEVA","ECOCADIZ-RECLUTAS"),
#                       colwidths = c(1,3,3,3,3))
# 
# ft2 <- colformat_double(ft2, digits=1, na_str = "")
# ft2 <- colformat_num(ft2,big.mark = "", na_str = "")
# ft2 <- align(ft2,part = "header", align = "center") 
# ft2 <- autofit(ft2)
# ft2

#'* estimates parameters: tb_params_est*
ft3<-params_est %>%flextable()
 # ft3<-merge_at(ft3,i=2:5, j=1)
 # ft3<-merge_at(ft3,i=6:9, j=1)
 # ft3<-set_header_labels(ft3,cut="")
ft3

#'*tb_natM*
natmort <- tibble::rownames_to_column(natmort, var = "Parameter")
ft4<-natmort %>% flextable()
ft4


#'*tb_maturity*
#'
maturity1<-maturity[1,3:6 , drop = FALSE]
row.names(maturity1)<-"Maturity"
maturity1 <- tibble::rownames_to_column(maturity1, var = "Parameter")

ft5 <- maturity1 %>% 
  setNames(c("Parameter","Age_0", "Age_1", "Age_2", "Age_3")) %>% 
  flextable()
ft5

#'*diagnostic table*
ft6<-diags%>% 
  flextable()
ft6

#'*index residuals: tb_run_cpue*
ft7<-run_cpue%>%
  flextable()
ft7

#'*aggregate index residuals: tb_jabba_cpue*
ft8<-jaba_cpue %>% flextable()
ft8

#'*age residuals: tb_run_age*
ft9<-run_age%>%
  flextable()
ft9

#'*aggregate age residuals: tb_jabba_age*
ft10<-jaba_age %>% flextable()
ft10

#'*time series*
ft11<-timeseries_final %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))%>%
  setNames(c("Year","SSB\nton", "CV\nSSB", "Recruits\nnumber", "CV\nRecruits","F apical\nyear-1", "CV\nF apical","Total Biomass\nton", "Catch\nton")) %>% 
  flextable()
ft11 <- colformat_double(ft11, digits=1, na_str = "")
ft11 <- colformat_num(ft11,big.mark = "", na_str = "")
ft11 <- align(ft11,part = "header", align = "center") 
ft11 <- fontsize(ft11, size = 8, part = "body")
ft11 <- autofit(ft11)
ft11

#'*catches*
ft12<-tb_catch %>%
  arrange(year)%>%
  mutate(across(where(is.numeric), ~round(.x, 0)))%>% 
  setNames(c("Year","Q1", "Q2", "Q3", "Q4")) %>% 
  mutate(Total=Q1+Q2+Q3+Q4)%>% 
  flextable()
ft12 <- add_header_row(ft12, 
                      values = c("", "Catches (ton)"),
                      colwidths = c(1, 5))
ft12 <- colformat_double(ft12, digits=1, na_str = "")
ft12 <- colformat_num(ft12,big.mark = "", na_str = "")
ft12 <- align(ft12,part = "header", align = "center") 
ft12 <- fontsize(ft12, size = 8, part = "body")
ft12 <- autofit(ft12)
ft12


# Input data type, model assumptions and settings for the assessment ----

col1<-c(rep("Data",12),"",
        rep("Structure and assumptions",10),"")

DAT<-c("Catch","Catch-at-age", 
       "Pelago survey",
       "Pelago numbers-at-age",
       "Ecocadiz survey",
       "Ecocadiz numbers-at-age",
       "Ecocadiz-Reclutas survey",
       "Ecocadiz-Reclutas numbers-at-age",
       "Bocadeva survey",
       "Weight at age in the stock in kg",
       "Weight at age in the catch in kg",
       "Maturity at age",
       "Natural mortality",
       "Structure and assumptions",
       "Recruitment",
       "Initial population",
       "Fishery selectivity-at-age",
       "Pelago selectivity-at-age",
       "Ecocadiz selectivity-at-age",
       "Ecocadiz-Reclutas selectivity-at-age",
       "Pelago catchability",
       "Ecocadiz catchability",
       "Bocadeva catchability",
       "Ecocadiz-Reclutas catchability",
       "Log-likelihood function",
       "Minimisation of the likehood",
       "Data weights",
       "Age data weights"
       )

desc_DAT<-c("Catch biomass 1989 to 2023. Combined Spanish and Portuguese fleets",
           "Proportion-at-age 1989 to 2023 [0 to 3]. Spanish fleets",
           "Biomass 1999 to 2023",
           "Proportion-at-age 1999 to 2023 [0 to 3].",
           "Biomass 2004 to 2023",
           "Proportion-at-age 2004 to 2023 [0 to 3].",
           "Spawning stock biomass 2005 to 2023",
           "Biomass 2014 to 2023",
           "Proportion-at-age 2014 to 2023 [0 to 3].",
           "1989 to 2023 [0 to 3]. mean weighted by all sources by quarters",
           "1989 to 2023 [0 to 3]. Same as in weight at age in the stock",
           "Proportion-at-age [0 to 3]. we assume that all individuals with age 1 or higher (B1+)",
           "Age specific [0 to 3]. Estimated at WD Rincón et al xxx",
           "",
           "Ignore steepness, sigmaR=0.6",
           "Starting equilibrium population age structure",
           "Age based. logistic. Fixed",
           "Age based. logistic. Fixed",
           "Age based. logistic. Fixed",
           "Age based. logistic. Fixed",
           "Simple q linear model",
           "Simple q linear model",
           "Simple q linear model",
           "Simple q linear model",
           "",
          "Implemented in phases using standard ADMB process",
          "Catch CV = 0.05, Pelago = 0.30, Ecocadiz = 0.30, Bocadeva = 0.30, Ecocadiz-Reclutas = 0.30",
          "The Francis method T.A.1.8 was selected for length data weighting in catches and surveys data"
          )






tablex<-data.frame(DAT,desc_DAT)


ft13<-tablex%>% 
  flextable()
ft13<-set_header_labels(ft13,DAT="Data",desc_DAT="Years and age range")
#ft13 <- align(ft13,part = "header", align = "center") 
ft13 <- bold(ft13, i = c(14,25), j = 1)
ft13 <- padding(ft13, i=c(14,25), j=1, padding.left = 15)
ft13 <- fontsize(ft13, size = 10, part = "body")
ft13 <- autofit(ft13)
ft13

#'*=================================================================*
# save tables
save_as_image(ft1, path = paste0(path_rep,"/tb_index.png"))
#save_as_image(ft2, path = paste0(path_rep,"/tb_cv_nm.png"))
save_as_image(ft3, path = paste0(path_rep,"/tb_params_est.png"))
save_as_image(ft4, path = paste0(path_rep,"/tb_natM.png"))
save_as_image(ft5, path = paste0(path_rep,"/tb_maturity.png"))
save_as_image(ft6, path = paste0(path_rep,"/tb_diagnostic.png"))

save_as_image(ft7, path = paste0(path_rep,"/tb_run_cpue.png"))
save_as_image(ft8, path = paste0(path_rep,"/tb_jabba_cpue.png"))

save_as_image(ft9, path = paste0(path_rep,"/tb_run_age.png"))
save_as_image(ft10, path = paste0(path_rep,"/tb_jabba_age.png"))

save_as_image(ft11, path = paste0(path_rep,"/tb_timeseries.png"))
save_as_image(ft12, path = paste0(path_rep,"/tb_catches.png"))

save_as_image(ft13, path = paste0(path_rep,"/tb_dat_stru.png"))

#'*=================================================================*
# save Rdata tables
save(ft1,ft3,ft4,ft5,ft6,ft7,ft8,ft9,ft10, file=paste0(path_rep,"/tables_run.RData"))

save(data, agg_Value2 ,agg_CV,file=paste0(path_rep,"/report.RData"))

 selectivityQ1<-subset(output$ageselex[output$ageselex$Fleet==1 & output$ageselex$Factor=="Asel2" & output$ageselex$Yr %in% c(output$startyr:(output$endyr)),c("Yr","Seas","0","1","2","3") ])
 selectivityQ2<-subset(output$ageselex[output$ageselex$Fleet==2 & output$ageselex$Factor=="Asel2" & output$ageselex$Yr %in% c(output$startyr:(output$endyr)),c("Yr","Seas","0","1","2","3") ])
 selectivityQ3<-subset(output$ageselex[output$ageselex$Fleet==3 & output$ageselex$Factor=="Asel2" & output$ageselex$Yr %in% c(output$startyr:(output$endyr)),c("Yr","Seas","0","1","2","3") ])
 selectivityQ4<-subset(output$ageselex[output$ageselex$Fleet==4 & output$ageselex$Factor=="Asel2" & output$ageselex$Yr %in% c(output$startyr:(output$endyr)),c("Yr","Seas","0","1","2","3") ])
 
 sel_4fleet<-rbind(selectivityQ1,selectivityQ2,selectivityQ3,selectivityQ4)
 
 
 
 # colnames(selectivity)[1]<-"year"
# 
# sel<-reshape::melt(selectivity,id.vars='year')
# sel_bar<-rowMeans(selectivity[,3:6])
# 
# startyr <- output$startyr
# endyr <- output$endyr
# aux <- output$derived_quants
# idx <- match(paste("F_",startyr:(endyr-1),sep=""), aux[,1])
# aux <- aux[idx, ]
# 
# F.dat <- data.frame(Year=startyr:(endyr-1),
#                     Value=aux$Value,
#                     CV=aux$StdDev/aux$Value,
#                     Lower=aux$Value-2*aux$StdDev,
#                     Upper=aux$Value+2*aux$StdDev,
#                     param="f apic")
# 
# # Fbar.dat <- data.frame(Year=startyr:(endyr-1),
# #                        Value=aux$Value*sel_bar[-length(sel_bar)],
# #                        CV=aux$StdDev/aux$Value*sel_bar[-length(sel_bar)],
# #                        Lower=(aux$Value*sel_bar[-length(sel_bar)])-2*aux$StdDev,
# #                        Upper=(aux$Value*sel_bar[-length(sel_bar)])+2*aux$StdDev,
# #                        param="f bar")
# 
# aux <- output$derived_quants
# idx <- match(paste("Recr_",startyr:endyr,sep=""), aux[,1])
# aux <- aux[idx, ] 
# rec.dat <- data.frame(Year=startyr:endyr, 
#                       Value=aux$Value,
#                       CV=aux$StdDev/aux$Value,
#                       Lower=aux$Value-2*aux$StdDev,
#                       Upper=aux$Value+2*aux$StdDev,
#                       param="rec")
# 
# aux <- subset(output$timeseries, Era=="TIME",c("Yr","Seas","Bio_smry"))
# idx <- grep("SSB_\\d",output$derived_quants$Label)
# aux$StdDev <- output$derived_quants[idx,"StdDev"]
# cv <- data.frame(cv=output$derived_quants[idx,"StdDev"]/output$derived_quants[idx,"Value"])
# cv$Yr <- output$startyr:output$endyr
# aux <- merge(aux,cv,by="Yr")
# bio.dat <- data.frame(Year=startyr:endyr, 
#                       Value=aux$Bio_smry,
#                       CV=aux$cv,
#                       Lower=aux$Bio_smry-2*aux$StdDev,
#                       Upper=aux$Bio_smry+2*aux$StdDev,
#                       param="bio1plus"
# )
# 
# 
# ggplot(bio.dat, aes(x = Year,y=Value/1000000)) +
#   geom_pointrange(aes(ymin = Lower/1000000, ymax = Upper/1000000),
#                   position = position_dodge(width=.5) ) +
#   scale_shape_manual(values = c(1, 16)) +
#   scale_linetype_manual(values = c(2, 1)) +
#   theme(text = element_text(size = 14),
#         plot.background =	element_rect(colour = NA, fill = NA),
#         axis.text.x=element_text(size=14),
#         axis.text.y=element_text(size=14),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         legend.position = c(0.9,0.875)) +
#   labs(x="Year", y="Biomass 1+, million tonnes",shape="Assessment", linetype="Assessment")
# #ggsave("report/biomass.png",width=12,height = 8)
