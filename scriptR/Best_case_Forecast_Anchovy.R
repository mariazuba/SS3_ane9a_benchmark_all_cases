#+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + 
# 
#   SS3 ENSEMBLE MODEL script                                                                                           
#   Copy and paste forecast scenarios SS files,                                                
#   run all forecast scenarios models at once and plot results                                                                                 
#   December 2021                                                                                                      
#                                                                                                                       
#   Authors:                                                                                                            
#   Francesco Masnadi (CNR-IRBIM & UNIBO, Ancona)                                                                       
#   Massimiliano Cardinale (SLU Aqua, Lysekil)                                                                          
#   mainly based on ss3diags functions developed by Hennig Winker (JRC-EC) and Felipe Carvalho (NOAA)  
#   Definition of Blim e Btrigger at line 170 and 172 of the script
#
#+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + 

#install.packages("kobe", repos="http://flr-project.org/R")

library(r4ss)
library(ss3diags)
library(readr)
library(kobe)
library(plyr)
library(reshape)
library(tidyverse)
library(parallel)
library(doParallel)
#registerDoParallel(10)
cl <- makeCluster(10, type="PSOCK")
registerDoParallel(cl)

#Set Blim
Blim = 9783.28

##
## Some system specific setups (Linux vs Windows)
##
# if (Sys.info()["sysname"] == "Windows") {
#   
#   SS_EXE   <- "C:/Users/mascar/Documents/Max/Stock_synthesis/ss3_3.22/ss3.exe"
#   main.dir <- "~/Max/Commitees/ICES/WKBENCH/2024/Anchovy 9a"
#   MC.CORES <- 3
#   print("mc.cores set to 1, NO PARALLELISATION on Windows :( ")
#   
# } else {
#   
  SS_EXE <- "~/MJZ/Executives_SS/ss3_linux" #"~/Max/Executives_SS/ss_linux"
  #main.dir <- "~/Max/WKBENCH 2023/Central Baltic herring/Ensemble"
#}

#set working directory in which you want to create the runs subfolder:
dir = "~/MJZ/Commitees/ICES/WKBENCH/2024/Anchovy 9a" #"~/Max/Commitees/ICES/WKBENCH/2024/Anchovy 9a"

setwd(dir)

# Subfolder for each model run from 1 to N: 
runs = paste0("Run",1:1)

# Subfolder within TAC levels scenarios for forecast:
tacs = c("FMult0","FMult1","FMult1.2","FMult1.6","FMult2","FMult3.977834","FMult5.93287")

##################################
# ENSEMBLE FORECAST 
#################################
setwd(dir)

dir.forecast <-  paste0(dir,"/Forecast")

kbproj.fr = NULL

runs = paste0("Run",1:1)

# Compile MVLN posteriors by scenario run
for(i in 1:length(runs)){
  # load all scenarios as list  
  #run = SSgetoutput(dirvec=file.path(runs[i],tacs))
  run = SSgetoutput(dirvec=file.path(dir.forecast, runs[i],tacs))
    # get MVLN mvn.temp for each scenario
  for(j in 1:length(tacs)){  
    # this is a super long projection so I decrease mc to 1000 to speed up 
    # Make sure you set correct Fref depending on your choice
    # Make sure you name run = according to TACs
    mvn.temp = SSdeltaMVLN(run[[j]],run=tacs[j],
                           years = run[[j]]$startyr:run[[j]]$endyr+run[[j]]$nforecastyears,
                           addprj = T,
                           mc=5000,
                           weight = 1,
                           Fref = "Btgt",plot=F)
    # build kbproj.fr and add model name [j] for each TAC i 
    kbproj.fr = rbind(kbproj.fr,data.frame(mvn.temp$kb,model=runs[i]))
    # save labels once
    if(i==1 & j ==1) labels = mvn.temp$labels
  } # end j loop
} # end k loop
run$replist1$F_report   #F_report_basis
#### save Ensemble r.data
save(kbproj.fr,file="Ensemble_Forecast_model.rdata")
load("Ensemble_Forecast_model.rdata")

####################################################################
# Probabilistic Forecast table and results (Ftarget, Btarget, Blim)
####################################################################
prob.dir <- paste0(dir,"/Forecast/prob_forecast")
dir.create(path=prob.dir, showWarnings = T, recursive = T)

nTAC = length(tacs)
yrs = 2024:2026 # Projection horizon   

#################################################################################
kbproj.fr$Ftrgtratio <- kbproj.fr$harvest
kbproj.fr$Btrftratio <- kbproj.fr$stock

# adding Blim 
kbproj.fr$Blim <- Blim  
kbproj.fr$Blimratio <- kbproj.fr$SSB/kbproj.fr$Blim   

kbproj.fr$tac <- kbproj.fr$run       # rename to fit the script
#################################################################################

# Table for SSB LIM
# Prepare matrices using library(kobe) 
t.lim = plyr::ddply(kbproj.fr, .(year,tac), with, kobe:::smry(Blimratio, Ftrgtratio)) 

#####################################
# csv forecast table for ICES
#####################################

prj_SBB_SBBtrg = aggregate(stock~year+tac,kbproj.fr,median)%>% dplyr::filter(year %in% yrs)
prj_F_Btrg = aggregate(harvest~year+tac,kbproj.fr,median) %>% dplyr::filter(year %in% yrs)
prj_SSB = aggregate(SSB~year+tac,kbproj.fr,median) %>% dplyr::filter(year %in% yrs)
prj_F = aggregate(F~year+tac,kbproj.fr,median) %>% dplyr::filter(year %in% yrs)
prj_Recr = aggregate(Recr~year+tac,kbproj.fr,median) %>% dplyr::filter(year %in% yrs)
prj_Catch = aggregate(Catch~year+tac,kbproj.fr,median) %>% dplyr::filter(year %in% yrs)

# join the quantities for the forecast yrs
prj_join <- join_all(list(prj_SBB_SBBtrg,prj_F_Btrg,prj_SSB,prj_F,prj_Recr,prj_Catch), type='left') 

# recall and modify the limit quantities table
lim <- t.lim %>% dplyr::filter(year %in% yrs) %>% dplyr::rename("overFished(Blim)" = overFished ) %>% dplyr::rename("underFished(Blim)" = underFished) %>% dplyr::rename(Flim_ratio = harvest)%>% dplyr::rename(SBBlim_ratio = stock) %>% select(year,tac,SBBlim_ratio,"overFished(Blim)","underFished(Blim)")

# final table
tab_join <- join_all(list(prj_join,lim), type='left')%>% dplyr::rename(SBBtarget_ratio = stock) %>% dplyr::rename(Ftarget_ratio = harvest)
tab_join <- tab_join[c(1,2,8,5,6,7,9,10,11)]
write.csv(tab_join , paste0(dir,"/Forecast/Forecast_table_ICES.csv"))

