# Script information ------------------------------------------------------

# Authors: María José Zúñiga

# Date: 2024

# Load libraries --------------------------------------------------------------
rm(list=ls())
wd_origin<-getwd()
library(TAF)
library(r4ss)
library(parallel)
library(doParallel)
library(foreach)
library(tidyverse)

run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 

# input data
model<-"model/run/"
data<-"data/run/"
output<-"output/run/"
report<-"report/run/"
brp<-"output/brp/"
output_stf<-"output/stf/"

#'*############################################################################*
#'*SR GEOMETRIC RECRUITMENT*
#'*############################################################################*
#'*GeomRecl_Blim_Bloss*
# esc_R<-"GeomRecl_Blim_Bloss" 
#'*----------------------------------------------------------------------------*
#'*GeomRecl_Blim_spf*
# esc_R<-"GeomRecl_Blim_spf" 
#'*----------------------------------------------------------------------------*
#'*GeomRecl_Blim_other*
esc_R<-"GeomRecl_Blim_other"
#'*############################################################################*
#'*SR Beverton&Holt*
#'*############################################################################*
#'*SR_Blim_Bloss*
# esc_R<-"SR_Blim_Bloss" 
#'*----------------------------------------------------------------------------*
#'*SR_Blim_spf*
# esc_R<-"SR_Blim_spf" 
#'*----------------------------------------------------------------------------*
#'*SR_Blim_other*
# esc_R<-"SR_Blim_other"
#'*############################################################################*

stf<-paste0("model/stf/",esc_R,"/")

run_model<-paste0(model,esc)
run_data<-paste0(data,esc)
run_out<-paste0(output,esc)
path_rep<-paste0(report,esc)
path_brp<-paste0(brp,esc)
path_stf<-paste0(stf,esc)
output_stf<-paste0(output_stf,esc_R,"/",esc)

mkdir(path_stf)
mkdir(output_stf)

load(paste0(run_out,"/output.RData"))
load(paste0(run_data,"/inputData.RData")) 
load(paste0(path_brp,"/brp.Rdata")) 
# working directory
mkdir(paste0(path_stf,"/Forecast_Files"))

#number of years (-1) over which to average weights, selectivity
Naver = 2  # most recent 3-years
#Create scenarios
file.copy(file.path(paste0(run_model, "/forecast.ss")),
          file.path( paste0(path_stf, "/forecast.ss")))

#read in the forecast file
fore <- r4ss::SS_readforecast(file = file.path(path_stf, "forecast.ss"),verbose = FALSE)

#read in assessment ouput
replist <- output
stdreptlist<-data.frame(replist$derived_quants[,1:3])

# Define the range of years to include
start_year <- dat$dat$styr
end_year <- dat$dat$endyr
nfleet<-dat$dat$Nfleet
#'*=============================================================================*
# Short-term forecast ----
#'*=============================================================================*
# #exploitation
dat <- replist$exploitation
dat <- dat[-c(3,4,5,6)]
 tail(dat)
# 
# #### mean years
Nfor <- fore$Nforecastyrs
 startyear <- max(dat$Yr) - Nfor - Naver
 endyear <- max(dat$Yr) - Nfor
 year_inter <- endyear+1
# 
#'*==========================================================================*
# SET Fsq 
# Options:  i) average of the last Nfor+1 years across seasons and fleets ii) use last year 
#'*==========================================================================*

data <- subset(dat,dat$Yr>=startyear & dat$Yr<=endyear)

# Option i) use mean 4 year in F for STF  by fleet
 Fsqmean <- aggregate(.~Seas,data=data,mean) # i) use mean 4 year in F for STF 
# Option ii) use last year by fleet
 Fsq1 <- data$SEINE_Q1[data$Yr==endyear]   #  ii) use last year
 Fsq2 <- data$SEINE_Q2[data$Yr==endyear] # ii) use last year
 Fsq3 <- data$SEINE_Q3[data$Yr==endyear] #  ii) use last year
 Fsq4 <- data$SEINE_Q4[data$Yr==endyear] #  ii) use last year

## prepare intermediate year data 
dat <- replist$exploitation
# keep year, seas and fleets
dat <- dat %>% select(-Seas_dur, -F_std, -annual_F, -annual_M)
# head(dat) 

## average of the last 3 years across seasons and fleets
startyear <- max(dat$Yr)-Nfor-Naver+1
endyear   <- max(dat$Yr)-Nfor
data_int <- dat %>% filter(Yr>=startyear & Yr<=endyear) %>%
  select(-Yr) %>% group_by(Seas) %>%
  summarise_all(mean)

## input intermediate year data
dimen <- dim(data_int)
Year  <- rep(endyear+1,dimen[1]*(dimen[2]-1))
fore_dat_int       <- data.frame(Year)
fore_dat_int$Seas  <- data_int$Seas
fore_dat_int$Fleet <- 1:nfleet
#fore_dat_int$F     <- c(Fsq1,Fsq2,Fsq3,Fsq4) 
fore_dat_int$F     <- as.vector(as.matrix(Fsqmean[,-which(names(Fsqmean)==c("Seas","Yr"))]))
fore_dat_int<-fore_dat_int[fore_dat_int$F != 0, ]
fore_dat_int


#'*============================================================================*
### Fmultipliers with Beverton-Holt ----
#'*SR Beverton&Holt*
#'*============================================================================*

#'----------------
#'*SR_Blim_Bloss*
#'----------------
# load(paste0("output/brp/",esc,"/brp.Rdata")) 
# Blim_new<-PBRs$Blim[2]

BHBloss_p2024_5<-0
BHBloss_p2024_50<-3.8
BHBloss_p2025_5<-4.09
BHBloss_p2025_50<-5.937

vectorpBH_Bloss<-c(1*BHBloss_p2024_5,
                   1*BHBloss_p2024_50,
                   1*BHBloss_p2025_5,
                   1*BHBloss_p2025_50) 
#'----------------
#'*SR_Blim_spf*
#'----------------
#vector0 <- c(1*4.75,1*12.750,1*10.36, 1*13.6)
#load(paste0("output/brp/",esc,"/brp.Rdata")) 
#Blim_new<-PBRs$Blim[3]

BHspf_p2024_5<-4.75
BHspf_p2024_50<-12.750
BHspf_p2025_5<-10.36
BHspf_p2025_50<-13.6

vectorpBH_spf<-c(1*BHspf_p2024_5,
                   1*BHspf_p2024_50,
                   1*BHspf_p2025_5,
                   1*BHspf_p2025_50) 

#'----------------
#'*SR_Blim_other*
#'----------------
#  vector0 <- c(1*8.3125,1*11.125,1*9.8, 1*1.75) 
#load(paste0("output/brp/",esc,"/brp.Rdata")) 
#Blim_new<-PBRs$Blim[4]

BHother_p2024_5<-1.75
BHother_p2024_50<-9.8
BHother_p2025_5<-8.3125
BHother_p2025_50<-11.125

vectorpBH_other<-c(1*BHother_p2024_5,
                 1*BHother_p2024_50,
                 1*BHother_p2025_5,
                 1*BHother_p2025_50) 


#'----------------
#'*GeomRecl_Blim_Bloss*
#'----------------
#load(paste0("output/brp/",esc,"/brp.Rdata")) 
# Blim_new<-PBRs$Blim[2]
#vector0 <- c(1*2.4025,1*3.74375,1*0,1*3.8) 
GRBloss_p2024_5<-0
GRBloss_p2024_50<-3.8
GRBloss_p2025_5<-2.4025
GRBloss_p2025_50<-3.74375

vectorpGR_Bloss<-c(1*GRBloss_p2024_5,
                   1*GRBloss_p2024_50,
                   1*GRBloss_p2025_5,
                   1*GRBloss_p2025_50) 

#'----------------
#'*GeomRecl_Blim_spf*
#'----------------
 # load(paste0("output/brp/",esc,"/brp.Rdata")) 
 # Blim_new<-PBRs$Blim[3]
# #solo encuentro el valor para p(SSB2024<Blim)=5%
#vector0 <- c(1*9.36,1*12.30000,1*4.75,1*12.7500) 

GRspf_p2024_5<-4.75
GRspf_p2024_50<-12.7500
GRspf_p2025_5<-9.36
GRspf_p2025_50<-12.30000

vectorpGR_spf<-c(1*GRspf_p2024_5,
                 1*GRspf_p2024_50,
                 1*GRspf_p2025_5,
                 1*GRspf_p2025_50) 

#'-----------------------
#'*GeomRecl_Blim_other*
#'-----------------------
 load(paste0("output/brp/",esc,"/brp.Rdata")) 
 Blim_new<-PBRs$Blim[4]

GRother_p2024_5_start<-1.65
GRother_p2024_5_end<-1.7
GRother_p2024_5<-1.7000
#GRother_p2024_5<-seq(from =GRother_p2024_5_start, to = GRother_p2024_5_end, length.out = 5)

GRother_p2024_50_start<-9.7500
GRother_p2024_50_end<-10
GRother_p2024_50<-9.8125 
#GRother_p2024_50<-seq(from =GRother_p2024_50_start, to = GRother_p2024_50_end, length.out = 5)

GRother_p2025_5_start<-6
GRother_p2025_5_end<-7
GRother_p2025_5<-6.7500
#GRother_p2025_5<-seq(from =GRother_p2025_5_start, to = GRother_p2025_5_end, length.out = 5)

GRother_p2025_50_start<-9.2
GRother_p2025_50_end<-9.25
GRother_p2025_50<-9.2500
#GRother_p2025_50<-seq(from =GRother_p2025_50_start, to = GRother_p2025_50_end, length.out = 5)

vectorpGR_other<-c(1*GRother_p2024_5,
                 1*GRother_p2024_50,
                 1*GRother_p2025_5,
                 1*GRother_p2025_50) 

# descarto folder que no sirven
borrar=TRUE
if(borrar==TRUE){
  carpetas <- list.files(path_stf, full.names = TRUE)
  carpetas_FMult <- carpetas[grep("^FMult", basename(carpetas))]
  sapply(carpetas_FMult, unlink, recursive = TRUE)
  cat("Carpetas eliminadas:", carpetas_FMult, "\n")
}
###  Using apical F multipliers are exact for Fsq multipliers 
vectorbase<-c(0,1, 1*1.2, 1*1.6, 1*2)

FMult <- c(vectorbase,vectorpGR_other) 

#FMult <- c(vectorpGR_other) 

FMult_names <- paste0("FMult",FMult)
l_FMult <- length(FMult)

#'*============================================================================*
##### ----- SET GEOMEAN Recruitment (use or not depends on uncertainty) ----
#'*============================================================================*
# Get assessment outputs for geomean recruitment
year_inter <- endyear+1
ass.sum <- SSsummarize(SSgetoutput(dirvec=run_model))

hist.rec <- as.data.frame(ass.sum$recruits) %>% filter(Yr %in% 2021:(year_inter-1)) %>% .[,1]  #  2021-2023
virg.rec <- as.data.frame(ass.sum$recruits) %>% filter(Label == "Recr_Virgin") %>% .[,1]

gmrec <- exp(mean(log(hist.rec)))


aux=fore_dat_int # F last year

#'*============================================================================*
### CREATE forecast.ss ----
#'*============================================================================*
 for (i in 1:l_FMult){
  aux_fore=fore_dat_int
   j<-1
     aux_fore$Year=endyear+j
     aux_fore$F=FMult[i]*aux$F
     fore_dat=aux_fore
     for(j in 2:(Nfor-1)){ 
       aux_fore$Year=endyear+j
       aux_fore$F=FMult[i]*aux$F
     fore_dat=rbind(fore_dat,aux_fore)
   }
  j=Nfor
  aux_fore$Year=endyear+j
  aux_fore$F=FMult[i]*aux$F
  fore_dat=rbind(fore_dat,aux_fore)
 
  # input ------------------------------------------------------------------------
  fore$InputBasis<-99 # 99 for F, 2 for Catch
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  if(esc_R %in% c("GeomRecl","GeomRecl_Blim_spf","GeomRecl_Blim_Bloss","GeomRecl_Blim_other")){
  fore$fcast_rec_option <- 2 #= value*(virgin recruitment) # comment lines to use model BH # how to replace last year assessment!?
  fore$fcast_rec_val <- gmrec/virg.rec # geomean / virgin rec # comment lines to use model BH
  } else {
    fore$fcast_rec_option <- 0 # use the model's recruitment (BH or others)
    fore$fcast_rec_val <- 1 # no additional adjustments to recruitment
  }
  
  
  ## write all forecast files/scenarios
  r4ss::SS_writeforecast(fore, dir = file.path(path_stf,"Forecast_Files"), 
                         file = paste0("Forecast",FMult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}

###### ------- change starter.ss! line 31 -2 # max yr for sdreport outputs (-1 for endyr+1; -2 for endyr+Nforecastyrs) 
# create forecast folder and subfolders
for (i in 1:l_FMult){
  
  dir.FMult <- file.path(path_stf,paste0("FMult",FMult[i]))
  dir.create(path = dir.FMult, showWarnings = F, recursive = T)
  
  file.copy(file.path(run_model, "starter.ss"), file.path(dir.FMult, "starter.ss"))
  file.copy(paste(run_model, "control.SS", sep="/"), paste(dir.FMult, "control.SS", sep="/"))
  file.copy(paste(run_model, "data.SS", sep="/"), paste(dir.FMult, "data.SS", sep="/"))	
  file.copy(paste(run_model, "ss3_linux", sep="/"), paste(dir.FMult, "ss3_linux", sep="/"))
  file.copy(paste(run_model, "wtatage.ss", sep="/"), paste(dir.FMult, "wtatage.ss", sep="/"))
  
  #copy the forecast file
  file.copy(paste(path_stf,"Forecast_Files",paste0("ForecastFMult",FMult[i],".ss") , sep="/"),
            paste(dir.FMult, "forecast.ss", sep="/"))
}


### -- run the forecast models
all.models <- c(paste0("FMult", FMult))

for (m in all.models){
wd <- path_stf
system(wd)
system(paste0("chmod 755 ",wd,"/ss3_linux")) 
r4ss::run(dir=file.path(path_stf,m), exe="ss3_linux", skipfinished=FALSE, show_in_console =T)
}

#retrieve and summarise outputs ----

forecastModels <- r4ss::SSgetoutput(dirvec = file.path(path_stf,c(FMult_names)), getcovar = FALSE)
save( esc_R,Blim_new,FMult, forecastModels, file = file.path(paste0(output_stf, "/STF.Rdata")))

