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

esc_R<-"GeomRecl" # use geomean / virgin rec
#rango entre 0-2 y p(SSB2025<Blim)=5%, p(SSB2025<Blim)=50%, p(SSB2024<Blim)=50%
vector0 <- c(0,1, 1*1.2, 1*1.6, 1*2, 1*2.406250,1*3.743750,1*3.790625) # "GeomRecl" # use geomean / virgin rec ["S1.0_InitCond_sigmaR"]
#esc_R<-"SR" # use BH model
#vector0 <- c(0,1, 1*1.2, 1*1.6, 1*2,1*3.78,1*5.931000 ) # vector usando SR=BH steepness=0.8

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
# Reference Points ----
# from load(paste0(path_brp,"/brp.Rdata"))
#'*=============================================================================*
Blim <- Blim
Bpa <-Bpa
Flim<-Flim #3.4522
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
#####------------


#'*============================================================================*
### Fmultipliers with Beverton-Holt ----
#'*============================================================================*
#'*Multipliers that adjust the value of F to achieve an SSB in 2025 equal to SSBlim*
start_value1 <- 7
end_value1 <- 8
vector1 <- seq(from = start_value1, to = end_value1, length.out = 5)

#'*Multipliers that adjust the value of F to achieve F=Flim=3.4522 as reference*3.977150
start_value2 <- 6.479687
end_value2 <- 3.97716
vector2 <- seq(from = start_value2, to = end_value2, length.out = 5)

#'*Multipliers that adjust the value of F to achieve p(SSB2025<Blim)=5%*3.78
start_value3 <- 3.78
end_value3 <- 3.79
vector3 <- seq(from = start_value3, to = end_value3, length.out = 5)


#'*============================================================================*
### Fmultipliers with geomean / virgin rec ----
#'*============================================================================*
#'*Multipliers that adjust the value of F to achieve an SSB in 2025 equal to SSBlim p(SSB2025<Blim)=50%*
start_value4 <- 3.72500
end_value4 <- 3.74375
vector4 <- seq(from = start_value4, to = end_value4, length.out = 5)

start_value4.1 <- 3.78125
end_value4.1 <- 3.8
vector4.1 <- seq(from = start_value4.1, to = end_value4.1, length.out = 5)


#'*Multipliers that adjust the value of F to achieve p(SSB2025<Blim)=5%*
start_value5 <- 2.40000
end_value5 <- 2.41250
vector5 <- seq(from = start_value5, to = end_value5, length.out = 5)

start_value5.1 <- 0
end_value5.1 <- 0.5
vector5.1 <- seq(from = start_value5.1, to = end_value5.1, length.out = 5)

# descarto folder que no sirven
borrar=TRUE
if(borrar==TRUE){
  carpetas <- list.files(path_stf, full.names = TRUE)
  carpetas_FMult <- carpetas[grep("^FMult", basename(carpetas))]
  sapply(carpetas_FMult, unlink, recursive = TRUE)
  cat("Carpetas eliminadas:", carpetas_FMult, "\n")
}
###  Using apical F multipliers are exact for Fsq multipliers 
#'*S1.0_InitCond_sigmaR_SelP_qpriorP*
#vector0 <- c(0,1, 1*1.2, 1*1.6, 1*2,1*3.977150,1*3.78,1*5.931000 ) # vector usando SR=BH steepness=0.8
#vector0 <- c(0,1, 1*1.2, 1*1.6, 1*2,1*3.78,1*5.931000 ) # vector usando SR=BH steepness=0.8
#vector0 <- c(0,1, 1*1.2, 1*1.6, 1*2,1*1.425,1*3.977150,1*2.900100 ) # "GeomRecl" # use geomean / virgin rec
#'*S1.0_4FLEETS_SelECO_RecIndex_Mnewfix* no funcionó!
#vector0 <- c(0,1, 1*1.2, 1*1.6, 1*2,1*6.479687,1*1.90000) # "GeomRecl" # use geomean / virgin rec

FMult <- c(vector0)

FMult_names <- paste0("FMult",FMult)
l_FMult <- length(FMult)

#'*============================================================================*
##### ----- SET GEOMEAN Recruitment (use or not depends on uncertainty)
#'*============================================================================*
# Get assessment outputs for geomean recruitment
year_inter <- endyear+1
ass.sum <- SSsummarize(SSgetoutput(dirvec=run_model))

hist.rec <- as.data.frame(ass.sum$recruits) %>% filter(Yr %in% 2021:(year_inter-1)) %>% .[,1]  #  2021-2023
virg.rec <- as.data.frame(ass.sum$recruits) %>% filter(Label == "Recr_Virgin") %>% .[,1]

gmrec <- exp(mean(log(hist.rec)))


aux=fore_dat_int # F last year

#'*============================================================================*
### CREATE forecast.ss 
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
  if(esc_R=="GeomRecl"){
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
save(forecastModels, file=file.path(paste0(output_stf,"/STF.Rdata")))
#' 
#' forecastSummary <- r4ss::SSsummarize(forecastModels)
#' 
#' # check dataframes in SSB, F, Recr
#' SSB <- as.data.frame(forecastSummary[17])
#' SSB.SD <- as.data.frame(forecastSummary[18])   #SD for pnormal Blim
#' SSB.Lower <- as.data.frame(forecastSummary[19])   #SSB + 1.96SD
#' SSB.Upper <- as.data.frame(forecastSummary[20])   #SSB - 1.96SD
#' Fvalue <- as.data.frame(forecastSummary[30])
#' Recr <- as.data.frame(forecastSummary[38])
#' 
#' 
#' ## --  WRITE ALL IN A DATAFRAME ----
#' all.scen <- c(FMult)
#' num.scen <- length(all.scen)
#' dfSTFSummary <- 
#'   data.frame(Scenario.Type = c(rep("FMult",length(FMult))),
#'              Val = all.scen,
#'              SSB_2024 = as.numeric(SSB[SSB$SpawnBio.Yr==2024,paste0('SpawnBio.replist',seq(1,num.scen))]),
#'              SSB_2024_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2024,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
#'              SSB_2025 = as.numeric(SSB[SSB$SpawnBio.Yr==2025,paste0('SpawnBio.replist',seq(1,num.scen))]),
#'              SSB_2025_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2025,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
#'              F_2024 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2024,paste0('Fvalue.replist',seq(1,num.scen))]),
#'              F_2025 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2025,paste0('Fvalue.replist',seq(1,num.scen))]),
#'              Rec_2024 = as.numeric(Recr[Recr$recruits.Yr==2024,paste0('recruits.replist',seq(1,num.scen))]),
#'              Rec_2025 = as.numeric(Recr[Recr$recruits.Yr==2025,paste0('recruits.replist',seq(1,num.scen))]),
#'              Catch_2024 = NA, 
#'              Catch_2025 = NA)
#' 
#' #probablility of being below Blim assuming pNormal
#' dfSTFSummary$pBlim_2024 <- round(pnorm(Blim, dfSTFSummary$SSB_2024, dfSTFSummary$SSB_2024_SD),3)
#' dfSTFSummary$pBlim_2025 <- round(pnorm(Blim, dfSTFSummary$SSB_2025, dfSTFSummary$SSB_2025_SD),3)
#' 
#' #catches
#' for (i in 1:num.scen){
#'   output = forecastModels[[i]]
#'   catch <- output$timeseries %>%
#'     filter(Era == "FORE") %>%
#'     select("Yr", starts_with("dead(B)")) %>%
#'     mutate(Total_deadB = `dead(B):_1` + `dead(B):_2` + `dead(B):_3` + `dead(B):_4`) %>%
#'     mutate(FMult = FMult[i]) %>%
#'     select(Yr, Total_deadB, FMult) %>%
#'     group_by(Yr) %>%
#'     summarise(Sum_Total_deadB = sum(Total_deadB))
#'   names(catch) <- c("Year","Catch","FMult")
#'   catch <- catch %>% pivot_wider(names_from = Year, values_from = Catch, names_prefix = "Catch_")
#'   dfSTFSummary$Catch_2024[i] <- catch$Catch_2024
#'   dfSTFSummary$Catch_2025[i] <- catch$Catch_2025
#' }
#' 
#' dfSTFSummary
#' 
#' diferencia0 <- abs(dfSTFSummary$F_2024 - Flim)
#' diferencia1 <- abs(dfSTFSummary$SSB_2025 - Blim)
#' diferencia2 <- abs(dfSTFSummary$pBlim_2025 - 0.05)
#' 
#' diferencia0
#' diferencia1
#' diferencia2
#' save(dfSTFSummary, file=paste0(path_stf,"/STFSummary.Rdata"))
#' # 
#' write.csv(dfSTFSummary, paste0(path_stf,"/STFSummary.csv"))
#' 
#' 
#' # Filtrar los datos entre 1989 y 2026
#' ssb_0 <- subset(SSB, SpawnBio.Yr >= 1989)
#' f_0 <- subset(Fvalue, Fvalue.Yr >= 1989)
#' R_0 <- subset(Recr, recruits.Yr >= 1989)
#' 
#' ssb_2<-reshape2::melt(ssb_0,id.vars = c("SpawnBio.Label",'SpawnBio.Yr'))
#' f_2<-reshape2::melt(f_0,id.vars = c("Fvalue.Label",'Fvalue.Yr'))
#' R_2<-reshape2::melt(R_0,id.vars = c("recruits.Label",'recruits.Yr'))
#' 
#' 
#' #'*==========================================================================*
#' 
#' # Crear una lista para almacenar los resultados
#' catchend <- purrr::map2(forecastModels, FMult, function(output, fmult_value) {
#'   
#'   output$timeseries %>% 
#'     filter(Yr >= 1989) %>%  # Filtrar por el año
#'     select("Yr", starts_with("dead(B)")) %>%  # Seleccionar columnas que comienzan con "dead(B)"
#'     mutate(Total_deadB = `dead(B):_1` + `dead(B):_2` + `dead(B):_3` + `dead(B):_4`) %>%  # Calcular el total de dead(B)
#'     mutate(FMult = fmult_value) %>%  # Agregar la columna FMult con el valor correspondiente
#'     select(Yr, Total_deadB, FMult) %>%  # Seleccionar las columnas importantes
#'     group_by(Yr) %>%  # Agrupar por año
#'     summarise(Sum_Total_deadB = sum(Total_deadB), FMult = first(FMult))  # Sumar Total_deadB y mantener FMult
#' })
#' catchend2<-plyr::ldply(catchend,data.frame)
#' 
#' 
#' ###############################################################################
#' 
#' 
#' # Crear los datos de SSB, Fvalue, y Recruits
#' ssb_2 <- reshape2::melt(ssb_0, id.vars = c("SpawnBio.Label", "SpawnBio.Yr"))
#' f_2 <- reshape2::melt(f_0, id.vars = c("Fvalue.Label", "Fvalue.Yr"))
#' R_2 <- reshape2::melt(R_0, id.vars = c("recruits.Label", "recruits.Yr"))
#' 
#' # Agregar un indicador de la variable para cada dataset
#' ssb_2$variable_group <- "SSB"
#' f_2$variable_group <- "F apical"
#' R_2$variable_group <- "Recruits"
#' 
#' # Renombrar las columnas para tener consistencia
#' names(ssb_2) <- c("Label", "Yr", "variable", "value", "variable_group")
#' names(f_2) <- c("Label", "Yr", "variable", "value", "variable_group")
#' names(R_2) <- c("Label", "Yr", "variable", "value", "variable_group")
#' 
#' # Combinar los tres datasets en uno solo
#' combined_data <- bind_rows(ssb_2, f_2, R_2)
#' 
#' 
#' # Paso 1: renombrar la columna
#' catchend3 <- catchend2 %>%
#'   dplyr::rename(value = Sum_Total_deadB)
#' 
#' # Paso 2: agregar la nueva columna
#' catchend3 <- catchend3 %>%
#'   mutate(variable_group = "Catch",
#'          variable=.id)
#' 
#' # Combinar todo en un solo conjunto de datos
#' final_combined_data <- bind_rows(combined_data, catchend3)
#' final_combined_data$variable <- sub(".*\\.(replist[0-9]+)", "\\1", final_combined_data$variable)
#' 
#' # Definir las etiquetas de la leyenda
#' all.scen <- FMult
#' #legend_labels <- paste0("FMult_", all.scen)
#' 
#' legend_labels <- c(paste0("Fsq",round(dfSTFSummary$F_2024[2],1),"*",round(all.scen[1:5],1)),
#'                           c(paste0("Fsq",round(dfSTFSummary$F_2024[2],1),"*",round(all.scen[6:7],1),
#'                                    c("\np(SSB2025<Blim)=5%","\np(SSB2025<Blim)=50%"))))
#' 
#' # Crear el gráfico con todas las variables y facet_wrap para separarlas
#' fig1<-ggplot2::ggplot(subset(final_combined_data, Yr < 2026), aes(x = Yr, y = value, color = variable)) +
#'   geom_point()+
#'   geom_line() + 
#'   # Agregar línea negra desde 1989 hasta 2023 para el primer escenario de SSB
#'   geom_line(data = subset(final_combined_data, Yr <= 2023 & variable == "replist1"), 
#'             aes(x = Yr, y = value), color = "black", ) + 
#'   geom_point(data = subset(final_combined_data, Yr <= 2023 & variable == "replist1"), 
#'            aes(x = Yr, y = value), color = "black") +
#'   labs(title = "", x = "Año", y = "") +
#'   theme_bw() +
#'   theme(plot.title = element_text(hjust = 0.5),legend.position = "right")+
#'   theme(plot.title = element_text(size =5),
#'         axis.title = element_text(size = 8),
#'         axis.text = element_text(size = 6),
#'         strip.text = element_text(size = 8),
#'         #axis.text.x = element_text(angle = 45, hjust = 1),
#'         legend.title = element_text(size = 10, face = "bold"), 
#'         legend.text = element_text(size = 8))+
#'   theme(legend.title = element_blank()) +
#'   # Usar una escala manual de colores y aplicar las etiquetas personalizadas de la leyenda
#'   scale_color_manual(values = rainbow(length(unique(final_combined_data$variable))), 
#'                      labels = legend_labels) +
#'   facet_wrap(~variable_group, scales = "free_y")  # Facetear por grupo de variables (SSB, Fvalue, Recruits, Catch)
#' fig1
#' ggsave(file.path(paste0(path_stf,"/fig_forecast.png")), fig1,  width=7, height=4)
