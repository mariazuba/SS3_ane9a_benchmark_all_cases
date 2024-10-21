
# Script information ------------------------------------------------------

# Authors: María José Zúñiga

# Date: 2024

# Load libraries --------------------------------------------------------------
#rm(list=ls())
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
run_stf<-"model/stf/"
output_stf<-"output/stf/"

esc_R<-"GeomRecl" # use geomean / virgin rec 
esc_Blim<-"Blim_Bloss"
#rango entre 0-2 y p(SSB2025<Blim)=5%, p(SSB2025<Blim)=50%, p(SSB2024<Blim)=50%
vector0 <- c(0,1, 1*1.2, 1*1.6, 1*2, 1*2.406250,1*3.743750,1*3.790625) # "GeomRecl" # use geomean / virgin rec ["S1.0_InitCond_sigmaR"]
#esc_R<-"SR" # use BH model
#vector0 <- c(0,1, 1*1.2, 1*1.6, 1*2,1*3.78,1*5.931000 ) # vector usando SR=BH steepness=0.8

#stf<-paste0("model/stf/",esc_R,"/")

run_model<-paste0(model,esc)
run_data<-paste0(data,esc)
run_out<-paste0(output,esc)
path_rep<-paste0(report,esc)
path_brp<-paste0(brp,esc)
path_stf<-paste0(run_stf,esc_R,esc)
output_stf<-paste0(output_stf,"/",esc_R,"/",esc)


load(paste0(run_out,"/output.RData"))
load(paste0(run_data,"/inputData.RData")) 
load(paste0(path_brp,"/brp.Rdata")) 
load(paste0(output_stf,"/STF.Rdata")) 

forecastSummary <- r4ss::SSsummarize(forecastModels)

# check dataframes in SSB, F, Recr
SSB <- as.data.frame(forecastSummary[17])
SSB.SD <- as.data.frame(forecastSummary[18])   #SD for pnormal Blim
SSB.Lower <- as.data.frame(forecastSummary[19])   #SSB + 1.96SD
SSB.Upper <- as.data.frame(forecastSummary[20])   #SSB - 1.96SD
Fvalue <- as.data.frame(forecastSummary[30])
Recr <- as.data.frame(forecastSummary[38])


## --  WRITE ALL IN A DATAFRAME ----
all.scen <- c(FMult)
num.scen <- length(all.scen)
dfSTFSummary <- 
  data.frame(Scenario.Type = c(rep("FMult",length(FMult))),
             Val = all.scen,
             SSB_2024 = as.numeric(SSB[SSB$SpawnBio.Yr==2024,paste0('SpawnBio.replist',seq(1,num.scen))]),
             SSB_2024_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2024,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
             SSB_2025 = as.numeric(SSB[SSB$SpawnBio.Yr==2025,paste0('SpawnBio.replist',seq(1,num.scen))]),
             SSB_2025_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2025,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
             F_2024 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2024,paste0('Fvalue.replist',seq(1,num.scen))]),
             F_2025 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2025,paste0('Fvalue.replist',seq(1,num.scen))]),
             Rec_2024 = as.numeric(Recr[Recr$recruits.Yr==2024,paste0('recruits.replist',seq(1,num.scen))]),
             Rec_2025 = as.numeric(Recr[Recr$recruits.Yr==2025,paste0('recruits.replist',seq(1,num.scen))]),
             Catch_2024 = NA, 
             Catch_2025 = NA)

#probablility of being below Blim assuming pNormal
dfSTFSummary$pBlim_2024 <- round(pnorm(Blim, dfSTFSummary$SSB_2024, dfSTFSummary$SSB_2024_SD),3)
dfSTFSummary$pBlim_2025 <- round(pnorm(Blim, dfSTFSummary$SSB_2025, dfSTFSummary$SSB_2025_SD),3)

#catches
for (i in 1:num.scen){
  output = forecastModels[[i]]
  catch <- output$timeseries %>%
    filter(Era == "FORE") %>%
    select("Yr", starts_with("dead(B)")) %>%
    mutate(Total_deadB = `dead(B):_1` + `dead(B):_2` + `dead(B):_3` + `dead(B):_4`) %>%
    mutate(FMult = FMult[i]) %>%
    select(Yr, Total_deadB, FMult) %>%
    group_by(Yr) %>%
    summarise(Sum_Total_deadB = sum(Total_deadB))
  names(catch) <- c("Year","Catch","FMult")
  catch <- catch %>% pivot_wider(names_from = Year, values_from = Catch, names_prefix = "Catch_")
  dfSTFSummary$Catch_2024[i] <- catch$Catch_2024
  dfSTFSummary$Catch_2025[i] <- catch$Catch_2025
}

dfSTFSummary

diferencia0 <- abs(dfSTFSummary$F_2024 - Flim)
diferencia1 <- abs(dfSTFSummary$SSB_2025 - Blim)
diferencia2 <- abs(dfSTFSummary$pBlim_2025 - 0.05)

diferencia0
diferencia1
diferencia2
save(dfSTFSummary, file=paste0(output_stf,"/STFSummary_",esc_Blim,".Rdata"))
# 
write.csv(dfSTFSummary, paste0(output_stf,"/STFSummary_",esc_Blim,".csv"))


# Filtrar los datos entre 1989 y 2026
ssb_0 <- subset(SSB, SpawnBio.Yr >= 1989)
f_0 <- subset(Fvalue, Fvalue.Yr >= 1989)
R_0 <- subset(Recr, recruits.Yr >= 1989)

ssb_2<-reshape2::melt(ssb_0,id.vars = c("SpawnBio.Label",'SpawnBio.Yr'))
f_2<-reshape2::melt(f_0,id.vars = c("Fvalue.Label",'Fvalue.Yr'))
R_2<-reshape2::melt(R_0,id.vars = c("recruits.Label",'recruits.Yr'))


#'*==========================================================================*
# Crear una lista para almacenar los resultados
catchend <- purrr::map2(forecastModels, FMult, function(output, fmult_value) {
  
  output$timeseries %>% 
    filter(Yr >= 1989) %>%  # Filtrar por el año
    select("Yr", starts_with("dead(B)")) %>%  # Seleccionar columnas que comienzan con "dead(B)"
    mutate(Total_deadB = `dead(B):_1` + `dead(B):_2` + `dead(B):_3` + `dead(B):_4`) %>%  # Calcular el total de dead(B)
    mutate(FMult = fmult_value) %>%  # Agregar la columna FMult con el valor correspondiente
    select(Yr, Total_deadB, FMult) %>%  # Seleccionar las columnas importantes
    group_by(Yr) %>%  # Agrupar por año
    summarise(Sum_Total_deadB = sum(Total_deadB), FMult = first(FMult))  # Sumar Total_deadB y mantener FMult
})
catchend2<-plyr::ldply(catchend,data.frame)


###############################################################################
# Crear los datos de SSB, Fvalue, y Recruits
ssb_2 <- reshape2::melt(ssb_0, id.vars = c("SpawnBio.Label", "SpawnBio.Yr"))
f_2 <- reshape2::melt(f_0, id.vars = c("Fvalue.Label", "Fvalue.Yr"))
R_2 <- reshape2::melt(R_0, id.vars = c("recruits.Label", "recruits.Yr"))

# Agregar un indicador de la variable para cada dataset
ssb_2$variable_group <- "SSB"
f_2$variable_group <- "F apical"
R_2$variable_group <- "Recruits"

# Renombrar las columnas para tener consistencia
names(ssb_2) <- c("Label", "Yr", "variable", "value", "variable_group")
names(f_2) <- c("Label", "Yr", "variable", "value", "variable_group")
names(R_2) <- c("Label", "Yr", "variable", "value", "variable_group")

# Combinar los tres datasets en uno solo
combined_data <- bind_rows(ssb_2, f_2, R_2)


# Paso 1: renombrar la columna
catchend3 <- catchend2 %>%
  dplyr::rename(value = Sum_Total_deadB)

# Paso 2: agregar la nueva columna
catchend3 <- catchend3 %>%
  mutate(variable_group = "Catch",
         variable=.id)

# Combinar todo en un solo conjunto de datos
final_combined_data <- bind_rows(combined_data, catchend3)
final_combined_data$variable <- sub(".*\\.(replist[0-9]+)", "\\1", final_combined_data$variable)

# Definir las etiquetas de la leyenda
all.scen <- FMult
#legend_labels <- paste0("FMult_", all.scen)

legend_labels <- c(paste0("Fsq","*",round(all.scen[1:5],1)),
                   c(paste0("Fsq","*",round(all.scen[6:7],1),
                            c("\np(SSB2025<Blim)=5%","\np(SSB2025<Blim)=50%"))))

# Crear el gráfico con todas las variables y facet_wrap para separarlas
fig1<-ggplot2::ggplot(subset(final_combined_data, Yr < 2026), aes(x = Yr, y = value, color = variable)) +
  geom_point()+
  geom_line() + 
  # Agregar línea negra desde 1989 hasta 2023 para el primer escenario de SSB
  geom_line(data = subset(final_combined_data, Yr <= 2023 & variable == "replist1"), 
            aes(x = Yr, y = value), color = "black", ) + 
  geom_point(data = subset(final_combined_data, Yr <= 2023 & variable == "replist1"), 
             aes(x = Yr, y = value), color = "black") +
  labs(title = "", x = "Año", y = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "right")+
  theme(plot.title = element_text(size =5),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8))+
  theme(legend.title = element_blank()) +
  # Usar una escala manual de colores y aplicar las etiquetas personalizadas de la leyenda
  scale_color_manual(values = rainbow(length(unique(final_combined_data$variable))), 
                     labels = legend_labels) +
  facet_wrap(~variable_group, scales = "free_y")  # Facetear por grupo de variables (SSB, Fvalue, Recruits, Catch)
fig1
ggsave(file.path(paste0(output_stf,"/fig_forecast_",esc_Blim,".png")), fig1,  width=7, height=4)
