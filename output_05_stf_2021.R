
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

#esc_R<-"GeomRecl_Blim_Bloss" # use geomean / virgin rec
esc_R<-"GeomRecl_Blim_spf" # use geomean / virgin rec
#esc_R<-"SR_Blim_Bloss" # use SR BH


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
             SSB_2022 = as.numeric(SSB[SSB$SpawnBio.Yr==2022,paste0('SpawnBio.replist',seq(1,num.scen))]),
             SSB_2022_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2022,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
             SSB_2023 = as.numeric(SSB[SSB$SpawnBio.Yr==2023,paste0('SpawnBio.replist',seq(1,num.scen))]),
             SSB_2023_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2023,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
             F_2022 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2022,paste0('Fvalue.replist',seq(1,num.scen))]),
             F_2023 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2023,paste0('Fvalue.replist',seq(1,num.scen))]),
             Rec_2022 = as.numeric(Recr[Recr$recruits.Yr==2022,paste0('recruits.replist',seq(1,num.scen))]),
             Rec_2023 = as.numeric(Recr[Recr$recruits.Yr==2023,paste0('recruits.replist',seq(1,num.scen))]),
             Catch_2022 = NA, 
             Catch_2023 = NA)

#probablility of being below Blim assuming pNormal
#Blim<-PBRs$Blim[2] #Blim=Bloss 
Blim<-PBRs$Blim[3] #Bloss=Bpa_spf
dfSTFSummary$pBlim_2022 <- round(pnorm(Blim, dfSTFSummary$SSB_2022, dfSTFSummary$SSB_2022_SD),3)
dfSTFSummary$pBlim_2023 <- round(pnorm(Blim, dfSTFSummary$SSB_2023, dfSTFSummary$SSB_2023_SD),3)

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
  dfSTFSummary$Catch_2022[i] <- catch$Catch_2022
  dfSTFSummary$Catch_2023[i] <- catch$Catch_2023
}

dfSTFSummary


save(dfSTFSummary, file=paste0(output_stf,"/STFSummary.Rdata"))
# 
write.csv(dfSTFSummary, paste0(output_stf,"/STFSummary.csv"))


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
                   c(paste0("Fsq","*",round(all.scen[6:9],1),
                            c("\np(SSB2022<Blim)=5%","\np(SSB2022<Blim)=50%",
                              "\np(SSB2023<Blim)=5%","\np(SSB2023<Blim)=50%"))))

# Crear el gráfico con todas las variables y facet_wrap para separarlas
fig1<-ggplot2::ggplot(subset(final_combined_data, Yr < 2024), aes(x = Yr, y = value, color = variable)) +
  geom_point()+
  geom_line() + 
  # Agregar línea negra desde 1989 hasta 2023 para el primer escenario de SSB
  geom_line(data = subset(final_combined_data, Yr <= 2021 & variable == "replist1"), 
            aes(x = Yr, y = value), color = "black", ) + 
  geom_point(data = subset(final_combined_data, Yr <= 2021 & variable == "replist1"), 
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
ggsave(file.path(paste0(output_stf,"/fig_forecast.png")), fig1,  width=7, height=4)

#Table ----
dfSTFSummary$FMult<-dfSTFSummary$Val
dfSTFSummary<-dfSTFSummary %>% mutate(esc=c(paste0("Fsq * ",round(dfSTFSummary$FMult[1:5],1)),
                                            c(paste0("Fsq * ",round(dfSTFSummary$FMult[6:9],1),
                                                     c("\np(SSB2022<Blim)=5%","\np(SSB2022<Blim)=50%",
                                                       "\np(SSB2023<Blim)=5%","\np(SSB2023<Blim)=50%")))))

table <- dfSTFSummary[, c("esc", "Catch_2022","SSB_2022", "SSB_2023", "pBlim_2022", "pBlim_2023")]

# Redondear las columnas numéricas (excepto 'esc')
table[, -1] <- round(table[, -1], 3)
names(table)<-c("esc", "Catch2022","SSB2022", "SSB2023", "p(SSB2023<Blim)", "p(SSB2023<Blim)")
write.csv(table, paste0(output_stf,"/tb_STF.csv"), row.names = FALSE)
##################################################################################33
# Table 2
dfSTFSummary$Rec_2022<-dfSTFSummary$Rec_2022
dfSTFSummary$Rec_2023<-dfSTFSummary$Rec_2023
dfSTFSummary2<-dfSTFSummary %>% mutate(FMult=c(paste0("Fsq",round(dfSTFSummary$F_2022[2],1),"*",round(dfSTFSummary$FMult[1:5],1)),
                                              c(paste0("Fsq",round(dfSTFSummary$F_2022[2],1),"*",round(dfSTFSummary$FMult[6:9],1),
                                                       c("\np(SSB2022<Blim)=5%","\np(SSB2022<Blim)=50%",
                                                         "\np(SSB2023<Blim)=5%","\np(SSB2023<Blim)=50%")))))
#dfSTFSummary<-dfSTFSummary[-7,]
# Crear la tabla sin intentar redondear la columna 'esc'
table2 <- dfSTFSummary2[, c("FMult", "F_2022", "Rec_2022")]

# Redondear las columnas numéricas (excepto 'esc')
table2[, -1] <- round(table2[, -1], 3)
write.csv(table2, paste0(output_stf,"/tb_STF_FMult.csv"), row.names = FALSE)
