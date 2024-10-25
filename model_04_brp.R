#'*=============================================================================*
# Biological Reference Points ----
#'*=============================================================================*
# Script information ------------------------------------------------------

# Authors: María José Zúñiga

# Date: 2024
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
model<-"model/run/"
data<-"data/run/"
output<-"output/run/"
report<-"report/run/"

brp_model<-"model/brp/"
brp_output<-"output/brp/"

run_model<-paste0(model,esc)
run_data<-paste0(data,esc)
run_out<-paste0(output,esc)
path_rep<-paste0(report,esc)

path_brp_model<-paste0(brp_model,esc)
path_brp_output<-paste0(brp_output,esc)

mkdir(path_brp_model)
mkdir(path_brp_output)

load(paste0(run_data,"/inputData.RData")) 


file.copy(file.path(run_model, "starter.ss"), file.path(path_brp_model, "starter.ss"))
file.copy(paste(run_model, "control.SS", sep="/"), paste(path_brp_model, "control.SS", sep="/"))
file.copy(paste(run_model, "data.SS", sep="/"), paste(path_brp_model, "data.SS", sep="/"))	
file.copy(paste(run_model, "ss3_linux", sep="/"), paste(path_brp_model, "ss3_linux", sep="/"))
file.copy(paste(run_model, "wtatage.ss", sep="/"), paste(path_brp_model, "wtatage.ss", sep="/"))
file.copy(paste(run_model, "forecast.ss", sep="/"), paste(path_brp_model, "forecast.ss", sep="/"))

# run model
# wd <- path_brp_model
# system(wd)
# system(paste0("chmod 755 ",wd,"/ss3_linux")) 
# r4ss::run(dir=wd, exe="ss3_linux", skipfinished=FALSE, show_in_console =T)

#read forecast
fore <- r4ss::SS_readforecast(file = file.path(path_brp_model, "forecast.ss"),verbose = FALSE)
#read in assessment ouput
replist <- r4ss::SS_output(dir = path_brp_model)
stdreptlist<-data.frame(replist$derived_quants[,1:3])

# Define the range of years to include
start_year <- dat$dat$styr
end_year <- dat$dat$endyr

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

# Apply the function to each dataset
ssb <- process_data(stdreptlist, "SSB", "Value", "StdDev")
ssb$type<-"SSB"
recr <- process_data(stdreptlist, "Recr", "Value", "StdDev")
recr$type<-"Rt"
ft <- process_data(stdreptlist, "F", "Value", "StdDev")
ft$type<-"Ft"

# Find the minimum value and the corresponding year within the filtered range.
min_value <- min(ssb$Value)
min_sd <-ssb$StdDev[which.min(ssb$Value)]
min_year <- ssb$year[which.min(ssb$Value)]
last_year<-end_year
last_value<-ssb$Value[ssb$year==last_year]
last_sd<-ssb$StdDev[ssb$year==last_year]
sigma<-sqrt(log(1+(last_sd/last_value)^2)) # sigma  is the estimated standard deviation of ln(SSB) in the last year of the assessment, accounting for the uncertainty in SSB for the terminal year. 
sigma_spf<-0.3
sigma_other<-0.2
#'*=============================================================================*
# Reference Points ----
#'*=============================================================================*
#'# Reference values for forecast Flim
h<-0.8
Rvirgin<-replist$derived_quants["Recr_Virgin","Value"] 
Bvirgin<-replist$derived_quants["SSB_Virgin","Value"] 
SSBunfished<-replist$derived_quants["SSB_unfished","Value"] 



# calcular Blim
Bloss<- min_value
Blim <- Bloss
Blim_B0<-0.2*SSBunfished
Bpa       <-Bloss*exp(-1.645*sigma)
Bpa_spf   <-Bloss*exp(-1.645*sigma_spf)
Bpa_other <-Bloss*exp(-1.645*sigma_other)

Blim_Bpa<-Bpa
Blim_spf<-Bpa_spf
Blim_other<-Bpa_other


#---------------------------------------
#Cálculos Andres para evaluar si Blim está bien estimado
# 0.4B0 es muy alto para Blim, se propone alternativa, considerar Blim=Bpa
ratioBlim<-Blim/Bvirgin #no lo uso
ratio0.2B0<-0.2
ratioBloss_B0<-Blim/SSBunfished
ratioBpa_B0<-Bpa/SSBunfished
ratioBpa_B0_spf<-Bpa_spf/SSBunfished
ratioBpa_B0_other<-Bpa_other/SSBunfished
#---------------------------------------
SSB_Btgt<-replist$derived_quants["SSB_Btgt","Value"] #SSB_Btgt=Blim
SPR_Btgt<-replist$derived_quants["SPR_Btgt","Value"] 
Flim <- replist$derived_quants["annF_Btgt","Value"] #annF_Btgt=Flim
Flim


PBRs<-data.frame(Basis=c("Without SigmaSSB","Without SigmaSSB","Default SigmaSSB SPF","Default SigmaSSB other Fishes", "SigmaSSB in assessment"),
                 SigmaSSB=round(c(NA,NA,sigma_spf,sigma_other,sigma),2),
                 "exp(-1.645*SigmaB"=round(c(NA,NA,exp(-1.645*sigma_spf),exp(-1.645*sigma_other),exp(-1.645*sigma)),2),
                 Basis_Bloss=c("0.2*B0","Blim=Bloss",rep("Bloss=Bpa",3)),
                 Fraction_B0=round(c(ratio0.2B0,ratioBloss_B0,ratioBpa_B0_spf,ratioBpa_B0_other,ratioBpa_B0),2),
                 Blim=round(c(Blim_B0,Blim,Bpa_spf,Bpa_other,Bpa),0))

write.csv(PBRs, paste0(path_brp_output,"/Scenarios.csv"), row.names = FALSE)


escPbrs<-data.frame(Technical_basis_Blim=c("0.2*B0",
                                      "Bloss",
                                      "Bpa*exp(-1.654*sigmaB)",
                                      "Bpa*exp(-1.654*sigmaB)",
                                      "Bpa*exp(-1.654*sigmaB)"),
                    sigmaB=c("","",0.3,0.2,0.1),
                    Blim=round(c(Blim_B0,Blim,Bpa_spf,Bpa_other,Bpa),0),
                    Fraction_B0=round(c(ratio0.2B0,ratioBloss_B0,ratioBpa_B0_spf,ratioBpa_B0_other,ratioBpa_B0),2))
escPbrs
write.csv(escPbrs, paste0(path_brp_output,"/Scenarios2.csv"), row.names = FALSE)
#'*#######################################################################*
#' Hacer ciclo for
# fore$Btarget <-ratioBloss_B0
# 
# r4ss::SS_writeforecast(fore, dir = path_brp_model, 
#                        file = "forecast.ss", 
#                        overwrite = TRUE, verbose = FALSE)
# # run model
# wd <- path_brp_model
# system(wd)
# system(paste0("chmod 755 ",wd,"/ss3_linux")) 
# r4ss::run(dir=wd, exe="ss3_linux", skipfinished=FALSE, show_in_console =T)
# replist2 <- r4ss::SS_output(dir = path_brp_model)
# 
# Blim/replist2$derived_quants["SSB_Btgt","Value"] #SSB_Btgt=Blim
# Flim<-replist2$derived_quants["annF_Btgt","Value"] #annF_Btgt=Flim
# Flim
#'*##########################################################################*
B0<-round(SSBunfished,0)
ratioBloss_B0<-round(ratioBloss_B0,2)
sigma<-round(sigma,2)
save(ratioBloss_B0, B0,PBRs,min_value,min_sd,min_year,
     last_year,last_value,last_sd,
     sigma,Blim,Bpa, 
     ssb,ft,Flim,
     file=paste0("output/brp/",esc,"/brp.Rdata"))

load(paste0("output/brp/",esc,"/brp.Rdata"))
# Plot biological reference points ----
#data<-rbind(ssb,ft)
data<-rbind(ssb)
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

hline_data <- data.frame(
  yintercept = c(Blim, Bpa),
  type = c("SSB", "SSB"),
  Line = c("Blim", "Bpa")
)

hline_data2 <- data.frame(
  yintercept = c(Blim_B0, Blim, Bpa_spf, Bpa_other, Bpa),
  type = rep("SSB", 5),
  Line = c("0.2*B0", 
           "Bloss", 
           "Bpa*exp(-1.654*sigma0.3)", 
           "Bpa*exp(-1.654*sigma0.2)", 
           "Bpa*exp(-1.654*sigma0.1)")
)

figbpr2 <- ggplot(data, aes(x = year, y = Value)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_wrap(.~type, scales = "free", ncol = 1, strip.position = "top",
             labeller = labeller(type = c("SSB" = "SSB", 
                                          "Ft" = "F apical"))) +
  labs(x = "", y = "", title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
  
  # Adding horizontal lines for biological reference points
  geom_hline(data = hline_data2, aes(yintercept = yintercept, color = Line, linetype = Line), size = 1) +
  
  # Combined scale_color_manual and scale_linetype_manual for all lines
  scale_color_manual(name = "", 
                     values = c("0.2*B0" = "red", 
                                "Bloss" = "blue",
                                "Bpa*exp(-1.654*sigma0.3)" = "green", 
                                "Bpa*exp(-1.654*sigma0.2)" = "orange", 
                                "Bpa*exp(-1.654*sigma0.1)" = "purple")) +
  scale_linetype_manual(name = "", 
                        values = c("0.2*B0" = "dashed", 
                                   "Bloss" = "dashed", 
                                   "Bpa*exp(-1.654*sigma0.3)" = "solid", 
                                   "Bpa*exp(-1.654*sigma0.2)" = "solid", 
                                   "Bpa*exp(-1.654*sigma0.1)" = "solid")) +
  
  # Adjusting the legend and theme
  theme(legend.position = "right", 
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 8)) +
  theme(plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
figbpr2
ggsave(file.path(paste0("output/brp/",esc,"/fig_bpr1.png")), figbpr2,  width=7, height=3)

# Plot report
hline_data <- data.frame(
  yintercept = c(Blim, Bpa),
  type = c("SSB", "SSB"),
  Line = c("Blim", "Bpa")
)

figbpr <- ggplot(data, aes(x = year, y = Value)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_wrap(.~type, scales = "free", ncol = 1, strip.position = "top",
             labeller = labeller(type = c("SSB" = "SSB", 
                                          "Ft" = "F apical"))) +
  labs(x = "", y = "", title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
  
  # Adding horizontal lines for biological reference points
  geom_hline(data = hline_data, aes(yintercept = yintercept, color = Line, linetype = Line), size = 1) +
  
  # Combined scale_color_manual and scale_linetype_manual for all lines
  scale_color_manual(name = "Biological Reference points", 
                     values = c("Blim" = "red")) +
  scale_linetype_manual(name = "Biological Reference points", 
                        values = c("Blim" = "dashed")) +
  
  # Adjusting the legend and theme
  theme(legend.position = "top", 
        legend.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
figbpr
ggsave(file.path(paste0("output/brp/",esc,"/fig_bpr2.png")), figbpr,  width=6, height=4)



sigmaB <- read.csv(paste0(path_rep,"/timeseries.csv"))
meansigmaB<-mean(sigmaB$CV_SSB)

cvssb<-ggplot(sigmaB,aes(x=year,y=CV_SSB))+geom_point()+
  labs(x = "years", y = "CV SSB", title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")+
  geom_hline(aes(yintercept = meansigmaB, color = "main(cv)=0.18", linetype = "main(cv)=0.18"), size = 1)+
  scale_color_manual(name = "", 
                     values = c("main(cv)=0.18" = "red")) +
  scale_linetype_manual(name = "", 
                        values = c("main(cv)=0.18" = "solid")) 

ggsave(file.path(paste0("output/brp/",esc,"/fig_cvssb.png")), cvssb,  width=6, height=4)

