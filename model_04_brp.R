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
wd <- path_brp_model
system(wd)
system(paste0("chmod 755 ",wd,"/ss3_linux")) 
r4ss::run(dir=wd, exe="ss3_linux", skipfinished=FALSE, show_in_console =T)

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

#'*=============================================================================*
# Reference Points ----
#'*=============================================================================*
Blim <- min_value
Bpa <-exp(1.645*sigma)*Blim

# Reference values for forecast Flim
Bvirgin<-replist$derived_quants["SSB_Virgin","Value"] 
SSBunfished<-replist$derived_quants["SSB_unfished","Value"] 
ratioBtarget<-Blim/Bvirgin
ratioBtarget2<-Blim/SSBunfished
SSB_Btgt<-replist$derived_quants["SSB_Btgt","Value"] #SSB_Btgt=Blim
SPR_Btgt<-replist$derived_quants["SPR_Btgt","Value"] 
Flim0 <- replist$derived_quants["annF_Btgt","Value"] #annF_Btgt=Flim
Flim0


#'*##########################################################################*
#' Hacer ciclo for
fore$Btarget <-ratioBtarget2

r4ss::SS_writeforecast(fore, dir = path_brp_model, 
                       file = "forecast.ss", 
                       overwrite = TRUE, verbose = FALSE)
# run model
wd <- path_brp_model
system(wd)
system(paste0("chmod 755 ",wd,"/ss3_linux")) 
r4ss::run(dir=wd, exe="ss3_linux", skipfinished=FALSE, show_in_console =T)
replist2 <- r4ss::SS_output(dir = path_brp_model)

Blim/replist2$derived_quants["SSB_Btgt","Value"] #SSB_Btgt=Blim
Flim<-replist2$derived_quants["annF_Btgt","Value"] #annF_Btgt=Flim
Flim
#'*##########################################################################*
#'
save(min_value,min_sd,min_year,
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

figbpr<-ggplot(data, aes(x = year, y = Value)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_wrap(.~type, scales = "free", ncol = 1, strip.position = "top",
             labeller = labeller(type = c("SSB" = "SSB", 
                                          "Ft" = "F apical"))) +
  labs(x = "", y = "", title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
  geom_hline(data = hline_data, aes(yintercept = yintercept, color = Line, linetype = Line), size = 1) +
  scale_color_manual(name = "Biological Reference points", values = c("Blim" = "red")) +
  scale_linetype_manual(name = "Biological Reference points", values = c("Blim" = "dashed")) +
  
  theme(legend.position = "top", legend.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
ggsave(file.path(paste0("output/brp/",esc,"/fig_bpr2.png")), figbpr,  width=6, height=4)




