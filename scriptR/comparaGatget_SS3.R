# Comparación SS3 y Gadget

# Script information ------------------------------------------------------


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

stdreptlist<-data.frame(output$derived_quants[,1:3])
head(stdreptlist)
head(summary)

# Define the range of years to include
start_year <- 1989
end_year <- 2023

# Define a function to process data
process_data <- function(datavb, pattern, value_col, stddev_col = NULL) {
  # Ensure the required column exists
  if (!"Label" %in% colnames(datavb)) {
    stop("The 'Label' column is missing in the data frame.")
  }
  
  filtered_data <- datavb %>%
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
process_summary_data <- function(datavb, pattern, value_col) {
  if (!"V1" %in% colnames(datavb)) {
    stop("The 'V1' column is missing in the summary data frame.")
  }
  
  filtered_data <- datavb %>%
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

datavb<-rbind(ssb,recr,ft,bt,catch)
datavb <- datavb %>%
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
agg_Value <- aggregate(Value ~ type, datavb, function(x) c(mean = mean(x), max = max(x), min = min(x)))

agg_Value2 <- agg_Value %>%
  mutate(
    year.max = sapply(type, function(t) datavb$year[datavb$type == t][which.max(datavb$Value[datavb$type == t])]),
    year.min = sapply(type, function(t) datavb$year[datavb$type == t][which.min(datavb$Value[datavb$type == t])])
  )

# Convertir a un formato de data frame manejable
agg_Value2 <- do.call(data.frame, agg_Value2)



agg_CV <- aggregate(CV ~ type, datavb, function(x) c(mean = mean(x), max = max(x), min = min(x)))
# Convertir a un formato de data frame manejable
agg_CV <- do.call(data.frame, agg_CV)

# Renombrar las columnas correctamente
names(agg_CV) <- c("type", "Value.mean", "Value.max", "Value.min")
agg_CV$year_max <- sapply(agg_CV$type, function(t) datavb$year[datavb$type == t][which.max(datavb$Value[datavb$type == t])])
agg_CV$year_min <- sapply(agg_CV$type, function(t) datavb$year[datavb$type == t][which.min(datavb$Value[datavb$type == t])])
agg_CV[, c("Value.mean", "Value.max", "Value.min")] <- lapply(agg_CV[, c("Value.mean", "Value.max", "Value.min")], round, 2)




ssb_gadget<-c(4514.385,5217.482,2742.85,2767.587,2787.56,1454.697,4420.85,
              10268.35,7630.03,9645.83,3581.95,4342.34,7452.71,6356.738,
              3467.04,2843.84,2348.776,4437.087,6518.9,3740.63,1449.29,1288.747,
              3840.229,3354.281,4829.93,4714.585,2653.91,3071.825,1769.513,
              1474.611,3426.387,4221.149,3405.62,2996.8,2695.304)

ssb_ss3<-ssb$Value

data_ssb<-data.frame(year=ssb$year,ssb_SS3=ssb$Value,ssb_GADGET=ssb_gadget) %>% melt(id.vars = c("year"))


fig1a<-ggplot(data_ssb,aes(x=year,y=value,color=variable)) + geom_point() + geom_line()+
labs(x = "",y = "SSB",title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "top")
fig1a
ggsave(file.path(paste0(path_rep,"/fig_SS3_vs_GADGET.png")), fig1a,  width=8, height=7)
