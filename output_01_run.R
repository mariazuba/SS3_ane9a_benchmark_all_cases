## Extract results of interest, write TAF output tables

## Before:
## After:
rm(list=ls())
boot<-"boot/initial/data/run/"
esc<-readLines(paste0(boot,"Esc.txt")) 
# Script information ------------------------------------------------------
# This script automates the extraction of key results and the generation of
# TAF output tables from SS3 model runs. For each model scenario in the
# `model/run` directory, the script loads the results, generates plots,
# and saves them in the `output/run` directory, while removing any previously 
# generated plot subdirectories. The results and summaries are stored 
# in `.RData` files. 

# Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024/08/30

# Load libraries ----------------------------------------------------------

library(icesTAF)

# Working directory and folders -------------------------------------------

# check working directory

getwd()



# Script information ------------------------------------------------------


# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)
library(ss3diags)

# Setup and read in files -----------------------------------------------------
run_esc<-paste0(getwd(),"/model/run/")
list.files(run_esc)
run_out<-paste0("output/run/",esc)
mkdir(run_out)

# directory with output files
run.dir  <- paste0(run_esc,esc)
# read output SS3
output <- r4ss::SS_output(dir = run.dir,forecast=FALSE)
# summary SS3
summary <- read.table(paste0(run.dir,"/ss_summary.sso"),header=F,sep="",na="NA",fill=T)

# remove directory plots 
dir_to_remove <- paste0("output/run/",esc,"/plots")

# Ejecutar el comando para borrar la carpeta "plots"
system(paste("rm -r", shQuote(dir_to_remove)))

# Create the standard ss3 plots ----
r4ss::SS_plots(replist = output, dir = run_out,
               printfolder = "plots",showpost = FALSE)


R0 <- output$estimated_non_dev_parameters["SR_LN(R0)", "Value"]
convergency<-output$maximum_gradient_component
like<-output$likelihoods_used

run_cpue<-SSplotRunstest(output,subplots = "cpue")
jaba_cpue<-SSplotJABBAres(output,subplots = "cpue")
run_age<-SSplotRunstest(output,subplots = "age")
jaba_age<-SSplotJABBAres(output,subplots = "age")


# Write .RData ----
save(output,  summary, R0,  convergency, like,  run_cpue, jaba_cpue, run_age,jaba_age,
     file=paste0(run_out,"/output.RData"))

