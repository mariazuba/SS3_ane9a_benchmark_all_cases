# Script information ------------------------------------------------------

# Title: Prepare tables and figures for the report

# Before running the script in folder output we have: 
#         - XXXX
# After running the script in folder report we have: 
#         - XXXX

# Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024


# Load libraries ----------------------------------------------------------

library(icesTAF)

# Working directory and folders -------------------------------------------

#check working directory

getwd()

# create model folder and subfolders using the function mkdir from icesTAF
boot<-"boot/initial/data/run/" 
esc<-"S1.0_4FLEETS"
write(esc, file = paste0(boot,"Esc.txt"))
mkdir("report")
mkdir("report/retro")
# mkdir("report/BRP")
# mkdir("report/STF")

# Create report figs and tabs ---------------------------------------------

source("report_01_run.R")
source("report_02_retro.R")
# source("report_03_brp.R")
# source("report_04_stf.R")

# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

