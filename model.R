# Script information ------------------------------------------------------

# Run stock assessment and retrospective 

# Before running the script in folder data we have: 
#         natmort.csv
#         catage.csv
#         catch.csv
#         natage_idx_ibts.csv
#         ntotal_idx_ibts.csv
#         survey_DEPM.csv
#         survey_CPUE.csv
#         waca.csv
#         west.csv
#         fecundity.csv
#         maturity.csv
# After running the script in folder model we have: 
#         folders with the assessment run and the retros

# Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024

# Load libraries ----------------------------------------------------------

library(icesTAF)

# Working directory and folders -------------------------------------------

# check working directory

getwd()

# create model folder and subfolders using the function mkdir from icesTAF

mkdir("model")
mkdir("model/run")
mkdir("model/retro")

# Run script for stock assessment -----------------------------------------

source("model_01_run.R")

# Run script for retro ----------------------------------------------------

source("model_02_retro.R")

# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

rm(list=ls())

