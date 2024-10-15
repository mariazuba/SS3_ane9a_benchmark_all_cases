# Script information ------------------------------------------------------

# Title: Stock assessment: Extract results of interest, write TAF output tables

# Before running the script in folder data we have: 
#         folders with the assessment run and the retros
# After running the script in folder model we have: 
#         folders with the corresponding outputs 

# Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024

# Load libraries ----------------------------------------------------------

library(icesTAF)

# Working directory and folders -------------------------------------------

# check working directory

getwd()

# create output folder and sub-folders using the function mkdir from icesTAF

mkdir("output")
mkdir("output/run")
mkdir("output/retro")
# mkdir("output/toFLR")
# mkdir("output/BRP")
# mkdir("output/STF")

# Outputs from stock assessment -------------------------------------------

source("output_01_run.R")

# Outputs from retro ------------------------------------------------------

source("output_02_retro.R")

# Outputs: create FLR objects ---------------------------------------------

#source("output_03_toFLR.R")

# Outputs from BRP (EqSim) -----------------------------------

# Run if needed (takes a long time!)

# source("output_04_brp.R")

# Outputs: create STF objects ---------------------------------------------

#source("output_05_STF.R")


# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

rm(list = ls())

