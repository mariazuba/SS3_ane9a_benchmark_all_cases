
# Script information ------------------------------------------------------
# Title: Stock assessment for anchovy 9a South (ICES WKBANSP)
#        1) TAF bootstrap procedure to set up required data and software

# See also: https://github.com/ices-taf/doc/wiki/Creating-a-TAF-analysis 

# Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024

# Load libraries ----------------------------------------------------------
library("icesTAF")  # Load the ICES Transparent Assessment Framework (TAF) package
library("r4ss")     # Load the r4ss package for working with Stock Synthesis (SS3)
# Check the working directory ---------------------------------------------

getwd()

# Create repository and R project -----------------------------------------
# Clone the github repository created by ICES secretariat
# The cloned repository is in D:/GitHUB/ices-taf/2023_hom.27.9a_assessment
# Create an R project in that folder

# Make the skeleton -------------------------------------------------------

# create initial directories and R scripts for a new TAF analysis
# 2023_hom.27.8c9a_assessment    
# ¦--bootstrap   
# ¦   --initial 
# ¦       --data
# ¦--data.R      
# ¦--model.R     
# ¦--output.R    
# °--report.R    

#taf.skeleton()

# Upload initial data -----------------------------------------------------

# Include data in the folder: \bootstrap\initial\data 
# Include all five input files needed to run Stock Synthesis
# It can be done by-hand or copying it using the following command:


# Create a DATA.bib file with metadata about the data used
# The SS3 scenarios metadata
draft.data(
  originator = "WKBANSP",           # Who created the data (e.g., Working Group)
  year = 2024,                      # Year of the data
  title = "SS3 data format",         # Title for the data
  period = "1989-2023",              # Period of the data
  file = TRUE                        # Export the metadata to a .bib file
)

# Bootstrap the data from the boot/data folder
taf.bootstrap()

# Document software and create the software.bib file ----------------------

# use function draft.software to create the software.bib file

# ?draft.software

# document the SS3 model
dir <- "boot/initial/software"       # Define the directory for the software
# Optionally, download the SS3 executable for a specific version
# r4ss::get_ss3_exe("boot/initial/software", version = "v3.30.22.1")

# Draft the software metadata
draft.software(c('boot/initial/software/ss3_linux'), file = TRUE)

# Process the data.bib and software.bib metafiles -------------------------

# the function taf.bootstrap() processes:
#   the data.bib file and creates/copies all the data files into the folder "bootstrap/data"
#   and 
#   the software.bib file and creates/copies the software into the folder bootstrap/software
# ?taf.bootstrap

# apply taf.bootstrap

taf.bootstrap()

# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------
