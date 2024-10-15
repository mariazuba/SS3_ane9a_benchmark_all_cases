

# Script information ------------------------------------------------------
# This script automates the process of running a retrospective analysis for SS3 
# model scenarios. It first loads the necessary libraries and sets the working 
# directory. For each model scenario, the script creates a corresponding directory
# in model/retro, copies the input files from the `model/run` directory, 
# and ensures the SS3 executable is available in the new directory. 
# The script then runs the retrospective analysis over the specified number of 
# years (0 to -5) in each scenario's directory. After the analysis is complete,
# the working directory is restored to its original location.



# run retrospective analysis 
rm(list=ls())
run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 

# Load packages -----------------------------------------------------------
library(r4ss)
library(icesTAF)
library(lubridate)


old_wd <- getwd()
# Run retrospective analysis ----------------------------------------------
# copy files to the retro directory --


run_esc<-paste0(getwd(),"/model/run/") 
retro_esc<-paste0(getwd(),"/model/retro/")

list.files(run_esc)


run.dir<-paste0(run_esc,esc)
retro.dir <- paste0(retro_esc,esc)

# delete  folder named "retro" 
system(paste("rm -r", shQuote(retro.dir)))

mkdir(retro.dir)
copy_SS_inputs(dir.old = run.dir, 
               dir.new =  retro.dir,
               copy_exe = FALSE,
               verbose = FALSE)

cp("boot/software/ss3", retro.dir )
wd <- retro.dir 
system(wd)
system(paste0("chmod 755 ",wd,"/ss3"))
retro(dir = wd, oldsubdir = "", newsubdir = "", 
      years = 0:-4,exe = "ss3")


# End of script -----------------------------------------------------------
setwd(old_wd)


