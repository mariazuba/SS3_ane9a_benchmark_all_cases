# Script information ------------------------------------------------------

# run retrospective analysis anchovy 9a South (ICES WKBANSP)

# Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024

# Load packages -----------------------------------------------------------


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

# Run retrospective analysis ----------------------------------------------

# copy files to the retro directory --

copy_SS_inputs(dir.old = run.dir, 
               dir.new =  retro.dir,
               copy_exe = FALSE,
               verbose = FALSE)

# copy ss3 to the retro directory --
cp("boot/software/ss3_linux", retro.dir )

# do retros from the current year to -4 --
# this creates and runs all the retros

wd <- retro.dir 
system(wd)
system(paste0("chmod 755 ",wd,"/ss3_linux"))
retro(dir = wd, oldsubdir = "", newsubdir = "", 
      years = 0:-4,exe = "ss3_linux")


# End of script -----------------------------------------------------------
setwd(old_wd)


