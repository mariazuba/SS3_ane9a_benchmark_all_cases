
# Script information ------------------------------------------------------


# load libraries ----------------------------------------------------------
rm(list=ls())
run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 

library(icesTAF)
library(icesAdvice)
library(tidyverse)
library(reshape)
library(ss3diags)

retro_esc<-paste0(getwd(),"/model/retro/")
retro_out<-paste0(getwd(),"/output/retro/")
list.files(retro_esc)


system(paste("rm -r", shQuote(retro_out)))
mkdir(retro_out)

  retro.dir  <- paste0(retro_esc,esc)

retroModels<-SSgetoutput(dirvec=file.path(retro_esc,esc,paste("retro",0:-4,sep="")))
retroSummary <- SSsummarize(retroModels)
# Save output objects -----------------------------------------------------

save(retroModels,retroSummary,
     file=paste0(retro_out,"retrospective_",esc,".RData"))



# End of script -----------------------------------------------------------

rm(list=ls())