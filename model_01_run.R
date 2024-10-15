# Script information ------------------------------------------------------

# run stock assessment for for anchovy 9a South (ICES WKBANSP)
# Before running the script in folder data we have: 
run_esc<-"boot/data/run/" 
esc<-readLines(paste0(run_esc,"Esc.txt"))
list.files(paste0("data/run/",esc), full.names = F)

# After running the script in folder model we have: 
#         folders with the assessment run and the retros
list.files(paste0("model/run/",esc), full.names = F)

# Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024

# Script information ------------------------------------------------------

rm(list=ls())
run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 
# libraries ------------------------------------------------------
library(r4ss) 
library(icesTAF)

# directorios ----
old_wd <- getwd()

mkdir("model/run")

path_esc<-"model/run"
path.data<-"boot/data/run" 
list.files(path.data)

#'*------------------------------------------------------------------------------------------*
path   <- file.path(path.data, esc) 
model  <- file.path(path_esc)
cp(path, model)
cp("boot/software/ss3_linux", paste0("model/run/",esc))
wd <- paste0(getwd(),"/model/run/",esc)
system(wd)
system(paste0("chmod 755 ",wd,"/ss3_linux"))
r4ss::run(dir=wd, exe="ss3_linux", skipfinished=FALSE, show_in_console =T)

#'*------------------------------------------------------------------------------------------*

setwd(old_wd)
rm(list=ls())





