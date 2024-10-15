# Script information ------------------------------------------------------

# run TAF analysis for Anchovy in ICES Subdivision 9a South.

#Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024


# Load packages -----------------------------------------------------------

library(icesTAF)
library(rmarkdown)

# clean the TAF directories (all except bootstrap/initial):
#clean()

# Run the TAF analysis ----------------------------------------------------

# run step by step

# sourceTAF("bootstrap")
# sourceTAF("data")
# sourceTAF("model")
# sourceTAF("output") 
# sourceTAF("report")
library(icesTAF)
library(rmarkdown)
boot<-"boot/initial/data/run/" 
list1<-list.files(boot)
list1
esc<- "S1.0_InitCond_sigmaR" #(S3)  
write(esc, file = paste0(boot,"Esc.txt"))
sourceTAF("bootstrap")
sourceTAF("data")
sourceTAF("model_01_run")
sourceTAF("output_01_run")
sourceTAF("report_01_run")

sourceTAF("model_02_retro")
sourceTAF("output_02_retro")
sourceTAF("report_02_retro")

sourceTAF("model_03_brp")
sourceTAF("model_04_stf")

# run reporte.Rmd 
mkdir("Reports")
esc<-"S1.0_InitCond_sigmaR"
  
render("Report_model_ane27.9a.Rmd", 
         output_file = paste0("Report_rmd/Report_model_ane27.9a",esc,".pdf"))

# render("Report_BRP_STF_ane27.9a.Rmd", 
#        output_file = paste0("Report_rmd/Report_BRP_STF_ane27.9a",esc,".pdf"))
# 
# render("Report_Further_analysis_ane27.9a.Rmd", 
#        output_file = paste0("Report_rmd/Report_Further_analysis_ane27.9a",esc,".pdf"))

#sourceAll()


# End of script -----------------------------------------------------------



