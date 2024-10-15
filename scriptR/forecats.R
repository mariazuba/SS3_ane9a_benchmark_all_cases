#=============================================================================
# Forecast
#=============================================================================
esc<-"S1.0_4FLEETS_SelECO_RecIndex_Mnewfix"
run.dir  <- paste0("model/run/",esc)
# # read output SS3
output <- r4ss::SS_output(dir = run.dir,forecast=FALSE)

output$natage

output$exploitation

