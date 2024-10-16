# Script information ------------------------------------------------------

# Read assessment data (after the bootstrap procedure) and write TAF data tables

# Before running the script in folder ./boot/data we have: 
#         and other files needed later
# After running the script in folder ./data we have:
#         inputData.RData with r4ss input object and TAF .csv data tables:

# Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024/08/30
rm(list=ls())
boot_esc<-"boot/data/run/" 
list.files(boot_esc, full.names = TRUE)
esc <-readLines(paste0(boot_esc,"Esc.txt")) 

# Load libraries ----------------------------------------------------------

library(icesTAF)
library(r4ss)

# Working directory and folders -------------------------------------------

# check working directory
wd<-getwd()
data<-"data/run/"
run_data<-paste0(data,esc)
# create data folder using the function mkdir in icesTAF
mkdir(run_data)


dat <- r4ss::SS_read(dir = paste0("boot/data/run/" ,esc))

#----------------------------------------------------------

# Prepare TAF tables ------------------------------------------------------

natmort <-dat$ctl$natM  #data.frame(rbind(M)) #ctl$natM
#names(natmort)<-age

# catch in tonnes
catch <- subset(dat$dat$catch, year>=(dat$dat$styr), c('year','seas','catch'))
# total biomass in the acoustic survey Pelago
btotal_idx_pelago <- subset(dat$dat$CPUE,index==5,c("year",'month',"obs"))
# total biomass in the acoustic survey Ecocadiz
btotal_idx_ecocadiz <- subset(dat$dat$CPUE,index==6,c("year",'month',"obs"))
# total biomass in the DEPM survey Bocadeva
btotal_idx_bocadeva <- subset(dat$dat$CPUE,index==7,c("year",'month',"obs"))
# total biomass in the acoustic survey EcocadizReclutas
btotal_idx_ecocadizRec <- subset(dat$dat$CPUE,index==8,c("year",'month',"obs"))


# historical numbers at age in the catch table
catageQ1    <- subset(dat$dat$agecomp,fleet==1 & year>=(dat$dat$styr) & year<(dat$dat$endyr),c("year","month","a0","a1","a2","a3"))
catageQ2    <- subset(dat$dat$agecomp,fleet==2 & year>=(dat$dat$styr) & year<(dat$dat$endyr),c("year","month","a0","a1","a2","a3"))
catageQ3    <- subset(dat$dat$agecomp,fleet==3 & year>=(dat$dat$styr) & year<(dat$dat$endyr),c("year","month","a0","a1","a2","a3"))
catageQ4    <- subset(dat$dat$agecomp,fleet==4 & year>=(dat$dat$styr) & year<(dat$dat$endyr),c("year","month","a0","a1","a2","a3"))
# numbers at age in the acoustic survey Pelago
natage_idx_pelago <- subset(dat$dat$agecomp,fleet==5 & year%in% dat$dat$styr:dat$dat$endyr,c("year","month","a0","a1","a2","a3"))
# numbers at age in the acoustic survey Ecocadiz
natage_idx_ecocadiz <- subset(dat$dat$agecomp,fleet==6 & year%in% dat$dat$styr:dat$dat$endyr,c("year","month","a0","a1","a2","a3"))
# numbers at age in the acoustic survey EcocadizReclutas
#natage_idx_ecocadizRec <- subset(dat$dat$agecomp,fleet==8 & year%in% dat$dat$styr:dat$dat$endyr,c("year","month","a0","a1","a2","a3"))
# weight at age in the catch
waca <- subset(dat$wtatage, fleet=="1" & year %in% dat$dat$styr:dat$dat$endyr,c("year","seas","0","1","2","3"))
# weight at age in the stock  mid season
west <- subset(dat$wtatage, fleet=="-1" & year %in% dat$dat$styr:dat$dat$endyr,c("year","seas","0","1","2","3"))
# fecundity
fecundity <- subset(dat$wtatage, fleet=="-2" & year %in% dat$dat$styr:dat$dat$endyr,c("year","seas","0","1","2","3"))
# maturity
maturity <- fecundity/west
maturity$'0'[maturity$'0'=="NaN"]<-0

# Save data in RData file  -----------------------------------------
#dir.create(paste0("data/run/",esc))
save.image(paste0(run_data,"/inputData.RData"))

# Write TAF tables in data folder -----------------------------------------
# write.taf(list(natmort=natmort, 
#                catageQ1 = catageQ1, catageQ2 = catageQ2,catageQ3 = catageQ3,catageQ4 = catageQ4,catch = catch, 
#                btotal_idx_pelago = btotal_idx_pelago, 
#                natage_idx_pelago=natage_idx_pelago,
#                btotal_idx_ecocadiz = btotal_idx_ecocadiz,
#                natage_idx_ecocadiz=natage_idx_ecocadiz,
#                btotal_idx_bocadeva = btotal_idx_bocadeva, 
#                btotal_idx_ecocadizRec = btotal_idx_ecocadizRec,
#                waca = waca, west = west, fecundity = fecundity, maturity = maturity),dir=paste0("data/run/",esc))
# 



# Script info -------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------


