## Prepare plots and tables for report

## Before:
## After:
#rm(list=ls())


# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ss3diags)

# Step 1. Define the root directory for the run

old_wd <- getwd()



# jitter analysis ----------------------------------------------
# copy files to the jitter directory ----

run_esc<-paste0(getwd(),"/model/run/S1.0_InitCond_sigmaR_SelP_qpriorP") 
jitter_esc<-paste0(getwd(),"/model/jitter/S1.0_InitCond_sigmaR_SelP_qpriorP")

esc<-list.files(run_esc)

dirname.root <- jitter_esc
dirname.root

# Step 2. Define the directory where a completed "base" model run is located
dirname.base <- run_esc
dirname.base

# Step 3. Create a subdirectory for the jitter run
dirname.jitter <- paste0(dirname.root,'/jitter')
dirname.jitter
dir.create(path=dirname.jitter, showWarnings = TRUE, recursive = TRUE)

# Step 4. Create a subdirectory for the output
dirname.plots <- paste0(dirname.root,"/plots")
dirname.plots
dir.create(dirname.plots)

# Step 2. Copy base model files to jitter directory 
file.copy(paste(dirname.base,       "starter.ss", sep="/"),
          paste(dirname.jitter,     "starter.ss", sep="/"))

file.copy(paste(dirname.base,       "control.SS", sep="/"),
          paste(dirname.jitter,     "control.SS", sep="/"))

file.copy(paste(dirname.base,       "data.SS", sep="/"),
          paste(dirname.jitter,     "data.SS", sep="/"))	

file.copy(paste(dirname.base,       "forecast.ss", sep="/"),
          paste(dirname.jitter,     "forecast.ss", sep="/"))

file.copy(paste(dirname.base,       "ss3_linux", sep="/"),
          paste(dirname.jitter,     "ss3_linux", sep="/"))

file.copy(paste(dirname.base,       "ss3.par", sep="/"),
          paste(dirname.jitter,     "ss3.par", sep="/"))

file.copy(paste(dirname.base,       "wtatage.ss", sep="/"),
          paste(dirname.jitter,     "wtatage.ss", sep="/"))

# Step 5. Make Changes to the Starter.ss file (r4ss example)
starter <- SS_readstarter(file.path(dirname.jitter, 'starter.ss'))

# Step 6. Change to use .par file
starter$init_values_src = 1

# Step 7. Change jitter (0.1 is an arbitrary, but common choice for jitter amount)
starter$jitter_fraction = 0.1

# Step 8. write modified starter file
SS_writestarter(starter, dir=dirname.jitter, overwrite=TRUE)


#------------ Run Jitter Test for Global Convergence with Stock Synthesis -------------------------------

# Step 9. Set the number of iteration  
Njitter=60

# Step 10. Run jitter using this function (deafult is nohess)
wd <- dirname.jitter 
system(wd)
system(paste0("chmod 755 ",wd,"/ss3_linux"))
jit.likes <- r4ss::jitter(dir=dirname.jitter, exe="ss3_linux",Njitter=Njitter, extras="")

save(jit.likes,
     file=paste0(jitter_esc,"/jitter.RData"))
# setwd(dirname.plots)
# getwd()



# Step 11. Total likelihoods necessary to assess global convergence are saved to "jit.likes"
x<- as.numeric(jit.likes)
global.convergence.check<-table(x,exclude = NULL)
write.table(jit.likes,"jit_like.csv")
write.table(global.convergence.check,"global_convergence_check.csv")


# Step 12. Summarize more Jitter results

wd <- dirname.jitter

jitter=seq(1:Njitter)
n=length(jitter)
n
witch_j <- SSgetoutput(keyvec=1:n, dirvec=wd, getcovar=F)
witch_j_summary <- SSsummarize(witch_j)

#Likelihood across runs
likes=witch_j_summary$likelihoods

#Derived quants across runs
quants=witch_j_summary$quants

#Estimated parameters across runs
pars=witch_j_summary$pars

#Write more output tables to jitter directory
write.table(quants,"Quants.csv")
write.table(pars,"Pars.csv")
write.table(likes,"Likelihoods.csv")

#Retabulate total likelihoods necessary to assess global convergence and compare to jit.likes from above 
x<-as.numeric(likes[likes$Label=="TOTAL",1:n])
global.convergence<-table(x,exclude = NULL)
write.table(global.convergence,"global_convergence.csv")


#------------ Make plots with r4ss for runs ending at a converged solution -------------------------------
#Base case read in manually
Base <- SS_output(dir=dirname.base,covar=T,forecast=T)

#make some plots#make some plots
# plotdir <- dirname.plots
# setwd(plotdir)
# getwd()

png("Jittering results.png", width = 480, height = 480)
par(mfrow=c(3,1), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(1:Njitter), witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter],
     ylab="LL", ylim=c(0,max(na.omit(jit.likes))*1.05)) 
mtext(side=3, line=1, "Jitter results: S1.0_InitCond_sigmaR_SelP_qpriorP")
abline(h=Base$likelihoods_used[1,1], col=2)

SSplotComparisons(witch_j_summary,     subplots =  c(2,8) , 
                  pch = "",legend=FALSE  ,lwd = 1 ,new = F, ylimAdj=1)
#mtext(outer=T, side=3, line=-1, "Jitter results")
dev.off()

png("jit likes.png", width = 480, height = 480)
par(mfrow=c(1,1), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(1:Njitter), 
     witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter],
     ylab="Total likelihood",
     ylim=c(0,max(na.omit(jit.likes))*1.05),
     xlab="Jitter model runs at a converged solution"
)
mtext(side=3, line=0, "Jittering")     
abline(h=Base$likelihoods_used[1,1], col=2)
dev.off()


# Repeat for all converged runs 
x<-which(!is.na(witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter]))

jitter.converged=x
jitter.converged
n.converged=length(jitter.converged)
n.converged
witch_j.converged <- SSgetoutput(keyvec=jitter.converged, dirvec=wd, getcovar=F)
witch_j_summary.converged <- SSsummarize(witch_j.converged)

png("Jittering results at converged solution.png", width = 480, height = 480)
par(mfrow=c(2,2), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(jitter.converged), 
     witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL", jitter.converged],
     ylab="Total likelihood",
     ylim=c(0,max(na.omit(jit.likes))*1.05),
     xlab="Jitter runs at a converged solution"
)
mtext(side=3, line=0, "Jittering")
abline(h=Base$likelihoods_used[1,1], col=2)

SSplotComparisons(witch_j_summary.converged,     subplots =  c(2,8,18) , pch = "",legend=FALSE  ,lwd = 1 ,new = F,  ylimAdj=1)
mtext(outer=T, side=3, line=-2.5, "Jitter results")
dev.off()


#Repeat for converged runs at the minimum solution 
#Converged runs at min converged solution (should be same as base case to pass the test) 
#min(na.omit(jit.likes))
y<-which(witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",
                                     1:Njitter]==min(na.omit(jit.likes)))


jitter.min=y
jitter.min
n.min=length(jitter.min)
n.min
witch_j.min <- SSgetoutput(keyvec=jitter.min,  dirvec=wd, getcovar=F)
witch_j_summary.min <- SSsummarize(witch_j.min)


png("Jittering results at min converged solution.png", width = 480, height = 480)
par(mfrow=c(2,2), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(jitter.min), 
     witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL", jitter.min],
     ylab="Total likelihood",
     ylim=c(0,max(na.omit(jit.likes))*1.05),
     xlab="Jitter runs at the minimum converged solution"
     )
mtext(side=3, line=0, "Jittering")
abline(h=Base$likelihoods_used[1,1], col=2)

SSplotComparisons(witch_j_summary.min,     subplots =  c(2,8,18) , pch = "",legend=FALSE  ,lwd = 1 ,new = F,  ylimAdj=1)
mtext(outer=T, side=3, line=-2.5, "Jitter results")
dev.off()

#Save image of all run data for later analysis
#file.name<-paste('jitter',format(Sys.time(), "%Y%m%d_%H%M"))
#save.image(paste0(dirname.plots, "/",file.name, ".RData"))


