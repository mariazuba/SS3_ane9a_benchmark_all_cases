
###########################################################################
# example two-dimensional profile
# (assumes you have an SS3 exe called "ss3.exe" or "ss3" in your PATH)
# based on r4ss expample
# modified by Leire Citores to do the profiling over natural mortality
# for age 2, with the same value for semester 1 and 2
###########################################################################

boot<-"boot/initial/data/run/"
list.files(boot)
base2<-"S1.0_4FLEETS_SelECO_RecIndex_Mfix2_3_M1_1.6"
dir<-paste0("model/run/" ,base2)
#dir_simple_small <- file.path("C:/USE/Leire/ICES/2024/WKBANSP/assessment_model/ss3_ane8/runs_fixed/run0_natMestim3kte")
#SS_parlines(ctlfile = "C:/USE/Leire/ICES/2024/WKBANSP/assessment_model/ss3_ane8/runs_fixed/run0_natMestim3kte/control.ss_new")

dir_simple_small <- file.path(dir)
SS_parlines(ctlfile = paste0(dir,"/control.ss_new"))


dir.create("model/perfilM", recursive = TRUE)

# create temporary directory and copy files into it
dir_prof <- file.path("model/perfilM", "profile_natMage2")
copy_SS_inputs(
  dir.old = dir_simple_small,
  dir.new = dir_prof,
  create.dir = TRUE,
  overwrite = TRUE,
  copy_par = TRUE,
  verbose = TRUE
)


# create table of M values 
# 1 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=Maunder_M;_6=Age-range_Lorenzen
# 7 #_N_BREAKPOINT
# 0 0.25 0.5 0.75 1 1.25 1.5
#
# 0	  3	       2.97	0	0	0	-3	0	0	   0	   0	0	0	0	#_M_break1   
# 0	  3	       2.97	0	0	0	-3	0	0	   0	   0	0	0	0	#_M_break2
# 0	  3	       1.33	0	0	0	 -3	0	0	   0	   0	0	0	0	#_M_break3   
# 0	  3	       1.33	0	0	0	-3	0	0	   0	   0	0	0	0	#_M_break4

# 0	  3	       1.33	0	0	0	 -1	0	0	   0	   0	0	0	0	#_M_break5
# 0	  3	       1.33	0	0	0	-3	0	0	   0	   0	0	0	0	#_M_break6
# 0	  3	       1.33	0	0	0	 1	0	0	   0	   0	0	0	0	#_M_break7

# 0	  3	       2.97	0	0	0	-3	0	0	   0	   0	0	0	0	#_M_break1   
# 0	  3	       2.97	0	0	0	-3	0	0	   0	   0	0	0	0	#_M_break2

# 0	  3	       1.6	0	0	0	 -3	0	0	   0	   0	0	0	0	#_M_break3   
# 0	  3	       1.6	0	0	0	-3	0	0	   0	   0	0	0	0	#_M_break4
# 0	  3	       1.6	0	0	0	 -1	0	0	   0	   0	0	0	0	#_M_break5
# 0	  3	       1.6	0	0	0	-3	0	0	   0	   0	0	0	0	#_M_break6

# 0	  3	       1.6	0	0	0	 -1	0	0	   0	   0	0	0	0	#_M_break7
# 0	  3	       1.6	0	0	0	 -1	0	0	   0	   0	0	0	0	#_M_break8
# 0	  3	       1.6	0	0	0	 -1	0	0	   0	   0	0	0	0	#_M_break9
# 0	  3	       1.6	0	0	0	 -1	0	0	   0	   0	0	0	0	#_M_break10

# 0	  3	       1.6	0	0	0	 1	0	0	   0	   0	0	0	0	#_M_break11

par_table<-as.data.frame(cbind(M1vec = seq(0.1,4,0.1),
                               M2vec = seq(0.1,4,0.1),
                               M3vec = seq(0.1,4,0.1),
                               M4vec = seq(0.1,4,0.1)))

# run model once to create control.ss_new with
# good starting parameter values
# exe is assumed to be in PATH, add "exe" argument if needed
cp("boot/software/ss3_linux", dir_prof)
wd <- dir_prof 
system(wd)
system(paste0("chmod 755 ",wd,"/ss3_linux"))
r4ss::run(dir_prof, extras = "-nohess",exe = "ss3_linux",skipfinished = F)

# the following commands related to starter.ss could be done by hand
# read starter file
starter <- SS_readstarter(file.path(dir_prof, "starter.ss"))
# change control file name in the starter file
starter[["ctlfile"]] <- "control_modified.ss"
# make sure the prior likelihood is calculated
# for non-estimated quantities
starter[["prior_like"]] <- 1
# write modified starter file
SS_writestarter(starter, dir = dir_prof, overwrite = TRUE)
# vector of values to profile over


# run profile using ss_new file as parameter source and
# overwriting original control file with new values
prof.table <- r4ss::profile(
  dir = dir_prof,
  oldctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",
  string = c("NatM_break_7_Fem_GP_1", "NatM_break_8_Fem_GP_1","NatM_break_9_Fem_GP_1","NatM_break_10_Fem_GP_1"),
  profilevec = par_table,
  extras = "-nohess",
  exe = "ss3_linux"
)


#string = c("NatM_break_3_Fem_GP_1", "NatM_break_4_Fem_GP_1","NatM_break_5_Fem_GP_1","NatM_break_6_Fem_GP_1"),


# get model output
profilemodels <- SSgetoutput(
  dirvec = dir_prof,
  keyvec = 1:nrow(par_table), getcovar = FALSE
)
n <- length(profilemodels)
profilesummary <- SSsummarize(profilemodels)

# add total likelihood (row 1) to table created above
par_table[["like"]] <- as.numeric(profilesummary[["likelihoods"]][1, 1:n])

# reshape data frame into a matrix for use with contour
like_matrix <- reshape2::acast(
  data = par_table,
  formula = M1vec ~ M2vec,
  value.var = "like"
)


#make timeseries plots comparing models in profile
SSplotComparisons(profilesummary, legendlabels = paste("m =", par_table[,1]),subplots=1)

# plot profile using summary created above
results <- SSplotProfile(profilesummary, # summary object
                         profile.string = "NatM_break_7_Fem_GP_1", # substring of profile parameter
                         profile.label = "M2"
) # axis label

plot(c(profilesummary$pars[5,1:40]),c(profilesummary$pars[6,1:40]),ylim=c(0,4),xlim=c(0,4),xlab="M2",ylab="M3")
abline(v=0.9)

# plots by component
PinerPlot(profilesummary,
          component = "Age_like",
          main = "Age",
          profile.string="NatM_break_5_Fem_GP_1",
          profile.label = "NatM_break_5_Fem_GP_1"
)
PinerPlot(profilesummary,
          component = "Surv_like",
          main = "Surveys",
          profile.string="NatM_break_5_Fem_GP_1",
          profile.label = "NatM_break_5_Fem_GP_1"
)
PinerPlot(profilesummary,
          component = "Catch_like",
          main = "Catch",
          profile.string="NatM_break_5_Fem_GP_1",
          profile.label = "NatM_break_5_Fem_GP_1"
)
PinerPlot(profilesummary,
          component = "Init_equ_like",
          main = "Init_equ",
          profile.string="NatM_break_5_Fem_GP_1",
          profile.label = "NatM_break_5_Fem_GP_1"
)
