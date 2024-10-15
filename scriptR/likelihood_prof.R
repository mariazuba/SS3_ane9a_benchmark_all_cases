
###########################################################################
# example two-dimensional profile
# (assumes you have an SS3 exe called "ss3.exe" or "ss3" in your PATH)
# based on r4ss expample
# modified by Leire Citores to do the profiling over natural mortality
# for age 2, with the same value for semester 1 and 2
###########################################################################

boot<-"boot/initial/data/run/"
list.files(boot)
#base2<-"S1.0_4FLEETS_SelECO_RecIndex_Mnewfix"
base2<-"S1.0_InitCond_sigmaR_SelP_qpriorP"   
dir<-paste0("model/run/" ,base2)
#dir_simple_small <- file.path("C:/USE/Leire/ICES/2024/WKBANSP/assessment_model/ss3_ane8/runs_fixed/run0_natMestim3kte")
#SS_parlines(ctlfile = "C:/USE/Leire/ICES/2024/WKBANSP/assessment_model/ss3_ane8/runs_fixed/run0_natMestim3kte/control.ss_new")

dir_simple_small <- file.path(dir)
SS_parlines(ctlfile = paste0(dir,"/control.ss_new"))


dir.create("model/perfil_h", recursive = TRUE)

# create temporary directory and copy files into it
dir_prof <- file.path("model/perfil_h", "profile_stepness")
copy_SS_inputs(
  dir.old = dir_simple_small,
  dir.new = dir_prof,
  create.dir = TRUE,
  overwrite = TRUE,
  copy_par = TRUE,
  verbose = TRUE
)



# create table of stepness values 

par_table<-seq(0.3,1,0.02)

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
  string = "SR_BH_steep",
  profilevec = par_table,
  extras = "-nohess",
  exe = "ss3_linux"
)


#string = c("NatM_break_3_Fem_GP_1", "NatM_break_4_Fem_GP_1","NatM_break_5_Fem_GP_1","NatM_break_6_Fem_GP_1"),



# get model output
profilemodels <- r4ss::SSgetoutput(
  dirvec = dir_prof,
  keyvec = 1:length(par_table), getcovar = FALSE
)
n <- length(profilemodels)
profilesummary <- r4ss::SSsummarize(profilemodels)

par_table<- as.numeric(profilesummary[["likelihoods"]][1,1:n])

# add total likelihood (row 1) to table created above
# like_matrix <- reshape2::acast(# reshape data frame into a matrix for use with contour
# 
#   data = par_table,
#   formula = M1vec ~ M2vec,
#   value.var = "like"
# )


#make timeseries plots comparing models in profile
# SSplotComparisons(profilesummary, legendlabels = paste("h =", par_table),subplots=1)

# plot profile using summary created above
png("Perfil_h.png", width = 400, height = 400)
 SSplotProfile(profilesummary, # summary object
                         profile.string = "SR_BH_steep", # substring of profile parameter
                         profile.label = "Stock-recruit steepness (h)",
               components = c("TOTAL", "Catch",  "Survey",  "Age_comp"),
               component.labels = c("Total", "Catch",  "Index data",  "Age data"),
) # axis label
 dev.off()
# 


#  plot(c(results$pars[15,1:n]),c(profilesummary$pars[6,1:n]),)
#  abline(v=0.9)
# # 
# # # plots by component
# PinerPlot(profilesummary,
#           component = "Age_like",
#           main = "Age",
#           profile.string="SR_BH_steep",
#           profile.label = "Stock-recruit steepness (h)"
# )
# PinerPlot(profilesummary,
#           component = "Surv_like",
#           main = "Surveys",
#           profile.string="NatM_break_5_Fem_GP_1",
#           profile.label = "NatM_break_5_Fem_GP_1"
# )
# PinerPlot(profilesummary,
#           component = "Catch_like",
#           main = "Catch",
#           profile.string="NatM_break_5_Fem_GP_1",
#           profile.label = "NatM_break_5_Fem_GP_1"
# )
# PinerPlot(profilesummary,
#           component = "Init_equ_like",
#           main = "Init_equ",
#           profile.string="NatM_break_5_Fem_GP_1",
#           profile.label = "NatM_break_5_Fem_GP_1"
# )
