##############################################################################
#
# Catchability scenarios
#
##############################################################################
library(icesTAF)
library(flextable)
library(r4ss)

boot<-"boot/initial/data/run/"
list.files(boot)

#'*===========================================================================*
#'*Qprior all surveys CV 0.1*
#'
old.esc<-"S1.0_4FLEETS_SelECO_RecIndex_Mnewfix"
new.esc<-"S1.2_qprior_allsurveys_0.1"
dirnew<-paste0(boot,new.esc)
dir.create(dirnew, recursive = TRUE)

copy_SS_inputs(dir.old = paste0(boot,old.esc), 
               dir.new = dirnew,
               copy_exe = FALSE,
               verbose = FALSE)

dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))

dat$ctl$Q_parms[["LO"]] <-rep(-7,4)
dat$ctl$Q_parms[["HI"]] <-rep(5,4)
dat$ctl$Q_parms[["INIT"]] <-rep(0,4)
dat$ctl$Q_parms[["PRIOR"]]<-rep(0,4)
dat$ctl$Q_parms[["PR_SD"]]<-rep(0.1,4)
dat$ctl$Q_parms[["PR_type"]]<-rep(6,4)
dat$ctl$Q_parms[["PHASE"]] <-rep(1,4)
dat$ctl$Q_parms

r4ss::SS_write(dat, dir = dirnew, overwrite = TRUE)
#'*===========================================================================*
#'*Qprior all surveys CV 0.2*
old.esc<-"S1.2_qprior_allsurveys_0.1"
new.esc<-"S1.2_qprior_allsurveys_0.2"
dirnew<-paste0(boot,new.esc)
dir.create(dirnew, recursive = TRUE)

copy_SS_inputs(dir.old = paste0(boot,old.esc), 
               dir.new = dirnew,
               copy_exe = FALSE,
               verbose = FALSE)

dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))

dat$ctl$Q_parms[["PR_SD"]]<-rep(0.2,4)
dat$ctl$Q_parms

r4ss::SS_write(dat, dir = dirnew, overwrite = TRUE)
#'*===========================================================================*
#'*Qprior all surveys CV 0.3*
old.esc<-"S1.2_qprior_allsurveys_0.1"
new.esc<-"S1.2_qprior_allsurveys_0.3"
dirnew<-paste0(boot,new.esc)
dir.create(dirnew, recursive = TRUE)

copy_SS_inputs(dir.old = paste0(boot,old.esc), 
               dir.new = dirnew,
               copy_exe = FALSE,
               verbose = FALSE)

dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))

dat$ctl$Q_parms[["PR_SD"]]<-rep(0.3,4)
dat$ctl$Q_parms

r4ss::SS_write(dat, dir = dirnew, overwrite = TRUE)



#'*============================================================================*
#'*Qprior PELAGO and ECOCADIZ_RECLUTAS*

old.esc<-"S1.2_qprior_allsurveys_0.1"
new.esc<-"S1.2_qprior_Pelago_EcoReclutas_0.1"
dirnew<-paste0(boot,new.esc)
dir.create(dirnew, recursive = TRUE)

copy_SS_inputs(dir.old = paste0(boot,old.esc), 
               dir.new = dirnew,
               copy_exe = FALSE,
               verbose = FALSE)

dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))
dat$ctl$Q_parms[["PR_type"]]<-c(6,0,0,6)

dat$ctl$Q_parms

r4ss::SS_write(dat, dir = dirnew, overwrite = TRUE)

#'*============================================================================*
#'*Qprior ECOCADIZ and BOCADEVA*
#'
boot<-"boot/initial/data/run/"
old.esc<-"S1.2_qprior_Pelago_EcoReclutas_0.1"
new.esc<-"S1.2_qprior_Ecocadiz_Bocadeva_0.1"
dirnew<-paste0(boot,new.esc)
dir.create(dirnew, recursive = TRUE)

copy_SS_inputs(dir.old = paste0(boot,old.esc), 
               dir.new = dirnew,
               copy_exe = FALSE,
               verbose = FALSE)

dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))
dat$ctl$Q_parms[["PR_type"]]<-c(0,6,6,0)
dat$ctl$Q_parms[["PHASE"]] <-c(1,1,2,1)
dat$ctl$Q_parms

r4ss::SS_write(dat, dir = dirnew, overwrite = TRUE)

#'*============================================================================*
#'*Qprior  BOCADEVA and ECOCADIZ-RECLUTAS*
#'
boot<-"boot/initial/data/run/"
old.esc<-"S1.2_qprior_Ecocadiz_Bocadeva_0.1"
new.esc<-"S1.2_qprior_Bocadeva_EcoReclutas_0.1"
dirnew<-paste0(boot,new.esc)
dir.create(dirnew, recursive = TRUE)

copy_SS_inputs(dir.old = paste0(boot,old.esc), 
               dir.new = dirnew,
               copy_exe = FALSE,
               verbose = FALSE)

dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))
dat$ctl$Q_parms[["PR_type"]]<-c(0,0,6,6)
dat$ctl$Q_parms[["PHASE"]] <-c(1,1,2,1)
dat$ctl$Q_parms

r4ss::SS_write(dat, dir = dirnew, overwrite = TRUE)

#'*============================================================================*
#'*Qprior  only ECOCADIZ-RECLUTAS*
#'
boot<-"boot/initial/data/run/"
old.esc<-"S1.2_qprior_Bocadeva_EcoReclutas_0.1"
new.esc<-"S1.2_qprior_only_EcoReclutas_0.1"
dirnew<-paste0(boot,new.esc)
dir.create(dirnew, recursive = TRUE)

copy_SS_inputs(dir.old = paste0(boot,old.esc), 
               dir.new = dirnew,
               copy_exe = FALSE,
               verbose = FALSE)

dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))
dat$ctl$Q_parms[["PR_type"]]<-c(0,0,0,6)
dat$ctl$Q_parms[["PHASE"]] <-c(1,1,2,1)
dat$ctl$Q_parms

r4ss::SS_write(dat, dir = dirnew, overwrite = TRUE)

#'*============================================================================*
#'*Qprior  only Pelago*
#'
boot<-"boot/initial/data/run/"
old.esc<-"S1.2_qprior_Bocadeva_EcoReclutas_0.1"
new.esc<-"S1.2_qprior_only_Pelago_0.1"
dirnew<-paste0(boot,new.esc)
dir.create(dirnew, recursive = TRUE)

copy_SS_inputs(dir.old = paste0(boot,old.esc), 
               dir.new = dirnew,
               copy_exe = FALSE,
               verbose = FALSE)

dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))
dat$ctl$Q_parms[["PR_type"]]<-c(6,0,0,0)
dat$ctl$Q_parms[["PHASE"]] <-c(1,1,2,1)
dat$ctl$Q_parms

r4ss::SS_write(dat, dir = dirnew, overwrite = TRUE)

#'*============================================================================*
#'*Qprior  only bocadeva*
#'
boot<-"boot/initial/data/run/"
old.esc<-"S1.2_qprior_Bocadeva_EcoReclutas_0.1"
new.esc<-"S1.2_qprior_only_Bocadeva_0.1"
dirnew<-paste0(boot,new.esc)
dir.create(dirnew, recursive = TRUE)

copy_SS_inputs(dir.old = paste0(boot,old.esc), 
               dir.new = dirnew,
               copy_exe = FALSE,
               verbose = FALSE)

dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))
dat$ctl$Q_parms[["PR_type"]]<-c(0,0,6,0)
dat$ctl$Q_parms[["PHASE"]] <-c(1,1,2,1)
dat$ctl$Q_parms

r4ss::SS_write(dat, dir = dirnew, overwrite = TRUE)

#'*============================================================================*
# run scenarios
#'*============================================================================*
list.files(boot)
ESCs<-c("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",                   
        "S1.2_qprior_allsurveys_0.1",
        "S1.2_qprior_allsurveys_0.2",
        "S1.2_qprior_allsurveys_0.3",          
        "S1.2_qprior_Bocadeva_EcoReclutas_0.1",
        "S1.2_qprior_Ecocadiz_Bocadeva_0.1",
        "S1.2_qprior_only_Bocadeva_0.1",       
        "S1.2_qprior_only_EcoReclutas_0.1",
        "S1.2_qprior_only_Pelago_0.1",
        "S1.2_qprior_Pelago_EcoReclutas_0.1")

for(i in 2:length(ESCs)){
  boot<-"boot/initial/data/run/"
  ESCs<-c("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",                   
          "S1.2_qprior_allsurveys_0.1",
          "S1.2_qprior_allsurveys_0.2",
          "S1.2_qprior_allsurveys_0.3",          
          "S1.2_qprior_Bocadeva_EcoReclutas_0.1",
          "S1.2_qprior_Ecocadiz_Bocadeva_0.1",
          "S1.2_qprior_only_Bocadeva_0.1",       
          "S1.2_qprior_only_EcoReclutas_0.1",
          "S1.2_qprior_only_Pelago_0.1",
          "S1.2_qprior_Pelago_EcoReclutas_0.1")
  esc<-ESCs[i]
  write(esc, file = paste0(boot,"Esc.txt"))
  sourceTAF("bootstrap")
  sourceTAF("data")
  sourceTAF("model_01_run")
  sourceTAF("output_01_run")
  sourceTAF("report_01_run")

}




#'*============================================================================*
dir.create("report/run/comparison/Qprior", recursive = TRUE)
path_mod<-"report/run/comparison/Qprior"

# Create a dataframe with the scenarios
list.files("boot/initial/data/run/" )

#'*============================================================================*
folder<-"Qprior"
dir.create(paste0("report/run/comparison/",folder), recursive = TRUE)
path_mod<-paste0("report/run/comparison/",folder)

ESCs<-c("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",                   
        "S1.2_qprior_allsurveys_0.1",
        "S1.2_qprior_allsurveys_0.2",
        "S1.2_qprior_allsurveys_0.3",   
        "S1.2_qprior_Pelago_EcoReclutas_0.1",
        "S1.2_qprior_Ecocadiz_Bocadeva_0.1",       
        "S1.2_qprior_Bocadeva_EcoReclutas_0.1",      
        "S1.2_qprior_only_EcoReclutas_0.1", 
        "S1.2_qprior_only_Pelago_0.1",
        "S1.2_qprior_only_Bocadeva_0.1")

scenarios <- data.frame(
  Scenario = ESCs,
  Description =  c(
    "Q  without prior",
    "Qprior all surveys CV=0.1 ",
    "Qprior all surveys CV=0.2 ",
    "Qprior all surveys CV=0.3 ",
    "Qprior PELAGO and ECOCADIZ_RECLUTAS CV=0.1",
    "Qprior ECOCADIZ and BOCADEVA CV=0.1",
    "Qprior  BOCADEVA and ECOCADIZ-RECLUTAS CV=0.1",
    " Qprior  only ECOCADIZ-RECLUTAS CV=0.1",
    " Qprior  only PELAGO CV=0.1",
    " Qprior  only BOCADEVA CV=0.1"
  )
)


ft0 <- flextable(scenarios)
ft0 <- colformat_double(ft0, digits=1, na_str = "")
ft0 <- colformat_num(ft0,big.mark = "", na_str = "")
ft0 <- align(ft0,part = "header", align = "center") 
ft0 <- fontsize(ft0, size = 8, part = "body")
ft0 <- autofit(ft0)
ft0 
invisible(save_as_image(ft0, path = paste0(path_mod, "/tb_scenarios.png")))


#'*--------------------------------------------------------------------------*
ESCs<-c("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",                   
        "S1.2_qprior_allsurveys_0.1",
        "S1.2_qprior_allsurveys_0.2",
        "S1.2_qprior_allsurveys_0.3",   
        "S1.2_qprior_Pelago_EcoReclutas_0.1",
        "S1.2_qprior_Ecocadiz_Bocadeva_0.1",       
        "S1.2_qprior_Bocadeva_EcoReclutas_0.1",      
        "S1.2_qprior_only_EcoReclutas_0.1", 
        "S1.2_qprior_only_Pelago_0.1",
        "S1.2_qprior_only_Bocadeva_0.1")

esc<-ESCs
replist<-list()
diag<-list()
params_est<-list()
Calc_Q<-list()
M<-list()
for(i in 1:length(esc)){
  Esc<-esc[i]
  load(paste0("output/run/",Esc,"/output.RData"))
  replist[[Esc]]<-output
  
  
  diag[[Esc]]<-data.frame(ESC=Esc,
                          convergency=output$maximum_gradient_component,
                          AIC=as.numeric(2*dim(output$estimated_non_dev_parameters)[1]+2*output$likelihoods_used[1,1]),
                          Total_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "TOTAL"],
                          Survey_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "Survey"],
                          Age_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "Age_comp"],
                          RMSE_index=jaba_cpue$RMSE.perc[jaba_cpue$indices == "Combined"],
                          RMSE_age=jaba_age$RMSE.perc[jaba_age$indices == "Combined"])
  
  params <- output$estimated_non_dev_parameters %>%
    rownames_to_column(var = "Parameter")
  
  params_est[[Esc]] <- params %>% 
    select(c(Parameter,Value))
  
  Calc_Q[[Esc]] <-output$cpue 
  
  M[[Esc]]<-output$Natural_Mortality[4,13:16]
}

diagsSS<-plyr::ldply(diag,data.frame)
diagsSS<-diagsSS %>% select(-ESC)
parmSS<-plyr::ldply(params_est,data.frame)
Q_SS<-plyr::ldply(Calc_Q,data.frame)
M_SS<-plyr::ldply(M,data.frame)

ft1.1.1<-M_SS %>% flextable()
invisible(save_as_image(ft1.1.1, path = paste0(path_mod,"/tb_M.png")))


df_diagsSS <-   pivot_longer(diagsSS, 
                             cols = c(convergency, AIC,Total_like, Survey_like, Age_like, RMSE_index, RMSE_age),
                             names_to = "Metric", 
                             values_to = "Value")

df1_diagsSS <- pivot_wider(df_diagsSS, names_from = .id, values_from = Value)

df_parmSS <- pivot_wider(parmSS, names_from = .id, values_from = Value)

Qdata<-Q_SS %>% select(c(".id","Yr","Fleet_name","Vuln_bio","Obs","Exp","Calc_Q"))

#results
path_mod<-paste0("report/run/comparison/",folder)
ft1 <- flextable(cbind(df1_diagsSS["Metric"], round(df1_diagsSS[,-which(names(df1_diagsSS) == "Metric")], 4)))
ft1 <- colformat_double(ft1, digits=1, na_str = "")
ft1 <- colformat_num(ft1,big.mark = "", na_str = "")
ft1 <- align(ft1,part = "header", align = "center") 
ft1 <- fontsize(ft1, size = 8, part = "header")
ft1 <- autofit(ft1)
invisible(save_as_image(ft1, path = paste0(path_mod,"/tb_Diagstics.png")))

ft2 <- flextable(cbind(df_parmSS["Parameter"], round(df_parmSS[,-which(names(df_parmSS) == "Parameter")], 3)))
ft2 <- colformat_double(ft2, digits=1, na_str = "")
ft2 <- colformat_num(ft2,big.mark = "", na_str = "")
ft2 <- align(ft2,part = "header", align = "center") 
ft2 <- fontsize(ft2, size = 8, part = "header")
ft2 <- autofit(ft2)
invisible(save_as_image(ft2, path = paste0(path_mod,"/tb_Parameters.png")))

Qdata$Fleet_name <- factor(Qdata$Fleet_name, 
                           levels = c("PELAGO", "ECOCADIZ", "BOCADEVA", "ECORECLUTAS"))

# Reordenar la columna .id en orden descendente
Qdata$.id <- Qdata$.id <- factor(Qdata$.id,
                                 levels = ESCs)


# Crear el gráfico
fig_q <- Qdata %>%
  ggplot(aes(x=Fleet_name, y=Calc_Q, colour=.id)) +
  geom_point(aes(size = ifelse(.id == "S1", 4, 2)), shape = 21, stroke = 1.5,
             fill = ifelse(Qdata$.id == "S1", "black", NA)) + 
  # Puntos más grandes para S1 y normales para los demás
  labs(x="Surveys", y="Catchability", title="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
  guides(size = "none",  # Eliminar la leyenda del tamaño
         colour = guide_legend(override.aes = list(size = c(4, rep(2, length(levels(Qdata$.id))-1)))))

ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_catchability2.png")), fig_q,  width=8, height=5)



mod.sum <- SSsummarize(replist)

SSplotComparisons(mod.sum, subplots=c(13,2,8,10),indexPlotEach = T,
                  legendlabels = esc,pwidth = 5,
                  pheight = 3,png=TRUE,plotdir=paste0("report/run/comparison/",folder),legendloc='topleft')

