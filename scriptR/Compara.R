##############################################################################
# Sensitivity test ----
##############################################################################

library(icesTAF)
library(flextable)
library(r4ss)
boot<-"boot/initial/data/run/" 
list1<-list.files(boot)
list1
#==============================================================================
# Sensitivity test of different selectivity assumptions for the ECOCADIZ survey ----
ESCs1<-c("S1.0_4FLEETS",
        "S1.0_4FLEETS_SelECO",                   
        "S1.0_4FLEETS_SelvarECO")

DESC1<-c("logistic fixed for commercial fleet and all surveys",
         "S1.0_4FLEETS + logistic selectivity ECOCADIZ: two blocks (2004-2014, 2015-2023)",
         "S1.0_4FLEETS + logistic selectivity ECOCADIZ: random-walk (2004-2014) and fixed (2015-2023)")
#==============================================================================
# Priors on catchabilities exercise ----
ESCs2<-c("S4FLEETS_SelECO_Qprior",
         "S4FLEETS_SelECO_QpriorPelEcoR",
         "S4FLEETS_SelECO_QpriorEco.Boca",
         "S4FLEETS_SelECO_QpriorBoca.EcoR",
         "S4FLEETS_SelECO_QpriorEcoR")

DESC2<-c("Qprior all surveys",
         "Qprior PELAGO and ECOCADIZ-RECLUTAS",
         "Qprior ECOCADIZ and BOCADEVA",
         "Qprior BOCADEVA and ECOCADIZ-RECLUTAS",
         "Qprior only ECOCADIZ-RECLUTAS")

#==============================================================================
# Sensibility to selectivity and natural mortality assumptions ----
ESCs3<-c("S4FLEETS_SelECO_Selfleet",
         "S4FLEETS_SelECO_Mage",
         "S4FLEETS_SelECO_MageSel",
         "S4FLEETS_SelECO_MfixSel")

DESC3<-c("Logistic fixed for all commercial fleet",
         "S1.0_4FLEETS_SelECO + Parameterize age-based fishery selectivity where
         age-0==0 (for Q1 and Q2 only, estimated for Q3-Q4)",
         "Estimate M for age-2+",
         "Combination of S4FLEETS_SelECO_Selfleet and S4FLEETS_SelECO_Mage: 
         Fishery selectivity and natural mortality",
         "Combination of S4FLEETS_SelECO_Selfleet and M fix = S4FLEETS_SelECO_Mage:
         Fishery selectivity and natural mortality fixed"
)

#==============================================================================
# Sensitivity to ECOCADIZ-RECLUTAS selectivity ----
ESCs4<-c("S4FLEETS_SelECO_Selfleet_EcoR",
         "S1.0_4FLEETS_SelECO_EcoR",
         "S1.0_4FLEETS_SelECO_EcoR3")

DESC4<-c("S4FLEETS_SelECO_Selfleet + Parameterize age_based ECOCADIZ-RECLUTAS 
         selectivity where age-0==1 and ages-1, ages-2 and ages-3 are estimated",
         "S1.0_4FLEETS_SelECO + Parameterize age_based ECOCADIZ-RECLUTAS 
         selectivity where age-0==1 and ages-1, ages-2 and ages-3 are estimated",
         "S1.0_4FLEETS_SelECO + Parameterize age_based ECOCADIZ-RECLUTAS 
         selectivity where age-3==0 and ages-0, ages-1 and ages-2 are estimated")


#==============================================================================
# ECOCADIZ-RECLUTAS as recruitment Index ----
ESCs5<-c("S1.0_4FLEETS_SelECO_RecIndex",
         "S1.0_4FLEETS_SelECO_RecIndex_M1_1.6")

DESC5<-c("Recruits index",
         "S1.0_4FLEETS_SelECO_RecIndex + Natural mortality by age fixed  Mage-0=2.97,Mage-1=1.6,Mage-2=1.33,Mage-3=1.33",
         "S1.0_4FLEETS_SelECO_RecIndex + Natural mortality by age fixed  Mage-0=2.97,Mage-1=1.6, estimated Mage-2=3 ")



ESCs<-c("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",
        "S1.0_InitCond",                   
        "S1.0_InitCond_sigmaR",
        "S1.0_InitCond_sigmaR_SelP",
        "S1.0_InitCond_sigmaR_SelP_qpriorP")





scenarios <- data.frame(
  Scenario = ESCs,
  Description =  c("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",
                   "S1.0_InitCond",                   
                   "S1.0_InitCond_sigmaR",
                   "S1.0_InitCond_sigmaR_SelP",
                   "S1.0_InitCond_sigmaR_SelP_qpriorP")
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
        "S1.0_InitCond",                   
        "S1.0_InitCond_sigmaR",
        "S1.0_InitCond_sigmaR_SelP",
        "S1.0_InitCond_sigmaR_SelP_qpriorP")

esc<-ESCs
replist<-list()
diag<-list()
params_est<-list()
Calc_Q<-list()
M<-list()
for(i in 1:length(esc)){
  Esc<-esc[i]
  load(paste0("output/run/",Esc,"/output.RData"))
  load(paste0("report/retro/",Esc,"/rho.RData"))
  replist[[Esc]]<-output
  
  
  diag[[Esc]]<-data.frame(ESC=Esc,
                          convergency=output$maximum_gradient_component,
                          AIC=as.numeric(2*dim(output$estimated_non_dev_parameters)[1]+2*output$likelihoods_used[1,1]),
                          Total_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "TOTAL"],
                          Survey_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "Survey"],
                          Age_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "Age_comp"],
                          RMSE_index=jaba_cpue$RMSE.perc[jaba_cpue$indices == "Combined"],
                          RMSE_age=jaba_age$RMSE.perc[jaba_age$indices == "Combined"],
                          Rho_ssb=rho_ssb,
                          Rho_f=rho_f)
  
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
                             cols = c(convergency, AIC,Total_like, Survey_like, Age_like, RMSE_index, RMSE_age,Rho_ssb,
                                      Rho_f),
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
tail(Qdata,n=100)

# Crear el gráfico
base<-"S1.0_4FLEETS_SelECO_RecIndex_Mnewfix"

fig_q <- Qdata %>%
  ggplot(aes(x=Fleet_name, y=Calc_Q, colour=.id)) +
  geom_point(aes(size = ifelse(.id == base, 4, 1)), shape = 19, stroke = 1.5,
             fill = ifelse(Qdata$.id == base, "black", NA)) + 
  labs(x="Surveys", y="Catchability", title="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
  guides(size = "none",  
         colour = guide_legend(override.aes = list(size = c(4, rep(2, length(levels(Qdata$.id))-1))))) +
  ylim(0, 5) 
ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_catchability2.png")), fig_q,  width=8, height=5)



mod.sum <- SSsummarize(replist)

SSplotComparisons(mod.sum, subplots=c(13,2,8,10),indexPlotEach = T,
                  legendlabels = esc,pwidth = 5,
                  pheight = 3,png=TRUE,plotdir=paste0("report/run/comparison/",folder),legendloc='topleft')

# Comparison abundance index ----
path_mod<-"report/run/comparison/Qprior"
# Cargar las imágenes
img1 <- png::readPNG(file.path(path_mod,"compare13_indices_flt5.png"))
img2 <- png::readPNG(file.path(path_mod,"compare13_indices_flt6.png"))
img3 <- png::readPNG(file.path(path_mod,"compare13_indices_flt7.png"))
img4 <- png::readPNG(file.path(path_mod,"compare13_indices_flt8.png"))

# Convertir a grobs (graphic objects) para grid.arrange
g1 <- rasterGrob(img1, interpolate=TRUE)
g2 <- rasterGrob(img2, interpolate=TRUE)
g3 <- rasterGrob(img3, interpolate=TRUE)
g4 <- rasterGrob(img4, interpolate=TRUE)

# Organizar las imágenes en una cuadrícula
fig222<-grid.arrange(g1, g2, g3, g4, ncol=1)

ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_escIndices.png")), fig222,  width=5, height=7)

# Comparison SSB, Fapical, Recruitment ----
path_mod<-"report/run/comparison/Qprior"
# Cargar las imágenes
img1 <- png::readPNG(file.path(path_mod,"compare2_spawnbio_uncertainty.png"))
img2 <- png::readPNG(file.path(path_mod,"compare8_Fvalue_uncertainty.png"))
img3 <- png::readPNG(file.path(path_mod,"compare10_recruits_uncertainty.png"))
# Convertir a grobs (graphic objects) para grid.arrange
g1 <- rasterGrob(img1, interpolate=TRUE)
g2 <- rasterGrob(img2, interpolate=TRUE)
g3 <- rasterGrob(img3, interpolate=TRUE)
# Organizar las imágenes en una cuadrícula
fig223<-grid.arrange(g3, g1,g2, ncol=1)
ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_escVarP.png")), fig223,  width=5, height=7)



ESCs<-c("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",
        "S1.0_InitCond",                   
        "S1.0_InitCond_sigmaR",
        "S1.0_InitCond_sigmaR_SelP",
        "S1.0_InitCond_sigmaR_SelP_qpriorP")

# Comparison selectivity ----
path_mod<-"report/run/comparison/Qprior"

title1 <- textGrob("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix", gp=gpar(fontsize=8, fontface="bold"))
title2 <- textGrob("S1.0_InitCond_sigmaR_SelP_qpriorP", gp=gpar(fontsize=8, fontface="bold"))

# Cargar las imágenes
img1 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_age_selectivity.png")))
img2 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_age_selectivity.png")))
# Convertir a grobs (graphic objects) para grid.arrange
g1 <- rasterGrob(img1, interpolate=TRUE)
g2 <- rasterGrob(img2, interpolate=TRUE)
# Organizar las imágenes en una cuadrícula
fig224<-grid.arrange(title1, g1, 
                     title2, g2, 
                     ncol=1, 
                     heights=c(0.5, 8, 0.5, 8))
ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_selectivity.png")), fig224,  width=5, height=7)

# Comparison selectivity ----
path_mod<-"report/run/comparison/Qprior"

title1 <- textGrob("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix", gp=gpar(fontsize=8, fontface="bold"))
title2 <- textGrob("S1.0_InitCond_sigmaR_SelP_qpriorP", gp=gpar(fontsize=8, fontface="bold"))

# Cargar las imágenes
img1 <- png::readPNG(file.path(paste0("report/retro/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","Retro.png")))
img2 <- png::readPNG(file.path(paste0("report/retro/S1.0_InitCond_sigmaR_SelP_qpriorP/","Retro.png")))
# Convertir a grobs (graphic objects) para grid.arrange
g1 <- rasterGrob(img1, interpolate=TRUE)
g2 <- rasterGrob(img2, interpolate=TRUE)
# Organizar las imágenes en una cuadrícula
fig225<-grid.arrange(title1, g1, 
                     title2, g2, 
                     ncol=1, 
                     heights=c(0.5, 8, 0.5, 8))
ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_retrospective.png")), fig225,  width=5, height=7)


# Comparison mean age ----
path_mod<-"report/run/comparison/Qprior"

title1 <- textGrob("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix", gp=gpar(fontsize=8, fontface="bold"))
title2 <- textGrob("S1.0_InitCond_sigmaR_SelP_qpriorP", gp=gpar(fontsize=8, fontface="bold"))

# Cargar las imágenes
list.files(file.path("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix"))

img1 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_meanage_fit_Ecocadiz.png")))
img2 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_meanage_fit_Ecocadiz.png")))
img3 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_meanage_fit_Pelago.png")))
img4 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_meanage_fit_Pelago.png")))
# Convertir a grobs (graphic objects) para grid.arrange
g1 <- rasterGrob(img1, interpolate=TRUE)
g2 <- rasterGrob(img2, interpolate=TRUE)
g3 <- rasterGrob(img3, interpolate=TRUE)
g4 <- rasterGrob(img4, interpolate=TRUE)
# Organizar las imágenes en una cuadrícula
fig226<-grid.arrange(
  arrangeGrob(title1, title2, ncol=2),  # Primera fila con títulos
  arrangeGrob(g1, g3, ncol=2),  # Segunda fila con imágenes de la primera columna
  arrangeGrob(g2, g4, ncol=2),  # Tercera fila con imágenes de la segunda columna
  nrow=3, heights=c(0.5, 4, 8)  # Ajuste de las alturas para títulos e imágenes
)
ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_meanage.png")), fig226,  width=5, height=7)



# Comparison sigmaR ----
path_mod<-"report/run/comparison/Qprior"

title1 <- textGrob("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix", gp=gpar(fontsize=8, fontface="bold"))
title2 <- textGrob("S1.0_InitCond_sigmaR_SelP_qpriorP", gp=gpar(fontsize=8, fontface="bold"))

# Cargar las imágenes
list.files(file.path("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix"))

img1 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_recdevs2_varcheck.png")))
img2 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_Recdevs.png")))
img3 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_stock-recluta.png")))
img4 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_recdevs2_varcheck.png")))
img5 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_Recdevs.png")))
img6 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_stock-recluta.png")))

# Convertir a grobs (graphic objects) y ajustar tamaño a una dimensión común
g1 <- rasterGrob(img1, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
g2 <- rasterGrob(img2, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
g3 <- rasterGrob(img3, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
g4 <- rasterGrob(img4, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
g5 <- rasterGrob(img5, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
g6 <- rasterGrob(img6, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

# Organizar las imágenes en una cuadrícula con tamaños iguales
fig227 <- grid.arrange(
  arrangeGrob(title1, title2, ncol=2),  # Primera fila con títulos
  arrangeGrob(g1, g4, ncol=2),  # Segunda fila con imágenes de la primera columna
  arrangeGrob(g2, g5, ncol=2),  # Tercera fila con imágenes de la segunda columna
  arrangeGrob(g3, g6, ncol=2),  # Tercera fila con imágenes de la segunda columna
  nrow=4, heights=c(0.5, 4, 3,4)  # Ajuste de las alturas para títulos e imágenes
)

fig227
ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_sigmaR.png")), fig227,  width=7, height=7)


