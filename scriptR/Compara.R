##############################################################################
# Sensitivity test ----
##############################################################################

library(icesTAF)
library(flextable)
library(r4ss)
library(reshape2)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ss3diags)
library(flextable)
library(reshape2)

boot<-"boot/initial/data/run/" 
list1<-list.files(boot)
list1
nesc<-length(list1[list1 != "Esc.txt"])
mkdir("report/run/comparison")
#==============================================================================
title0<- "Initial preliminar model"

ESCs0<-c("S0",
         "S1.0_4FLEETS")

DESC0<-c("One fleet for four season (1 fleet, 4 season)",
         "One fleet by season (4 fleet, 4 season)")

#"Empirical Weight-at-age variable years"
# "logistic  for commercial fleet and PELAGO, ECOCADIZ, ECOCADIZ-RELUTAS"
#==============================================================================
title1<- "Fixed weight at age exercise"
ESCs1<-c("S1.0_4FLEETS_WatageFix")

DESC1<-c("Empirical Weight-at-age fixed years")
#==============================================================================
title2<- "Fixing catchabilities exercise"
ESCs2<-c("S1.0_4FLEETS_q1PEL",
         "S1.0_4FLEETS_q1ECO",
         "S1.0_4FLEETS_q1BOCA",
         "S1.0_4FLEETS_q1ECOREC")

DESC2<-c("S1.0_4FLEETS + Catchability fixed q=1 PELAGO",
         "S1.0_4FLEETS + Catchability fixed q=1 ECOCADIZ",
         "S1.0_4FLEETS + Catchability fixed q=1 BOCADEVA",
         "S1.0_4FLEETS + Catchability fixed q=1 ECOCADIZ-RECLUTAS")
#==============================================================================
title3<- "Sensitivity test of different selectivity assumptions for the ECOCADIZ survey"
ESCs3<-c("S1.0_4FLEETS_SelECO",                   
         "S1.0_4FLEETS_SelvarECO")

DESC3<-c("S1.0_4FLEETS + logistic selectivity ECOCADIZ: two blocks (2004-2014, 2015-2023)",
         "S1.0_4FLEETS + logistic selectivity ECOCADIZ: random-walk (2004-2014) and fixed (2015-2023)")
#==============================================================================
title4<-"Priors on catchabilities exercise"
ESCs4<-c("S4FLEETS_SelECO_Qprior",
         "S4FLEETS_SelECO_QpriorPelEcoR",
         "S4FLEETS_SelECO_QpriorEco.Boca",
         "S4FLEETS_SelECO_QpriorBoca.EcoR",
         "S4FLEETS_SelECO_QpriorEcoR")

DESC4<-c("Qprior all surveys",
         "Qprior PELAGO and ECOCADIZ-RECLUTAS",
         "Qprior ECOCADIZ and BOCADEVA",
         "Qprior BOCADEVA and ECOCADIZ-RECLUTAS",
         "Qprior only ECOCADIZ-RECLUTAS")

#==============================================================================
title5<-"Sensibility to selectivity and natural mortality assumptions "
ESCs5<-c("S4FLEETS_SelECO_Selfleet",
         "S4FLEETS_SelECO_Mage",
         "S4FLEETS_SelECO_MageSel",
         "S4FLEETS_SelECO_MfixSel")

DESC5<-c("Logistic fixed for all commercial fleet",
         "S1.0_4FLEETS_SelECO + Parameterize age-based fishery selectivity where
         age-0==0 (for Q1 and Q2 only, estimated for Q3-Q4)",
         "Estimate M for age-2+",
         "Combination of S4FLEETS_SelECO_Selfleet and S4FLEETS_SelECO_Mage: 
         Fishery selectivity and natural mortality",
         "Combination of S4FLEETS_SelECO_Selfleet and M fix = S4FLEETS_SelECO_Mage:
         Fishery selectivity and natural mortality fixed"
)

#==============================================================================
title6<- "Sensitivity to ECOCADIZ-RECLUTAS selectivity" 
ESCs6<-c("S4FLEETS_SelECO_Selfleet_EcoR",
         "S1.0_4FLEETS_SelECO_EcoR",
         "S1.0_4FLEETS_SelECO_EcoR3")

DESC6<-c("S4FLEETS_SelECO_Selfleet + Parameterize age_based ECOCADIZ-RECLUTAS 
         selectivity where age-0==1 and ages-1, ages-2 and ages-3 are estimated",
         "S1.0_4FLEETS_SelECO + Parameterize age_based ECOCADIZ-RECLUTAS 
         selectivity where age-0==1 and ages-1, ages-2 and ages-3 are estimated",
         "S1.0_4FLEETS_SelECO + Parameterize age_based ECOCADIZ-RECLUTAS 
         selectivity where age-3==0 and ages-0, ages-1 and ages-2 are estimated")
#==============================================================================
title7<- "ECOCADIZ-RECLUTAS as recruitment Index and natural mortality by age "
ESCs7<-c("S1.0_4FLEETS_SelECO_RecIndex",
         "S1.0_4FLEETS_SelECO_RecIndex_M1_1.6",
         "S1.0_4FLEETS_SelECO_RecIndex_Mest2_3_M1_1.6",
         "S1.0_4FLEETS_SelECO_RecIndex_Mnewfix")

DESC7<-c("S1.0_4FLEETS_SelECO + ECOCADIZ-RECLUTAS as recruitment Index",
         "S1.0_4FLEETS_SelECO_RecIndex + Natural mortality by age fixed  Mage-0=2.97,Mage-1=1.6,Mage-2=1.33,Mage-3=1.33",
         "S1.0_4FLEETS_SelECO_RecIndex + Natural mortality by age fixed  Mage-0=2.97,Mage-1=1.6, estimated Mage-2=3 ",
         "S1.0_4FLEETS_SelECO_RecIndex + Natural mortality by age fixed  Mage-0=2.97,Mage-1=1.6,Mage-2=2.4,Mage-3=2.4")

#==============================================================================
title8<- "Further analysis"
ESCs8<-c("S1.0_InitCond",                   
        "S1.0_InitCond_sigmaR",
        "S1.0_InitCond_sigmaR_SelP",
        "S1.0_InitCond_sigmaR_qpriorP",
        "S1.0_InitCond_sigmaR_SelP_qpriorP")

DESC8<-c("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix + Initial equilibrium catch was 
         assumed to be equal to the average catch from 1989-1994 for each fleet and season)",
         "S1.0_InitCond + sigmaR=0.33, as specified by the sigma_R_info in SS3",
         "S1.0_InitCond_sigmaR + Selectivity PELAGO was fixed at 1 from age 1 onwards",
         "S1.0_InitCond_sigmaR + A normal prior with a standard deviation of 0.1 was applied to PELAGO catchability",
         "S1.0_InitCond_sigmaR_SelP + S1.0_InitCond_sigmaR_qpriorP")


#==============================================================================
# Load necessary libraries
library(flextable)

Title = c("Initial preliminar model",
          "Fixed weight at age exercise",
          "Fixing catchabilities exercise",
          "Sensitivity test of different selectivity assumptions for the ECOCADIZ survey",
          "Priors on catchabilities exercise",
          "Sensibility to selectivity and natural mortality assumptions",
          "Sensitivity to ECOCADIZ-RECLUTAS selectivity",
          "ECOCADIZ-RECLUTAS as recruitment Index and natural mortality by age",
          "Further analysis")



# Data for the table
data <- data.frame(
  Title = c(rep("Initial implementing model", 2),
            "Fixed weight at age exercise",
            rep("Fixing catchabilities exercise", 4),
            rep("Sensitivity test of different selectivity\n assumptions for the ECOCADIZ survey", 2),
            rep("Priors on catchabilities exercise", 5),
            rep("Sensibility to selectivity and\n natural mortality assumptions", 4),
            rep("Sensitivity to ECOCADIZ-RECLUTAS selectivity", 3),
            rep("ECOCADIZ-RECLUTAS as recruitment Index\n and natural mortality by age", 4),
            rep("Further analysis", 5)),
  Scenario = c("S0", "S1.0_4FLEETS", "S1.0_4FLEETS_WatageFix",
               "S1.0_4FLEETS_q1PEL", "S1.0_4FLEETS_q1ECO", "S1.0_4FLEETS_q1BOCA", "S1.0_4FLEETS_q1ECOREC",
               "S1.0_4FLEETS_SelECO", "S1.0_4FLEETS_SelvarECO",
               "S4FLEETS_SelECO_Qprior", "S4FLEETS_SelECO_QpriorPelEcoR", "S4FLEETS_SelECO_QpriorEco.Boca", 
               "S4FLEETS_SelECO_QpriorBoca.EcoR", "S4FLEETS_SelECO_QpriorEcoR",
               "S4FLEETS_SelECO_Selfleet", "S4FLEETS_SelECO_Mage", "S4FLEETS_SelECO_MageSel", "S4FLEETS_SelECO_MfixSel",
               "S4FLEETS_SelECO_Selfleet_EcoR", "S1.0_4FLEETS_SelECO_EcoR", "S1.0_4FLEETS_SelECO_EcoR3",
               "S1.0_4FLEETS_SelECO_RecIndex", "S1.0_4FLEETS_SelECO_RecIndex_M1_1.6", 
               "S1.0_4FLEETS_SelECO_RecIndex_Mest2_3_M1_1.6", "S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",
               "S1.0_InitCond", "S1.0_InitCond_sigmaR", "S1.0_InitCond_sigmaR_SelP", "S1.0_InitCond_sigmaR_SelP_qpriorP",
               "S1.0_InitCond_sigmaR_qpriorP"),
  Description = c("One fleet for four season (1 fleet, 4 season)", 
                  "One fleet per season (4 fleet, 4 season)", 
                  "Empirical Weight-at-age fixed years",
                  "S1.0_4FLEETS + Catchability fixed q=1 PELAGO",
                  "S1.0_4FLEETS + Catchability fixed q=1 ECOCADIZ",
                  "S1.0_4FLEETS + Catchability fixed q=1 BOCADEVA",
                  "S1.0_4FLEETS + Catchability fixed q=1 ECOCADIZ-RECLUTAS",
                  "S1.0_4FLEETS + logistic selectivity ECOCADIZ: two blocks (2004-2014, 2015-2023)",
                  "S1.0_4FLEETS + logistic selectivity ECOCADIZ: random-walk (2004-2014) and fixed (2015-2023)",
                  "Qprior all surveys",
                  "Qprior PELAGO and ECOCADIZ-RECLUTAS",
                  "Qprior ECOCADIZ and BOCADEVA",
                  "Qprior BOCADEVA and ECOCADIZ-RECLUTAS",
                  "Qprior only ECOCADIZ-RECLUTAS",
                  "S1.0_4FLEETS_SelECO + Parameterize age-based fishery selectivity\n where age-0==0 (for Q1 and Q2 only, 
                  estimated for Q3-Q4) and age-2==1, and age-1 and age-3 are estimated.",
                  "S1.0_4FLEETS_SelECO + Estimate M for age-2+",
                  "Combination of S4FLEETS_SelECO_Selfleet and S4FLEETS_SelECO_Mage:\n Fishery selectivity and natural mortality",
                  "Combination of S4FLEETS_SelECO_Selfleet and Mfix=S4FLEETS_SelECO_Mage:\n Fishery selectivity and natural mortality fixed.",
                  "S4FLEETS_SelECO_Selfleet + Parameterize age_based ECOCADIZ-RECLUTAS\n selectivity where age-0==1 and ages-1, ages-2 and ages-3 are estimated",
                  "S1.0_4FLEETS_SelECO + Parameterize age_based ECOCADIZ-RECLUTAS\n selectivity where age-0==1 and ages-1, ages-2 and ages-3 are estimated",
                  "S1.0_4FLEETS_SelECO + Parameterize age_based ECOCADIZ-RECLUTAS\n selectivity where age-3==0 and ages-0, ages-1 and ages-2 are estimated",
                  "S1.0_4FLEETS_SelECO + ECOCADIZ-RECLUTAS as recruitment Index",
                  "S1.0_4FLEETS_SelECO_RecIndex + Natural mortality by age fixed\n  Mage-0=2.97,Mage-1=1.6,Mage-2=1.33,Mage-3=1.33",
                  "S1.0_4FLEETS_SelECO_RecIndex + Natural mortality by age fixed\n  Mage-0=2.97,Mage-1=1.6, estimated Mage-2=3",
                  "S1.0_4FLEETS_SelECO_RecIndex + Natural mortality by age fixed\n  Mage-0=2.97,Mage-1=1.6,Mage-2=2.4,Mage-3=2.4",
                  "S1.0_4FLEETS_SelECO_RecIndex_Mnewfix + Initial equilibrium catch\n was assumed to be equal to the average catch from 1989-1994 for each fleet and season",
                  "S1.0_InitCond + sigmaR=0.33, as specified by the sigma_R_info in SS3",
                  "S1.0_InitCond_sigmaR + Selectivity PELAGO was fixed at 1 from age 1 onwards",
                  "S1.0_InitCond_sigmaR_SelP + A normal prior with a standard deviation\n of 0.1 was applied to PELAGO catchability",
                  "S1.0_InitCond_sigmaR +  A normal prior with a standard deviation\n of 0.1 was applied to PELAGO catchability")
)

# Group data to remove duplicates in the 'Title' column
data_grouped <- data %>%
  group_by(Title) %>%
  mutate(Title = ifelse(row_number() == 1, Title, ""))

# Create the flextable
ft <- flextable(data_grouped)
# Aplicar negrita a las filas seleccionadas en la columna "Scenario"
ft <- bold(ft, i = c(2, 8, 22,25,27), j = "Scenario", bold = TRUE)
# Agregar líneas horizontales en algunas filas del cuerpo
ft <- hline(ft, i = c(2,3, 7,9,14,18,21,25), border = fp_border(width = 1.5))
# Agregar un pie de tabla para especificar los escenarios seleccionados en cada etapa
#ft <- add_footer_lines(ft, values = c("The scenarios highlighted in bold represent the options selected on each day of the benchmark."))

# Ajustar el tamaño y alineación de la tabla
ft <- autofit(ft) %>%
  align(j = 1:2, align = "left")

# Mostrar la flextable
ft

write.csv(data_grouped, "report/run/comparison/Scenarios.csv", row.names = FALSE)
invisible(save_as_image(ft, path = paste0("report/run/comparison/tb_scenarios.png")))
invisible(save_as_image(ft, path = paste0("report/run/comparison/tb_scenarios.svg")))
#'*--------------------------------------------------------------------------*
ESCs<-c("S0", "S1.0_4FLEETS", "S1.0_4FLEETS_WatageFix",
        "S1.0_4FLEETS_q1PEL", "S1.0_4FLEETS_q1ECO", "S1.0_4FLEETS_q1BOCA", "S1.0_4FLEETS_q1ECOREC",
        "S1.0_4FLEETS_SelECO", "S1.0_4FLEETS_SelvarECO",
        "S4FLEETS_SelECO_Qprior", "S4FLEETS_SelECO_QpriorPelEcoR", "S4FLEETS_SelECO_QpriorEco.Boca", 
        "S4FLEETS_SelECO_QpriorBoca.EcoR", "S4FLEETS_SelECO_QpriorEcoR",
        "S4FLEETS_SelECO_Selfleet", "S4FLEETS_SelECO_Mage", "S4FLEETS_SelECO_MageSel", "S4FLEETS_SelECO_MfixSel",
        "S4FLEETS_SelECO_Selfleet_EcoR", "S1.0_4FLEETS_SelECO_EcoR", "S1.0_4FLEETS_SelECO_EcoR3",
        "S1.0_4FLEETS_SelECO_RecIndex", "S1.0_4FLEETS_SelECO_RecIndex_M1_1.6", 
        "S1.0_4FLEETS_SelECO_RecIndex_Mest2_3_M1_1.6", "S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",
        "S1.0_InitCond", "S1.0_InitCond_sigmaR", "S1.0_InitCond_sigmaR_SelP", "S1.0_InitCond_sigmaR_SelP_qpriorP",
        "S1.0_InitCond_sigmaR_qpriorP")
esc<-ESCs
replist<-list()
diag<-list()
params_est<-list()
Calc_Q<-list()
M<-list()
Runs_test_cpue<-list()
Runs_test_age<-list()

SSB<-list()
Ft<-list()
Recr<-list()

for(i in 1:length(esc)){
  Esc<-esc[i]
  load(paste0("output/run/",Esc,"/output.RData"))
  load(paste0("report/retro/",Esc,"/rho.RData"))
  replist[[Esc]]<-output
  
  fore_retro_ssb<-rho_retro %>% filter(type == "SSB" & peel == "Combined")
  fore_retro_f<-rho_retro %>% filter(type == "SSB" & peel == "Combined")
  
  diag[[Esc]]<-data.frame(ESC=Esc,
                          Convergency=output$maximum_gradient_component,
                          AIC=as.numeric(2*dim(output$estimated_non_dev_parameters)[1]+2*output$likelihoods_used[1,1]),
                          Total_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "TOTAL"],
                          N_Params=as.numeric(2*dim(output$estimated_non_dev_parameters)[1]),
                          Survey_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "Survey"],
                          Age_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "Age_comp"],
                          RMSE_index=jaba_cpue$RMSE.perc[jaba_cpue$indices == "Combined"],
                          RMSE_age=jaba_age$RMSE.perc[jaba_age$indices == "Combined"],
                          Retro_Rho_ssb=rho_ssb,
                          Forecast_Rho_ssb=fore_retro_ssb$ForcastRho,
                          Forecast_Rho_f=fore_retro_f$ForcastRho,
                          Rho_f=rho_f)
  
  params <- output$estimated_non_dev_parameters %>%
    rownames_to_column(var = "Parameter")
  
  params_est[[Esc]] <- params %>% 
    select(c(Parameter,Value))
  
  Calc_Q[[Esc]] <-output$cpue 
  
  M[[Esc]]<-output$Natural_Mortality[4,13:16]
  
  Runs_test_cpue[[Esc]]<-run_cpue %>% select(Index,test)
  Runs_test_age[[Esc]]<-run_age %>% select(Index,test)
  
  ssb_filtered <- subset(output$derived_quants, 
                         grepl("^SSB_\\d{4}$", Label) & 
                           as.numeric(sub("SSB_", "", Label)) >= 1989 & 
                           as.numeric(sub("SSB_", "", Label)) <= 2023, 
                         select = c(Label, Value, StdDev))
  
  F_filtered <- subset(output$derived_quants, 
                         grepl("^F_\\d{4}$", Label) & 
                           as.numeric(sub("F_", "", Label)) >= 1989 & 
                           as.numeric(sub("F_", "", Label)) <= 2023, 
                         select = c(Label, Value, StdDev))
  
  Recr_filtered <- subset(output$derived_quants, 
                         grepl("^Recr_\\d{4}$", Label) & 
                           as.numeric(sub("Recr_", "", Label)) >= 1989 & 
                           as.numeric(sub("Recr_", "", Label)) <= 2023, 
                         select = c(Label, Value, StdDev))
  
  SSB[[Esc]]<-  ssb_filtered$Value
  Ft[[Esc]]<-  F_filtered$Value
  Recr[[Esc]]<-  Recr_filtered$Value

}



SSB2<-plyr::ldply(SSB,data.frame) %>% mutate(year=rep(seq(1989,2023,1),30),Variable="SSB")
names(SSB2)<-c("esc","value","year","Variable")
Ft2<-plyr::ldply(Ft,data.frame)%>% mutate(year=rep(seq(1989,2023,1),30),Variable="Ft")
names(Ft2)<-c("esc","value","year","Variable")
Recr2<-plyr::ldply(Recr,data.frame)%>% mutate(year=rep(seq(1989,2023,1),30),Variable="Rt")
names(Recr2)<-c("esc","value","year","Variable")


DataVarComb<-rbind(SSB2,Ft2,Recr2)

ggplot(DataVarComb) +
  geom_line(aes(x = year, y = value, color = esc)) + # Línea principal
  geom_line(data = DataVarComb %>% filter(esc == "S1.0_InitCond_sigmaR"), # Datos filtrados
            aes(x = year, y = value), 
            color = "black", 
            linewidth = 1.5) + # Línea negra destacada con linewidth
  facet_wrap(.~Variable, 
             scales = "free", 
             ncol = 3, 
             strip.position = "top",
             labeller = labeller(Variable = c("SSB2" = "SSB", 
                                              "Recr2" = "Recruits",
                                              "Ft2" = "F apical"))) +
  theme_bw() +
  theme(
    legend.position = "bottom",       # Mueve la leyenda hacia abajo
    legend.direction = "horizontal", # Organiza la leyenda en una fila
    legend.title = element_text(size = 8), # Ajusta el tamaño del título de la leyenda
    legend.text = element_text(size = 8)    # Ajusta el tamaño del texto de la leyenda
  )

Runs_test_age2<-plyr::ldply(Runs_test_age,data.frame)
Runs_test_cpue2<-plyr::ldply(Runs_test_cpue,data.frame)

pivoted_runs_age <- pivot_wider(Runs_test_age2, names_from = .id, values_from = test)%>% as.data.frame()
pivoted_runs_cpue <- pivot_wider(Runs_test_cpue2, names_from = .id, values_from = test) %>% as.data.frame()


diagsSS<-plyr::ldply(diag,data.frame)
diagsSS<-diagsSS %>% select(-ESC)
pivoted_diagSS <- melt(diagsSS, id.vars = ".id") %>%  
                  pivot_wider( names_from = .id, values_from = value) %>% as.data.frame()
names(pivoted_diagSS)[1]<-"Index"

DiagSS3_full<-rbind(pivoted_diagSS,pivoted_runs_cpue,pivoted_runs_age)

write.csv(DiagSS3_full, "report/run/comparison/DiagSS3_full.csv", row.names = FALSE)

colnames(diagsSS)[1] <- "Scenario"

parmSS<-plyr::ldply(params_est,data.frame)
Q_SS<-plyr::ldply(Calc_Q,data.frame)
M_SS<-plyr::ldply(M,data.frame)

ft1.1.1<-M_SS %>% flextable()
invisible(save_as_image(ft1.1.1, path = "report/run/comparison/tb_M.png"))


df_diagsSS <-   pivot_longer(diagsSS, 
                             cols = c(Convergency, AIC,Total_like, Survey_like, Age_like, RMSE_index, RMSE_age,Rho_ssb,
                                      Rho_f),
                             names_to = "Metric", 
                             values_to = "Value")

df1_diagsSS <- pivot_wider(df_diagsSS, names_from = Scenario, values_from = Value)

df_parmSS <- pivot_wider(parmSS, names_from = .id, values_from = Value)

Qdata<-Q_SS %>% select(c(".id","Yr","Fleet_name","Vuln_bio","Obs","Exp","Calc_Q"))

#results
path_mod<-"report/run/comparison/"
#diagfinal<-cbind(df1_diagsSS["Metric"], round(df1_diagsSS[,-which(names(df1_diagsSS) == "Metric")], 4))
ft1 <- flextable(diagsSS)
ft1 <- colformat_double(ft1, digits=1, na_str = "")
ft1 <- colformat_num(ft1,big.mark = "", na_str = "")
#ft1 <- align(ft1,part = "header", align = "center") 
ft1 <- fontsize(ft1, size = 11, part = "header")
ft1 <- bold(ft1, i = c(2, 8, 22,25,27), j = 1:10, bold = TRUE)
ft1 <- hline(ft1, i = c(2,3, 7,9,14,18,21,25), border = fp_border(width = 1.5))
ft1 <- add_footer_lines(ft1, values = c("The scenarios highlighted in bold represent the options selected on each day of the benchmark."))
ft1 <- autofit(ft1)

write.csv(diagsSS, "report/run/comparison/diagsSS.csv", row.names = FALSE)

invisible(save_as_image(ft1, path = "report/run/comparison/tb_Diagstics.png"))

ft2 <- flextable(cbind(df_parmSS["Parameter"], round(df_parmSS[,-which(names(df_parmSS) == "Parameter")], 3)))
ft2 <- colformat_double(ft2, digits=1, na_str = "")
ft2 <- colformat_num(ft2,big.mark = "", na_str = "")
ft2 <- align(ft2,part = "header", align = "center") 
ft2 <- fontsize(ft2, size = 8, part = "header")
ft2 <- autofit(ft2)
invisible(save_as_image(ft2, path = "report/run/comparison/tb_Parameters.png"))

Qdata$Fleet_name <- factor(Qdata$Fleet_name, 
                           levels = c("PELAGO", "ECOCADIZ", "BOCADEVA", "ECORECLUTAS"))

# Reordenar la columna .id en orden descendente
Qdata$.id <- Qdata$.id <- factor(Qdata$.id,
                                 levels = ESCs)
tail(Qdata,n=100)

# Crear el gráfico
base<-"S1.0_InitCond_sigmaR"

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
ggsave(file.path(paste0("report/run/comparison/fig_catchability2.png")), fig_q,  width=8, height=5)



mod.sum <- SSsummarize(replist)

SSplotComparisons(mod.sum, subplots=c(13,2,8,10),indexPlotEach = T,
                  legendlabels = esc,pwidth = 5,
                  pheight = 3,png=TRUE,plotdir=paste0("report/run/comparison"),legendloc='topleft')

# Comparison abundance index ----
path_mod<-"report/run/comparison"
# Cargar las imágenes
img1 <- png::readPNG(file.path(path_mod,"compare13_indices_index1.png"))
img2 <- png::readPNG(file.path(path_mod,"compare13_indices_index2.png"))
img3 <- png::readPNG(file.path(path_mod,"compare13_indices_index3.png"))
img4 <- png::readPNG(file.path(path_mod,"compare13_indices_index4.png"))

# Convertir a grobs (graphic objects) para grid.arrange
g1 <- rasterGrob(img1, interpolate=TRUE)
g2 <- rasterGrob(img2, interpolate=TRUE)
g3 <- rasterGrob(img3, interpolate=TRUE)
g4 <- rasterGrob(img4, interpolate=TRUE)

# Organizar las imágenes en una cuadrícula
fig222<-grid.arrange(g1, g2, g3, g4, ncol=1)

ggsave(file.path(paste0("report/run/comparison/fig_escIndices.png")), fig222,  width=5, height=7)

# Comparison SSB, Fapical, Recruitment ----
path_mod<-"report/run/comparison"
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
ggsave(file.path(paste0("report/run/comparison/fig_escVarP.png")), fig223,  width=5, height=7)


# 
# ESCs<-c("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix",
#         "S1.0_InitCond",                   
#         "S1.0_InitCond_sigmaR",
#         "S1.0_InitCond_sigmaR_SelP",
#         "S1.0_InitCond_sigmaR_SelP_qpriorP")
# 
# # Comparison selectivity ----
# path_mod<-"report/run/comparison/Qprior"
# 
# title1 <- textGrob("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix", gp=gpar(fontsize=8, fontface="bold"))
# title2 <- textGrob("S1.0_InitCond_sigmaR_SelP_qpriorP", gp=gpar(fontsize=8, fontface="bold"))
# 
# # Cargar las imágenes
# img1 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_age_selectivity.png")))
# img2 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_age_selectivity.png")))
# # Convertir a grobs (graphic objects) para grid.arrange
# g1 <- rasterGrob(img1, interpolate=TRUE)
# g2 <- rasterGrob(img2, interpolate=TRUE)
# # Organizar las imágenes en una cuadrícula
# fig224<-grid.arrange(title1, g1, 
#                      title2, g2, 
#                      ncol=1, 
#                      heights=c(0.5, 8, 0.5, 8))
# ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_selectivity.png")), fig224,  width=5, height=7)
# 
# # Comparison selectivity ----
# path_mod<-"report/run/comparison/Qprior"
# 
# title1 <- textGrob("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix", gp=gpar(fontsize=8, fontface="bold"))
# title2 <- textGrob("S1.0_InitCond_sigmaR_SelP_qpriorP", gp=gpar(fontsize=8, fontface="bold"))
# 
# # Cargar las imágenes
# img1 <- png::readPNG(file.path(paste0("report/retro/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","Retro.png")))
# img2 <- png::readPNG(file.path(paste0("report/retro/S1.0_InitCond_sigmaR_SelP_qpriorP/","Retro.png")))
# # Convertir a grobs (graphic objects) para grid.arrange
# g1 <- rasterGrob(img1, interpolate=TRUE)
# g2 <- rasterGrob(img2, interpolate=TRUE)
# # Organizar las imágenes en una cuadrícula
# fig225<-grid.arrange(title1, g1, 
#                      title2, g2, 
#                      ncol=1, 
#                      heights=c(0.5, 8, 0.5, 8))
# ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_retrospective.png")), fig225,  width=5, height=7)
# 
# 
# # Comparison mean age ----
# path_mod<-"report/run/comparison/Qprior"
# 
# title1 <- textGrob("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix", gp=gpar(fontsize=8, fontface="bold"))
# title2 <- textGrob("S1.0_InitCond_sigmaR_SelP_qpriorP", gp=gpar(fontsize=8, fontface="bold"))
# 
# # Cargar las imágenes
# list.files(file.path("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix"))
# 
# img1 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_meanage_fit_Ecocadiz.png")))
# img2 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_meanage_fit_Ecocadiz.png")))
# img3 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_meanage_fit_Pelago.png")))
# img4 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_meanage_fit_Pelago.png")))
# # Convertir a grobs (graphic objects) para grid.arrange
# g1 <- rasterGrob(img1, interpolate=TRUE)
# g2 <- rasterGrob(img2, interpolate=TRUE)
# g3 <- rasterGrob(img3, interpolate=TRUE)
# g4 <- rasterGrob(img4, interpolate=TRUE)
# # Organizar las imágenes en una cuadrícula
# fig226<-grid.arrange(
#   arrangeGrob(title1, title2, ncol=2),  # Primera fila con títulos
#   arrangeGrob(g1, g3, ncol=2),  # Segunda fila con imágenes de la primera columna
#   arrangeGrob(g2, g4, ncol=2),  # Tercera fila con imágenes de la segunda columna
#   nrow=3, heights=c(0.5, 4, 8)  # Ajuste de las alturas para títulos e imágenes
# )
# ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_meanage.png")), fig226,  width=5, height=7)
# 
# 
# 
# # Comparison sigmaR ----
# path_mod<-"report/run/comparison/Qprior"
# 
# title1 <- textGrob("S1.0_4FLEETS_SelECO_RecIndex_Mnewfix", gp=gpar(fontsize=8, fontface="bold"))
# title2 <- textGrob("S1.0_InitCond_sigmaR_SelP_qpriorP", gp=gpar(fontsize=8, fontface="bold"))
# 
# # Cargar las imágenes
# list.files(file.path("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix"))
# 
# img1 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_recdevs2_varcheck.png")))
# img2 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_Recdevs.png")))
# img3 <- png::readPNG(file.path(paste0("report/run/S1.0_4FLEETS_SelECO_RecIndex_Mnewfix/","fig_stock-recluta.png")))
# img4 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_recdevs2_varcheck.png")))
# img5 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_Recdevs.png")))
# img6 <- png::readPNG(file.path(paste0("report/run/S1.0_InitCond_sigmaR_SelP_qpriorP/","fig_stock-recluta.png")))
# 
# # Convertir a grobs (graphic objects) y ajustar tamaño a una dimensión común
# g1 <- rasterGrob(img1, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
# g2 <- rasterGrob(img2, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
# g3 <- rasterGrob(img3, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
# g4 <- rasterGrob(img4, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
# g5 <- rasterGrob(img5, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
# g6 <- rasterGrob(img6, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
# 
# # Organizar las imágenes en una cuadrícula con tamaños iguales
# fig227 <- grid.arrange(
#   arrangeGrob(title1, title2, ncol=2),  # Primera fila con títulos
#   arrangeGrob(g1, g4, ncol=2),  # Segunda fila con imágenes de la primera columna
#   arrangeGrob(g2, g5, ncol=2),  # Tercera fila con imágenes de la segunda columna
#   arrangeGrob(g3, g6, ncol=2),  # Tercera fila con imágenes de la segunda columna
#   nrow=4, heights=c(0.5, 4, 3,4)  # Ajuste de las alturas para títulos e imágenes
# )
# 
# fig227
# ggsave(file.path(paste0("report/run/comparison/",folder,"/fig_sigmaR.png")), fig227,  width=7, height=7)
# 
# 
