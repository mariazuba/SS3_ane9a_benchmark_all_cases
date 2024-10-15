
# wtatege
rm(list=ls())
# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ss3diags)
library(flextable)
library(reshape2)

# working directory
wd <- getwd()


run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 

input<-"boot/data"
report<-"report/run/"

path_rep<-paste0(report,esc)
mkdir(path_rep)

load(paste0(input,"/inputbase.RData"))  



# Datos de pesos medios por edad

#Fleet comercial SEINE----
wage_seine$weight[wage_seine$weight==0]<-NA
wage_seine0 <- wage_seine %>%
  filter(!(age %in% c(2, 3) & weight <= 15)) %>%
  filter(!(step %in% c(1, 2) & age==0)) %>%
  filter(!(age==0 & weight <=2)) %>%
  filter(year >= 1989 & weight <= 40)
wage_seine0<-wage_seine[,c(1,2,4,5)]
wage_seine0



fig0<-ggplot(wage_seine0, aes(x = year, y = weight/1000, color = factor(age)), shape = 1, size = 2) +
  geom_point( ) +
  geom_line() +
  labs(title = "Commercial fleet - SEINE",
                     x = "Year",
                     y = "Weight mean (Kg)",
                     color = "Age") +
  facet_wrap(~ step,ncol=2,as.table = TRUE, strip.position = "top",
             labeller = labeller(step = c("1" = "Q1", 
                                          "2" = "Q2",
                                          "3" = "Q3", 
                                          "4" = "Q4"))) +
  theme(panel.grid=element_line(color=NA)) +
  theme(plot.title = element_text(size =10),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(colour="gray",fill = "gray99"),
        strip.background = element_rect(colour = "gray", fill = "gray99"),
        legend.title = element_text(size = 6, face = "bold"), 
        legend.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
        axis.text.y = element_text(size = 10),  # Tamaño de etiquetas del eje Y
        axis.title.x = element_text(margin = margin(t = 10)),  # Margen para el título del eje X
        axis.title.y = element_text(margin = margin(r = 10))) 
ggsave(file.path(paste0(path_rep,"/fig_weight_by_quarters_SEINE.png")), fig0,  width=5, height=5)

#Pelago spring survey ----
wage_pela$weight[wage_pela$weight==0]<-NA
wage_Pelago0 <- wage_pela[,c(1,2,4,5)] %>%
  mutate(step = case_when(
    step %in% 1:3   ~ 1,
    step %in% 4:6   ~ 2,
    step %in% 7:9   ~ 3,
    step %in% 10:12 ~ 4
  ))
wage_Pelago0
wage_Pelago0.1<-wage_Pelago0 %>% mutate(type="Pelago")

#Ecocadiz summer survey ----
wage_eco$weight[wage_eco$weight==0]<-NA
wage_Ecocadiz0 <- wage_eco[,c(1,2,4,5)]  %>%
  mutate(step = case_when(
    step %in% 1:3   ~ 1,
    step %in% 4:6   ~ 2,
    step %in% 7:9   ~ 3,
    step %in% 10:12 ~ 4
  ))
wage_Ecocadiz0
wage_Ecocadiz0.1<-wage_Ecocadiz0 %>% mutate(type="Ecocadiz")

#EcocadizReclutas fall survey----
wage_ecoR$weight[wage_ecoR$weight==0]<-NA
wage_EcocadizRec0 <- wage_ecoR[,c(1,2,4,5)] %>%
  mutate(step = case_when(
    step %in% 1:3   ~ 1,
    step %in% 4:6   ~ 2,
    step %in% 7:9   ~ 3,
    step %in% 10:12 ~ 4
  ))
wage_EcocadizRec0

wage_EcocadizRec0.1<-wage_EcocadizRec0 %>% mutate(type="EcocadizRec")

datasurveys<-rbind(wage_Pelago0.1,wage_Ecocadiz0.1,wage_EcocadizRec0.1)

fig0.1<-ggplot(datasurveys, aes(x = year, y = weight/1000, color = factor(age)), shape = 1, size = 2) +
  geom_point( ) +
  geom_line() +
  labs(title = "SURVEYS",
       x = "Year",
       y = "Weight mean (Kg)",
       color = "Age") +
  facet_wrap(~ type,ncol=2,as.table = TRUE, strip.position = "top",
             labeller = labeller(type = c("Pelago" = "1.PELAGO", 
                                          "Ecocadiz" = "2.ECOCADIZ",
                                          "EcocadizRec" = "3.ECOCADIZ-RECLUTAS"))) +
  theme(panel.grid=element_line(color=NA)) +
  theme(plot.title = element_text(size =10),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(colour="gray",fill = "gray99"),
        strip.background = element_rect(colour = "gray", fill = "gray99"),
        legend.title = element_text(size = 6, face = "bold"), 
        legend.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
        axis.text.y = element_text(size = 10),  # Tamaño de etiquetas del eje Y
        axis.title.x = element_text(margin = margin(t = 10)),  # Margen para el título del eje X
        axis.title.y = element_text(margin = margin(r = 10))) 
ggsave(file.path(paste0(path_rep,"/fig_weight_by_quarters_SURVEY.png")), fig0.1,  width=5, height=5)


dataall1<-rbind(wage_seine0,wage_Pelago0,wage_Ecocadiz0,wage_EcocadizRec0)

# Filtrar datos con peso no NA
data_filtered <- dataall1[!is.na(dataall1$weight), ]
# Transformar la columna weight a logaritmo natural
data_filtered$log_weight <- log(data_filtered$weight)

data_quarter1<-data_filtered %>% filter(step==1)
data_quarter1<-data_quarter1[,c(1,3,5)]
data_quarter2<-data_filtered %>% filter(step==2)
data_quarter2<-data_quarter2[,c(1,3,5)]
data_quarter3<-data_filtered %>% filter(step==3)
data_quarter3<-data_quarter3[,c(1,3,5)]
data_quarter4<-data_filtered %>% filter(step==4)
data_quarter4<-data_quarter4[,c(1,3,5)]
data_quarter4 <- data_quarter4[-which(data_quarter4$age == 3 & data_quarter4$log_weight < 3), ]

library(lme4)

# Ajustar el modelo lineal mixto

modelo_mixto1 <- lm(log_weight ~ age, data = data_quarter1)
modelo_mixto2 <- lm(log_weight ~ age, data = data_quarter2)
modelo_mixto3 <- lm(log_weight ~ age, data = data_quarter3)
modelo_mixto4 <- lm(log_weight ~ age, data = data_quarter4)

# modelo_mixto1 <- lmer(log_weight ~ age + (1 | year), data = data_quarter1)
# modelo_mixto2 <- lmer(log_weight ~ age + (1 | year), data = data_quarter2)
# modelo_mixto3 <- lmer(log_weight ~ age + (1 | year), data = data_quarter3)
# modelo_mixto4 <- lmer(log_weight ~ age + (1 | year), data = data_quarter4)

# Ver resumen del modelo
summary(modelo_mixto1)

# Crear un nuevo data frame para las predicciones
nuevos_datos1 <- expand.grid(year = 1989:2024, age = 1:3)
nuevos_datos2 <- expand.grid(year = 1989:2024, age = 1:3)
nuevos_datos3 <- expand.grid(year = 1989:2024, age = 0:3)
nuevos_datos4 <- expand.grid(year = 1989:2024, age = 0:3)
# Convertir las columnas 'year' y 'age' en factores
nuevos_datos1$year <- as.factor(nuevos_datos1$year)
nuevos_datos1$age <- as.factor(nuevos_datos1$age)

nuevos_datos2$year <- as.factor(nuevos_datos2$year)
nuevos_datos2$age <- as.factor(nuevos_datos2$age)

nuevos_datos3$year <- as.factor(nuevos_datos3$year)
nuevos_datos3$age <- as.factor(nuevos_datos3$age)

nuevos_datos4$year <- as.factor(nuevos_datos4$year)
nuevos_datos4$age <- as.factor(nuevos_datos4$age)

# Realizar predicciones usando el modelo mixto
nuevos_datos1$predicciones <- predict(modelo_mixto1, newdata = nuevos_datos1, allow.new.levels = TRUE)
nuevos_datos2$predicciones <- predict(modelo_mixto2, newdata = nuevos_datos2, allow.new.levels = TRUE)
nuevos_datos3$predicciones <- predict(modelo_mixto3, newdata = nuevos_datos3, allow.new.levels = TRUE)
nuevos_datos4$predicciones <- predict(modelo_mixto4, newdata = nuevos_datos4, allow.new.levels = TRUE)

# Ver los resultados
# head(nuevos_datos)
# head(data_quarter2)

# Unir las bases de datos por las columnas 'year' y 'age'
datos_unidos1 <- merge(data_quarter1, nuevos_datos1, by = c("year", "age"), all = TRUE)
datos_unidos2 <- merge(data_quarter2, nuevos_datos2, by = c("year", "age"), all = TRUE)
datos_unidos3 <- merge(data_quarter3, nuevos_datos3, by = c("year", "age"), all = TRUE)
datos_unidos4 <- merge(data_quarter4, nuevos_datos4, by = c("year", "age"), all = TRUE)

# Ver los primeros registros de los datos unidos
head(datos_unidos1)

library(ggplot2)

# Graficar los valores observados y las predicciones
ggplot(datos_unidos1, aes(x = year)) +
  geom_point(aes(y = exp(log_weight), color = factor(age)), shape = 1, size = 2) +
  geom_line(aes(y = exp(predicciones), color = factor(age), group = age), size = 1) +
  labs(title = "Comparación entre Observados y Predicciones del Modelo",
       x = "Año",
       y = "Logaritmo del Peso",
       color = "Edad") +
  theme_minimal()

ggplot(datos_unidos2, aes(x = year)) +
  geom_point(aes(y = exp(log_weight), color = factor(age)), shape = 1, size = 2) +
  geom_line(aes(y = exp(predicciones), color = factor(age), group = age), size = 1) +
  labs(title = "Comparación entre Observados y Predicciones del Modelo",
       x = "Año",
       y = "Logaritmo del Peso",
       color = "Edad") +
  theme_minimal()

ggplot(datos_unidos3, aes(x = year)) +
  geom_point(aes(y = exp(log_weight), color = factor(age)), shape = 1, size = 2) +
  geom_line(aes(y = exp(predicciones), color = factor(age), group = age), size = 1) +
  labs(title = "Comparación entre Observados y Predicciones del Modelo",
       x = "Año",
       y = "Logaritmo del Peso",
       color = "Edad") +
  theme_minimal()

ggplot(datos_unidos4, aes(x = year)) +
  geom_point(aes(y = exp(log_weight), color = factor(age)), shape = 1, size = 2) +
  geom_line(aes(y = exp(predicciones), color = factor(age), group = age), size = 1) +
  labs(title = "Comparación entre Observados y Predicciones del Modelo",
       x = "Año",
       y = "Logaritmo del Peso",
       color = "Edad") +
  theme_minimal()

#'*------------------------------------------------------------------------------------------*
# Añadir una columna que identifique el trimestre en cada conjunto de datos
datos_unidos1$trimestre <- "Q1"
datos_unidos2$trimestre <- "Q2"
datos_unidos3$trimestre <- "Q3"
datos_unidos4$trimestre <- "Q4"

# Unir los cuatro conjuntos de datos en uno solo
datos_combinados <- rbind(datos_unidos1, datos_unidos2, datos_unidos3, datos_unidos4)

# Graficar los valores observados y las predicciones usando facet_wrap para separar por trimestre
# Asegurarse de que la columna 'year' es numérica
datos_combinados$year <- as.numeric(datos_combinados$year)

# Ahora puedes aplicar el gráfico con las mejoras
fig1<-ggplot(datos_combinados, aes(x = year)) +
  geom_point(aes(y = exp(log_weight)/1000, color = factor(age)), shape = 1, size = 2) +
  geom_line(aes(y = exp(predicciones)/1000, color = factor(age), group = age), size = 1) +
  labs(title = "",
       x = "Year",
       y = "Weight mean (Kg)",
       color = "Age") +
  facet_wrap(~ trimestre) +
  theme(panel.grid=element_line(color=NA)) +
  theme(plot.title = element_text(size =5),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(colour="gray",fill = "gray99"),
        strip.background = element_rect(colour = "gray", fill = "gray99"),
        legend.title = element_text(size = 6, face = "bold"), 
        legend.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
        axis.text.y = element_text(size = 10),  # Tamaño de etiquetas del eje Y
        axis.title.x = element_text(margin = margin(t = 10)),  # Margen para el título del eje X
        axis.title.y = element_text(margin = margin(r = 10))) + # Margen para el título del eje Y
  scale_x_continuous(breaks = seq(min(datos_combinados$year), max(datos_combinados$year), by = 5)) +  # Mostrar un intervalo de años más claro
  scale_y_continuous(limits = c(0, 50/1000))  # Ajustar los límites del eje Y
ggsave(file.path(paste0(path_rep,"/fig_weight_by_quarters_obs_est.png")), fig1,  width=5, height=5)

#'*------------------------------------------------------------------------------------------*
nuevos_datos1$step<-1
nuevos_datos2$step<-2
nuevos_datos3$step<-3
nuevos_datos4$step<-4

dataall_end<-rbind(nuevos_datos1,nuevos_datos2,nuevos_datos3,nuevos_datos4)
dataall_end$predicciones<-exp(dataall_end$predicciones)/1000
dataall_end_wide <- dataall_end %>%
  pivot_wider(names_from = age, values_from = predicciones, names_prefix = "age_")
dataall_end_wide[is.na(dataall_end_wide)] <- 0
dataall_end_wide<-dataall_end_wide %>%
  select(year, step, age_0, age_1, age_2, age_3)


data_esc<-"data"
write.taf(list(wtage=dataall_end_wide),dir=data_esc)
