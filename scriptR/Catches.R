# Análisis de serie de tiempo de las capturas

run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 

data<-"data/run/"
run_data<-paste0(data,esc)
load(paste0(run_data,"/inputData.RData")) 

#'*########################################################################*
# Filtrar los primeros tres años
catch_first_3_years <- subset(catch, year <= 1994)

# Calcular el promedio por trimestre (seas)
mean_catch_per_seas <- aggregate(catch ~ seas, data = catch_first_3_years, FUN = mean)

# Mostrar el resultado
round(mean_catch_per_seas,0)
sum(mean_catch_per_seas$catch)
#'*########################################################################*


# Calcular la captura media por temporada
mean_catch_per_seas <- aggregate(catch ~ seas, data = catch, FUN = mean)

# Calcular la proporción de captura por temporada
total_catch <- sum(catch$catch)
proportion_catch_per_seas <- aggregate(catch ~ seas, data = catch, FUN = sum)
proportion_catch_per_seas$proportion <- proportion_catch_per_seas$catch / total_catch

# Unir los resultados
result <- merge(mean_catch_per_seas, proportion_catch_per_seas, by = "seas")

# Mostrar los resultados
print(result)

# Filtrar los datos del año 2023
catch_2023 <- subset(catch, year == 2023)

# Calcular la media histórica de cada trimestre 
historical_mean <- aggregate(catch ~ seas, data = catch, FUN = mean)

# Calcular el cambio porcentual para cada trimestre del 2023 respecto a la media histórica
catch_2023 <- merge(catch_2023, historical_mean, by = "seas", suffixes = c("_2023", "_historical"))

# Calcular el porcentaje sobre o bajo la media histórica
catch_2023$percentage_change <- ((catch_2023$catch_2023 - catch_2023$catch_historical) / catch_2023$catch_historical) * 100

# Mostrar los resultados
print(catch_2023)


library(strucchange)

capturas_ts <- ts(catch, start = c(1989, 1), frequency = 4)

# Realizar el análisis de quiebres estructurales
bp_model <- breakpoints(capturas_ts ~ 1)

# Resumen de los resultados
summary(bp_model)

# Gráfico de los quiebres
plot(bp_model)

# Agregar las líneas de los quiebres en el gráfico de la serie de tiempo
plot(capturas_ts, main = "Structural Breaks in Catches")
lines(bp_model)

# Extraer los puntos de quiebre estimados
breakpoints(bp_model)

# El modelo óptimo con 5 quiebres estima que los cambios estructurales en la serie de tiempo de capturas de peces ocurrieron en los siguientes momentos:
#   1994 (Q1): Primer cambio estructural.
# 1999 (Q4): Segundo cambio.
# 2005 (Q4): Tercer cambio.
# 2011 (Q4): Cuarto cambio.
# 2017 (Q4): Quinto cambio.
# Estos quiebres representan momentos en los que la tendencia o el nivel de capturas cambió significativamente, 
# lo cual puede estar relacionado con factores ambientales, 
# cambios en políticas pesqueras, cambios en el esfuerzo de pesca o variaciones en las poblaciones de peces.
# Suponiendo que ya tienes el modelo de quiebres bp_model y la serie capturas_ts

# Obtener los breakpoints
bp <- breakpoints(bp_model)$breakpoints

# Crear una lista para almacenar los resultados
mean_captures <- list()

# Calcular la media de las capturas para cada segmento
for (i in 1:(length(bp) + 1)) {
  if (i == 1) {
    # Primer segmento, antes del primer quiebre
    segment <- window(capturas_ts, end = time(capturas_ts)[bp[i]])
  } else if (i == length(bp) + 1) {
    # Último segmento, después del último quiebre
    segment <- window(capturas_ts, start = time(capturas_ts)[bp[i-1]] + 0.25)
  } else {
    # Segmento intermedio
    segment <- window(capturas_ts, start = time(capturas_ts)[bp[i-1]] + 0.25, end = time(capturas_ts)[bp[i]])
  }
  
  # Calcular la media para el segmento actual
  mean_captures[[i]] <- mean(segment, na.rm = TRUE)
}

# Mostrar los resultados
mean_captures

