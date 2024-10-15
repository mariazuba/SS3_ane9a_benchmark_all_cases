

# input data
inputs<-"data/run"
outputs<-"output/run"
esc<-list.files(inputs)
#============================================================================
# bootstrap
# Se hace commit y push de los cambios cada vez que se ejecuta el modelo
run_boot <- "boot/"
# Agregar todos los archivos en la carpeta específica al área de preparación
system2("git", args = c("add",run_boot))
system2("git", args = c("add","bootstrap.R"))
# Realizar el commit con un mensaje descriptivo
fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
commit_message <- paste0("Actualiza boot",fecha_hora)
# Usar shQuote para manejar correctamente los espacios en el mensaje de commit
commit_message_quoted <- shQuote(commit_message)
# Ejecutar el comando git commit
system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
# (Opcional) Subir los cambios al repositorio remoto
system2("git", args = c("push"))


#============================================================================
# data
#'*Se hace commit y push de los cambios cada vez que se ejecuta el modelo*
#'# Se hace commit y push de los cambios cada vez que se ejecuta el modelo
run_esc<-paste0(getwd(),"/model/run/")
esc<-list.files(run_esc)

for(i in 1:length(esc)){
  run_dat <- paste0("data/run/",esc[i])
  # Agregar todos los archivos en la carpeta específica al área de preparación
  system2("git", args = c("add",run_dat))
  system2("git", args = c("add","data.R"))
  # Realizar el commit con un mensaje descriptivo
  fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  commit_message <- paste0("Actualizados data/run/", esc[i]," ",fecha_hora)
  # Usar shQuote para manejar correctamente los espacios en el mensaje de commit
  commit_message_quoted <- shQuote(commit_message)
  # Ejecutar el comando git commit
  system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
  # (Opcional) Subir los cambios al repositorio remoto
 # system2("git", args = c("push"))
}


#============================================================================
# model
# Se hace commit y push de los cambios cada vez que se ejecuta el modelo
run_esc<-paste0(getwd(),"/model/run/")
esc<-list.files(run_esc)

for(i in 1:length(esc)){
  run_mod <- paste0("model/run/",esc[i])
  # Agregar todos los archivos en la carpeta específica al área de preparación
  system2("git", args = c("add",run_mod))
  system2("git", args = c("add","model_01_run.R"))
  # Realizar el commit con un mensaje descriptivo
  fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  commit_message <- paste0("Actualizados model/run/", esc[i]," ", fecha_hora)
  # Usar shQuote para manejar correctamente los espacios en el mensaje de commit
  commit_message_quoted <- shQuote(commit_message)
  # Ejecutar el comando git commit
  system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
  # (Opcional) Subir los cambios al repositorio remoto
#  system2("git", args = c("push"))
}

#============================================================================
# output
# Se hace commit y push de los cambios 
for(i in 1:length(esc)){
  
  run_out <- paste0("output/run/",esc[i])
  # Agregar todos los archivos en la carpeta específica al área de preparación
  system2("git", args = c("add",run_out))
  system2("git", args = c("add","output_01_run.R"))
  # Realizar el commit con un mensaje descriptivo
  fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  commit_message <- paste0("Actualizados output/run/", esc[i]," ",fecha_hora)
  # Usar shQuote para manejar correctamente los espacios en el mensaje de commit
  commit_message_quoted <- shQuote(commit_message)
  # Ejecutar el comando git commit
  system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
  # (Opcional) Subir los cambios al repositorio remoto
  #system2("git", args = c("push"))
}

#============================================================================
# report
# Se hace commit y push de los cambios 
for(i in 1:length(esc)){
  run_rep <- paste0("report/run/",esc[i])
  # Agregar todos los archivos en la carpeta específica al área de preparación
  system2("git", args = c("add",run_rep))
  system2("git", args = c("add","report_01_run.R"))
  # Realizar el commit con un mensaje descriptivo
  fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  commit_message <- paste0("Actualizados report/run/", esc[i]," ",fecha_hora)
  # Usar shQuote para manejar correctamente los espacios en el mensaje de commit
  commit_message_quoted <- shQuote(commit_message)
  # Ejecutar el comando git commit
  system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
  # (Opcional) Subir los cambios al repositorio remoto
  #system2("git", args = c("push"))
}

#============================================================================
# Report Rmd
run_rmd<-getwd()
# Agregar todos los archivos en la carpeta específica al área de preparación
system2("git", args = c("add",run_rmd))
system2("git", args = c("add","Report_SS3_quarter_with_age_data.Rmd"))
system2("git", args = c("add","Report_SS3_quarter_with_age_data.docx"))
# Realizar el commit con un mensaje descriptivo
fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
commit_message <- paste0("Actualizados report/run/", esc[i]," ",fecha_hora)
# Usar shQuote para manejar correctamente los espacios en el mensaje de commit
commit_message_quoted <- shQuote(commit_message)
# Ejecutar el comando git commit
system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
# (Opcional) Subir los cambios al repositorio remoto
system2("git", args = c("push"))

#============================================================================
# files TAF
system2("git", args = c("add","model.R"))
system2("git", args = c("add","output.R"))
system2("git", args = c("add","report.R"))
system2("git", args = c("add","runTAF.R"))
system2("git", args = c("add","script_git.R"))
# Realizar el commit con un mensaje descriptivo
fecha_hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
commit_message <- paste0("Actualizados data/run/", esc[i]," ",fecha_hora)
# Usar shQuote para manejar correctamente los espacios en el mensaje de commit
commit_message_quoted <- shQuote(commit_message)
# Ejecutar el comando git commit
system2("git", args = c("commit", "-m", commit_message_quoted), stdout = TRUE, stderr = TRUE)
# (Opcional) Subir los cambios al repositorio remoto
system2("git", args = c("push"))

