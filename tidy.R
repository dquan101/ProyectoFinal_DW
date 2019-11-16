library(dplyr)
library(tidyverse)
library(readxl)
library(reshape2)

files = list.files('ProyectoFinal/', all.files=FALSE,
           full.names=TRUE)

test = read_xlsx('ProyectoFinal/caracteristicas_departamental.xlsx')
clean_up <- function(file) {
  df_name = read_xlsx(file)
  df_name = df_name[-c(1:8), -c(1)]
  df_name = head(df_name, -4)
  df_name = df_name %>% setNames(as.character(df_name[1,]))
  df_name = df_name[-c(1),]
  return(df_name)
}
car_dep = clean_up('ProyectoFinal/caracteristicas_departamental.xlsx')

melting <- function(old_frame, col_range, col_names, var_name, val_name) {
  new_frame = old_frame[c(1,2,col_range)]
  colnames(new_frame) <- col_names
  new_frame = melt(new_frame, id.vars=c('Codigo', 'Departamento'), variable.name = var_name, value.name = 'frecuencia')
  return (new_frame)
}
lugar_nacimiento = melting(car_dep, 4:7, c('Codigo', 'Departamento','Mismo', 'Otro', 'Otro_pais', 'ND'), 'lugar_nacimiento')
lugar_residencia = melting(car_dep, 8:12, c('Codigo', 'Departamento', 'No_Nacido', 'Mismo', 'Otro', 'Otro_pais', 'ND'), 'lugar_residencia_2013')
dificultad_ver = melting(car_dep, 14:16, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_ver')
dificultad_oir = melting(car_dep, 17:19, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_oir')
dificultad_caminar = melting(car_dep, 20:22, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_caminar')
dificultad_recordar = melting(car_dep, 23:25, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_recordar')
dificultad_personal = melting(car_dep, 26:28, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_personal')
dificultad_comunicarse = melting(car_dep, 29:31, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_comunicarse')
