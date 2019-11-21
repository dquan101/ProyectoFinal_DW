library(dplyr)
library(tidyverse)
library(readxl)
library(reshape2)
library(DataExplorer)
library(skimr)
library(plotly)

clean_up <- function(file) {
  df_name = read_xlsx(file)
  df_name = df_name[-c(1:8), -c(1)]
  df_name = head(df_name, -4)
  df_name = df_name %>% setNames(as.character(df_name[1,]))
  df_name = df_name[-c(1),]
  return(df_name)
}
car_dep = clean_up('ProyectoFinal/caracteristicas_departamental.xlsx')
edu_dep = clean_up('ProyectoFinal/educacion_departamental.xlsx')
emp_dep = clean_up('ProyectoFinal/empleo_departamental.xlsx')
hogar_dep = clean_up('ProyectoFinal/hogares_departamental.xlsx')
poblacion_dep = clean_up('ProyectoFinal/poblacion_departamental.xlsx')
puebo_dep = clean_up('ProyectoFinal/pueblo_departamental.xlsx')
tec_dep = clean_up('ProyectoFinal/tecnologia_departamental.xlsx')
viv_dep = clean_up('ProyectoFinal/vivienda_departamental.xlsx')

#### ---------------------------- Make all as.numeric ------------------ #####

##Deptos 
car_dep[3:ncol(car_dep)] <- lapply(car_dep[3:ncol(car_dep)], as.numeric)
edu_dep[3:ncol(edu_dep)] <- lapply(edu_dep[3:ncol(edu_dep)], as.numeric)
emp_dep[3:ncol(emp_dep)] <- lapply(emp_dep[3:ncol(emp_dep)], as.numeric)
hogar_dep[3:ncol(hogar_dep)] <- lapply(hogar_dep[3:ncol(hogar_dep)], as.numeric)
poblacion_dep[3:ncol(poblacion_dep)] <- lapply(poblacion_dep[3:ncol(poblacion_dep)], as.numeric)
puebo_dep[3:ncol(puebo_dep)] <- lapply(puebo_dep[3:ncol(puebo_dep)], as.numeric)
tec_dep[3:ncol(tec_dep)] <- lapply(tec_dep[3:ncol(tec_dep)], as.numeric)
viv_dep[3:ncol(viv_dep)] <- lapply(viv_dep[3:ncol(viv_dep)], as.numeric)


melting <- function(old_frame, col_range, col_names, var_name, val_name) {
  new_frame = old_frame[c(1,2,col_range)]
  colnames(new_frame) <- col_names
  new_frame = melt(new_frame, id.vars=c('Codigo', 'Departamento'), variable.name = var_name, value.name = val_name)
  new_frame <- new_frame[-c(1)]
  return (new_frame)
}
new_melt <- function(old_frame, col_range, col_names, var_name, val_name) {
  new_frame = old_frame[c(1,2,col_range)]
  colnames(new_frame) <- col_names
  new_frame <- new_frame[-c(1)]
  return (new_frame)
}
remove_rows <- function(frame, regex, regex2, column){
  frame <- frame[!grepl(regex, column),]
  frame <- frame[!grepl(regex2, column),]
  return(frame)
}
#Bloque de funciones aplicadas a caracteristicas_departamento
cardep_lugar_nacimiento = melting(car_dep, 4:7, c('Codigo', 'Departamento', 'Nacimiento_Mismo', 'Nacimiento_Otro', 'Nacimiento_Otro_pais', 'Nacimiento_ND'), 'lugar_nacimiento', 'frecuencia_nac')
cardep_lugar_residencia = melting(car_dep, 8:12, c('Codigo', 'Departamento', 'Residencia_No_Nacido', 'Residencia_Mismo', 'Residencia_Otro', 'Residencia_Otro_pais', 'Residencia_ND'), 'lugar_residencia_2013', 'frecuencia_res')

#Total de mujeres con hijos nacidos vs mujeres fertiles
cardep_mujeres_hijos_15_nacidos = melting(car_dep, 33:39, c('Codigo', 'Departamento', '0_hijo_Nacidos','1_hijo_Nacidos','2_hijo_Nacidos','3_hijo_Nacidos','4_hijo_Nacidos', '5_omas_Nacidos', 'Nacidos_ND'), 'mujeres_hijos_nacidos', 'frecuencia_hijos')
cardep_mujeres_hijos_15_sobre = melting(car_dep, 40:45, c('Codigo', 'Departamento', '0_hijo_Sobreviviente','1_hijo_Sobreviviente','2_hijo_Sobreviviente','3_hijo_Sobreviviente','4_hijo_Sobreviviente', '5_omas_Sobreviviente'), 'mujeres_hijos_sobrevivientes', 'frecuencia_hijos')
mujeres_c_hijos <- aggregate(cardep_mujeres_hijos_15_nacidos$frecuencia_hijos, by=list(Departamento=cardep_mujeres_hijos_15_nacidos$Departamento), FUN = sum)
mujeres_c_hijos$total_mujeres_fertiles <- car_dep$`Total de mujeres en edad fértil`
mujeres_c_hijos <- mujeres_c_hijos %>% rename(mujeres_hij_nacidos = x)

muj <- plot_ly(mujeres_c_hijos, x = ~mujeres_hij_nacidos, y = ~total_mujeres_fertiles, text = ~Departamento, type = 'scatter', mode = 'markers', size=~total_mujeres_fertiles, color = ~Departamento, colors = 'Paired',
             marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Relacion de mujeres fertiles con mujeres con hijos',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)
muj
#Dificultades totales promedio
dificultades = new_melt(car_dep, 14:31, c('Codigo','Departamento', 'sin_ver', 'con_ver', 'NA_ver', 'sin_oir', 'con_oir', 'NA_oir', 'sin_caminar', 'con_caminar', 'NA_caminar', 'sin_recordar', 'con_recordar', 'NA_recordar', 'sin_personal', 'con_personal', 'NA_personal', 'sin_comunicarse', 'con_comunicarse', 'NA_comunicarse'), 'test', 'another')
dificultades = melt(dificultades, id.vars = 'Departamento', variable.name = 'Dificultades', value.name = 'Frecuencia')
freq_diff_prom <- aggregate(Frecuencia~Departamento,dificultades, FUN=mean)
freq_diff_sum <- aggregate(Frecuencia~Departamento, dificultades, FUN=sum)
tipo_diff_prom <- aggregate(Frecuencia~Dificultades, dificultades, FUN=mean)
tipo_diff_sum <- aggregate(Frecuencia~Dificultades, dificultades, FUN=sum)
tipo_diff_prom <- remove_rows(tipo_diff_prom, "^NA", "^sin", tipo_diff_prom$Dificultades)
tipo_diff_prom <- tipo_diff_prom[!grepl("^sin", tipo_diff_prom$Dificultades),]
tipo_diff_prom <- tipo_diff_prom[!grepl("^NA", tipo_diff_prom$Dificultades),]
tipo_diff_sum <- tipo_diff_sum[!grepl("^sin", tipo_diff_sum$Dificultades),]
tipo_diff_sum <- tipo_diff_sum[!grepl("^NA", tipo_diff_sum$Dificultades),]

#Bloque de funciones aplicadas a educacion_departamento

eddep_nivel_educativo = melting(edu_dep, 3:11, c('Codigo', 'Departamento', 'Ninguno', 'Preprimaria', 'Primaria_1_3', 'Primaria_4_5', 'Primaria_6', 'Basico', 'Diversificado', 'Licenciatura', 'Maestria_doctorado'), 'Nivel_Educacion', 'frecuencia_educacion')
eddep_causa_inasistencia = melting(edu_dep, 12:20, c('Codigo', 'Departamento', 'Dinero', 'Trabajo', 'No_hay_institucion', 'Padres_Pareja', 'Quehaceres', 'No_Gusta', 'Terminados', 'Otras', 'ND'), 'Razon_Inasistencia', 'frecuencia_inasistencia')
eddep_alfabetismo = melting(edu_dep, 22:23, c('Codigo', 'Departamento', 'Alfabeta', 'Analfabeta'), 'Educacion', 'frecuencia_educacion')
eddep_asistencia = melting(edu_dep, 24:25, c('Codigo', 'Departamento', 'Asiste', 'No_asiste'), 'Asistencia', 'frecuencia_asistencia')
eddep_lugar_estudio = melting(edu_dep, 26:29, c('Codigo', 'Departamento', 'Mismo_dep', 'Otro_dep', 'Otro_pais', 'ND'), 'Lugar_estudio', 'frecuencia_lugar')

#Alfabetismo correlacion con tecnologia
edu_tec <- merge(eddep_alfabetismo, tecdep_celular, by="Departamento")
edu_tec <- merge(edu_tec, tecdep_pc, by="Departamento")
edu_tec <- merge(edu_tec, tecdep_internet, by="Departamento")
edu_tec <- edu_tec[!grepl("ND", edu_tec$Celular),]
edu_tec <- edu_tec[!grepl("^Usa", edu_tec$Celular),]
edu_tec <- edu_tec[!grepl("ND", edu_tec$PC),]
edu_tec <- edu_tec[!grepl("^Usa", edu_tec$PC),]
edu_tec <- edu_tec[!grepl("ND", edu_tec$Internet),]
edu_tec <- edu_tec[!grepl("^Usa", edu_tec$Internet),]
edu_tec <- edu_tec[!grepl("Alfabeta", edu_tec$Educacion),]
edu_tec <- edu_tec[c('Departamento','frecuencia_educacion', 'frecuencia_uso_cel','frecuencia_uso_pc', 'frecuencia_uso_inter')]
colnames(edu_tec) <- c('Departamento', 'Analfabetas', 'No_Celular', 'No_Pc', 'No_Inter')
ed <- plot_ly(data = edu_tec, x = ~Departamento, y= ~Analfabetas, type='bar', name='Analfabetas') %>% 
  add_trace(y=~No_Celular, name='No usa celular') %>%
  add_trace(y=~No_Pc, name='No usa pc') %>%
  add_trace(y=~No_Inter, name='No usa internet') %>%
  layout(yaxis = list(title='Frecuencia'), barmode = 'group')
ed
#Bloque de funciones aplicadas a empleo_departamental
empleo_dep = melting(emp_dep, 4:8, c('Codigo', 'Departamento', 'Poblacion_Activa', 'Poblacion_Ocupada', 'Cesante', 'Aspirante', 'ND'), 'Estado', 'frecuencia_estado')

#Bloque de funciones aplicadas a hogares_departamental
hogdep_hogares = melting(hogar_dep, 3:4, c('Codigo', 'Departamento', 'Urbana', 'Rural'), 'Area', 'distribucion')
hogdep_personas_hogar = melting(hogar_dep, 5:6, c('Codigo', 'Departamento', 'Hogar', 'Dormitorio'), 'Por', 'Promedio')
#Bloque de funciones aplicadas a poblacion_departamental
pobdep_sexo_dep = melting(poblacion_dep, 4:5, c('Codigo', 'Departamento', 'Hombre', 'Mujer'), 'Sexo', 'frecuencia_sexo')
pobdep_edad_general = melting(poblacion_dep, 6:10, c('Codigo', 'Departamento', '0-14', '15-29', '30-64', '65-84', '85+'), 'Edad', 'frecuencia_edad')
pobdep_edad_esp = melting(poblacion_dep, 10:31, c('Codigo', 'Departamento', '0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64','65-69', '70-74', '75-79', '80-84', '85-89', '90-94','95-99', '100+'), 'Edad', 'frecuencia_edad')
pobdep_parentesco_jefe = melting(poblacion_dep, 34:44, c('Codigo', 'Departamento', 'Jefe', 'Pareja', 'Hijo/a', 'Nuera-Yerno', 'Nietos', 'Hermanos', 'Padres', 'Suegros', 'Cuniado', 'Otro', 'No_pariente'), 'Relacion', 'frecuencia_relacion')
pobdep_estado_civil = melting(poblacion_dep, 47:52, c('Codigo', 'Departamento', 'Soltero', 'Unido', 'Casado', 'Separado', 'Divorviado', 'Viudo'), 'Estado_Civil', 'frecuencia_civil')
jefe <- pobdep_parentesco_jefe[grepl("Jefe", pobdep_parentesco_jefe$Relacion),]
mantenidos <- pobdep_parentesco_jefe[grepl("No_pariente", pobdep_parentesco_jefe$Relacion),]
jefe_manuten <- jefe
jefe$Total_poblacion <- poblacion_dep$`Total de personas`
jefe_manuten <- merge(jefe_manuten, mantenidos, by="Departamento")
jefe_manuten <- jefe_manuten[c('Departamento','frecuencia_relacion.x', 'frecuencia_relacion.y')]
colnames(jefe_manuten) <- c('Departamento', 'Jefes', 'Inquilino_sin_relacion')
jmdep <- plot_ly(data=jefe_manuten, x=~Departamento, y=~Inquilino_sin_relacion)
jmdep
#Bloque de funciones aplicadas a pueblo_departamental
puebdep_pertenencia = melting(puebo_dep, 4:9, c('Codigo', 'Departamento', 'Maya', 'Garifuna', 'Xinka', 'Afro', 'Ladino', 'Extranjero'), 'Pueblo', 'frecuencia_pueblo')
puebdep_lengua = melting(puebo_dep, 10:31, c('Codigo', 'Departamento', 'Achi', 'Akateka', 'Awakateka', 'Chorti', 'Chalchiteka', 'Chuj', 'Itza', 'Ixil', 'Popti', 'Kiche', 'Kaqchiquel', 'Mam', 'Mopan', 'Poqomam', 'Poqomchi', 'Qanjobal', 'Qeqchi', 'Sakapulteka', 'Sipakapense', 'Tektiteka', 'Tzutujil', 'Uspanteka'), 'Lengua', 'frecuencia_lengua')
puebdep_lengua_aprendido = melting(puebo_dep, 32:54, c('Codigo', 'Departamento', 'Achi', 'Akateka', 'Awakateka', 'Chorti', 'Chalchiteka', 'Chuj', 'Itza', 'Ixil', 'Popti', 'Kiche', 'Kaqchiquel', 'Mam', 'Mopan', 'Poqomam', 'Poqomchi', 'Qanjobal', 'Qeqchi', 'Sakapulteka', 'Sipakapense', 'Tektiteka', 'Tzutujil', 'Uspanteka', 'No_habla'), 'Aprendio_lengua', 'frecuencia_lengua')

#Bloque de funciones aplicadas a tecnologia_departamental
tecdep_celular = melting(tec_dep, 4:6, c('Codigo', 'Departamento', 'Usa_Celular', 'No_Usa', 'ND'), 'Celular', 'frecuencia_uso_cel')
tecdep_pc = melting(tec_dep, 7:9, c('Codigo', 'Departamento', 'Usa', 'No_Usa', 'ND'), 'PC', 'frecuencia_uso_pc')
tecdep_internet = melting(tec_dep, 10:12, c('Codigo', 'Departamento', 'Usa', 'No_Usa', 'ND'), 'Internet', 'frecuencia_uso_inter')
dep_tec <- melt(tec_dep[-c(1)], id.vars = "Departamento")
#Bloque de funciones aplicadas a vivienda_departamental
vivdep_tipo = melting(viv_dep, 6:12, c('Codigo', 'Departamento', 'Casa_Formal', 'Apartamento', 'Cuarto_vecindad','Rancho','Improvisada', 'Otro', 'No_Registrada'), 'Tipo', 'frecuencia_tipo')
vivdep_tipo_oc = melting(viv_dep, 13:16, c('Codigo', 'Departamento', 'Ocupada', 'Temporal', 'Desocupada', 'Rechazo'), 'Tipo', 'frecuencia_tipo')
vivdep_pared = melting(viv_dep, 17:27, c('Codigo', 'Departamento', 'Ladrillo','Block','Concreto','Adobe','Madera','Lamina','Bajareque','Palo','Material_desecho', 'Otro', 'ND'), 'Material', 'frecuencia_material')
vivdep_techo = melting(viv_dep, 28:35, c('Codigo', 'Departamento', 'Concreto','Lamina','Asbesto','Teja','Paja','Desecho','Otro','ND'), 'Material', 'frecuencia_material')
vivdep_piso = melting(viv_dep, 36:43, c('Codigo', 'Departamento', 'Ladrillo_ceramico','Ladrillo_cemento','Ladrillo_barro','Cemento','Vinil','Madera','Tierra','Otro'), 'Material', 'frecuencia_material')

viv_ed_dep <- merge(eddep_alfabetismo, vivdep_tipo, by="Departamento")
viv_ed_dep <- viv_ed_dep[!grepl("Alfabeta", viv_ed_dep$Educacion),]
viv_ed_dep <- viv_ed_dep[!grepl("No_Registrada", viv_ed_dep$Tipo),]
viv_ed_dep <- viv_ed_dep[!grepl("Casa_Formal", viv_ed_dep$Tipo),]
viv_ed_dep <- viv_ed_dep[!grepl("Apartamento", viv_ed_dep$Tipo),]
viv_ed_dep <- viv_ed_dep[!grepl("Otro", viv_ed_dep$Tipo),]
viv_ed_dep <- viv_ed_dep[c("Departamento", "frecuencia_educacion", "Tipo", "frecuencia_tipo")]
colnames(viv_ed_dep) <- c("Departamento", "Analfabetas","Tipo_Vivienda", "frecuencia")
test<-merge(cardep_dificultad_oir, cardep_dificultad_ver, by="Departamento")
test<-merge(test, cardep_dificultad_caminar, by='Departamento')



####---------------- Municipios -----------------------####
car_mun = clean_up('ProyectoFinal/caracteristicas_municipal.xlsx')
edu_mun = clean_up('ProyectoFinal/educacion_municipal.xlsx')
emp_mun = clean_up('ProyectoFinal/empleo_municipal.xlsx')
hogar_mun = clean_up('ProyectoFinal/hogares_municipal.xlsx')
poblacion_mun = clean_up('ProyectoFinal/poblacion_municipal.xlsx')
#puebo_mun = clean_up('ProyectoFinal/pueblo_municipal.xlsx')
tec_mun = clean_up('ProyectoFinal/tecnologia_municipal.xlsx')
viv_mun = clean_up('ProyectoFinal/vivienda_municipal.xlsx')

#### ---------------------------- Make all as.numeric ------------------ #####

##Municipios
car_mun[3:ncol(car_mun)] <- lapply(car_mun[3:ncol(car_mun)], as.numeric)
car_mun <- car_mun[!duplicated(car_mun$Municipio),]
edu_mun[3:ncol(edu_mun)] <- lapply(edu_mun[3:ncol(edu_mun)], as.numeric)
edu_mun <- edu_mun[!duplicated(edu_mun$Municipio),]
emp_mun[3:ncol(emp_mun)] <- lapply(emp_mun[3:ncol(emp_mun)], as.numeric)
emp_mun <- emp_mun[!duplicated(emp_mun$Municipio),]
hogar_mun[3:ncol(hogar_mun)] <- lapply(hogar_mun[3:ncol(hogar_mun)], as.numeric)
hogar_mun <- hogar_mun[!duplicated(hogar_mun$Municipio),]
poblacion_mun[3:ncol(poblacion_mun)] <- lapply(poblacion_mun[3:ncol(poblacion_mun)], as.numeric)
poblacion_mun <- poblacion_mun[!duplicated(poblacion_mun$Municipio),]
#puebo_mun = clean_up('ProyectoFinal/pueblo_municipal.xlsx')
tec_mun[3:ncol(tec_mun)] <- lapply(tec_mun[3:ncol(tec_mun)], as.numeric)
tec_mun <- tec_mun[!duplicated(tec_mun$Municipio),]
viv_mun[3:ncol(viv_mun)] <- lapply(viv_mun[3:ncol(viv_mun)], as.numeric)
viv_mun <- viv_mun[!duplicated(viv_mun$Municipio),]

melting <- function(old_frame, col_range, col_names, var_name, val_name) {
  new_frame = old_frame[c(1,2,col_range)]
  colnames(new_frame) <- col_names
  new_frame = melt(new_frame, id.vars=c('Codigo', 'Municipio'), variable.name = var_name, value.name = val_name)
  new_frame <- new_frame[-c(1)]
  return (new_frame)
}

#Bloque de funciones aplicadas a caracteristicas_municipio
carmun_lugar_nacimiento = melting(car_mun, 3:7, c('Codigo', 'Municipio', 'Mismo', 'Otro', 'Otro_pais', 'ND'), 'lugar_nacimiento', 'frecuencia_nac')
carmun_lugar_residencia = melting(car_mun, 8:12, c('Codigo', 'Municipio', 'No_Nacido', 'Mismo', 'Otro', 'Otro_pais', 'ND'), 'lugar_residencia_2013', 'frecuencia_res')
carmun_mujeres_hijos_15_nacidos = melting(car_mun, 33:38, c('Codigo', 'Municipio', '0_hijo','1_hijo','2_hijo','3_hijo','4_hijo', '5_omas'), 'mujeres_hijos_nacidos', 'frecuencia_hijos')
carmun_mujeres_hijos_15_sobre = melting(car_mun, 40:45, c('Codigo', 'Municipio', '0_hijo','1_hijo','2_hijo','3_hijo','4_hijo', '5_omas'), 'mujeres_hijos_sobrevivientes', 'frecuencia_hijos')

mun_dificultades = new_melt(car_mun, 14:31, c('Codigo','Municipio', 'sin_ver', 'con_ver', 'NA_ver', 'sin_oir', 'con_oir', 'NA_oir', 'sin_caminar', 'con_caminar', 'NA_caminar', 'sin_recordar', 'con_recordar', 'NA_recordar', 'sin_personal', 'con_personal', 'NA_personal', 'sin_comunicarse', 'con_comunicarse', 'NA_comunicarse'), 'test', 'another')
mun_dificultades = melt(mun_dificultades, id.vars = 'Municipio', variable.name = 'Dificultades', value.name = 'Frecuencia')
mun_freq_diff_prom <- aggregate(Frecuencia~Municipio,mun_dificultades, FUN=mean)
mun_freq_diff_sum <- aggregate(Frecuencia~Municipio, mun_dificultades, FUN=sum)
mun_tipo_diff_prom <- aggregate(Frecuencia~Dificultades, mun_dificultades, FUN=mean)
mun_tipo_diff_sum <- aggregate(Frecuencia~Dificultades, mun_dificultades, FUN=sum)
mun_tipo_diff_prom <- remove_rows(mun_tipo_diff_prom, "^NA", "^sin", mun_tipo_diff_prom$Dificultades)
mun_tipo_diff_prom <- mun_tipo_diff_prom[!grepl("^sin", mun_tipo_diff_prom$Dificultades),]
mun_tipo_diff_prom <- mun_tipo_diff_prom[!grepl("^NA", mun_tipo_diff_prom$Dificultades),]
mun_tipo_diff_sum <- mun_tipo_diff_sum[!grepl("^sin", mun_tipo_diff_sum$Dificultades),]
mun_tipo_diff_sum <- mun_tipo_diff_sum[!grepl("^NA", mun_tipo_diff_sum$Dificultades),]

mun_mujeres_c_hijos <- aggregate(carmun_mujeres_hijos_15_nacidos$frecuencia_hijos, by=list(Municipio=carmun_mujeres_hijos_15_nacidos$Municipio), FUN = sum)
mun_mujeres_c_hijos$total_mujeres_fertiles <- car_mun$`Total de mujeres en edad fértil`
mun_mujeres_c_hijos <- mun_mujeres_c_hijos %>% rename(mujeres_hij_nacidos = x)

#Bloque de funciones aplicadas a educacion_municipio
edmun_nivel_educativo = melting(edu_mun, 3:11, c('Codigo', 'Municipio', 'Ninguno', 'Preprimaria', 'Primaria_1_3', 'Primaria_4_5', 'Primaria_6', 'Basico', 'Diversificado', 'Licenciatura', 'Maestria_doctorado'), 'Nivel_Educacion', 'frecuencia_educacion_nivel')
edmun_causa_inasistencia = melting(edu_mun, 12:20, c('Codigo', 'Municipio', 'Dinero', 'Trabajo', 'No_hay_institucion', 'Padres_Pareja', 'Quehaceres', 'No_Gusta', 'Terminados', 'Otras', 'ND'), 'Razon_Inasistencia', 'frecuencia_inasistencia')
edmun_alfabetismo = melting(edu_mun, 22:23, c('Codigo', 'Municipio', 'Alfabeta', 'Analfabeta'), 'Educacion', 'frecuencia_educacion')
edmun_asistencia = melting(edu_mun, 24:25, c('Codigo', 'Municipio', 'Asiste', 'No_asiste'), 'Asistencia', 'frecuencia_asistencia')
edmun_lugar_estudio = melting(edu_mun, 26:29, c('Codigo', 'Municipio', 'Mismo_dep', 'Otro_dep', 'Otro_pais', 'ND'), 'Lugar_estudio', 'frecuencia_lugar')


mun_edu_tec <- merge(edmun_alfabetismo, tecmun_celular, by="Municipio")
mun_edu_tec <- merge(mun_edu_tec, tecmun_pc, by="Municipio")
mun_edu_tec <- merge(mun_edu_tec, tecmun_internet, by="Municipio")
mun_edu_tec <- mun_edu_tec[!grepl("ND", mun_edu_tec$Celular),]
mun_edu_tec <- mun_edu_tec[!grepl("^Usa", mun_edu_tec$Celular),]
mun_edu_tec <- mun_edu_tec[!grepl("ND", mun_edu_tec$PC),]
mun_edu_tec <- mun_edu_tec[!grepl("^Usa", mun_edu_tec$PC),]
mun_edu_tec <- mun_edu_tec[!grepl("ND", mun_edu_tec$Internet),]
mun_edu_tec <- mun_edu_tec[!grepl("^Usa", mun_edu_tec$Internet),]
mun_edu_tec <- mun_edu_tec[!grepl("Alfabeta", mun_edu_tec$Educacion),]
mun_edu_tec <- mun_edu_tec[c('Municipio','frecuencia_educacion', 'frecuencia_uso_cel','frecuencia_uso_pc', 'frecuencia_uso_inter')]
colnames(mun_edu_tec) <- c('Departamento', 'Analfabetas', 'No_Celular', 'No_Pc', 'No_Inter')
#Bloque de funciones aplicadas a empleo_municipal
empleo_mun = melting(emp_mun, 4:8, c('Codigo', 'Municipio', 'Poblacion_Activa', 'Poblacion_Ocupada', 'Cesante', 'Aspirante', 'ND'), 'Estado', 'frecuencia_estado')

#Bloque de funciones aplicadas a hogares_municipal
hogmun_hogares = melting(hogar_mun, 3:4, c('Codigo', 'Municipio', 'Urbana', 'Rural'), 'Area', 'distribucion')
hogmun_personas_hogar = melting(hogar_mun, 5:6, c('Codigo', 'Municipio', 'Hogar', 'Dormitorio'), 'Por', 'Promedio')

#Bloque de funciones aplicadas a poblacion_municipal
pobmun_sexo_mun = melting(poblacion_mun, 4:5, c('Codigo', 'Municipio', 'Hombre', 'Mujer'), 'Sexo', 'frecuencia_sexo')
pobmun_edad_general = melting(poblacion_mun, 6:10, c('Codigo', 'Municipio', '0-14', '15-29', '30-64', '65-84', '85+'), 'Edad', 'frecuencia_edad')
pobmun_edad_esp = melting(poblacion_mun, 10:31, c('Codigo', 'Municipio', '0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64','65-69', '70-74', '75-79', '80-84', '85-89', '90-94','95-99', '100+'), 'Edad', 'frecuencia_edad')
pobmun_parentesco_jefe = melting(poblacion_mun, 34:44, c('Codigo', 'Municipio', 'Jefe', 'Pareja', 'Hijo/a', 'Nuera-Yerno', 'Nietos', 'Hermanos', 'Padres', 'Suegros', 'Cuniado', 'Otro', 'No_pariente'), 'Relacion', 'frecuencia_relacion')
pobmun_estado_civil = melting(poblacion_mun, 47:52, c('Codigo', 'Municipio', 'Soltero', 'Unido', 'Casado', 'Separado', 'Divorviado', 'Viudo'), 'Estado_Civil', 'frecuencia_civil')

mun_jefe <- pobmun_parentesco_jefe[grepl("Jefe", pobmun_parentesco_jefe$Relacion),]
mun_mantenidos <- pobmun_parentesco_jefe[grepl("No_pariente", pobmun_parentesco_jefe$Relacion),]
mun_jefe_manuten <- mun_jefe
mun_jefe$Total_poblacion <- poblacion_mun$`Total de personas`
mun_jefe_manuten <- merge(mun_jefe_manuten, mun_mantenidos, by="Municipio")

#Bloque de funciones aplicadas a tecnologia_municipal
tecmun_celular = melting(tec_mun, 4:6, c('Codigo', 'Municipio', 'Usa_Celular', 'No_Usa', 'ND'), 'Celular', 'frecuencia_uso_cel')
tecmun_pc = melting(tec_mun, 7:9, c('Codigo', 'Municipio', 'Usa', 'No_Usa', 'ND'), 'PC', 'frecuencia_uso_pc')
tecmun_internet = melting(tec_mun, 10:12, c('Codigo', 'Municipio', 'Usa', 'No_Usa', 'ND'), 'Internet', 'frecuencia_uso_inter')

#Bloque de funciones aplicadas a vivienda_municipal
vivmun_tipo = melting(viv_mun, 5:12, c('Codigo', 'Municipio', 'Total', 'Casa_Formal', 'Apartamento', 'Cuarto_vecindad','Rancho','Improvisada', 'Otro', 'No_Registrada'), 'Tipo', 'frecuencia_tipo')
vivmun_tipo_oc = melting(viv_mun, 13:16, c('Codigo', 'Municipio', 'Ocupada', 'Temporal', 'Desocupada', 'Rechazo'), 'Tipo', 'frecuencia_tipo')
vivmun_pared = melting(viv_mun, 17:27, c('Codigo', 'Municipio', 'Ladrillo','Block','Concreto','Adobe','Madera','Lamina','Bajareque','Palo','Material_desecho', 'Otro', 'ND'), 'Material', 'frecuencia_material')
vivmun_techo = melting(viv_mun, 28:35, c('Codigo', 'Municipio', 'Concreto','Lamina','Asbesto','Teja','Paja','Desecho','Otro','ND'), 'Material', 'frecuencia_material')
vivmun_piso = melting(viv_mun, 36:43, c('Codigo', 'Municipio', 'Ladrillo_ceramico','Ladrillo_cemento','Ladrillo_barro','Cemento','Vinil','Madera','Tierra','Otro'), 'Material', 'frecuencia_material')



skim(car_dep)
plot_correlation(na.omit(viv_dep), maxcat = 20L)

plot_boxplot(car_dep, by = "Departamento")


create_report(new_car_dep)



####------------ Test de Merge -----------------####
new_car_dep <- merge(cardep_lugar_nacimiento,cardep_lugar_residencia,by="Departamento")
new_car_dep <- merge(new_car_dep,cardep_dificultad_ver,by="Departamento")
new_car_dep <- merge(new_car_dep,cardep_dificultad_oir,by="Departamento")
new_car_dep <- merge(new_car_dep,cardep_dificultad_caminar,by="Departamento")
new_car_dep <- merge(new_car_dep,cardep_dificultad_recordar,by="Departamento")
new_car_dep <- merge(new_car_dep,cardep_dificultad_personal,by="Departamento")
new_car_dep <- merge(new_car_dep,cardep_dificultad_comunicarse,by="Departamento")
new_car_dep <- merge(new_car_dep,cardep_mujeres_hijos_15_nacidos,by="Departamento")
new_car_dep <- merge(new_car_dep,cardep_mujeres_hijos_15_sobre,by="Departamento")

