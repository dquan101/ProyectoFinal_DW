library(dplyr)
library(tidyverse)
library(readxl)
library(reshape2)

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


melting <- function(old_frame, col_range, col_names, var_name, val_name) {
  new_frame = old_frame[c(1,2,col_range)]
  colnames(new_frame) <- col_names
  new_frame = melt(new_frame, id.vars=c('Codigo', 'Departamento'), variable.name = var_name, value.name = val_name)
  return (new_frame)
}

#Bloque de funciones aplicadas a caracteristicas_departamento
cardep_lugar_nacimiento = melting(car_dep, 3:7, c('Codigo', 'Departamento', 'Mismo', 'Otro', 'Otro_pais', 'ND'), 'lugar_nacimiento', 'frecuencia_nac')
cardep_lugar_residencia = melting(car_dep, 8:12, c('Codigo', 'Departamento', 'No_Nacido', 'Mismo', 'Otro', 'Otro_pais', 'ND'), 'lugar_residencia_2013', 'frecuencia_res')
cardep_dificultad_ver = melting(car_dep, 14:16, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_ver', 'frecuencia_ver')
cardep_dificultad_oir = melting(car_dep, 17:19, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_oir', 'frecuencia_oir')
cardep_dificultad_caminar = melting(car_dep, 20:22, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_caminar', 'frecuencia_caminar')
cardep_dificultad_recordar = melting(car_dep, 23:25, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_recordar','frecuencia_recordar')
cardep_dificultad_personal = melting(car_dep, 26:28, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_personal', 'frecuencia_personal')
cardep_dificultad_comunicarse = melting(car_dep, 29:31, c('Codigo', 'Departamento', 'Sin', 'Con', 'ND'), 'dificultad_comunicarse', 'frecuencia_comunicarse')
cardep_mujeres_hijos_15_nacidos = melting(car_dep, 33:39, c('Codigo', 'Departamento', '0_hijo','1_hijo','2_hijo','3_hijo','4_hijo', '5_omas', 'ND'), 'mujeres_hijos_nacidos', 'frecuencia_hijos')
cardep_mujeres_hijos_15_sobre = melting(car_dep, 40:45, c('Codigo', 'Departamento', '0_hijo','1_hijo','2_hijo','3_hijo','4_hijo', '5_omas'), 'mujeres_hijos_sobrevivientes', 'frecuencia_hijos')

#Bloque de funciones aplicadas a educacion_departamento

eddep_nivel_educativo = melting(edu_dep, 3:11, c('Codigo', 'Departamento', 'Ninguno', 'Preprimaria', 'Primaria_1_3', 'Primaria_4_5', 'Primaria_6', 'Basico', 'Diversificado', 'Licenciatura', 'Maestria_doctorado'), 'Nivel_Educacion', 'frecuencia_educacion')
eddep_causa_inasistencia = melting(edu_dep, 12:20, c('Codigo', 'Departamento', 'Dinero', 'Trabajo', 'No_hay_institucion', 'Padres_Pareja', 'Quehaceres', 'No_Gusta', 'Terminados', 'Otras', 'ND'), 'Razon_Inasistencia', 'frecuencia_inasistencia')
eddep_alfabetismo = melting(edu_dep, 22:23, c('Codigo', 'Departamento', 'Alfabeta', 'Analfabeta'), 'Educacion', 'frecuencia_educacion')
eddep_asistencia = melting(edu_dep, 24:25, c('Codigo', 'Departamento', 'Asiste', 'No_asiste'), 'Asistencia', 'frecuencia_asistencia')
eddep_lugar_estudio = melting(edu_dep, 26:29, c('Codigo', 'Departamento', 'Mismo_dep', 'Otro_dep', 'Otro_pais', 'ND'), 'Lugar_estudio', 'frecuencia_lugar')


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

#Bloque de funciones aplicadas a pueblo_departamental
puebdep_pertenencia = melting(puebo_dep, 4:9, c('Codigo', 'Departamento', 'Maya', 'Garifuna', 'Xinka', 'Afro', 'Ladino', 'Extranjero'), 'Pueblo', 'frecuencia_pueblo')
puebdep_lengua = melting(puebo_dep, 10:31, c('Codigo', 'Departamento', 'Achi', 'Akateka', 'Awakateka', 'Chorti', 'Chalchiteka', 'Chuj', 'Itza', 'Ixil', 'Popti', 'Kiche', 'Kaqchiquel', 'Mam', 'Mopan', 'Poqomam', 'Poqomchi', 'Qanjobal', 'Qeqchi', 'Sakapulteka', 'Sipakapense', 'Tektiteka', 'Tzutujil', 'Uspanteka'), 'Lengua', 'frecuencia_lengua')
puebdep_lengua_aprendido = melting(puebo_dep, 32:54, c('Codigo', 'Departamento', 'Achi', 'Akateka', 'Awakateka', 'Chorti', 'Chalchiteka', 'Chuj', 'Itza', 'Ixil', 'Popti', 'Kiche', 'Kaqchiquel', 'Mam', 'Mopan', 'Poqomam', 'Poqomchi', 'Qanjobal', 'Qeqchi', 'Sakapulteka', 'Sipakapense', 'Tektiteka', 'Tzutujil', 'Uspanteka', 'No_habla'), 'Aprendio_lengua', 'frecuencia_lengua')

#Bloque de funciones aplicadas a tecnologia_departamental
tecdep_celular = melting(tec_dep, 4:6, c('Codigo', 'Departamento', 'Usa_Celular', 'No_Usa', 'ND'), 'Celular', 'frecuencia_uso')
tecdep_pc = melting(tec_dep, 7:9, c('Codigo', 'Departamento', 'Usa', 'No_Usa', 'ND'), 'PC', 'frecuencia_uso')
tecdep_internet = melting(tec_dep, 10:12, c('Codigo', 'Departamento', 'Usa', 'No_Usa', 'ND'), 'Internet', 'frecuencia_uso')

#Bloque de funciones aplicadas a vivienda_departamental
vivdep_tipo = melting(viv_dep, 5:12, c('Codigo', 'Departamento', 'Total', 'Casa_Formal', 'Apartamento', 'Cuarto_vecindad','Rancho','Improvisada', 'Otro', 'No_Registrada'), 'Tipo', 'frecuencia_tipo')
vivdep_tipo_oc = melting(viv_dep, 13:16, c('Codigo', 'Departamento', 'Ocupada', 'Temporal', 'Desocupada', 'Rechazo'), 'Tipo', 'frecuencia_tipo')
vivdep_pared = melting(viv_dep, 17:27, c('Codigo', 'Departamento', 'Ladrillo','Block','Concreto','Adobe','Madera','Lamina','Bajareque','Palo','Material_desecho', 'Otro', 'ND'), 'Material', 'frecuencia_material')
vivdep_techo = melting(viv_dep, 28:35, c('Codigo', 'Departamento', 'Concreto','Lamina','Asbesto','Teja','Paja','Desecho','Otro','ND'), 'Material', 'frecuencia_material')
vivdep_piso = melting(viv_dep, 36:43, c('Codigo', 'Departamento', 'Ladrillo_ceramico','Ladrillo_cemento','Ladrillo_barro','Cemento','Vinil','Madera','Tierra','Otro'), 'Material', 'frecuencia_material')


####---------------- Municipios -----------------------####
car_mun = clean_up('ProyectoFinal/caracteristicas_municipal.xlsx')
edu_mun = clean_up('ProyectoFinal/educacion_municipal.xlsx')
emp_mun = clean_up('ProyectoFinal/empleo_municipal.xlsx')
hogar_mun = clean_up('ProyectoFinal/hogares_municipal.xlsx')
poblacion_mun = clean_up('ProyectoFinal/poblacion_municipal.xlsx')
#puebo_mun = clean_up('ProyectoFinal/pueblo_municipal.xlsx')
tec_mun = clean_up('ProyectoFinal/tecnologia_municipal.xlsx')
viv_mun = clean_up('ProyectoFinal/vivienda_municipal.xlsx')

melting <- function(old_frame, col_range, col_names, var_name, val_name) {
  new_frame = old_frame[c(1,2,col_range)]
  colnames(new_frame) <- col_names
  new_frame = melt(new_frame, id.vars=c('Codigo', 'Municipio'), variable.name = var_name, value.name = val_name)
  return (new_frame)
}

#Bloque de funciones aplicadas a caracteristicas_municipio
carmun_lugar_nacimiento = melting(car_mun, 3:7, c('Codigo', 'Municipio', 'Mismo', 'Otro', 'Otro_pais', 'ND'), 'lugar_nacimiento', 'frecuencia_nac')
carmun_lugar_residencia = melting(car_mun, 8:12, c('Codigo', 'Municipio', 'No_Nacido', 'Mismo', 'Otro', 'Otro_pais', 'ND'), 'lugar_residencia_2013', 'frecuencia_res')
carmun_dificultad_ver = melting(car_mun, 14:16, c('Codigo', 'Municipio', 'Sin', 'Con', 'ND'), 'dificultad_ver', 'frecuencia_ver')
carmun_dificultad_oir = melting(car_mun, 17:19, c('Codigo', 'Municipio', 'Sin', 'Con', 'ND'), 'dificultad_oir', 'frecuencia_oir')
carmun_dificultad_caminar = melting(car_mun, 20:22, c('Codigo', 'Municipio', 'Sin', 'Con', 'ND'), 'dificultad_caminar', 'frecuencia_caminar')
carmun_dificultad_recordar = melting(car_mun, 23:25, c('Codigo', 'Municipio', 'Sin', 'Con', 'ND'), 'dificultad_recordar','frecuencia_recordar')
carmun_dificultad_personal = melting(car_mun, 26:28, c('Codigo', 'Municipio', 'Sin', 'Con', 'ND'), 'dificultad_personal', 'frecuencia_personal')
carmun_dificultad_comunicarse = melting(car_mun, 29:31, c('Codigo', 'Municipio', 'Sin', 'Con', 'ND'), 'dificultad_comunicarse', 'frecuencia_comunicarse')
carmun_mujeres_hijos_15_nacidos = melting(car_mun, 33:39, c('Codigo', 'Municipio', '0_hijo','1_hijo','2_hijo','3_hijo','4_hijo', '5_omas', 'ND'), 'mujeres_hijos_nacidos', 'frecuencia_hijos')
carmun_mujeres_hijos_15_sobre = melting(car_mun, 40:45, c('Codigo', 'Municipio', '0_hijo','1_hijo','2_hijo','3_hijo','4_hijo', '5_omas'), 'mujeres_hijos_sobrevivientes', 'frecuencia_hijos')

#Bloque de funciones aplicadas a educacion_municipio
edmun_nivel_educativo = melting(edu_mun, 3:11, c('Codigo', 'Municipio', 'Ninguno', 'Preprimaria', 'Primaria_1_3', 'Primaria_4_5', 'Primaria_6', 'Basico', 'Diversificado', 'Licenciatura', 'Maestria_doctorado'), 'Nivel_Educacion', 'frecuencia_educacion')
edmun_causa_inasistencia = melting(edu_mun, 12:20, c('Codigo', 'Municipio', 'Dinero', 'Trabajo', 'No_hay_institucion', 'Padres_Pareja', 'Quehaceres', 'No_Gusta', 'Terminados', 'Otras', 'ND'), 'Razon_Inasistencia', 'frecuencia_inasistencia')
edmun_alfabetismo = melting(edu_mun, 22:23, c('Codigo', 'Municipio', 'Alfabeta', 'Analfabeta'), 'Educacion', 'frecuencia_educacion')
edmun_asistencia = melting(edu_mun, 24:25, c('Codigo', 'Municipio', 'Asiste', 'No_asiste'), 'Asistencia', 'frecuencia_asistencia')
edmun_lugar_estudio = melting(edu_mun, 26:29, c('Codigo', 'Municipio', 'Mismo_dep', 'Otro_dep', 'Otro_pais', 'ND'), 'Lugar_estudio', 'frecuencia_lugar')

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

#Bloque de funciones aplicadas a tecnologia_municipal
tecmun_celular = melting(tec_mun, 4:6, c('Codigo', 'Municipio', 'Usa_Celular', 'No_Usa', 'ND'), 'Celular', 'frecuencia_uso')
tecmun_pc = melting(tec_mun, 7:9, c('Codigo', 'Municipio', 'Usa', 'No_Usa', 'ND'), 'PC', 'frecuencia_uso')
tecmun_internet = melting(tec_mun, 10:12, c('Codigo', 'Municipio', 'Usa', 'No_Usa', 'ND'), 'Internet', 'frecuencia_uso')

#Bloque de funciones aplicadas a vivienda_municipal
vivmun_tipo = melting(viv_mun, 5:12, c('Codigo', 'Municipio', 'Total', 'Casa_Formal', 'Apartamento', 'Cuarto_vecindad','Rancho','Improvisada', 'Otro', 'No_Registrada'), 'Tipo', 'frecuencia_tipo')
vivmun_tipo_oc = melting(viv_mun, 13:16, c('Codigo', 'Municipio', 'Ocupada', 'Temporal', 'Desocupada', 'Rechazo'), 'Tipo', 'frecuencia_tipo')
vivmun_pared = melting(viv_mun, 17:27, c('Codigo', 'Municipio', 'Ladrillo','Block','Concreto','Adobe','Madera','Lamina','Bajareque','Palo','Material_desecho', 'Otro', 'ND'), 'Material', 'frecuencia_material')
vivmun_techo = melting(viv_mun, 28:35, c('Codigo', 'Municipio', 'Concreto','Lamina','Asbesto','Teja','Paja','Desecho','Otro','ND'), 'Material', 'frecuencia_material')
vivmun_piso = melting(viv_mun, 36:43, c('Codigo', 'Municipio', 'Ladrillo_ceramico','Ladrillo_cemento','Ladrillo_barro','Cemento','Vinil','Madera','Tierra','Otro'), 'Material', 'frecuencia_material')
