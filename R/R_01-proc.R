####TAREA 1####

#cargar y llamar paquetes
pacman::p_load(haven, tidyverse, sjmisc, sjPlot)

#carga de datos

#ENE 2019
datos = read_dta("input/data/ano-2019.dta")

#ENE 2021
data = read_dta("input/data/ano-2021.dta")

#Exploración variables ENE 2019
table(datos$mes_central)
frq(datos$mes_central)
table(datos$cine)
frq(datos$cine)
table(datos$sexo)
frq(datos$sexo)
class(datos$cine)
class(datos$edad)

#Exploración variables ENE 2021
table(data$mes_central)
frq(data$mes_central)
table(data$cine)
frq(data$cine)
table(data$sexo)
frq(data$sexo)
class(data$cine)
class(data$edad)


#Codificación de sexo, filtro de edad, selección de variables, clasificación de
#nivel educacional, definición de tramos etarios y selección de tiempo octubre-diciembre (ENE 2019)
datos$sexo = factor(datos$sexo, levels = c(1:2), labels = c("Hombre", "Mujer"))
proc_19 = datos%>%
      mutate(cine = case_when(cine>=1 & cine<=3~"Basica o menos incompleta",
                              cine>=4 & cine<=5~"Media y basica completa",
                              cine>=6 & cine<=9~"Superior completa"))%>%
      mutate(edad = case_when(edad>=15 & edad<=39~"15 a 39 años",
                              edad>=40 & edad<=64~"40 a 64 años",
                              edad>=65~"65 años y mas"))%>%
      select(ano_trimestre, mes_central, edad, cae_general, 
      cae_especifico, activ, sexo, cine, c2_1_1, c2_1_3)%>%
      filter(mes_central == 11, edad>=15)%>%mutate_if(is.labelled, ~(forcats::as_factor(.)))

      
#Codificación de sexo, filtro de edad, selección de variables, clasificación de
#nivel educacional, definición de tramos etarios y selección de tiempo octubre-diciembre (ENE 2021)     
data$sexo = factor(data$sexo, levels = c(1:2), labels = c("Hombre", "Mujer"))  
proc_21 = data%>%
      mutate(cine = case_when(cine>=1 & cine<=3~"Basica o menos incompleta",
                              cine>=4 & cine<=5~"Media y basica completa",
                              cine>=6 & cine<=9~"Superior completa"))%>%
      mutate(edad = case_when(edad>=15 & edad<=39~"15 a 39 años",
                              edad>=40 & edad<=64~"40 a 64 años",
                              edad>=65~"65 años y mas"))%>%
      select(ano_trimestre, mes_central, edad, cae_general, 
      cae_especifico, activ, sexo, cine, c2_1_1, c2_1_3)%>%
      filter(mes_central == 11, edad>=15)%>%mutate_if(is.labelled, ~(forcats::as_factor(.)))


#1 Análisis de datos a través de tablas (edad, sexo, nivel educacional)

##2019##
prop.table(table(proc_19$cine))*100
frq(proc_19$cine)

prop.table(table(proc_19$edad))*100
frq(proc_19$edad)

prop.table(table(proc_19$sexo))*100
frq(proc_19$sexo)

##2021##
prop.table(table(proc_21$cine))*100
frq(proc_21$cine)

prop.table(table(proc_21$edad))*100
frq(proc_21$edad)

prop.table(table(proc_21$sexo))*100
frq(proc_21$sexo)


#2 Análisis de datos a través de tablas (condición de actividad económica, 
#situación de empleo y la suma de horas trabajadas)

##2019##
prop.table(table(proc_19$cae_general))*100
frq(proc_19$cae_general)

prop.table(table(proc_19$cae_especifico))*100
frq(proc_19$cae_especifico)

prop.table(table(proc_19$activ))*100
frq(proc_19$activ)

prop.table(table(proc_19$c2_1_1))*100
frq(proc_19$c2_1_1)

prop.table(table(proc_19$c2_1_3))*100
frq(proc_19$c2_1_3)

##2021##
prop.table(table(proc_21$cae_general))*100
frq(proc_21$cae_general)

prop.table(table(proc_21$cae_especifico))*100
frq(proc_21$cae_especifico)

prop.table(table(proc_21$activ))*100
frq(proc_21$activ)

prop.table(table(proc_21$c2_1_1))*100
frq(proc_21$c2_1_1)

prop.table(table(proc_21$c2_1_3))*100
frq(proc_21$c2_1_3)


#Tablas de información de datos ya filtrados
sjPlot::view_df(proc_19)
sjPlot::view_df(proc_21)


#Unir ambos objetos (datos filtrados de ENE 2019 y 2021) en uno solo
ene_unida = merge(proc_19, proc_21, all = T)

#Tabla ene_unida
sjPlot::view_df(ene_unida)

#ASignacion NA
ene_unida$cine <- na_if(ene_unida$cine, 999)
ene_unida$c2_1_1 <- na_if(ene_unida$c2_1_1, "No sabe")
ene_unida$c2_1_1 <- na_if(ene_unida$c2_1_1, "No responde")
ene_unida$c2_1_3 <- na_if(ene_unida$c2_1_3, "No sabe")
ene_unida$c2_1_3 <- na_if(ene_unida$c2_1_3, "No responde")

#Tablas ene_unida
prop.table(table(ene_unida$cae_general))*100
frq(ene_unida$cae_general)

prop.table(table(ene_unida$cae_especifico))*100
frq(ene_unida$cae_especifico)

prop.table(table(ene_unida$activ))*100
frq(ene_unida$activ)

prop.table(table(ene_unida$c2_1_1))*100
frq(ene_unida$c2_1_1)

prop.table(table(ene_unida$c2_1_3))*100
frq(ene_unida$c2_1_3)


#Guardar dataset
save(proc_19, proc_21, ene_unida, file = "output/data/datos_proc.RData")

saveRDS(ene_unida, file = "output/data/datos_proc.rds")

saveRDS(datos, file = "output/data/ene-2019")

saveRDS(data, file = "output/data/ene-2021")
