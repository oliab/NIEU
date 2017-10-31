
## Script para carga de datos
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


estudiantes <- read_csv("~/Documents/NIEU/Skola_CDA_MongoExport_16Oct17/heroku_0n98456g.Estudiantes.csv")
usuarios <- read_csv("~/Documents/NIEU/Skola_CDA_MongoExport_16Oct17/heroku_0n98456g._User.csv")
acceso<-read_csv("~/Documents/NIEU/Skola_CDA_MongoExport_16Oct17/heroku_0n98456g.Acceso.csv")


usuarios<-usuarios[,20:ncol(usuarios)]
#de accesos hay que dividir p_student

acceso$id_estudiante<-substr(acceso$`_p_student`,13,nchar(acceso$`_p_student`))
acceso$id_usuario<-substr(acceso$`_p_user`,7,nchar(acceso$`_p_user`))

##Hay que dividir en fecha y hora en acceso
acceso$escaneoOcurrido<-format(acceso$escaneoOcurrido,tz="America/Mexico_City")

acceso$dia <- sapply(strsplit(as.character(acceso$escaneoOcurrido), " "), "[", 1)
acceso$hora <- sapply(strsplit(as.character(acceso$escaneoOcurrido), " "), "[", 2)

acceso$dia<-as.Date(acceso$dia)
acceso$hora<-chron(times=acceso$hora)
##Siguientesetiquetar como entradas y salidas

acceso$operacion<-"1"
i<-1

for (i in 1:length(acceso$hora)){
  if (is.na(acceso$hora[i])){
    acceso$hora[i]<-NA
  } else {
    if (acceso$hora[i]<"12:00:00"){
      acceso$operacion[i]<-"entrada"
      i<-i+1
    } else {
      acceso$operacion[i]<-"salida"
      i<-i+1
    }
  }
}


## Limpiando la parte de la puntualidad

i<-1

for (i in 1:nrow(acceso)){
  if (is.na(acceso$puntualidad[i])){
    acceso$puntualidad[i]<-NA
    i<-i+1
  } else {
    if (acceso$puntualidad[i]==0){
      acceso$puntualidad[i]<-"Ok"
      i<-i+1
    } else {
      if (acceso$puntualidad[i]==1) {
        acceso$puntualidad[i]<-"Tolerancia"
        i<-i+1
      } else{
        if (acceso$puntualidad[i]==2){
          acceso$puntualidad[i]<-"Tarde"
          i<-i+1
        } else {
          if (acceso$puntualidad[i]==9){
            acceso$puntualidad[i]<-"Nose"
            i<-i+1
          } else {
            acceso$puntualidad[i]<-"Temprano"
            i<-i+1
          }
        }
        
      }
    }
  }
}

## Vamos a ver qué dia de la semana es
acceso$dia_semana<-weekdays(acceso$dia)


##Hay que borrar lo que se hace de prueba

domingos<-acceso %>% filter(dia_semana=="Sunday")
acceso<-acceso %>% filter(dia_semana!="Sunday") ## Estoy quitando todos los que están en domingo

 ## Ahora busco a los usuarios de prueba

lista<-(c(grep("Prueba", usuarios$username),grep("prueba", usuarios$username)))

for (i in 1:length(lista)){
  id_lista[i]<-usuarios$`_id`[i]
  i<-i+1
}

acceso<- subset(acceso, !(acceso$id_usuario %in% id_lista))



###Hay que pegar los accesos con los datos de usuario y alumnos

acceso<-merge(acceso, estudiantes, by.x="id_estudiante", by.y = "_id")
acceso<-merge(prueba, usuarios, by.x="id_usuario", by.y = "_id")

##Agregando las entradas y salidas por tiempo






