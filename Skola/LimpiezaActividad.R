
##Carga y limpieza para Uso

library(readr)
library(chron)

actividad<- read_csv("C:/Users/ACER/Downloads/heroku_0n98456g.actividad.csv")
anuncio <- read_csv("C:/Users/ACER/Desktop/NIEU-master/Skola_CDA_MongoExport_16Oct17/heroku_0n98456g.anuncio.csv")
tipoAnuncio <- read_csv("C:/Users/ACER/Desktop/NIEU-master/Skola_CDA_MongoExport_16Oct17/heroku_0n98456g.tipoAnuncio.csv")

anuncio<-anuncio[,-(1:12)]
actividad<-actividad[,-(1:23)]

#View(anuncio)
#View(actividad)


##Vamos a limpiar actividad

actividad<-subset(actividad,select=-c(5,6,8))
colnames(actividad)<-c("fecha_creacion","id","id_anuncio","id_usuario","fecha_update","tipo")

## fecha creacion, hago el split

actividad$fecha_creacion<-format(actividad$fecha_creacion,tz="America/Mexico_City")
actividad$hora_creacion <- sapply(strsplit(as.character(actividad$fecha_creacion), " "), "[", 2)
actividad$fecha_creacion <- sapply(strsplit(as.character(actividad$fecha_creacion), " "), "[", 1)
actividad$fecha_creacion<-as.Date(actividad$fecha_creacion)
actividad$hora_creacion<-chron(times=actividad$hora_creacion)

##Limpiar id_anuncio
actividad$id_anuncio<-substr(actividad$id_anuncio,9,18)

##Limpiar id_usuario
actividad$id_usuario<-substr(actividad$id_usuario,7,16)

#limpiar fecha update


actividad$fecha_update<-format(actividad$fecha_update,tz="America/Mexico_City")
actividad$hora_update <- sapply(strsplit(as.character(actividad$fecha_update), " "), "[", 2)
actividad$fecha_update <- sapply(strsplit(as.character(actividad$fecha_update), " "), "[", 1)
actividad$fecha_update<-as.Date(actividad$fecha_update)
actividad$hora_update<-chron(times=actividad$hora_update)



##Limpio anuncio

anuncio<-anuncio[,-c(15:27)]
anuncio<-subset(anuncio,select=-c(6,7,9,10))

#Cambio titulo
colnames(anuncio)<-c("fecha_creacion","id_anuncio","id_autor","id_estudiante","id_tipo","fecha_update","accion","aprobado","texto","fecha_entrega","titulo")

##fecha creacion
anuncio$fecha_creacion<-format(anuncio$fecha_creacion,tz="America/Mexico_City")
anuncio$hora_creacion <- sapply(strsplit(as.character(anuncio$fecha_creacion), " "), "[", 2)
anuncio$fecha_creacion <- sapply(strsplit(as.character(anuncio$fecha_creacion), " "), "[", 1)
anuncio$fecha_creacion<-as.Date(anuncio$fecha_creacion)


##id autor
anuncio$id_autor<-substr(anuncio$id_autor,7,16)

#id estudiante
anuncio$id_estudiante<-substr(anuncio$id_estudiante,13,22)

#id tipo
anuncio$id_tipo<-substr(anuncio$id_tipo,13,22)


#fecha update

##fecha update
anuncio$fecha_update<-format(anuncio$fecha_update,tz="America/Mexico_City")
anuncio$hora_update <- sapply(strsplit(as.character(anuncio$fecha_update), " "), "[", 2)
anuncio$fecha_update <- sapply(strsplit(as.character(anuncio$fecha_update), " "), "[", 1)
anuncio$fecha_update<-as.Date(anuncio$fecha_update)


##fecha entrega
anuncio$fecha_entrega<-format(anuncio$fecha_entrega,tz="America/Mexico_City")
anuncio$hora_entrega <- sapply(strsplit(as.character(anuncio$fecha_entrega), " "), "[", 2)
anuncio$fecha_entrega <- sapply(strsplit(as.character(anuncio$fecha_entrega), " "), "[", 1)
anuncio$fecha_entrega<-as.Date(anuncio$fecha_entrega)


##Ahora vemos que pedo con los tipos de Anuncios
tipoAnuncio$nombre[is.na(tipoAnuncio$nombre)] <- "Sin tipo"

anuncio<-merge(anuncio,tipoAnuncio, by.x = "id_tipo",by.y = "_id")


