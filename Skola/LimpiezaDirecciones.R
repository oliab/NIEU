
library(ggmap)
library(qdap)

###primero hay que agregarle el mex a los direcciones
i<-1
for (i in 1:nrow(usuarios)){
  if (is.na(usuarios$domicilio[i])){
    usuarios$domicilio[i]<-NA
    i<-i+1
  } else {
    usuarios$domicilio[i]<-paste(usuarios$domicilio[i],", Mexico", sep="")
    i<-i+1
  }
}

usuarios$lon<-1
usuarios$lat<-1
usuarios$geoAddress<-1

i<-1
for(i in 1:nrow(usuarios)){
  # Print("Working...")
  if(is.na(usuarios$domicilio[i])){
    usuarios$lon[i] <- NA
    usuarios$lat[i] <- NA
    usuarios$geoAddress[i] <- NA
    i<-i+1
    
  } else {
    result <- geocode(usuarios$domicilio[i], output = "latlona", source = "google")
    if (is.na(result)){
      usuarios$lon[i] <- "error"
      usuarios$lat[i] <- "error"
      usuarios$geoAddress[i] <- "error"
      i
      i<-i+1
    } else {
      usuarios$lon[i] <- as.numeric(result[1])
      usuarios$lat[i] <- as.numeric(result[2])
      usuarios$geoAddress[i] <- as.character(result[3])
      i<-i+1
    }
  }
}




###vamos a ver si podemos encontrar nuevas  direcciones

usuarios$longitud<-1
i<-1
for (i in 1:nrow(usuarios)){
  if (is.na(usuarios$lon[i])){
    i<-i+1
  } else {
    punto<-gregexpr("[.]",usuarios$lon[i])[[1]][1]
    usuarios$longitud[i]<-substr(usuarios$lon[i],2, punto-1)
    i<-i+1
  }
  
}

prueba_latlon<-subset(usuarios, usuarios$longitud!="99")

prueba<-prueba_latlon
prueba<-prueba[,c(1:ncol(prueba)-1)]

##Primero las de infonavit san grancisco

i<-1
prueba$nuevo_dom<-1
infonavit<-as.list(grep("Inf", prueba$domicilio))
san<-grep("San", prueba$domicilio)
colonia<-grep("Col", prueba$domicilio)
fraccionamiento<-grep("Fracc", prueba$domicilio)
cod_pos<-grep("Cp", prueba$domicilio)
####

castaño<-grep("Castaño", prueba$domicilio)
asuncion<-grep("Asunci", prueba$domicilio)
virgen<-grep("La Virgen", prueba$domicilio)
providencia<-grep("Providencia", prueba$domicilio)
izcalli<-grep("Izcall", prueba$domicilio)

for (i in 1:nrow(prueba)){
  if (i %in% infonavit){
    prueba$nuevo_dom[i]<-"Infonavit San Francisco, Metepec, Estado de Mexico"
    i<-i+1
  } else {
    
    i<-i+1
  }
} 

###
i<-1
for (i in 1:nrow(prueba)){
  if (i %in% san){
    posicion<-gregexpr(pattern="San", prueba$domicilio[i])[[1]][1]
    #posicion_mex<-posicion<-gregexpr(pattern=", Mexico", prueba$domicilio[i])[[1]][1]
    nombre<-substr(prueba$domicilio[i],posicion,nchar(prueba$domicilio[i])-8)
    mexico<-list("Mex.", "Mexico", "México")
    mexico<-paste(unlist(mexico), collapse = "|")
    nombre<-gsub(mexico, "", nombre)
    #nombre <- mgsub(mexico,"",nombre)
    nombre<-paste(nombre,", Estado de Mexico", sep="")
    
    prueba$nuevo_dom[i]<-nombre
    i<-i+1
  } else {
    
    i<-i+1
  }
} 

####
###
i<-1
for (i in 1:nrow(prueba)){
  if (i %in% colonia){
    cols<-c("Col","col")
    posicion<-gregexpr(pattern=paste(cols,collapse = "|"), prueba$domicilio[i])[[1]][1]
    #posicion_mex<-posicion<-gregexpr(pattern=", Mexico", prueba$domicilio[i])[[1]][1]
    nombre<-substr(prueba$domicilio[i],posicion,nchar(prueba$domicilio[i])-8)
    mexico<-list("Mex.", "Mexico", "México")
    mexico<-paste(unlist(mexico), collapse = "|")
    nombre<-gsub(mexico, "", nombre)
    #nombre <- mgsub(mexico,"",nombre)
    nombre<-paste(nombre,", Estado de Mexico", sep="")
    
    prueba$nuevo_dom[i]<-nombre
    i<-i+1
  } else {
    
    i<-i+1
  }
} 

####
###
i<-1
for (i in 1:nrow(prueba)){
  if (i %in% fraccionamiento){
    posicion<-gregexpr(pattern="Fracc", prueba$domicilio[i])[[1]][1]
    #posicion_mex<-posicion<-gregexpr(pattern=", Mexico", prueba$domicilio[i])[[1]][1]
    nombre<-substr(prueba$domicilio[i],posicion,nchar(prueba$domicilio[i])-8)
    mexico<-list("Mex.", "Mexico", "México")
    mexico<-paste(unlist(mexico), collapse = "|")
    nombre<-gsub(mexico, "", nombre)
    #nombre <- mgsub(mexico,"",nombre)
    nombre<-paste(nombre,", Estado de Mexico", sep="")
    
    prueba$nuevo_dom[i]<-nombre
    i<-i+1
  } else {
    
    i<-i+1
  }
} 

###
i<-1
for (i in 1:nrow(prueba)){
  if (i %in% cod_pos){
    posicion<-gregexpr(pattern="Cp", prueba$domicilio[i])[[1]][1]
    #posicion_mex<-posicion<-gregexpr(pattern=", Mexico", prueba$domicilio[i])[[1]][1]
    nombre<-substr(prueba$domicilio[i],posicion,nchar(prueba$domicilio[i])-8)
    mexico<-list("Mex.", "Mexico", "México")
    mexico<-paste(unlist(mexico), collapse = "|")
    nombre<-gsub(mexico, "", nombre)
    #nombre <- mgsub(mexico,"",nombre)
    nombre<-paste(nombre,", Estado de Mexico", sep="")
    
    prueba$nuevo_dom[i]<-nombre
    i<-i+1
  } else {
    
    i<-i+1
  }
} 

###################################### AQUÍ HAGO DE NUEVO EL GEOCODING SOBRE LOS NUEVOS DOMICILIOS
i<-1
for(i in 1:nrow(prueba)){
  # Print("Working...")
  if(prueba$nuevo_dom[i]==1){
    prueba$lon[i] <- NA
    prueba$lat[i] <- NA
    prueba$geoAddress[i] <- NA
    i<-i+1
    
  } else {
    result <- geocode(prueba$nuevo_dom[i], output = "latlona", source = "google")
    if (is.na(result)){
      prueba$lon[i] <- "error"
      prueba$lat[i] <- "error"
      prueba$geoAddress[i] <- "error"
      i
      i<-i+1
    } else {
      prueba$lon[i] <- as.numeric(result[1])
      prueba$lat[i] <- as.numeric(result[2])
      prueba$geoAddress[i] <- as.character(result[3])
      i<-i+1
    }
  }
}



####gUARDO


