setwd("d:/Documents/UFRGS/PROYECTO MAESTRIA/histo_encuentros")
library(readxl)
separando_visitas <- read_excel("datos_sep_jan.xlsx")
View(separando_visitas)
tab_com <- separando_visitas

#SEPARANDO LAS COLUMNAS CON LAS QUE VOY A TRABAJAR
tab_lim <- subset(tab_com, select = c("adtAP","NEAR_FC") ) 
tab_lim
#LIMPIANDO LOS NOMBRES DE LOS DATOS
tab_lim$adtAP <- substring(tab_lim$adtAP, 1, 10)
tab_lim$NEAR_FC <- substring(tab_lim$NEAR_FC, 3, 11)

#ELIMINAMOS LOS DUPLICADOS
tab_l1 <- tab_lim[!duplicated(tab_lim),]

# AGRUPAR SEGUN LOS LOCAIS
tab_l2 <- split.data.frame(tab_l1, tab_l1$NEAR_FC)
tab_l3<- lapply(tab_l2,"[[","adtAP")


####reemplazar LOCAL_101 x LOCAL_112, LOCAL_206 x LOCAL_205 y LOCAL_302 x LOCAL_304 #EL RESERVATORIO ESTA INCLUIDO EN ZONA1 LOCAL107 Y105 EN EL 105 PASABA DOBLE CRUCE CON PORTOGRANDE

tab_l3$LOCAL_101 <- tab_l3$LOCAL_112
tab_l3$LOCAL_206 <- tab_l3$LOCAL_205
tab_l3$LOCAL_302 <- tab_l3$LOCAL_304
tab_l3$LOCAL_105 <- tab_l3$LOCAL_107
tab_l3

##separar por visitas
maxdata <- list() #lista para guardar maximo de visitas por mes


for(i in ( 1:length(tab_l3) )) {
  fechas <- tab_l3[[i]]
  mesano <- substring (fechas,1,7) # cortando solo ano y mes de cada fecha d elos locales
  
  tabl_fecha <- table(mesano) #tabla que cuenta cuantos tengo para cada mes
  print(tabl_fecha)
  
  for(j in names(tabl_fecha)) { 
    if (is.null(maxdata[[j]])){
      maxdata[[j]] <- 0  
    } 
    if (maxdata[[j]] < tabl_fecha[j]) {
      maxdata[[j]] <- tabl_fecha[j]  #para saber el maximo de visitas por mes
    }
    
  }
}

maxdata <- maxdata[order(names(maxdata))] #ordenar mesees desde maxdata
maxcols <- Reduce("+", maxdata)
#creando el data frame
tab_enc <- data.frame( matrix(0, nrow = length(tab_l3), ncol = (Reduce("+", maxdata))) ) #cree un data frame  para colocar los datos # n columnas con el maximo de visitas

colunames <- c ()
for (k in names(maxdata)) {
  for (l in 1:maxdata[[k]]) {
    colunames <- c(colunames, paste(k,"(",l,")", sep="")) #sep elimina especios 
  }
}          #para crear el nombre d elas columnas

colnames(tab_enc) <- colunames
rownames(tab_enc) <-  names(tab_l3)

### rellanar datos con 0 , 1 y .

for(i in ( 1:length(tab_l3) )) { #recorriendo el data frame tab_enc
  fechas <- tab_l3[[i]]
  mesano <- substring (fechas,1,7) # cortando solo ano y mes de cada fecha d elos locales
  
  tabl_fecha <- table(mesano) #tabla que cuenta cuantos tengo para cada mes
  recorre <- 1
  
  for(j in names(maxdata)) { #recorriendo por meses
    if (!is.na (tabl_fecha[j])){ # ! niega  entonces si no es na va a llenar con puntos y ceros
      zeros_tab <- tabl_fecha[j] #hallando la cantidad de ceros que voy a tener por mes
      puntos_tab <- maxdata[[j]] - zeros_tab #hallando la cantidad de puntos por mes
      for(k in rep(1, each =zeros_tab)) {  #colocando los ceros 
        tab_enc[i,recorre]<- "0"
        recorre <- recorre + 1
      }
      for(k in rep(1, each =puntos_tab)) { #colocando los puntos
        tab_enc[i,recorre]<- "."
        recorre <- recorre + 1
      } 
    }
    else { # si es na o sea no hay datos de un mes  va a llenar ese mes con puntos
      for(k in rep(1, each = maxdata[[j]])) { #colocando los puntos
        tab_enc[i,recorre]<- "."
        recorre <- recorre + 1
      } 
    }
    if (recorre > maxcols){print(zeros_tab)
      print(puntos_tab)
      print(recorre)
      print(j)
    }
  }
}

##### Agregando datos de encuentro (1)

encuentros <- read_excel("encuen2020.xlsx")

#SEPARANDO LAS COLUMNAS CON LAS QUE VOY A TRABAJAR
enc <- subset(encuentros, select = c("tipo_censo","NEAR_FC", "adate", "Tipo", "Especie", "ESTADO") )

enc_censo <- enc[enc$tipo_censo== "censo", ] #separamos solo los que son censo !extracenso

#LIMPIANDO LOS NOMBRES DE LOS DATOS
enc_censo$adate <- substring(enc_censo$adate, 1, 11)
enc_censo$NEAR_FC <- substring(enc_censo$NEAR_FC, 3, 11)

#Limpiando datos indirectos 'inativos'

encu_censo <-  enc_censo[enc_censo$ESTADO == "NA",]

#separando por especie

enc_ari <- encu_censo[encu_censo$Especie== "Ariranha", ]
enc_lon <- encu_censo[encu_censo$Especie== "Lontra", ]

# separando directos de indirectos

enc_ari_dir <- enc_ari[enc_ari$Tipo== "Direto", ]
enc_ari_ind <- enc_ari[enc_ari$Tipo== "Indireto", ]

enc_lon_dir <- enc_lon[enc_lon$Tipo== "Direto", ]
enc_lon_ind <- enc_lon[enc_lon$Tipo== "Indireto", ]

consulta<-enc_ari #cambiar segun lo que quiero ver

# AGRUPAR SEGUN LOS LOCAIS
loc_consulta <- split.data.frame(consulta, consulta$NEAR_FC)
loc_consulta <- lapply(loc_consulta,"[[","adate")

# ubicar las fechasde encuentros en la tab_l3 de fechas de visitas por local
match(loc_consulta[["LOCAL_304"]], tab_l3[["LOCAL_304"]])

#
loc_enc <- list() # para que se guarden los resultados en una lista

for( l in names(loc_consulta)) {
  loc_enc[[l]] <- match(loc_consulta[[l]] , tab_l3[[l]])
} 

#separar variables por meses

match_list <- list() #creamos una lista donde sabemos el lugar que ocupa cadauno de los encuentros segun las visitas

for( m in names(loc_consulta)) {
  match_list[[m]] <- list()
  for ( n in names(maxdata) ){
    
    gg1 <- grep( n , loc_consulta[[m]])
    gg2 <- grep( n , tab_l3[[m]])
    par1 <- loc_consulta[[m]][gg1]
    par2 <- tab_l3[[m]][gg2]
    match_list[[m]][[n]] <-match (par1, par2)
  }
}

## colocar los 1 en sus respectivas posiciones

for (o in names(match_list)){
  for (p in names (match_list[[o]])){ 
    valor <- match_list[[o]][[p]]
    for (q in valor) {
      tab_enc[o, paste(p,"(", q, ")", sep="")] <- 1
    }
  }
}


tab_enc$`2019-11(1)` <- NULL # anulamos nov porque no tiene suficientes visitas para mi simulacion
## Adicionar columna de frecuencia

# tab_enc["Frecuencia_lon_ind"] <- "0"
# for (l in names(maxdata)) {
#   tab_enc[paste("Lontras_ind_",l, sep="")] <- 0
# }
# 
# for (o in names(match_list)){   
#   total_avist <- 0
#   for (p in names (match_list[[o]])){ #en match_list tenemos la ubicacion para cada mes por local 
#     valor <- length(match_list[[o]][[p]] ) # numero de avistamientos por mes
#     tab_enc[o, paste("Lontras_ind_",p, sep="")] <- valor
#     total_avist <- total_avist+ valor 
#   }
#   tab_enc[o, "Frecuencia_lon_ind" ] <- paste(total_avist)
# }


#####DARLE FORMATO INP #####
maxcols2<- maxcols-1
tab_enc$eh <- apply(tab_enc[1:maxcols2],1, paste, collapse="") #concatenar encuentros#CAMBIAR SEGUN NUMERO DE VISITAS MAX
tab_enc[1:maxcols2] <- NULL # se eliminan las columnas de encuentros
# creando comentario 

tab_enc$tag <-paste("/*", rownames(tab_enc),"*/", sep= " ")

tab_enc$end <- ";"

tab_enc$frec <- paste( "1", tab_enc$end, sep= "")
tab_enc$end <- NULL
#tab_enc$`Lontras_ind_2019-10` <- NULL

#creamos una tabla con las columnas ordenadas
tab_enc_inp <- data.frame(tab_enc$tag,tab_enc$eh,tab_enc$frec)

# Guardamos como un archivo inp
write.table(tab_enc_inp,file="tabenc_loconsulta.inp",sep=" ",quote=F,col.names=F,row.names=F)
View("tabenc_loconsulta.inp")
