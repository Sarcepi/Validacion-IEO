library(openxlsx)
library(dplyr)




#-------------------IMPORTAR DOCUMENTOS-------------------------------------
hauls <- read.csv2("hauls_03_2023.csv", fileEncoding = "latin1")

tallas<-read.csv2("Tallas_BJP_09_23.csv",sep=",",header=T)

catches<-read.csv2("catches.csv",sep=",",header=T)



#----------------------ARREGLOS PREVIOS-------------------------------------

subdivision_map <- c("Subdivisión VIIIc-Oeste" = "8c-O",
                     "Subdivisión VIIIc-Este" = "8c-E",
                     "Subdivisión IXa-Norte" = "9a-N",
                     "Divisiones VIIIab"="8a-B")

especies<-c("CIG","DES","GAL","JVR","MER","RAM","RAP","RMC")# Especies que se convierten a VBL

hauls <- hauls %>%
  mutate(Origen = recode(Origen, !!!subdivision_map))%>%
  mutate(Abert.Ver = ifelse(Abert.Ver == "", NA, Abert.Ver))%>%
  mutate(Velocidad = ifelse(Velocidad == "", NA, Velocidad))%>%
  mutate(Profundidad.Largada = ifelse(Profundidad.Largada == "", NA, Profundidad.Largada))%>%
  mutate(CodEspObj=ifelse(Metier.ICES=="OTB11","VBL",CodEspObj))%>%
  mutate(Lat.Vir.1= as.numeric(Lat.Vir.1),
         Lon.Vir.1 = as.numeric(Lon.Vir.1)
  )

catches<-catches%>%
  filter(Especie != "VIDA")
  

#------------------------DATAFRAME DE ERRORES-----------------------------------
global_errores <- data.frame(Marea = character(), 
                             Lance = character(), 
                             TIPO_ERROR = character(), 
                             stringsAsFactors = FALSE)

catches_errores<-data.frame(Marea = character(), 
                            Lance = character(), 
                            TIPO_ERROR = character(), 
                            stringsAsFactors = FALSE)

tallas_errores<-data.frame(Marea = character(), 
                           Lance = character(), 
                           TIPO_ERROR = character(), 
                           stringsAsFactors = FALSE) 

#---------------------------Maestros-----------------------------------
#Maestro de lances
features_hauls<-read.table("feature_hauls.txt",sep=";",header=T)
features_hauls[7,5]<-420
limites<-read.csv2("limites.csv",header = T,sep=",",row.names=1)

limites <- limites %>%
  mutate(
    Latitud_Maxima = as.numeric(Latitud_Maxima),
    Latitud_Minima = as.numeric(Latitud_Minima),
    Longitud_Maxima = as.numeric(Longitud_Maxima),
    Longitud_Minima = as.numeric(Longitud_Minima)
  )

#Maestro de tallas
tallas_minimas<-read.csv2("tallas_minimas.csv",header=T,sep=",")
colnames(tallas_minimas)[colnames(tallas_minimas) == "Alfa"] <- "Alfa3"


source("/Users/aa/Desktop/Validacion/Codigo/Codigo_Funciones_Validacion_final.R")
source("/Users/aa/Desktop/Validacion/Codigo/Tallas_funciones.R")


#---------------------Llamada a las funciones-------------------------------

duration()
size_malla()
length_cable()
largada_depth()
log_lat_limits()
coherence_target_species_metier_ieo()
check_velocidad_range()
check_abertura_vertical()
check_abertura_horizontal()
peso_muestreado()
retain_weight()
especie_rara()
check_talla()



#-------------Ordenar global errores en funcion de marea y lance---------------
global_errores <- global_errores %>%
  arrange(Marea, Lance)

tallas_errores<-tallas_errores%>%
  arrange(Marea,Lance)

catches_errores<-catches_errores%>%
  arrange(Marea,Lance)
#----------------------------EXPORTAR EXCEL-------------------------------------
#Excel con los errores, por defecto sale en el directorio de trabajo

output_dir<-"/Users/aa/Desktop/Validacion/Errores"

write.xlsx(global_errores, file.path(output_dir, "global_errores3.xlsx"))
write.xlsx(catches_errores, file.path(output_dir, "errores_catches3.xlsx"))
write.xlsx(tallas_errores, file.path(output_dir, "talla_errores3.xlsx"))










