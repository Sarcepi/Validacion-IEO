library(dplyr)
library(openxlsx)

#-------------------------------TALLAS---------------------------------------

#Especies no incluidas en el maestro, que se hacen con ellas?

#Selecciono solo especies del maestro para probar

check_talla <- function() {
  
  tallas1<-tallas%>%
    filter(Tipo.Captura=="C")%>%
    select(Marea,Lance,Alfa3,Talla)%>%
    filter(Alfa3 %in% c("COD", "HAD", "POK", "POL", "HKE", "LBD", "MEG", 
                        "SOL", "PLE", "WHB", "LIN", "CIG", "MAG", "HOM", 
                        "ANE", "BSS", "PIL", "SBR")) %>%
    mutate(Talla = ifelse(Talla == "", NA, Talla))%>%
    mutate(Talla = as.numeric(Talla))%>%
    na.omit()
  
  merged_data <- merge(tallas1, tallas_minimas[, c("Alfa3","Talla_minima")], by = "Alfa3", all.x = TRUE)
  
  out_of_range <- merged_data$Talla < merged_data$Talla_minima

  if (any(out_of_range)) {
    errors <- merged_data[out_of_range, c("Marea", "Lance","Alfa3","Talla","Talla_minima")]
    
    error_message <- paste("Valor de talla", errors$Talla, "inferior a lo establecido para la especie", errors$Alfa3, 
                           "(Min:", errors$Talla_minima,")")
    
    errors$TIPO_ERROR <- error_message
    
    errors <- errors[, names(tallas_errores)]
    
    
    tallas_errores <<- rbind(tallas_errores, errors)
  }
  
  return(tallas_errores)
}
  