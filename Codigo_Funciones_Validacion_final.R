library(dplyr)


############################    FUNCIONES    ####################################

#--------------------------DURACION---------------------------------------------

# Que hacemos con los NA

duration <- function() {
  duration <- hauls %>%
    select(Marea, Lance, Duracion.lance) %>%
    filter(Duracion.lance <= 0)
  
  # Check if there are any rows with invalid duration
  if (nrow(duration) > 0) {
    # Add error message directly to the data frame
    duration$TIPO_ERROR <- "WARNING: Duration of haul < 0"
    
    # Append the error details to the global_errores dataframe
    global_errores <<- rbind(global_errores, duration[, c("Marea", "Lance", "TIPO_ERROR")])
  }
  
  return(global_errores)  # Return the updated global_errores
}
duration()
#--------------------------TAMAÑO MALLA-----------------------------------------

size_malla <- function() {
  #Establecer maximos y mínimos.
  min_size <- 55
  max_size <- 100
  
  err <- hauls %>%
    select(Marea, Lance, Tamaño.Malla) %>%
    filter(Tamaño.Malla < min_size | Tamaño.Malla > max_size)
  
  if (nrow(err) != 0) {
    for (i in 1:nrow(err)) {
      error_message <- paste("ERROR: Tamaño de malla (", err$Tamaño.Malla[i], ") fuera de los límites", sep = "")
      err[i, "TIPO_ERROR"] <- error_message
    }
    global_errores <<- rbind(global_errores, err)
  } 
  return(global_errores)
}

size_malla()

#-------------------------- CABLE-----------------------------------

length_cable <- function() {
  
  # Establecer Profundidad máxima de cable aquí
  profunidad_maxima <- 2000
  
  # Filtrar las filas donde la profundida del cable es mayor que el límite y no es NA
  err <- hauls[hauls$Cable > profunidad_maxima & !is.na(hauls$Cable), c("Marea", "Lance", "Cable")]
  
  # Si hay errores, agregar un mensaje a global_errores
  if (nrow(err) > 0) {
    for (i in seq_len(nrow(err))) {
      # Crear el mensaje de error
      war <- paste("ERROR: Profundidad del cable (", err$Cable[i], ") fuera de los límites")
      
      # Añadir el mensaje de error a global_errores
      err[i, "TIPO_ERROR"] <- war
      global_errores <<- rbind(global_errores, err[i, c("Marea", "Lance", "TIPO_ERROR")])
    }
  }
  
  return(global_errores)
}

length_cable()

#---------------------------PROFUNDIDAD LARGADA Y VIRADA------------------------

#AVISAR A HORTENSIA QUE NO VALORA LOS NA'S
#Para cambiar valores de la función cambiar fichero feature_hauls.txt

largada_depth <- function() {
  
  f <- features_hauls[, c("Metier.ICES", "Cod.Metier", "Largada.Max", "Largada.Min")]
  h <- hauls[, c("Marea", "Lance", "Metier.ICES", "Cod.Metier", "Profundidad.Largada", "Profundidad..Virada")]
  
  merged_df <- left_join(h, f, by = c("Metier.ICES", "Cod.Metier"), relationship = "many-to-many") %>%
    na.omit() %>% 
    mutate(
      Profundidad.Largada = as.numeric(Profundidad.Largada),
      Profundidad..Virada = as.numeric(Profundidad..Virada)
    ) %>% 
    mutate(
      Profundidad_Check = ifelse(is.na(Profundidad.Largada) | is.na(Largada.Min) | is.na(Largada.Max), 
                                 FALSE, 
                                 Profundidad.Largada >= Largada.Min & Profundidad.Largada <= Largada.Max),
      Profundidad_V_Check = ifelse(is.na(Profundidad..Virada) | is.na(Largada.Min) | is.na(Largada.Max), 
                                   FALSE, 
                                   Profundidad..Virada >= Largada.Min & Profundidad..Virada <= Largada.Max)
    ) %>%
    na.omit()
  
  for (i in 1:nrow(merged_df)) {
    
    codigo <- hauls[i, "Cod.Metier"]
    
    # Check Profundidad.Largada
    if (!merged_df$Profundidad_Check[i]) {
      error_message <- paste("ERROR: Profundidad largada (", merged_df$Profundidad.Largada[i], 
                             ") fuera de los límites para COD.METIER:", codigo)
      if (!any(global_errores$TIPO_ERROR == error_message)) {
        error_row <- data.frame(Marea = merged_df$Marea[i],
                                Lance = merged_df$Lance[i],
                                TIPO_ERROR = error_message,
                                stringsAsFactors = FALSE)
        global_errores <<- rbind(global_errores, error_row)
      }
    }
    
    # Check Profundidad..Virada
    if (!merged_df$Profundidad_V_Check[i]) {
      error_message <- paste("ERROR: Profundidad Virada (", merged_df$Profundidad..Virada[i], 
                             ") fuera de los límites para COD.METIER:", codigo)
      if (!any(global_errores$TIPO_ERROR == error_message)) {
        error_row <- data.frame(Marea = merged_df$Marea[i],
                                Lance = merged_df$Lance[i],
                                TIPO_ERROR = error_message,
                                stringsAsFactors = FALSE)
        global_errores <<- rbind(global_errores, error_row)
      }
    }
  }
  
  return(global_errores)
}

largada_depth()
                             
#------------------------RECTANGULO ESTADISTICO-------------------------------


re <- function() {
  #Rectángulos estadísticos válidos
  rec_valido <- list("'16E2", "'16E3", "'14E0", "'16E4", "'13E0", 
                     "'17E2", "'16E1", "'15E5", "'17E1", 
                     "'15E0", "'16E5", "'16E7", "'16E6", "'12E0")
  for (i in 1:nrow(hauls)) {
    if (!(hauls$C.ICES[i] %in% rec_valido) & !is.na(hauls$C.ICES[i])) {
      war <- addTypeOfError(
        hauls[i, c("Marea", "Lance")], 
        paste("ERROR: Rectángulo estadístico (", hauls$C.ICES[i], ") no valido")
      )
      global_errores <<- rbind(global_errores, war)
    }
  }
  
  return(global_errores)
}
#---------------------------Cuadrícula ICES-------------------------------------
#QUE VA AQUI ?
#---------------------------LIMITAR DIVISIONES EN FUNCION LAT Y LONG------------

log_lat_limits <- function() {
  # Check if global_errores has the correct columns before processing
  if (!all(c("Marea", "Lance", "TIPO_ERROR") %in% colnames(global_errores))) {
    global_errores <<- data.frame(Marea = character(0), 
                                  Lance = character(0), 
                                  TIPO_ERROR = character(0), 
                                  stringsAsFactors = FALSE)
  }
  
  for (i in 1:nrow(hauls)) {
    origen <- hauls[i, "Origen"]
    
    lat <- hauls[i, "Lat.Vir.1"]
    lon <- hauls[i, "Lon.Vir.1"]
    
    if (origen %in% rownames(limites)) {
      lat_max <- limites[origen, "Latitud_Maxima"]
      lat_min <- limites[origen, "Latitud_Minima"]
      lon_max <- limites[origen, "Longitud_Maxima"]
      lon_min <- limites[origen, "Longitud_Minima"]
      
      # Check if latitude is within the allowed range
      if (!is.na(lat)) {
        if (lat < lat_min || lat > lat_max) {
          error_message <- paste("Origen:", origen, "Latitud fuera de límites:", lat, "(Permitido:", lat_min, "-", lat_max, "),")
          # Check if this error is already in global_errores to avoid duplication
          if (!any(global_errores$TIPO_ERROR == error_message)) {
            error_row <- data.frame(Marea = hauls[i, "Marea"],
                                    Lance = hauls[i, "Lance"],
                                    TIPO_ERROR = error_message,
                                    stringsAsFactors = FALSE)
            global_errores <<- rbind(global_errores, error_row)
          }
        }
      } else {
        error_message <- paste("Latitud es NA:", lat)
        if (!any(global_errores$TIPO_ERROR == error_message)) {
          error_row <- data.frame(Marea = hauls[i, "Marea"],
                                  Lance = hauls[i, "Lance"],
                                  TIPO_ERROR = error_message,
                                  stringsAsFactors = FALSE)
          global_errores <<- rbind(global_errores, error_row)
        }
      }
      
      # Check if longitude is within the allowed range
      if (!is.na(lon)) {
        if (lon < lon_min || lon > lon_max) {
          error_message <- paste("Origen:", origen, "Longitud fuera de límites:", lon, "(Permitido:", lon_min, "-", lon_max, ")")
          if (!any(global_errores$TIPO_ERROR == error_message)) {
            error_row <- data.frame(Marea = hauls[i, "Marea"],
                                    Lance = hauls[i, "Lance"],
                                    TIPO_ERROR = error_message,
                                    stringsAsFactors = FALSE)
            global_errores <<- rbind(global_errores, error_row)
          }
        }
      } else {
        error_message <- paste("Longitud es NA:", lon)
        if (!any(global_errores$TIPO_ERROR == error_message)) {
          error_row <- data.frame(Marea = hauls[i, "Marea"],
                                  Lance = hauls[i, "Lance"],
                                  TIPO_ERROR = error_message,
                                  stringsAsFactors = FALSE)
          global_errores <<- rbind(global_errores, error_row)
        }
      }
    } else {
      error_message <- paste("Origen no encontrado:", origen)
      if (!any(global_errores$TIPO_ERROR == error_message)) {
        error_row <- data.frame(Marea = hauls[i, "Marea"],
                                Lance = hauls[i, "Lance"],
                                TIPO_ERROR = error_message,
                                stringsAsFactors = FALSE)
        global_errores <<- rbind(global_errores, error_row)
      }
    }
  }
  
  return(global_errores)
}

log_lat_limits()
#--------------------CRUZAR METIER CODIGO METIER Y CODIGO ESPECIE---------------

coherence_target_species_metier_ieo <- function() {
  
  # Initialize an empty data frame to store errors
  errores <- data.frame(Marea = character(), Lance = character(), Error = character(), stringsAsFactors = FALSE)
  
  # Selecting relevant columns for checking coherence
  ms <- features_hauls[c("Metier.ICES", "Cod.Metier", "CodEspObj")]
  ms$OK <- "ok"  
  
  # Joining the hauls data with the ms dataframe to check for consistency
  hc <- hauls[, c("Marea", "Lance", "Metier.ICES", "Cod.Metier", "CodEspObj")]
  
  errors <- merge(hc, ms, all.x = TRUE, by = c("Metier.ICES", "Cod.Metier", "CodEspObj"))
  
  # Filter rows with missing values in 'OK' column, indicating inconsistency
  errors <- errors[is.na(errors[["OK"]]),]
  
  # If there are errors, assign error messages
  if (nrow(errors) > 0) {
    errors$TIPO_ERROR <- "ERROR: the target species is not coherent with metier ieo."
    
    # Special case for missing Metier.ICES
    errors[errors$Metier.ICES == "", "TIPO_ERROR"] <- "ERROR: the target species field can't be checked because the METIER_IEO is empty."
    
    # Append errors to global_errores
    global_errores <<- rbind(global_errores, errors[, c("Marea", "Lance", "TIPO_ERROR")])
    
    return(global_errores)
  }
  
  return(NULL)  
}

# Run the function
coherence_target_species_metier_ieo()

    
#--------------------------VELOCIDAD--------------------------------------------

check_velocidad_range <- function() {
  
  hc <- hauls %>%
    select(Marea, Lance, Metier.ICES, Velocidad) %>%
    mutate(Velocidad = ifelse(Velocidad == "", NA, Velocidad))%>%
    filter(!is.na(Velocidad))
  
  merged_data <- merge(hc, features_hauls[, c("Metier.ICES", "Velocidad.Max", "Velocidad.Min")], by = "Metier.ICES", all.x = TRUE)
  
  out_of_range <- merged_data$Velocidad < merged_data$Velocidad.Min | merged_data$Velocidad > merged_data$Velocidad.Max
  
  if (any(out_of_range)) {
    errors <- merged_data[out_of_range, c("Marea", "Lance", "Metier.ICES", "Velocidad", "Velocidad.Min", "Velocidad.Max")]
    
    error_message <- paste("Velocidad", errors$Velocidad, "out of range for Metier.ICES", errors$Metier.ICES, 
                           "(Min:", errors$Velocidad.Min, "Max:", errors$Velocidad.Max, ")")
    
    errors$TIPO_ERROR <- error_message
    
    errors <- errors[, names(global_errores)]
    

    global_errores <<- rbind(global_errores, errors)
  }
  
  return(global_errores)
}
check_velocidad_range()
#---------------------------ABERTURA VERTICAL-----------------------------------

check_abertura_vertical <- function() {
  
  hc <- hauls %>%
    select(Marea, Lance, Metier.ICES, Abert.Ver) %>%
    mutate(Abert.Ver = ifelse(Abert.Ver == "", NA, Abert.Ver))%>%
    mutate(Abert.Ver = ifelse(Abert.Ver == "0", NA, Abert.Ver))%>%
    filter(!is.na(Abert.Ver))%>%
    mutate(Abert.Ver = as.numeric(Abert.Ver))
  
  ms <- features_hauls[, c("Metier.ICES", "Abert.Ver.Max", "Abert.Ver.Min")]
  
  merged_data <- merge(hc, ms, by = "Metier.ICES", all.x = TRUE)

  out_of_range <- merged_data$Abert.Ver < merged_data$Abert.Ver.Min | merged_data$Abert.Ver > merged_data$Abert.Ver.Max
  
  if (any(out_of_range)) {
    errors <- merged_data[out_of_range, c("Marea", "Lance", "Metier.ICES", "Abert.Ver", "Abert.Ver.Min", "Abert.Ver.Max")]
    
    error_message <- paste("Abertura Vertical", errors$Abert.Ver, "out of range for Metier.ICES", errors$Metier.ICES, 
                           "(Min:", errors$Abert.Ver.Min, "Max:", errors$Abert.Ver.Max, ")")
    
    errors$TIPO_ERROR <- error_message
    
    errors <- errors[, names(global_errores)]
    
    global_errores <<- rbind(global_errores, errors)
  }
  
  return(global_errores)
}

check_abertura_vertical()




#-------------------------ABERTURA HORIZONTAL-------------------------------------

check_abertura_horizontal <- function() {
  
  hc <- hauls %>%
    select(Marea, Lance, Metier.ICES, Abert.Hor) %>%
    mutate(Abert.Hor = ifelse(Abert.Hor == "", NA, Abert.Hor))%>%
    mutate(Abert.Hor = ifelse(Abert.Hor == 0, NA, Abert.Hor))%>%
    filter(!is.na(Abert.Hor))%>%
    mutate(Abert.Hor = as.numeric(Abert.Hor))
  
  ms <- features_hauls[, c("Metier.ICES", "Abert.Hor.Max", "Abert.Hor.Min")]
  
  merged_data <- merge(hc, ms, by = "Metier.ICES", all.x = TRUE)
  
  out_of_range <- merged_data$Abert.Hor < merged_data$Abert.Hor.Min | merged_data$Abert.Hor > merged_data$Abert.Hor.Max
  
  if (any(out_of_range)) {
    errors <- merged_data[out_of_range, c("Marea", "Lance", "Metier.ICES", "Abert.Hor", "Abert.Hor.Min", "Abert.Hor.Max")]
    
    error_message <- paste("Abertura Horizontal", errors$Abert.Hor, "out of range for Metier.ICES", errors$Metier.ICES, 
                           "(Min:", errors$Abert.Hor.Min, "Max:", errors$Abert.Hor.Max, ")")
    
    errors$TIPO_ERROR <- error_message
    
    errors <- errors[, names(global_errores)]
    
    global_errores <<- rbind(global_errores, errors)
  }
  
  return(global_errores)
}

check_abertura_horizontal()

#----------------------PESO TOTAL DESCARTE-------------------------------------

peso_muestreado <- function() {
  
  # Check if Muestreado == 'S' but Peso.Tot.Descarte is NA or less than 0
  muestreado_nulo <- hauls %>%
    select(Marea, Lance, Peso.Tot.Descarte, Muestreado) %>%
    filter(Muestreado == "S" & (Peso.Tot.Descarte < 0 | is.na(Peso.Tot.Descarte)))
  
  # Check if Muestreado == 'N' but Peso.Tot.Descarte is greater than 0
  no_muestreado_distinto_0 <- hauls %>%
    select(Marea, Lance, Peso.Tot.Descarte, Muestreado) %>%
    filter(Muestreado == "N" & (Peso.Tot.Descarte > 0))
  
  # Check if Peso.Muestra.Descarte is greater than Peso.Tot.Descarte
  peso_muestra_superior <- hauls %>%
    select(Marea, Lance, Peso.Muestra.Descarte, Peso.Tot.Descarte) %>%
    mutate(
      Peso.Muestra.Descarte = as.numeric(Peso.Muestra.Descarte),
      Peso.Tot.Descarte = as.numeric(Peso.Tot.Descarte)
    ) %>%
    filter(Peso.Muestra.Descarte > Peso.Tot.Descarte)
  
  # If there are errors, add error messages directly
  if (nrow(muestreado_nulo) != 0) {
    muestreado_nulo$TIPO_ERROR <- "WARNING: Muestreado == 'S' pero Peso.Tot.Descarte=0 o NA"
    catches_errores <<- rbind(catches_errores, muestreado_nulo[, c("Marea", "Lance", "TIPO_ERROR")])
  }
  
  if (nrow(no_muestreado_distinto_0) != 0) {
    no_muestreado_distinto_0$TIPO_ERROR <- "WARNING: Muestreado == 'N' pero Peso.Tot.Descarte difiere de 0"
    catches_errores <<- rbind(catches_errores, no_muestreado_distinto_0[, c("Marea", "Lance", "TIPO_ERROR")])
  }
  
  if (nrow(peso_muestra_superior) != 0) {
    peso_muestra_superior$TIPO_ERROR <- "WARNING: Peso.Muestra.Descarte superior a Peso.Tot.Descarte"
    catches_errores <<- rbind(catches_errores, peso_muestra_superior[, c("Marea", "Lance", "TIPO_ERROR")])
  }
  
  # Return the updated catches_errores data frame
  return(catches_errores)  
}

# Run the function
peso_muestreado()

#-----------------------------FICHERO CAPTURAS----------------------------------

#----------------------------PESO RETENIDO--------------------------------------
retain_weight <- function() {
  
  # Comprobar si el peso retenido de la muestra es superior al peso retenido total
  retain <- catches %>%
    select(Marea, Lance, Peso.Retenido.Kg., Peso.Muestra.Retenida.kg., Especie) %>%
    mutate(
      Peso.Retenido.Kg. = as.numeric(Peso.Retenido.Kg.),
      Peso.Muestra.Retenida.kg. = as.numeric(Peso.Muestra.Retenida.kg.)
    ) %>%
    filter(Peso.Retenido.Kg. < Peso.Muestra.Retenida.kg.)
  
  # Comprobar si el peso retenido de Peso.Muestra.Tot.Descarte.Kg. es superior al Peso.Descartado.Kg
  discard <- catches %>%
    select(Marea, Lance, Peso.Descartado.Kg., Peso.Muestra.Tot.Descarte.Kg., Especie) %>%
    mutate(
      Peso.Descartado.Kg. = as.numeric(Peso.Descartado.Kg.),
      Peso.Muestra.Tot.Descarte.Kg. = as.numeric(Peso.Muestra.Tot.Descarte.Kg.)
    ) %>%
    filter(Peso.Descartado.Kg. < Peso.Muestra.Tot.Descarte.Kg.)
  
  # Comprobar que peso submuestra no sea superior a peso muestra total
  submuestra <- catches %>%
    select(Marea, Lance, Peso.Submuestra.Descarte.kg., Peso.Muestra.Tot.Descarte.Kg., Especie) %>%
    mutate(
      Peso.Submuestra.Descarte.kg. = as.numeric(Peso.Submuestra.Descarte.kg.),
      Peso.Muestra.Tot.Descarte.Kg. = as.numeric(Peso.Muestra.Tot.Descarte.Kg.)
    ) %>%
    filter(Peso.Submuestra.Descarte.kg. > Peso.Muestra.Tot.Descarte.Kg.)
  
  # Error 1
  if (nrow(retain) != 0) {
    retain$TIPO_ERROR <- paste("WARNING: Sampled retain weight higher than retain total weight for species:", retain$Especie)
    catches_errores <<- rbind(catches_errores, retain[, c("Marea", "Lance", "TIPO_ERROR")])
  }
  
  # Error 2
  if (nrow(discard) != 0) {
    discard$TIPO_ERROR <- paste("WARNING: Sampled discard weight higher than discard weight for species:", discard$Especie)
    catches_errores <<- rbind(catches_errores, discard[, c("Marea", "Lance", "TIPO_ERROR")])
  }
  
  # Error 3
  if (nrow(submuestra) != 0) {
    submuestra$TIPO_ERROR <- paste("WARNING: Subsample discard weight higher than sample discard weight for species:", submuestra$Especie)
    catches_errores <<- rbind(catches_errores, submuestra[, c("Marea", "Lance", "TIPO_ERROR")])
  }
  
  return(catches_errores)
}
retain_weight()

#-----------------------------EJEMPLARES RETENIDOS------------------------------
especie_rara <- function() {
  
  ejem_retenidos <- catches %>%
    select(Marea, Lance, Cod.Especie, Ejem.Retenidos, Peso.Muestra.Retenida.kg.) %>%
    filter((Cod.Especie == "20194" | grepl("^1", Cod.Especie)) & 
             Ejem.Retenidos < 0 &  
             Peso.Muestra.Retenida.kg. > 0)
  
  ejem_descartados <- catches %>%
    select(Marea, Lance, Cod.Especie, Ejem..Descartados, Peso.Muestra.Retenida.kg.) %>%
    filter((Cod.Especie == "20194" | grepl("^1", Cod.Especie)) & 
             Ejem..Descartados < 0 & 
             Peso.Muestra.Retenida.kg. > 0)
  
  # Error for retained specimens
  if (nrow(ejem_retenidos) != 0) {
    ejem_retenidos$TIPO_ERROR <- "WARNING: Ejemplares retenidos <=0"
    catches_errores <<- rbind(catches_errores, ejem_retenidos[, c("Marea", "Lance", "TIPO_ERROR")])
  }
  
  # Error for discarded specimens
  if (nrow(ejem_descartados) != 0) {
    ejem_descartados$TIPO_ERROR <- "WARNING: Ejemplares descartados <=0"
    catches_errores <<- rbind(catches_errores, ejem_descartados[, c("Marea", "Lance", "TIPO_ERROR")])
  }
  
  return(catches_errores)
}
especie_rara()






  
  
  
  
  
  
  
  
  
  
  
  
  







