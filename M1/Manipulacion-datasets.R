library(datasets)


# Instanciamos nuestra base de datos 
airquality <- datasets::airquality

# Renombramos filas de una columna en un data.frame
replace_ref <- list("5" = "May", "6"="Jun", "7"="Jul", "8"="Ago", "9"="Sep")
airquality$Month <- as.character(airquality$Month)

# Reemplazamos los valores de la columna Month
for (m in names(replace_ref)){
  airquality[airquality['Month']==m, "Month"] <- replace_ref[[m]]  
}

meses <- unique(airquality$Month)

# Creo una coleccion con las temporadas a comparar
temporadas <- list(
  temporada1 = head(meses,2), # nos quedamos con los primeros meses de la serie
  temporada2 = tail(meses,2) # nos quedamos con los ultimos meses de la serie
  )

# Seleccionamos un conjunto de observaciones, los correspondientes a la temporada 1 y a la 2
idx <- 1
for (t in seq(2)){
  # Subset data
  key <- paste("temporada", idx, sep="")
  airquality_df <- subset(airquality, Month %in% temporadas[[key]], select = c(Temp, Wind))
  
  # Almacenamos los datasets en la lista
  new_key <- paste("airquality", idx, sep="")
  temporadas[[new_key]] <- airquality_df  
  idx <- idx + 1
}

# Resumen general
temporadas[["Summary1"]] <- summary(temporadas$airquality1)
temporadas[["Summary2"]] <- summary(temporadas$airquality2)

# Moda
calcula.moda <- function(seq.val){
  return(as.numeric(names(which.max(table(seq.val))))) 
}

temporadas[["Mod.Temp1"]] <- calcula.moda(temporadas$airquality1$Temp)
temporadas[["Mod.Temp2"]] <- calcula.moda(temporadas$airquality2$Temp)






