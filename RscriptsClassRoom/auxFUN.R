
# --- Esta funcion devuelve un caracter con el nombre
# --- del estudiante y el ejercicio a resolver
rifa_ejercicios <- function( ejercicios, nombreEstudiantes ) { 
  
  # ejercicios = exercise[-c(5,11,1,4,7,12,2,8,9,3,10)]
  
  if ( length(ejercicios) == 1 & is.numeric(ejercicios) ) {
    ejercicio <- ejercicios
  } else {
    ejercicio <- sample( x = ejercicios, size = 1 )
  }
  
  nombre <- sample( x = nombreEstudiantes, size = 1 )
  
  cat( nombre, "muéstranos tu solución al ejercicio", ejercicio, "\n" )
  
}


# --- Genera capas de NDVI (filtrado por calidad de pixel)
# --- Y exporta el raster a un archivo .tif
getReliableNDVI <- function(rasterNDVI, rasterReliability, 
                            layer, cropSHP, ndviNAMES, dirToSave) {
  
  if ( !dir.exists(dirToSave) ) { 
    dir.create( dirToSave ) 
  }
  
  tempNDVI <- subset(rasterNDVI, layer)
  tempRELIA <- subset(rasterReliability, layer)
  
  tempNDVI[ tempRELIA >= 2 ] <- NA
  
  cropNDVI_RELIA <- crop( tempNDVI, cropSHP,
                          mask = TRUE )
  
  NAME <- strsplit( basename( ndviNAMES[layer] ), ".tif" )[[1]][1]
  
  writeRaster( cropNDVI_RELIA,
               filename = paste0( dirToSave, "/", NAME, "_QA.tif" ) ,
               datatype = "INT2S",
               overwrite = TRUE )
} 
