
# --- Vamos hablar de ciclos o bucles (if-else, for)
# --- funciones (definicion)

library(sf)
library(terra)
library(dplyr)
library( stringr )

# ---

mohinoraDIRS <- list.dirs( path = paste0( getwd(), "/data/mohinora" ),
                           recursive = FALSE )

ndviFILES <- list.files( path = mohinoraDIRS[1],
                         pattern = ".tif",
                         full.names = TRUE )

reliaFILES <- list.files( path = mohinoraDIRS[2],
                          pattern = ".tif",
                          full.names = TRUE )

# ---

shpANPfile <- list.files( path = paste0( getwd(), "/data/ANP_2021" ),
                          pattern = ".shp$",
                          full.names = TRUE)

ANP2021 <- read_sf( shpANPfile )

APFF <- ANP2021[ ANP2021$CAT_DECRET == "APFF" , ]

mohinora_sf <- APFF[ 23, ]

# ---

NDVI <- rast( ndviFILES )

reliability <- rast( reliaFILES )

mohinora_sf_sinu <- st_transform( x = mohinora_sf,
                                  crs = crs(NDVI) )

# --- 

cropNDVI <- crop( subset(NDVI, 10), mohinora_sf_sinu,
                  mask = TRUE )

plot( cropNDVI, 
      main = "Banda10 NDVI Cerro Mohinora" )
plot( st_geometry( mohinora_sf_sinu ),
      border = "darkred", lwd = 4,
      add = TRUE)

# ---

# Pixel reliability codes usually mean:
# • 	0 = good data
# • 	1 = marginal
# • 	2 = snow/ice
# • 	3 = cloudy

par( mfrow = c(1,2) )

plot( subset(NDVI, 10), 
      main = "Banda10 NDVI Cerro Mohinora" )
plot( subset(reliability, 10), 
      main = "Banda10 reliability Cerro Mohinora" )

tempNDVI <- subset(NDVI, 10)
tempRELIA <- subset(reliability, 10)

tempNDVI[ tempRELIA >= 2 ] <- NA

# tempNDVI_ifel <- ifel( tempRELIA >= 2, NA, tempNDVI )

# if (condition) { do_this } else { do_that }

# tempNDVI <- ifel( tempRELIA < 2, tempNDVI, NA )

plot( tempNDVI, 
      main = "Banda10 NDVI Cerro Mohinora" )
plot( tempRELIA, 
      main = "Banda10 reliability Cerro Mohinora" )

# ---

cropNDVI_RELIA <- crop( tempNDVI, mohinora_sf_sinu,
                        mask = TRUE )

par( mfrow = c(1,1) )
plot( cropNDVI_RELIA, 
      main = "Banda10 NDVI Cerro Mohinora" )
plot( st_geometry( mohinora_sf_sinu ),
      border = "darkred", lwd = 4,
      add = TRUE)

# --- funcion { aplico_mask; hago_recorte; exporto_producto }

NAME <- strsplit( basename( ndviFILES[10] ), ".tif" )[[1]][1]

paste0( NAME, "_QA.tif" )

whereToSave <- file.path( getwd(), "data/mohinora/250m_16_days_NDVI_reliability" )

if ( !dir.exists(whereToSave) ) { dir.create( whereToSave ) }

writeRaster( cropNDVI_RELIA,
             filename = paste0( whereToSave, "/", NAME, "_QA.tif" ) ,
             datatype = "INT2S",
             overwrite = TRUE )

# --- definicion de funcion para realizar todo lo de arriba a una capa 

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

getReliableNDVI(rasterNDVI = NDVI, rasterReliability = reliability,
                layer = 10, cropSHP = mohinora_sf_sinu, ndviNAMES = ndviFILES,
                dirToSave = whereToSave)

# ---

for (i in 1:5) {
  cat( "El for está tomando el valor: ", i, "\n" )
}

# ---

for ( i in 1:549 ) {
  getReliableNDVI(rasterNDVI = NDVI, rasterReliability = reliability,
                  layer = i, cropSHP = mohinora_sf_sinu, ndviNAMES = ndviFILES,
                  dirToSave = whereToSave)
}
































