
# --- Vamos hablar sobre data.frames, list, 
# --- funciones (definicion) y ciclos o bucles (if-else, for)

library(sf)
library(terra)
library(dplyr)
library( stringr )

# --- data.frames + georeferencia

shpANPfile <- list.files( path = paste0( getwd(), "/data/ANP_2021" ),
                          pattern = ".shp$",
                          full.names = TRUE
)

ANP2021 <- read_sf( shpANPfile )

class(ANP2021)
names( ANP2021 )
head( ANP2021 )

st_geometry( ANP2021 )

plot( st_geometry( ANP2021 ) )

ANP2021[ 1:3 , ]

plot( st_geometry( ANP2021[ 1:3 , ] ) )

ANP2021$NOMBRE[1:3]

# ANP2021$test <- rep( TRUE, 183 )

unique(ANP2021$CAT_DECRET)

table( ANP2021$CAT_DECRET )

APFF <- ANP2021[ ANP2021$CAT_DECRET == "APFF" , ]

APFF$NOMBRE[!is.na(APFF$NOMBRE)]

mohinora_sf <- APFF[ 23, ]

plot( st_geometry( mohinora_sf ),
      col = "orange",
      border = "red", lwd = 4,
      main = mohinora_sf$NOMBRE ) #APFF$NOMBRE[23]

# --- primera introduccion al tidyverse

ANP2021 %>% 
  st_drop_geometry() %>%
  count( CAT_DECRET, sort = TRUE )

# --- filter

desiertos <- ANP2021 %>%
  filter( str_detect( NOMBRE, "Desierto" )  )

ANP2021 %>%
  filter( str_detect( NOMBRE, "Desierto" )  ) %>%
  st_geometry() %>%
  plot(col = "darkgoldenrod", main = "ANPs con Desierto")

# library(mapview)
# 
# mp_desierto <- ANP2021 %>%
#   filter( str_detect( NOMBRE, "Desierto" )  ) %>%
#   # st_geometry() %>%
#   mapview()
# 
# mp_desierto

# ---

mohinoraDIRS <- list.dirs( path = paste0( getwd(), "/data/mohinora" ),
                           recursive = FALSE )

ndviFILES <- list.files( path = mohinoraDIRS[1],
                         pattern = ".tif",
                         full.names = TRUE )

head(ndviFILES)

NDVI <- rast( ndviFILES )

nlyr( NDVI )
names( NDVI )

NDVI

NDVI[[1]]

NDVI[[ c( 1, 3, 5, 7 ) ]]

plot( NDVI[[1]], main = "Banda1 NDVI Cerro Mohinora" )

plot( subset(NDVI, 10), 
      main = "Banda10 NDVI Cerro Mohinora" )

# ---

st_crs( mohinora_sf )
NDVI

plot( subset(NDVI, 10), 
      main = "Banda10 NDVI Cerro Mohinora" )
plot( st_geometry( mohinora_sf ),
      border = "darkred", lwd = 4,
      add = TRUE)

mohinora_sf_sinu <- st_transform( x = mohinora_sf,
                                  crs = crs(NDVI) )

plot( subset(NDVI, 10), 
      main = "Banda10 NDVI Cerro Mohinora" )
plot( st_geometry( mohinora_sf_sinu ),
      border = "darkred", lwd = 4,
      add = TRUE)

# --- crop

cropNDVI <- crop( subset(NDVI, 10), mohinora_sf_sinu,
                  mask = TRUE )

plot( cropNDVI, 
      main = "Banda10 NDVI Cerro Mohinora" )
plot( st_geometry( mohinora_sf_sinu ),
      border = "darkred", lwd = 4,
      add = TRUE)

# --- 

reliaFILES <- list.files( path = mohinoraDIRS[2],
                         pattern = ".tif",
                         full.names = TRUE )

reliability <- rast( reliaFILES )

reliability

hist( subset(reliability, 10) )

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

# if (condition) { do_this } else { do_that }

tempNDVI <- ifel( tempRELIA < 2, tempNDVI, NA )

plot( tempNDVI, 
      main = "Banda10 NDVI Cerro Mohinora" )
plot( tempRELIA, 
      main = "Banda10 reliability Cerro Mohinora" )


































