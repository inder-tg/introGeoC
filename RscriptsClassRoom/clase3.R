
# --- Feb 19, 2026
# --- Operaciones básicas, coersion, objetos espaciales (sf, SpatRaster)


ejercicios <- 1:7

sample( x = ejercicios, size = 1 )

estudiantes <- c( "mariela", "adrian", "esau", "javier" )

sample( x = estudiantes, size = 1 )



# --- Definiendo una función

rifa_ejercicios <- function( ejercicios, nombreEstudiantes ) { 
  
  ejercicio <- sample( x = ejercicios, size = 1 )
  
  nombre <- sample( x = nombreEstudiantes, size = 1 )
  
  cat( nombre, "muéstranos tu solución al ejercicio", ejercicio, "\n" )
  
}


rifa_ejercicios( ejercicios = 1:7, nombreEstudiantes = estudiantes )

rifa_ejercicios( ejercicios = 2:7, nombreEstudiantes = estudiantes[-1] )

rifa_ejercicios( ejercicios = 3:7, nombreEstudiantes = estudiantes[ - c(1, 4)] )

rifa_ejercicios( ejercicios = c(3:4, 6:7), nombreEstudiantes = estudiantes[2] )


# ---


v = c(13,24,1,0.5,93,41,103,4,0.015,NA)

v[ v > 10 ]


# --- volcano

library(mapview)
library(RColorBrewer)
library(terra)

# Esto no funcionó

# r <- rast(x = volcano)
# 
# crs(r) <- "EPSG:27200"
# 
# mp <- mapview( r )

# --- Rasterizando dataset volcano

xrange <- range( seq( from = 2667400, length.out = 62, by = 10 ) )
yrange <- range( seq( from = 6478700, length.out = 88, by = 10 ) )

volcanoRast <- rast(
  crs = "EPSG:27200",
  xmin = xrange[1],
  xmax = xrange[2],
  ymin = yrange[1],
  ymax = yrange[2],
  resolution = 10
)

volcanoRast

values( volcanoRast ) <- as.vector( t( volcano[87:1, 61:1] ) )

mp <- mapview( volcanoRast )


volcanoRast[ volcanoRast < 160  ] <- NA

mp_160 <- mapview( volcanoRast, na.color = "transparent" ) 

# ---

library(leaflet)
library(dplyr)

pal <- colorNumeric(
  palette = brewer.pal(n=10, "Spectral"),
  domain = values(volcanoRast),
  na.color = "transparent"
)


leaflet() %>% 
  addTiles( "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" ) %>%
  addRasterImage(volcanoRast, colors = pal) %>%
  addLegend(pal = pal, values = values(volcanoRast), title = "Elevación")
































































