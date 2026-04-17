

# --- PSEUDO código para resolver Ejercicio 4 Hwork 6

# 1. Integrar el código de clase7 en una función: La salida de esta función pueden
# contener a los objetos conteo_colonias, leafletCinesColonia, ..., etc. getStatsAlcaldia( ... )

# 2. Escribir un for-loop para aplicar getStatsAlcaldia() a cada una de las alcaldias:
#   
#   nombreAlcaldias <- c(...)
#   
#   contenedorSalidas <- vector( "list", 16 )
#   
#   for( i in 1:length( nombreAlcaldias ) ){
#     
#     TEMP <- getStatsAlcaldia( nombreAlcaldias[ i ], ... )
#     
#     contenedorSalidas[[ i ]] <- TEMP
#     
#   }
  
# 3. "Pegar" los objetos leaflet contenidos en contenedorSalidas: Este obj va a contener la información
# sobre los cines de la CDMX (frequencia por colonia, ubicación, nombre de los cines, etc)

# --- Marzo 26, 2026
# --- Tidyverse & Análisis espacial - temporal

library(terra) # Abrir raster
library(sf) # Abrir un shapeefile
library(tidyr) # Uso de funciones del tidyverse
library(RColorBrewer) # Elaboración de paletas de colores
library(tmap) # Elaboración de mapa estático
library(ggplot2) # Elaboración de gráficas
library(leaflet) # Elaboración de mapa dinámico
library(dplyr)

# --- Carga de datos

mohinoraDIRS <- list.dirs( path = paste0( getwd(), "/data/mohinora" ),
                           recursive = FALSE )

ndviRelia <- list.files( path = mohinoraDIRS[2],
                         pattern = ".tif",
                         full.names = TRUE )

# ---

shp_mohinora <- list.files( path = paste0( getwd(), "/data/mohinora_usv7" ),
                          pattern = ".shp$",
                          full.names = TRUE)
# ---

NDVI <- rast( ndviRelia )

mohinoraUSV <- st_read( shp_mohinora )

# ---

mohinoraUSV %>%
  count( DESCRIPCIO ) %>%
  st_drop_geometry() %>%
  arrange(  )

# ---

tm_shape( mohinoraUSV ) +
  tm_polygons( "DESCRIPCIO",
               # fill.scale = tm_scale(palette = "brewer.set3")
               palette = "set3"
                ) #+
  # tm_layout( legend.outside = FALSE )

# ---

tm_shape( NDVI[[1]] ) +
  tm_raster( palette = "Greens", col.scale = tm_scale_continuous() ) +
  tm_shape( mohinoraUSV ) +
  tm_borders( col = "blue", lwd = 3 )

# --- 

NDVI_vals <- terra::extract( NDVI[[1]], mohinoraUSV )

NDVI_vals <- terra::extract( NDVI[[1]], mohinoraUSV,
                      fun = mean, na.rm = TRUE )

unique(mohinoraUSV$DESCRIPCIO)

# --- Calcular Promedio NDVI por USV

mohinoraUSV$ndvi <- NDVI_vals[,2]

ndvi_summary <- mohinoraUSV %>%
  group_by( DESCRIPCIO ) %>%
  st_drop_geometry() %>%
  summarise( promedioNDVI = mean( ndvi, na.rm = TRUE ) )


ggplot( ndvi_summary, 
        aes( x = DESCRIPCIO, y = promedioNDVI, fill = DESCRIPCIO ) ) +
  geom_col() +
  coord_flip()


ggplot( mohinoraUSV, 
        aes( x = DESCRIPCIO, y = ndvi, fill = DESCRIPCIO ) ) +
  geom_boxplot() +
  coord_flip()

# ---

pal <- brewer.pal( n = length( unique( mohinoraUSV$DESCRIPCIO ) ),
                   "Set3"  )

ggplot( ndvi_summary, 
        aes( x = DESCRIPCIO, y = promedioNDVI, fill = DESCRIPCIO ) ) +
  geom_col() +
  coord_flip() +
  scale_fill_manual( values = pal )


ggplot( mohinoraUSV, 
        aes( x = DESCRIPCIO, y = ndvi, fill = DESCRIPCIO ) ) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual( values = pal )

# ---

names(NDVI) <- paste0( "NDVI_", 1:nlyr(NDVI) )

NDVI_vals_all <- terra::extract( NDVI, mohinoraUSV,
                                 fun = mean, na.rm = TRUE )


# --- Format long

ndvi_long <- NDVI_vals_all %>%
  as_tibble()
  
# Error in `as_tibble()`:
#   ! Column names `250m 16 days NDVI`, `250m 16 days NDVI`, `250m 16 days NDVI`, `250m 16 days NDVI`,
# `250m 16 days NDVI`, and 543 more must not be duplicated.
# Use `.name_repair` to specify repair.
# Caused by error in `repaired_names()`:
#   ! Names must be unique.
# ✖ These names are duplicated:
#   * "250m 16 days NDVI" at locations 2, 3, 4, 5, 6, etc.
# Run `rlang::last_trace()` to see where the error occurred


ndvi_long <- NDVI_vals_all %>%
  as_tibble() %>%
  mutate( DESCRIPCION = mohinoraUSV$DESCRIPCIO,
          polygon_id = 1:nrow( mohinoraUSV ) ) %>%
  pivot_longer( cols = starts_with("NDVI"),
                names_to = "layer", values_to = "NDVI" )


ndvi_stats <- ndvi_long %>%
  group_by( DESCRIPCION, layer ) %>%
  summarise( promedioNDVI = mean( NDVI ),
             sdNDVI = sd( NDVI ),
             .groups = "drop")


ggplot( ndvi_stats, aes( x = as.numeric(gsub( "NDVI_", "", layer )) , 
                         y = promedioNDVI, 
                         color = DESCRIPCION, group = DESCRIPCION ) ) +
  geom_line( linewidth = 1.2 ) +
  scale_color_brewer(palette = "Set3") +
  labs( x = "NDVI, 16 días", y = "Valor Promedio NDVI" )


# ---

agro_stats <- ndvi_stats %>%
  filter( DESCRIPCION == "AGRICULTURA DE TEMPORAL ANUAL" )


ggplot( agro_stats, aes( x = as.numeric(gsub( "NDVI_", "", layer )) , 
                         y = promedioNDVI, 
                         color = DESCRIPCION, group = DESCRIPCION ) ) +
  geom_line( linewidth = 1.2, col = pal[1]  )  +
  geom_ribbon( aes( ymin = promedioNDVI - sdNDVI, ymax = promedioNDVI + sdNDVI ),
               alpha = 0.1 ) +
  labs( x = "NDVI, 16 días", y = "Valor Promedio NDVI", 
        title = "Serie de tiempo NDVI, Agricultura" ) +
  theme_minimal()






























































































