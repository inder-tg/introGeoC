
library( terra )

# --- Solucion Ejercicio 5 Hoja 4
FILES <- list.files( path = paste0( getwd(), "/data/mohinora/250m_16_days_NDVI_reliability" ),
                     pattern = ".tif",
                     full.names = TRUE )

r <- rast(FILES[ 330 ])

plot( r,  main = paste0("NDVI capa 340") )
# ---

# --- Soluciones Ejercicios Hoja 5

source( "RscriptsClassRoom/auxFUN.R" )

rifa_ejercicios( ejercicios = 1:1,  nombreEstudiantes = c("mariela", "esau", "adrian"))

rifa_ejercicios( ejercicios = 1:1,  nombreEstudiantes = c("mariela", "esau"))

# --- 1b

rootDIR <- "C:/Users/inder/OneDrive/Desktop/introGeoC"

sni <- read.csv( paste0( rootDIR, 
                         "/data/examenFinal/Investigadores-SNI-Vigentes-2018.csv" ),
                 fileEncoding = "latin1",
                 check.names = FALSE )

paterno <- table( sni$`Apellido Paterno` )

which.max(paterno)

paterno[2583]

sni %>%
  count( Nombre, sort = TRUE ) %>%
  slice_max(n, n=5)


# --- Uso leaflet | tmap | mapview

library( leaflet )
library( tmap )
library( sf )
library( readr )
library( ggplot2 )
library( dplyr )
library( here )

# --- Cargando archivos

csvFILES <- list.files( path = here( "data", "cinesCDMX" ),
                        pattern = ".csv",
                        full.names = TRUE )

shpFILES <- list.files( path = here( "data", "cinesCDMX", "coloniascdmx" ),
                        pattern = ".shp",
                        full.names = TRUE )

cines <- read_csv( file = csvFILES,
                   locale = locale( encoding = "latin1" ) )

SHP <- st_read( shpFILES )

# ---

cinesCDMX <- cines %>%
  filter( `Entidad federativa` == "CIUDAD DE MÉXICO" )


# --- Crear un mapa con leaflet

leaflet( data = cinesCDMX ) %>%
  addTiles() %>%
  addCircleMarkers( ~Longitud, ~Latitud,
                    popup = ~`Nombre de la Unidad Económica`,
                    radius = 4, 
                    color = "red")

conteo <- cinesCDMX %>%
  group_by( Municipio ) %>%
  summarise( num_cines = n() ) %>%
  arrange( desc(num_cines) )

# --- R base

default_mar <- par( "mar" )

par( mar = c( 5, 12, 4, 2 ) )
barplot( conteo$num_cines,
         names.arg = conteo$Municipio,
         col = "steelblue",
         main = "Distribución de cines por alcaldía en CDMX",
         xlab = "Número de cines",
         ylab = "",
         horiz = TRUE,
         cex.names = .7,
         las = 1)

# -- ggplot

cinesCDMX %>%
  group_by( Municipio ) %>%
  summarise( num_cines = n() ) %>%
  arrange( desc(num_cines) ) %>%
  ggplot( aes( x = reorder( Municipio, num_cines ), y = num_cines ) ) +
  geom_col( fill = "steelblue" ) +
  labs( x = "Municipio", y = "Número de cines",
        title = "Distribución de cines por alcaldía en CDMX") +
  coord_flip()


# --- agregamos info de num cines x alcaldia a cinesCDMX

cinesCDMX <- cinesCDMX %>%
  add_count( Municipio, name = "n_cines" )

leaflet( data = cinesCDMX ) %>%
  addTiles() %>%
  addCircleMarkers( ~Longitud, ~Latitud,
                    radius = 4, 
                    color = "red", 
                    
                    popup = ~paste0(
                      `Nombre de la Unidad Económica`, "<br>",
                      "<b>Alcaldía: </b>", Municipio, "<br>",
                      "<b>No. cines por Alcaldía: </b>", n_cines
                    ))

# --- Cines en la Cuauhtemoc

SHP_alcaldia_cuauhtemoc <- SHP %>%
  filter( alcaldi == "CUAUHTEMOC" )

plot( st_geometry(SHP_alcaldia_cuauhtemoc) )

cinesCuauhtemoc <- cinesCDMX %>%
  filter( Municipio == "Cuauhtémoc" )

leaflet( data = SHP_alcaldia_cuauhtemoc ) %>%
  addTiles() %>%
  addPolygons( fillColor = "lightblue",
               popup = ~paste("Alcaldía: ", alcaldi)) %>%
  addCircleMarkers( data = cinesCuauhtemoc,
                    ~Longitud, ~Latitud,
                    radius = 4, 
                    color = "red", 
                    
                    popup = ~paste0(
                      `Nombre de la Unidad Económica`, "<br>",
                      "<b>Alcaldía: </b>", Municipio, "<br>",
                      "<b>No. cines por Alcaldía: </b>", n_cines
                    ))

# --- cines por colonia en la Cuauhtémoc


cinesCuauhtemoc_sf <- st_as_sf(cinesCuauhtemoc,
                               coords = c("Longitud", "Latitud"),
                               crs = 4326)

plot(st_geometry(cinesCuauhtemoc_sf))

cinesCuauhtemoc_colonia <- st_join(cinesCuauhtemoc_sf, SHP_alcaldia_cuauhtemoc)

conteo_colonias <- cinesCuauhtemoc_colonia %>%
  group_by( nombre ) %>%
  summarise( num_cines_colonia = n() ) %>%
  st_drop_geometry()


SHP_alcaldia_cuauhtemoc <- SHP_alcaldia_cuauhtemoc %>%
  left_join(conteo_colonias, by = "nombre" )


leaflet( data = SHP_alcaldia_cuauhtemoc ) %>%
  addTiles() %>%
  addPolygons( fillColor = "lightblue",
               popup = ~paste("Alcaldía: ", alcaldi, "<br>",
                              "Colonia: ", nombre, "<br>",
                              "Número cines en la colonia: ", num_cines_colonia)) %>%
  addCircleMarkers( data = cinesCuauhtemoc_colonia,
                    # ~Longitud, ~Latitud,
                    radius = 4, 
                    color = "red", 
                    
                    popup = ~paste0(
                      `Nombre de la Unidad Económica`, "<br>",
                      "<b>Alcaldía: </b>", Municipio, "<br>",
                      "<b>No. cines por Alcaldía: </b>", n_cines
                    ))

# ---

# Modo interactivo
tmap_mode("view")

tm_shape(SHP_alcaldia_cuauhtemoc) +
  tm_polygons("num_cines_colonia",
              palette = "Reds",
              title = "Número de cines",
              border.col = "gray") +
  tm_shape(cinesCuauhtemoc_colonia) +
  tm_dots(col = "blue", size = 0.8,
          popup.vars = c("Nombre de la Unidad Económica" = "Nombre de la Unidad Económica",
                         "Colonia" = "nombre"))



tm_shape(SHP_alcaldia_cuauhtemoc) +
  tm_polygons("num_cines_colonia",
              palette = "brewer.reds",
              fill.legend = "Número de cines",
              border.col = "gray",
              colorNA = "transparent",
              textNA = "Sin cines",
              popup.vars = c( "Colonia" = "nombre", "Alcaldía" = "alcaldi" )) +
  tm_shape(cinesCuauhtemoc_colonia) +
  tm_dots(fill = "blue", size = 0.8,
          popup.vars = c("Nombre de la Unidad Económica" = "Nombre de la Unidad Económica",
                         "Colonia" = "nombre"))


































































