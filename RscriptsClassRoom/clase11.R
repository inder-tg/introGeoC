
# --- Clase 11, Abril 16, 2026
# --- Errores comunes (debugging) | Manejo de memoria | Código eficiente

library(terra)
library(raster)
library(geoTS)
library(foreach)
library(doParallel)
library(progressr)

# --- Errores comunes (debugging)

mean( xyz ) # Error: object 'xyz' not found

xyz <- rnorm(10)


"a" + 2 # Error in "a" + 2 : non-numeric argument to binary operator

a <- 20

a + 2

x <- 1:10

x[12] # Error: subscript out of bounds

m <- matrix( 1:9, nrow=3 )

m[4,1] # Error in m[4, 1] : subscript out of bound

df <- data.frame( a=1:3, b=4:6 )

df[4,1]

df[5,]

r <- raster( nrows=3, ncol=3 )
values(r) <- rnorm(9)

r[1]
r[9]
r[10]

y <- c( 1,2,3, 4, 5 ) # Error: unexpected numeric constant in "y <- c( 1,2,3, 4 5)"


mean( xyz, by = TRUE )

args( mean )

args( sum )

sum( x, length = 40 )

testFunction <- function( x, ... ){
  cat( "La clase del objeto", x, "es:", class(x) )
}

testFunction( x = m )

testFunction( x = xyz )

testFunction( x = m, by = TRUE ) # Error in testFunction(x = m, by = TRUE) : unused argument (by = TRUE)


read.csv( "no_existe.csv" ) # Error in file(file, "rt") : cannot open the connection

# matrix( runif( 1e9 ) ) # Cannot allocate vector of size XXX

# --- Manejo de memoria 

set.seed( 123 )

nrows <- 500
ncols <- 500
ncapas <- 20

valores <- matrix( runif( nrows * ncols * ncapas ),
                   nrow = nrows * ncols, ncol = ncapas)

s <- rast( nrows = nrows, ncols = ncols, nlyrs = ncapas,
           vals = c(valores) )

plot( subset(s,20) )


system.time({

  mean_s <- app( s, mean )  
  
})


plot( mean_s )

writeRaster( mean_s, filename = here( "data/outputs/mean_app_example.tif" ),
             datatype = "FLT4S", overwrite = TRUE )

# --- Misma idea de arriba pero utilizando el paquete raster

capas_raster <- lapply( 1:ncapas, function(i) 
  raster( nrows = nrows, ncols = ncols, vals = valores[,i] )  )

s_raster <- stack( capas_raster )

plot( subset(s_raster, 20) )


system.time( {

  out <- raster( nrows = nrows, ncols = ncols )
  vals_out <- numeric( ncell(out) )
  
  for( i in 1:ncapas ){
    
    vals_out <- vals_out + getValues( s_raster[[i]] )
    
  }
  
  vals_out <- vals_out / nlayers( s_raster )
  
  out <- setValues( out, vals_out )
  
})

plot( out, main = "Promedio por celda del objeto s_raster" )

hist( out )
hist( mean_s )

raster::writeRaster( out, filename = here( "data/outputs/mean_raster_example.tif" ),
             datatype = "FLT4S", overwrite = TRUE )

# --- Cómputo en paralelo

source( "Rscripts/auxFUN.R" )

ndviFILES <- list.files( path = here( "data", "mohinora", "250m_16_days_NDVI_reliability" ),
                         pattern = "NDVI_QA",
                         full.names = TRUE )

NDVI <- rast( ndviFILES )

mohinora_DATA_rTp <- spRast_valueCoords(NDVI)

# ---

plot( subset(NDVI, 108) )

xy <- locator()

XY <- get_timeSeries_byClicking( c( xy$x, xy$y ),
                                 df = mohinora_DATA_rTp$coords )

pixel <- mohinora_DATA_rTp$values[XY$coord, ]

pixel_ts <- ts( pixel, start = c(2000,1), end = c(2023, 23), frequency = 23 )

plot( pixel_ts )

pixel_ts[550:552]

as.numeric(pixel[547:549])

as.numeric(pixel[1:3])

pixel_aug <- c(NA,NA,NA, as.numeric(pixel))

pixel_aug_ts <- ts( pixel_aug,  start = c(2000,1), end = c(2023, 23), frequency = 23 )

plot(pixel_aug_ts)

# --- imputation by climatology curve

clima <- climatology(x=pixel_aug, lenPeriod=23)

boxplot(clima$matrix)

pixel_aug[1:3] <- ceiling(apply(clima$matrix[-1,1:3], MARGIN=2, 
                                FUN=median))

pixel_ts_correct <- ts(pixel_aug, start = c(2000,1), end = c(2023,23), 
                       frequency = 23)

plot(pixel_ts_correct, xlab="Años", ylab="NDVI", col="darkgreen", 
     main="pixel aumentado como objeto 'ts'")




























































































































