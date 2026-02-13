
# Elaboro NOMBRE
# Fecha
# RESUMEN: Objetos I (vectors, matrix, data.frame, etc ...)

# ----

library( tidyverse )

# ---- Ejemplos object numeric

v <- 1:10
class(v)
typeof(v)

w <- seq( 0 , 1, by = 0.1) 
class(w)
typeof(w)

# ---- Ejemplos object character
nombres <- c("esau", "adrian", "javier", "mariela", "inder")
class(nombres)
typeof(nombres)


# ---- Ejemplos otros object 
muestra <- rnorm( n=20 )
class(muestra)
typeof(muestra)

muestra_enteros <- rpois(n=20, lambda = 0.5)
class(muestra_enteros)
typeof(muestra_enteros)

# ---- Ejemplos matrices
mat_cbind <- cbind( muestra, muestra_enteros )

mat_cbind[9,]

mat_cbind[16,2]

muestra_corta <- c( rnorm( n=18 ), rep(NA, 2) )

mat_cbind_pregunta <- cbind( muestra, muestra_corta )

mat_rbind <- rbind( muestra, muestra_corta )

str( mat_cbind )

str( mat_rbind )

num_poi <- rpois(12, lambda = 2)

mat <- matrix( num_poi, nrow = 3, ncol = 4)

mat_renglon <- matrix(num_poi, nrow = 3, ncol = 4,
                      byrow = TRUE)

mat[  1:3, 2:3 ]

mat[ 2:3, c(1, 4) ]

?rowMeans


# ---- Ejemplos object logico
objetoLogicoVerdadero <- TRUE

objetoLogicoFalso <- FALSE


# ---- Ejemplos opereaciones sobre renglones & matrices; uso apply
mat
rowMeans(mat)

colMeans(mat)

mat_cbind_pregunta

rowMeans( x = mat_cbind_pregunta )

rowMeans( x = mat_cbind_pregunta, na.rm = TRUE )

mat

apply( X = mat, MARGIN = 1, FUN = sd )

apply( X = mat, MARGIN = 2, FUN = sd )

apply( X = mat, MARGIN = 2, FUN = sd )

# --- Ejemplo, dataset volcano

?volcano

volcano

filled.contour(volcano)

filled.contour(volcano, 
               color.palette = terrain.colors, 
               asp = 1)

library(RColorBrewer)

display.brewer.all()

filled.contour(volcano, 
               # color.palette = brewer.pal(n=11, name = "BrBG"), 
               col = brewer.pal(n=11, name = "BrBG"),
               asp = 1)

filled.contour(volcano, 
               # color.palette = brewer.pal(n=11, name = "BrBG"), 
               col = brewer.pal(n=11, name = "Spectral"))

library(mapview)

mp <- mapview(volcano)






























