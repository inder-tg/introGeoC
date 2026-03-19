
# --- Marzo 12, 2026
# --- Solución de Ejercicios

# ----

source( "RscriptsClassRoom/auxFUN.R" )

exercise <- 2:5
estudiantes <- c("esau", "adrian", "javier", "mariela")

rifa_ejercicios(ejercicios = exercise,
                nombreEstudiantes = estudiantes)

rifa_ejercicios(ejercicios = exercise[-c(5,11,1,4,7,12,2,8,9,3,10)],
                nombreEstudiantes = estudiantes)


y <- list( "a", "b", "c" )

q <- list( "A", "B", "C", "a", "b", "c" )

library(dplyr)

df_y <- data.frame( valor = unlist(y) )
df_q <- data.frame( valor = unlist(q) )

anti_join( df_q, df_y, by = "valor" )








