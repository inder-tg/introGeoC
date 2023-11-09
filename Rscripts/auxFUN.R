hola_estudiante <- function(){
  message("Hola!")
  
  yourName <- readline("Cómo te llamas, o k ase? ")
  
  message(paste("Pues, bienvenide", yourName))
}

rifa_ejercicios <- function(asistentes){
  ganadore <- sample(x = asistentes, size=1)
  
  cat(ganadore, paste("muéstranos tu solución al ejercicio"))
}

get_coords <- function(orig_coords, table){
  nRow <- length(unlist(orig_coords)) / 2
  
  mat_toPlot <- matrix(as.numeric(unlist(orig_coords)), nrow = nRow)
  
  dX <- matrix(nrow = nrow(table))
  
  dY <- matrix(nrow = nrow(table))
  
  aproxX <- numeric(nRow)
  
  aproxY <- numeric(nRow)
  
  dX <- sapply(1:nRow, function(s) abs(table[,1] - mat_toPlot[s,1]))
  
  aproxX <- sapply(1:nRow, function(s) table[which.min(dX[,s]),1] )
  
  dY <- sapply(1:nRow, function(s) abs(table[,2] - mat_toPlot[s,2]))
  
  aproxY <- sapply(1:nRow, function(s) table[which.min(dY[,s]),2] )
  
  toExtract <- matrix(nrow = nRow, ncol = 2)
  
  toExtract[,1] <- aproxX
  toExtract[,2] <- aproxY
  
toExtract
}

get_timeSeries_byClicking <- function(toPlot, df){
  
  nRow <- length(unlist(toPlot)) / 2

  toExtract <- get_coords(orig_coords = toPlot, table = df)
  
  pixels <- matrix(nrow = nRow, ncol = ncol(df)-2)
  
  for(i in 1:nRow){
    pixels[i,] <- df[(df[,1] == toExtract[i,1]) & (df[,2] == toExtract[i,2])][-c(1:2)]
  }
  
  list(ts = pixels, coord = toExtract)
}


LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env) 
}
