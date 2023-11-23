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

missVal_sieve <- function(DIR, amountFiles, totalPixels, startYear, endYear){
  percentMat <- matrix(nrow=length(DIR), ncol=amountFiles)
  totalPixels <- totalPixels #36 * 36
  
  for(i in seq_len(nrow(percentMat))){
    TIFs <- mixedsort(list.files(path = DIR[i], full.names = TRUE))
    for(j in seq_len(ncol(percentMat))){
      r <- rast(TIFs[j])
      percentMat[i,j] <- as.numeric(global(r, fun="isNA")) / totalPixels
    }
  }
  
  row.names(percentMat) <- startYear:endYear # 2000:2022
  colnames(percentMat) <- month.name
  
  # ---
  
  percentMat  
}

spRast_valueCoords <- function(spRaster, na_rm=FALSE){
  
  spPoints <- as.points(spRaster, na.rm=na_rm)
  
  spValues <- extract(spRaster, spPoints)
  
  DIM <- dim(spValues)
  
  spRasterToPoints <- as.matrix(spValues[1:DIM[1],2:DIM[2]])
  
  spCoords <- crds(spRaster, na.rm=na_rm)
  
  list(values=spRasterToPoints, coords=spCoords)  
}

