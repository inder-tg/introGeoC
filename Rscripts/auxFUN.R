hola_estudiante <- function(){
  message("Hola!")
  
  yourName <- readline("Cómo te llamas, o k ase? ")
  
  message(paste("Pues, bienvenide", yourName))
}

rifa_ejercicios <- function(asistentes){
  ganadore <- sample(x = asistentes, size=1)
  
  cat(ganadore, paste("muéstranos tu solución al ejercicio"))
}

# get_coords <- function(orig_coords, table){
#   nRow <- length(unlist(orig_coords)) / 2
#   
#   mat_toPlot <- matrix(as.numeric(unlist(orig_coords)), nrow = nRow)
#   
#   dX <- matrix(nrow = nrow(table))
#   
#   dY <- matrix(nrow = nrow(table))
#   
#   aproxX <- numeric(nRow)
#   
#   aproxY <- numeric(nRow)
#   
#   dX <- sapply(1:nRow, function(s) abs(table[,1] - mat_toPlot[s,1]))
#   
#   aproxX <- sapply(1:nRow, function(s) table[which.min(dX[,s]),1] )
#   
#   dY <- sapply(1:nRow, function(s) abs(table[,2] - mat_toPlot[s,2]))
#   
#   aproxY <- sapply(1:nRow, function(s) table[which.min(dY[,s]),2] )
#   
#   toExtract <- matrix(nrow = nRow, ncol = 2)
#   
#   toExtract[,1] <- aproxX
#   toExtract[,2] <- aproxY
#   
# toExtract
# }
# 
# get_timeSeries_byClicking <- function(toPlot, df){
#   
#   nRow <- length(unlist(toPlot)) / 2
# 
#   toExtract <- get_coords(orig_coords = toPlot, table = df)
#   
#   pixels <- matrix(nrow = nRow, ncol = ncol(df)-2)
#   
#   for(i in 1:nRow){
#     pixels[i,] <- df[(df[,1] == toExtract[i,1]) & (df[,2] == toExtract[i,2])][-c(1:2)]
#   }
#   
#   list(ts = pixels, coord = toExtract)
# }


get_timeSeries_byClicking <- function(toPlot, df){
  nRow <- length(unlist(toPlot)) / 2
  
  mat_toPlot <- matrix(as.numeric(unlist(toPlot)), nrow = nRow)
  
  dX <- matrix(nrow = nrow(df))
  
  dY <- matrix(nrow = nrow(df))
  
  aproxX <- numeric(nRow)
  
  aproxY <- numeric(nRow)
  
  dX <- sapply(1:nRow, function(s) abs(df[,1] - mat_toPlot[s,1]))
  
  aproxX <- sapply(1:nRow, function(s) df[which.min(dX[,s]),1] )
  
  dY <- sapply(1:nRow, function(s) abs(df[,2] - mat_toPlot[s,2]))
  
  aproxY <- sapply(1:nRow, function(s) df[which.min(dY[,s]),2] )
  
  toExtract <- matrix(nrow = nRow, ncol = 2)
  
  toExtract[,1] <- aproxX
  toExtract[,2] <- aproxY
  #
  IND <- 1:length(df)
  xTemp <- which(df[,1] == toExtract[1,1])
  yTemp <- which(df[xTemp,2] == toExtract[1,2])
  #
  xyRow <- xTemp[yTemp] # df[xTemp[yTemp],1:2]
  
  list(coord = xyRow)
  # xyRow
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

# --- Added on April 9, 2026

myPlot <- function(RASTER, nombreRASTER){
  
  TEMP <- getValues(RASTER)
  
  MEAN <- mean(TEMP, na.rm = TRUE)
  
  DS <- sd(TEMP, na.rm = TRUE)
  
  TITLE <- paste0("ANP: ", nombreRASTER, "\n",
                  "media: ", round(MEAN,4), "\n",
                  "desv. est.: ", round(DS,4))
  
  plot(RASTER, main=TITLE )
  
}


myPlot2 <- function(x, nombre, ...){
  
  # TEMP <- getValues(RASTER)
  
  MEAN <- mean(x, na.rm = TRUE)
  
  DS <- sd(x, na.rm = TRUE)
  
  TITLE <- paste0("ANP: ", nombre, "\n",
                  "media: ", round(MEAN,4), "\n",
                  "desv. est.: ", round(DS,4))
  
  plot(x, main=TITLE, ...)
}


# --- Added on April 16, 2026

get_pixel_matrix <- function(x,lenPeriod=23){
  output <- matrix(nrow=length(x)/lenPeriod, ncol=lenPeriod)
  
  for(i in seq_len(nrow(output))){
    output[i,] <- x[((i-1) * lenPeriod + 1):(i * lenPeriod)]
  }
  output
}

climatology <- function(x, lenPeriod){
  
  MAT <- get_pixel_matrix(x=x, lenPeriod=lenPeriod)
  
  BOXPLOT <- boxplot(MAT, plot=FALSE)
  
  list(matrix=MAT, boxplot=BOXPLOT)
}


