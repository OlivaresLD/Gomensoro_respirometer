#################################################################
#################################################################
######## Script uTray Edaphic Basal Respiration     #############
########                                               ##########
######## Developed by: Luis Daniel Olivares Martínez ############
######## July 2023                           ####################
######## Universidad Miguel Hernández       #####################
#################################################################
#################################################################

ruta <- "/raw" # folder with the Gomensoro txt files (in same folder than the file with this script)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Working Directory at file location
setwd(ruta)
lf <- list.files(path = ruta, pattern = "txt$") # patrón que termina con ese texto

Tablas <- function(Nm) {
  DF <- read.delim(Nm, header = F, sep = "\t", skip = 41)
  DF[,3] <- NULL; colnames(DF) <- c("h","M")
  return(DF)
}

a <- lapply(lf, Tablas)
names(a) <- sub("\\.txt", "", lf) #drop .txt from lf

#### Bucle chido ####
ta <- data.frame(Pos = NA, SeqNo = NA, Archivo = lf, ID = NA, Peso = NA, Fecha = NA, Preincubacion = NA, Umbral = NA, M = NA, Tiempo_h = NA, M_2.5h = NA)

for(i in 1:length(lf)){
  Nm <- lf[i]
  
  conn <- file(Nm, open = "r")
  tex <- readLines(conn, n=35)
  close(conn)
  
  ta$Pos[i] <-  read.table(text = tex[4])[,2]
  ta$SeqNo[i] <- read.table(text = tex[2])[,2]
  ta$Fecha[i] <- as.character(read.table(text = tex[6], sep = '\t')[,2])
  ta$Preincubacion[i] <- read.table(text = tex[7], sep = '\t')[,2]
  imp <- read.table(text = tex[14], sep = '\t')[,2]
  ta$Umbral[i] <- imp
  ta$ID[i] <- as.character(read.table(text = tex[27])[,2])
  ta$Peso[i] <- read.table(text = tex[28])[,2]
  
  fi <- which.min(abs(a[[i]]$h - 24))
  ta$M[i] <- a[[i]][fi,2]
  ta$Tiempo_h[i] <- a[[i]][fi,1]
  
  # m2_5 <- which(a[[i]][,1] == 2.5) # Uncomment both lines if you want the respiration value at 2.5 hours
  # ta$M_2.5h[i] <- a[[i]][m2_5,2] # Uncomment both lines if you want the respiration value at 2.5 hours
}

# ta$Fecha <- as.POSIXct(ta$Fecha, format = "%d.%m.%Y %H:%M:%S") # uncomment to set column as POSIX format (commented because problems in elder R versions)
ta$mgCO2 <- round(0.000005*abs(ta$M)^3-0.0008*abs(ta$M)^2+0.0462*abs(ta$M)+0.031,5)
ta$mlCO2 <- round(ta$mgCO2*0.56,5)
ta$mg_h <- round(ta$mgCO2 / ta$Tiempo_h,5)
ta$mg_h_g <- round(ta$mg_h / ta$Peso,5)
ta$ml_g <- round(ta$mlCO2 / ta$Peso,5)
ta$ml_h_g <- round(ta$ml_g / ta$Tiempo_h,5)
ta$mgCO2_h_kg <- ta$mg_h_g * 1000
# ta$mgCO2_2.5h_kg <- NULL # Uncomment if you don't want this column

#### output table ####
write.csv(ta,paste0("Respiracion_", Sys.Date(),".csv"), row.names = F)