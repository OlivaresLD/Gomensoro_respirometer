#################################################################
#################################################################
######## Script uTray Biomasa y SIR            ##################
########                                               ##########
######## Developed by: Luis Daniel Olivares Martínez ############
######## July 2023                           ####################
######## Universidad Miguel Hernández       #####################
#################################################################
#################################################################

ruta <- "/raw" # folder with the Gomensoro txt files (in same folder than the file with this script)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Working Directory at file location
setwd(ruta)
lf <- list.files(path = ruta, pattern = "txt$") # select just the txt files in the folder

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
  
  fi <- which.min(abs(imp -  a[[i]][,2]))
  ta$M[i] <- a[[i]][fi,2]
  ta$Tiempo_h[i] <- a[[i]][fi,1]
  
  m2_5 <- which.min(abs(a[[i]][,1] - 2.5))
  ta$M_2.5h[i] <- a[[i]][m2_5,2]
}

# ta$Fecha <- as.POSIXct(ta$Fecha, format = "%d.%m.%Y %H:%M:%S") # uncomment to set column as POSIX format (commented because problems in elder R versions)
ta$mgCO2_2.5h_kg <- round((0.000005*abs(ta$M_2.5h)^3-0.0008*abs(ta$M_2.5h)^2+0.0462*abs(ta$M_2.5h)+0.031) / 2.5 / ta$Peso * 1000,2) # Based on the Gomensoro equations on its templates
ta$BIOM_mgC_kg_2.5h_Beare <- round(188.3 + 15.5 * ta$mgCO2_2.5h_kg + 231.5 + 17.25 * ta$mgCO2_2.5h_kg, 1) # Beare et al, 1990 ### Beare, M.H., Neely, C.L., Coleman, D.C., Hargrove, W.L. (1990). A substrate-induced respiration (SIR) method for measurement of fungal and bacterial biomas on plan residues ###

ta$mgCO2_h_CBAS <- round(10^(-0.0002*ta$Tiempo_h^3+0.0107*ta$Tiempo_h^2-0.1964*ta$Tiempo_h-0.00007),5) # Stimation method by the CBAS Spanish Institution 
ta$mlCO2_h_CBAS <- round(10^(-0.0002*ta$Tiempo_h^3+0.0107*ta$Tiempo_h^2-0.1964*ta$Tiempo_h-0.2517),5) # Stimation method by the CBAS Spanish Institution
ta$mlCO2_h_g_CBAS <- ta$mlCO2_h_CBAS / ta$Peso
ta$mgCO2_h_kg_CBAS <- ta$mgCO2_h_CBAS / ta$Peso * 1000 # Pasando g a kg de suelo para mgCO2_h
ta$BIOM_mgC_kg_CBAS <- ta$mlCO2_h_g_CBAS * 1000 * 40.04 # mlCO2_h * 1000 para pasar de g a kg


#### output table ####
write.csv(ta,paste0("BiomassSIR_", Sys.Date(),".csv"))