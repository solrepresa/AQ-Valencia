### Calidad del Aire en Comunitat Valenciana
### 11/02/2019 Valencia, Spain
### Sol Represa
### Archivo 16



### Objetivo: Abrir .hdf de MERRA y guardar en tif las SDS

library(gdalUtils)
library(raster)
library(rgdal)
library(R.utils)
library(maptools)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# CORROBORAR:
# gdal_setInstallation(verbose=TRUE) # ver version gdal
# getOption("gdalUtils_gdalPath")  #verificar que podamos abrir HDF5

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


### ATENCION
#  Este codigo tira error cuando en el .hdf hay solo una SDS
#  en ese caso utilizar el código que está más abajo :)


setwd("C:\\Users\\narep\\Desktop\\MERRA") #fundamental para q funcione gdal!
#id <- dir(pattern = ".hdf") 

id <- list.files(path = getwd(),
                 pattern = "*.hdf",
                 full.names = FALSE)

## Shape recorte

shape <- readOGR("C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\mapa\\valencia.shp")
shape_trans <- spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))


# Para resampling
# Uso imagen MODIS MCD19A2 como modelo para crear raster
MCD19A2 <- raster("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_res\\MCD19A2.A2008-01-01.h17v04.tif")

raster_template <- raster(nrows = 239, ncols = 158, #1 km de resolucion aprox
                          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ", 
                          ext = extent(MCD19A2))  # toma las extensiones



mapply(file = id,
       FUN = function(file){
         sds <- get_subdatasets(file) #lista de nombres de las SDS 
         
         for( j in 1:length(sds)){
           name_sds <- sds[j]
           filename <- paste(substr(name_sds, 19, 54), substr(name_sds, 71, nchar(name_sds)), ".tif", sep="")
           gdal_translate(name_sds, dst_dataset = filename)
           MIRRAraster <- raster(filename)
           
           # 2) Reproyectar
           MIRRAraster <- projectRaster(MIRRAraster,
                                        crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ",
                                        method = "bilinear")
          
           # 3)Recortar
           data_recorte <- crop(MIRRAraster, shape_trans)  #recorto imagen para Valencia
          
           # 4) Guardar recorte
           writeRaster(data_recorte, paste(getwd(), "\\raster\\", filename, sep = ""), format = "GTiff")
           
           
           # 5) Resampling
           rst_resampling <- raster::resample(data_recorte, raster_template)
           
           # 6) Guardar resampling
           writeRaster(rst_resampling, paste(getwd(), "\\raster_res\\", filename, sep = ""), format = "GTiff")
           
           
           rm(data_recorte, MIRRAraster, rst_resampling)
         }
      }
)





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## VERSION 2  >> cuando solo hay 1 SDS   ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

### CUando en el .hdf solo tiene una unica SDS


setwd("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\MERRA\\otros\\a") #fundamental paa q funcione gdal!
id <- dir(pattern = ".hdf") 


## Shape recorte

shape <- readOGR("C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\mapa\\valencia.shp")
shape_trans <- spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

SDS = "H1000"

for( i in 1: length(id)){
  # 1) Abrir
  filename <- paste(substring(id[i], 0, 36), SDS, ".tif", sep="") 
  
  gdal_translate(id[i], dst_dataset = filename)
  MIRRAraster <- raster(filename)
  
  # 2) Reproyectar
  MIRRAraster <- projectRaster(MIRRAraster, 
                               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ",
                               method = "bilinear")
  
  # 3)Recortar
  data_recorte <- crop(MIRRAraster, shape_trans)  #recorto imagen para Valencia
  
  # 4) Guardar recorte
  writeRaster(data_recorte, paste(getwd(), "\\raster\\", filename, sep = ""), format = "GTiff")
  
  # 5) Resampling
  rst_resampling <- raster::resample(data_recorte, raster_template)
  
  # 6) Guardar resampling
  writeRaster(rst_resampling, paste(getwd(), "\\raster_res\\", filename, sep = ""), format = "GTiff")
  
  rm(data_recorte, MIRRAraster, rst_resampling)

}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## RESEAMPLING  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Lista de raster SDS MERRA
id_MERRA <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\MERRA\\raster", pattern = ".tif")


# Uso imagen MODIS MCD19A2 como modelo para crear raster
MCD19A2 <- raster("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_res\\MCD19A2.A2008-01-01.h17v04.tif")

raster_template <- raster(nrows = 239, ncols = 158, #1 km de resolucion aprox
                          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ", 
                          ext = extent(MCD19A2))  # toma las extensiones



for( i in 1:length(id_MERRA)){
  rst <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\MERRA\\raster\\", id_MERRA[i], sep = ""))
  rst_resampling <- raster::resample(rst, raster_template)
  writeRaster(rst_resampling, paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\MERRA\\raster_res\\", id_MERRA[i], sep = ""), format = "GTiff")
  
}












# # # # # # # # # # # # # # # # # # # # # # # # #

# Variables de interes de MERRA

SDS <- vector( mode = "list", length = 5)
names(SDS) <- c("tavg1_2d_aer_Nx", "tavg1_2d_slv_Nx", "tavg1_2d_flx_Nx", "avg1_2d_rad_Nx", "inst3_3d_asm_Np" )

SDS[[1]] <- c("BCSMASS", "DMSSMASS", "DUSMASS", "DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS", "SSSMASS25")
SDS[[2]] <- "H1000" 
SDS[[3]] <- c("PBLH" , "SPEED", "SPEEDMAX", "USTAR", "PRECTOT")
SDS[[4]] <- c("ALBEDO", "CLDHGH", "CLDLOW")
SDS[[5]] <- c("PS", "T", "RH", "U", "V")


