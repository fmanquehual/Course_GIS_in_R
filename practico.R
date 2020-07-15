# install.packages('raster')
# install.packages('rgdal')
# install.packages('rgeos')
# install.packages('dplyr')
# install.packages('gdistance')
# install.packages('prettymapr')

# plantear el problema
# decir que las coberturas vectoriales las debo rasterizar y reclasificar
# tener una imagen preparada para mostrar las consecuencias de dissolve=TRUE EN RED VIAL

library(raster) # lectura y herramientas para coberturas raster
library(rgdal) # lectura de coberturas vectoriales 
library(rgeos) # herramientas SIG
library(dplyr) # join entre db de cobertura y data frame
library(gdistance) # Para calculo del camino mas corto
library(prettymapr) # barra de escala y Norte

# rm(list=ls())
# dev.off()

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

# Division administrativa ----
setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/DivisionPoliticaAdministrativa2019/')

lim <- readOGR('.', 'DivisionPoliticaAdministrativa2019')
crs(lim)

id.reg <- which(lim@data$CUT_REG == '09')
lim.09 <- lim[id.reg,]
head(lim.09@data)

plot(lim.09)
text(lim.09, labels = lim.09@data$COMUNA, cex = 0.7, col = 'red')

comuna.de.interes <- 'Galvarino'
id.comuna<- which(lim.09@data$COMUNA==comuna.de.interes)
lim.comuna <- lim.09[id.comuna,]

plot(lim.comuna)

# fin ---



# DEM ----

setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/')

dem <- raster('9.jp2')
crs(dem)

compareCRS(dem, lim.comuna)

lim.comuna.utm18 <- spTransform(lim.comuna, utm18)

dem.comuna0 <- crop(dem, lim.comuna.utm18)
dem.comuna <- mask(dem.comuna0, lim.comuna.utm18)
plot(dem.comuna)

# fin ---


# pendiente ----

pendiente.comuna <- terrain(dem.comuna, opt = "slope", unit = "degrees")
plot(pendiente.comuna)

# fin ---



# Red vial ----

setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/Red_Vial_Chile_05_05_2020/')

red.vial <- readOGR(dsn = 'Red_Vial_Chile_05_05_2020.gdb')
head(red.vial@data)
crs(red.vial)

compareCRS(lim.comuna, red.vial)

red.vial.comuna <- crop(red.vial, lim.comuna)
plot(lim.comuna)
plot(red.vial.comuna, lty = 2, col = 'red', add = TRUE)

red.vial.comuna.utm18 <- spTransform(red.vial.comuna, utm18)


red.vial.comuna.utm18.buffer <- buffer(red.vial.comuna.utm18, width=12.5, dissolve = FALSE)
plot(red.vial.comuna.utm18.buffer)

head(red.vial.comuna.utm18.buffer@data)
red.vial.comuna.utm18.buffer@data$valor <- 0
head(red.vial.comuna.utm18.buffer@data)

r.red.vial.comuna.utm18 <- rasterize(red.vial.comuna.utm18.buffer, dem.comuna, 
                                     field='valor', fun='last', background=1)

plot(r.red.vial.comuna.utm18)
unique(r.red.vial.comuna.utm18)

zoom(r.red.vial.comuna.utm18, ext=drawExtent())

# fin ---


# Hospitales ----

setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/hospitales/')

hospitales <- readOGR('.', 'HOSPITALES_SNSS_24102018')
head(hospitales@data)
str(hospitales@data)

hospitales@data$COD_REGION <- as.character(hospitales@data$COD_REGION)
unique(hospitales@data$COD_REGION)

id.reg <- which(hospitales@data$COD_REGION=='9')
hospitales.09 <- hospitales[id.reg,]

compareCRS(hospitales.09, lim.comuna.utm18)
hospitales.09.utm18 <- spTransform(hospitales.09, utm18)

hospitales.09.utm18@data$NOM_COMUNA <- as.character(hospitales.09.utm18@data$NOM_COMUNA)
unique(hospitales.09.utm18@data$NOM_COMUNA)

id.comuna <- which(hospitales.09.utm18@data$NOM_COMUNA==comuna.de.interes)
hospital.comuna <- hospitales.09.utm18[id.comuna,]

plot(lim.comuna.utm18)
plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3, cex = 0.8)

# fin ---


# uso de suelo ----

setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/Catastro_uso_suelo_y_vegetacion/Catastro_uso_suelo_y_vegetacion/')
list.files(pattern = '.shp')

cbn <- readOGR('.', 'Catastro_RV_R09_2014') # se demora 4 min
head(cbn@data)
str(cbn@data)

crs(cbn)
compareCRS(cbn, dem.comuna)

colnames(cbn@data)

cbn@data <- cbn@data[,c("ID_SUBUSO", "SUBUSO")]
head(cbn@data)
levels(cbn@data$ID_SUBUSO)
levels(cbn@data$SUBUSO)

longitud.subuso <- length( levels(cbn@data$SUBUSO) ) ; longitud.subuso
categorias.subuso <- levels(cbn@data$SUBUSO) ; categorias.subuso

subuso.id <- data.frame(id=1:longitud.subuso, SUBUSO=categorias.subuso)
head(subuso.id)

cbn@data <- left_join(cbn@data, subuso.id, by='SUBUSO')
head(cbn@data)

cbn.comuna <- crop(cbn, lim.comuna.utm18)
r.cbn.comuna <- rasterize(cbn.comuna, dem.comuna, field="id", fun="last", background=NA)
plot(r.cbn.comuna)

# fin ---



# Reclasificacion ----

# dem 
minValue(dem.comuna)
maxValue(dem.comuna)

# (minimo, maximo, nuevo valor) -> (no se incluye, <=, nuevo valor) por default
pre.m1 <- c(0, 100, 100,
       100, 200, 200,
       200, 300, 500,
       300, 400, 1000,
       400, maxValue(dem.comuna), 3000)

m1 <- matrix(pre.m1, ncol=3, byrow=TRUE) ; m1
reclas.dem.comuna <- reclassify(dem.comuna, m1)

plot(reclas.dem.comuna)
hist(reclas.dem.comuna)


# pendiente 
minValue(pendiente.comuna)
maxValue(pendiente.comuna)

pre.m2 <- c(-1, 10, 10000,
       10, 20, 50000,
       20, 30, 100000,
       30, maxValue(pendiente.comuna), 1000000)

m2 <- matrix(pre.m2, ncol=3, byrow=TRUE)
reclas.pendiente.comuna <- reclassify(pendiente.comuna, m2)

plot(reclas.pendiente.comuna)
hist(reclas.pendiente.comuna)


# uso de suelo 
ids.comuna <- unique(r.cbn.comuna)

class(subuso.id$SUBUSO)
subuso.id$SUBUSO <- as.character(subuso.id$SUBUSO)
subusos.comuna <- subuso.id$SUBUSO[subuso.id$id%in%ids.comuna]

cbind(ids.comuna, subusos.comuna)

pre.m3 <- c( ids.comuna[1], ids.comuna[1], 100000,
             ids.comuna[2], ids.comuna[2], 100000,
             ids.comuna[3], ids.comuna[3], 100,
             ids.comuna[4], ids.comuna[4], 100000,
             ids.comuna[5], ids.comuna[5], 50000,
             ids.comuna[6], ids.comuna[6], 100000,
             ids.comuna[7], ids.comuna[7], 100000,
             ids.comuna[8], ids.comuna[8], 200000,
             ids.comuna[9], ids.comuna[9], 100000,
             ids.comuna[10], ids.comuna[10], 1000,
             ids.comuna[11], ids.comuna[11], 500000,
             ids.comuna[12], ids.comuna[12], 5000,
             ids.comuna[13], ids.comuna[13], 800,
             ids.comuna[14], ids.comuna[14], 10000)

m3 <- matrix(pre.m3, ncol=3, byrow=TRUE)
reclas.cbn.comuna <- reclassify(r.cbn.comuna, m3, include.lowest=TRUE)

plot(reclas.cbn.comuna)
hist(reclas.cbn.comuna)


# red vial
plot(r.red.vial.comuna.utm18)
reclas.red.vial.comuna <- r.red.vial.comuna.utm18

# fin ----


# Calculo pre-transition ----

r.costo <- overlay(reclas.dem.comuna, reclas.pendiente.comuna, reclas.cbn.comuna,
                   reclas.red.vial.comuna,
                   fun = function(r1, r2, r3, r4) { return( (r1+r2+r3)*r4 ) })
plot(r.costo)
plot(red.vial.comuna.utm18, lty = 2, col = 'red', add=TRUE)
zoom(r.costo, ext=drawExtent())

# writeRaster(r.costo, filename="r_costo.tif", format="GTiff", overwrite=TRUE)

# fin ---


# 5.Calculo transition layer ----

t1 <- transition(r.costo, function(x) 1/mean(x), 8) # se demoro 3 minutos
t1.g <- geoCorrection(t1) # se demoro 5 minutos

# fin ---


# Calculo Shortest Path ----

# origen y destino

plot(r.costo)
plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3)
destino <- hospital.comuna

coordenadas <- locator(n=1)
origen0 <- SpatialPoints(c(coordenadas[1], coordenadas[2]), proj4string = CRS(utm18))

db.origen <- data.frame(nombre = 'origen')

origen <- SpatialPointsDataFrame(origen, data = db.origen, match.ID = TRUE)

plot(origen, pch = 16, col = 'red', add=TRUE)
text(origen, 'origen', pos = 3)


# calculo del camino mas corto

origen.al.destino <- shortestPath(t1.g, origen, destino, output="SpatialLines") # se demora 1 minuto
destino.al.origen <- shortestPath(t1.g, destino, origen, output="SpatialLines")



# resultado

plot(lim.comuna.utm18, axes = TRUE)
plot(red.vial.comuna.utm18, lty = 2, col = 'red', add = TRUE)

plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3, halo = TRUE)

plot(origen, pch = 16, col = 'red', add=TRUE)
text(origen, 'origen', pos = 3, halo = TRUE)

plot(origen.al.destino, lty = 1, lwd = 2, col = 'blue', add = TRUE)
plot(destino.al.origen, lty = 2, lwd = 2, col = 'green', add = TRUE)


longitud.origen.al.destino <- round(gLength(origen.al.destino)/1000, 2)
longitud.origen.al.destino

longitud.destino.al.origen <- round(gLength(destino.al.origen)/1000, 2)
longitud.destino.al.origen

leyenda.longitud.origen.al.destino <- paste('Desde origen al hospital', '=', longitud.origen.al.destino, 'km', sep = ' ') 
leyenda.longitud.origen.al.destino

leyenda.longitud.destino.al.origen <- paste('Desde el hospital al origen', '=', longitud.destino.al.origen, 'km', sep = ' ') 
leyenda.longitud.destino.al.origen

legend('bottomleft', lty = c(1, 2), lwd = c(2, 2), col = c('blue', 'green'),
       legend = c(leyenda.longitud.origen.al.destino, leyenda.longitud.destino.al.origen), 
       bty = 'n', cex = 0.8)

addscalebar(pos = 'bottomright', label.col = 'black', plotepsg = 32718, style = 'ticks') 
addnortharrow(pos = "topleft", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.6) 

# opti <- SpatialLinesDataFrame(BtoA, 
#                               data = data.frame(id= c(1:nrow(origen@coords)) ), 
#                               match.ID = TRUE)

#writeOGR(opti, ".", "shortestPath", driver="ESRI Shapefile", 
#         overwrite_layer = TRUE)

# fin ---