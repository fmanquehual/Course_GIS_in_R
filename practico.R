# Curso de SIG en R.                                 //
# Bloque 3: practico.                               //
# Autor: Francisco Fernando Manquehual Cheuque.    //  
# Fecha de ultima modificacion: 17-07-2020.       //
# /////////////////////////////////////////////////




# Instalar los siguientes paquetes ----

# install.packages('raster')
# install.packages('rgdal')
# install.packages('rgeos')
# install.packages('dplyr')
# install.packages('gdistance')
# install.packages('prettymapr')

# fin ---


# pedir que instalen previamente los paquetes
# plantear el problema
# decir que las coberturas vectoriales las debo rasterizar y reclasificar
# tener una imagen preparada para mostrar las consecuencias de dissolve=TRUE EN RED VIAL
# colocar la funcion de guar al terminar cada seccion
# estudiar la funcion transition()


# Cargar librerias ----

library(raster) # lectura y herramientas para coberturas raster y vectoriales
library(rgdal) # lectura de coberturas vectoriales 
library(rgeos) # herramientas SIG
library(dplyr) # join entre db de cobertura y data frame
library(gdistance) # Para calculo del camino mas corto
library(prettymapr) # barra de escala y Norte

# fin ---


# rm(list=ls())
# dev.off()


# Sistemas de coordenadas que se utilizaran ----

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84

# fin ---




# Division administrativa ----

# Lectura de capa

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/DivisionPoliticaAdministrativa2019/')

lim <- readOGR('.', 'DivisionPoliticaAdministrativa2019')
crs(lim)


# Filtro

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

# Lectura de capa

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/')

dem <- raster('9.jp2')
crs(dem)

compareCRS(dem, lim.comuna)
lim.comuna.utm18 <- spTransform(lim.comuna, utm18)


# Recorte de capa

dem.comuna0 <- crop(dem, lim.comuna.utm18)
dem.comuna <- mask(dem.comuna0, lim.comuna.utm18)
plot(dem.comuna)

# fin ---




# Pendiente ----

pendiente.comuna <- terrain(dem.comuna, opt = "slope", unit = "degrees")
plot(pendiente.comuna)

# fin ---




# Red vial ----

# Lectura de capa

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/red_vial_labgeo/')

red.vial <- readOGR('.', 'cl_09red_vial_utm18')
head(red.vial@data)
crs(red.vial)

compareCRS(dem.comuna, red.vial)


# Recorte de capa

red.vial.comuna <- crop(red.vial, lim.comuna.utm18)
plot(lim.comuna.utm18)
plot(red.vial.comuna, lty = 2, col = 'red', add = TRUE)


# Buffer

red.vial.comuna.buffer <- buffer(red.vial.comuna, width=25, dissolve = FALSE)
plot(red.vial.comuna.buffer)


# Rasterizacion

head(red.vial.comuna.buffer@data)
red.vial.comuna.buffer@data$valor <- 0
head(red.vial.comuna.buffer@data)

r.red.vial.comuna <- rasterize(red.vial.comuna.buffer, dem.comuna, 
                               field='valor', fun='last', background=1)

plot(r.red.vial.comuna)
unique(r.red.vial.comuna)

zoom(r.red.vial.comuna, ext=drawExtent())

# fin ---




# Hospitales ----

# Lectura de capa

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/hospitales/')

hospitales <- readOGR('.', 'HOSPITALES_SNSS_24102018')
head(hospitales@data)
str(hospitales@data)

hospitales@data <- hospitales@data[, c('NOMBRE', 'COD_REGION', 'NOM_REGION', 'SERVIC_SAL', 'NOM_COMUNA')]
head(hospitales@data)

unique(hospitales@data$COD_REGION)


# Filtro

id.reg <- which(hospitales@data$COD_REGION=='9')
hospitales.09 <- hospitales[id.reg,]

compareCRS(hospitales.09, dem.comuna)
hospitales.09.utm18 <- spTransform(hospitales.09, utm18)

unique(hospitales.09.utm18@data$NOM_COMUNA)

id.comuna <- which(hospitales.09.utm18@data$NOM_COMUNA==comuna.de.interes)
hospital.comuna <- hospitales.09.utm18[id.comuna,]

plot(lim.comuna.utm18)
plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3, cex = 0.8)

# fin ---




# Uso de suelo ----

# Lectura de capa

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/')
list.files(pattern = '.shp')

cbn <- readOGR('.', 'Catastro_RV_R09_2014')
head(cbn@data)
str(cbn@data)

crs(cbn)
compareCRS(cbn, dem.comuna)


# Join

colnames(cbn@data)

cbn@data <- cbn@data[,c("ID_SUBUSO", "SUBUSO")]
head(cbn@data)
unique(cbn@data$ID_SUBUSO)
unique(cbn@data$SUBUSO)

longitud.subuso <- length( unique(cbn@data$SUBUSO) ) ; longitud.subuso
categorias.subuso <- unique(cbn@data$SUBUSO) ; categorias.subuso

subuso.id <- data.frame(id=1:longitud.subuso, SUBUSO=categorias.subuso)
head(subuso.id)

cbn@data <- left_join(cbn@data, subuso.id, by='SUBUSO')
head(cbn@data)


# Recorte de capa

cbn.comuna <- crop(cbn, lim.comuna.utm18)


# Rasterizacion

r.cbn.comuna <- rasterize(cbn.comuna, dem.comuna, field="id", fun="last", background=NA)

plot(r.cbn.comuna)

# fin ---




# Distancia respecto al hospital ----

# Rasterizacion

head(hospital.comuna@data)
hospital.comuna@data$id <- 1:nrow(hospital.comuna@data)
head(hospital.comuna@data)

r.hospital.comuna <- rasterize(hospital.comuna, dem.comuna, field="id", fun="last", background=NA)
plot(r.hospital.comuna)


# Calculo de distancia en km

dist.hospital.comuna <- distance(r.hospital.comuna)/1000 

plot(dist.hospital.comuna)
plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3, cex = 0.8)


# Distancia acumulada en caminos

dist.hospital.red.vial <- mask(dist.hospital.comuna, red.vial.comuna.buffer)
plot(dist.hospital.red.vial)

zoom(dist.hospital.red.vial, ext = drawExtent())

# fin ---




# Distancia respecto al hospital ----

# Escogiendo un punto de origen

plot(lim.comuna.utm18)
plot(red.vial.comuna, lty = 2, col = 'red', add = TRUE)
plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3)

coordenadas <- locator(n=1)


# Transformacion a Spatial Points Data Frame

origen0 <- SpatialPoints(c(coordenadas[1], coordenadas[2]), proj4string = CRS(utm18))

db.origen <- data.frame(nombre = 'origen', id = 1)

origen <- SpatialPointsDataFrame(origen0, data = db.origen, match.ID = TRUE)

plot(origen, pch = 16, col = 'red', add=TRUE)
text(origen, origen$nombre, pos = 3)


# Rasterizacion

r.origen <- rasterize(origen, dem.comuna, field="id", fun="last", background=NA)


# Calculo de distancia en km

dist.origen <- distance(r.origen)/1000
plot(dist.origen)
plot(origen, pch = 16, col = 'red', add=TRUE)
text(origen, 'origen', pos = 3)


# Distancia acumulada en caminos

dist.origen.red.vial <- mask(dist.origen, red.vial.comuna.buffer)
plot(dist.origen.red.vial)

# fin ---




# Suma de distancias ----

dist.origen.y.hospital <- sum(dist.hospital.red.vial, dist.origen.red.vial)
plot(dist.origen.y.hospital)

zoom(dist.origen.y.hospital, ext = drawExtent())

# fin ---




# Reclasificacion ----

# DEM 
minValue(dem.comuna)
maxValue(dem.comuna)

# (minimo, maximo, nuevo valor) -> (no se incluye, <=, nuevo valor) por default
pre.m1 <- c(0, 100, 100,
       100, 200, 200,
       200, 300, 500,
       300, 400, 1000,
       400, maxValue(dem.comuna), 3000)
pre.m1 

m1 <- matrix(pre.m1, ncol=3, byrow=TRUE) ; m1
reclas.dem.comuna <- reclassify(dem.comuna, m1)

plot(reclas.dem.comuna)
hist(reclas.dem.comuna, xlab = 'Costo', main = 'Elevación')


# Pendiente 
minValue(pendiente.comuna)
maxValue(pendiente.comuna)

pre.m2 <- c(-1, 10, 10000,
       10, 20, 50000,
       20, 30, 100000,
       30, maxValue(pendiente.comuna), 1000000)

m2 <- matrix(pre.m2, ncol=3, byrow=TRUE)
reclas.pendiente.comuna <- reclassify(pendiente.comuna, m2)

plot(reclas.pendiente.comuna)
hist(reclas.pendiente.comuna, xlab = 'Costo', main = 'Pendiente')


# Uso de suelo 
ids.comuna <- unique(r.cbn.comuna)

cbn.comuna.unicos <- cbn.comuna@data[!duplicated(cbn.comuna@data$SUBUSO), ]
cbn.comuna.ordenados <- cbn.comuna.unicos[order(cbn.comuna.unicos$id),]
cbn.comuna.ordenados

pre.m3 <- c( 1, 50000,
             2, 100000,
             3, 200000,
             4, 100000,
             5, 1000,
             6, 50000,
             7, 0,
             8, 50000,
             9, 100000,
             10, 1000,
             11, 1000,
             13, 100000,
             14, 50000,
             15, 50000)

m3 <- matrix(pre.m3, ncol=2, byrow=TRUE)
reclas.cbn.comuna <- reclassify(r.cbn.comuna, m3)

plot(reclas.cbn.comuna)
hist(reclas.cbn.comuna, xlab = 'Costo', main = 'Uso de suelo')


# Red vial

plot(r.red.vial.comuna)
reclas.red.vial.comuna <- r.red.vial.comuna


# Distancia entre origen y hospital

plot(dist.origen.y.hospital)
dist.origen.y.hospital[]

dist.origen.y.hospital[dist.origen.y.hospital[]%in%NA] <- 0
reclas.dist.origen.y.hospital <- dist.origen.y.hospital

plot(reclas.dist.origen.y.hospital)
hist(reclas.dist.origen.y.hospital, xlab = 'Distancia (km)', main = 'Distancia acumulada')

# fin ---




# Calculo costo total ----

r.costo <- overlay(reclas.dem.comuna, reclas.pendiente.comuna, reclas.cbn.comuna,
                   reclas.red.vial.comuna, reclas.dist.origen.y.hospital,
                   fun = function(r1, r2, r3, r4, r5) { return(( (r1 + r2 + r3)*r4 ) + r5) })
plot(r.costo)
plot(red.vial.comuna, lty = 2, col = 'red', add=TRUE)
zoom(r.costo, ext=drawExtent())

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/resultados_de_procesos/')
# 
# writeRaster(r.costo, filename="r_costo.tif", format="GTiff", overwrite=TRUE)

# fin ---




# Calculo transition layer ----

t <- transition(r.costo, function(x) 1/mean(x), 4)
t.g <- geoCorrection(t)

# fin ---




# Calculo camino mas corto ----

# Calculo desde el origen hasta el hospital

origen.al.hospital0 <- shortestPath(t.g, origen, hospital.comuna, output="SpatialLines")

plot(lim.comuna.utm18, axes = TRUE)

plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3, halo = TRUE)

plot(origen, pch = 16, col = 'red', add=TRUE)
text(origen, 'origen', pos = 1, halo = TRUE)

plot(origen.al.hospital0, col = 'blue', add = TRUE)

# Transformacion a Spatial Lines Data Frame

db.origen.al.hospital <- data.frame(sentido = 'origen.al.hospital')
db.origen.al.hospital

origen.al.hospital <- SpatialLinesDataFrame(sl = origen.al.hospital0,
                                            data = db.origen.al.hospital,
                                            match.ID = TRUE)
origen.al.hospital


# Calculo desde el hospital hasta el origen

hospital.al.origen0 <- shortestPath(t.g, hospital.comuna, origen, output="SpatialLines")

plot(lim.comuna.utm18, axes = TRUE)

plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3, halo = TRUE)

plot(origen, pch = 16, col = 'red', add=TRUE)
text(origen, 'origen', pos = 1, halo = TRUE)

plot(hospital.al.origen0, lty = 2, col = 'green', add = TRUE)


# Transformacion a Spatial Lines Data Frame

db.hospital.al.origen <- data.frame(sentido = 'hospital.al.origen')
db.hospital.al.origen

hospital.al.origen <- SpatialLinesDataFrame(sl = hospital.al.origen0,
                                            data = db.hospital.al.origen,
                                            match.ID = TRUE)
hospital.al.origen

# fin ---




# Union de caminos cortos ----

caminos.mas.cortos <- raster::union(origen.al.hospital, hospital.al.origen)

plot(lim.comuna.utm18, axes = TRUE)

plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3, halo = TRUE)

plot(origen, pch = 16, col = 'red', add=TRUE)
text(origen, 'origen', pos = 1, halo = TRUE)

plot(caminos.mas.cortos, lty = c(1, 2), lwd = 2, col = c('blue', 'green'), add = TRUE)

zoom(caminos.mas.cortos, lty = c(1, 2), lwd = 2, col = c('blue', 'green'), 
     ext = drawExtent(), new = TRUE)

#writeOGR(caminos.mas.cortos, ".", "caminos.mas.cortos", driver="ESRI Shapefile", 
#         overwrite_layer = TRUE)

# fin ---




# Mapa final ----

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/plots/')
# pdf('camino_mas_corto.pdf', width = 7.5, height = 7.2)

plot(lim.comuna.utm18, axes = TRUE)
plot(red.vial.comuna, lty = 2, col = 'red', add = TRUE)

plot(hospital.comuna, pch = 16, add = TRUE)
text(hospital.comuna, hospital.comuna@data$NOMBRE, pos = 3, halo = TRUE)

plot(origen, pch = 16, col = 'red', add=TRUE)
text(origen, 'origen', pos = 1, halo = TRUE)

plot(origen.al.hospital, lty = 1, lwd = 2, col = 'blue', add = TRUE)
plot(hospital.al.origen, lty = 2, lwd = 2, col = 'green', add = TRUE)

longitud.origen.al.destino <- round(gLength(origen.al.hospital)/1000, 2)
longitud.origen.al.destino

longitud.destino.al.origen <- round(gLength(hospital.al.origen)/1000, 2)
longitud.destino.al.origen

leyenda.longitud.origen.al.destino <- paste('Desde origen al hospital', '=', 
                                            longitud.origen.al.destino, 'km', sep = ' ') 
leyenda.longitud.origen.al.destino

leyenda.longitud.destino.al.origen <- paste('Desde el hospital al origen', '=', 
                                            longitud.destino.al.origen, 'km', sep = ' ') 
leyenda.longitud.destino.al.origen

legend('topright', pch = c(0, NA), lty = c(NA, 2), col = c('black', 'red'), 
       legend = c('Galvarino', 'Red vial'))

legend('bottomleft', lty = c(1, 2), lwd = c(2, 2), col = c('blue', 'green'),
       legend = c(leyenda.longitud.origen.al.destino, leyenda.longitud.destino.al.origen), 
       bty = 'n', cex = 0.8)

addscalebar(pos = 'bottomright', label.col = 'black', plotepsg = 32718, style = 'ticks') 
addnortharrow(pos = "topleft", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.6) 

# dev.off()

# fin ---