# install.packages('raster')
# install.packages('rgdal')
# install.packages('rgeos')
# install.packages('prettymapr')

library(raster) # lectura y herramientas para coberturas raster
library(rgdal) # lectura de coberturas vectoriales 
library(rgeos) # herramientas SIG
library(prettymapr) # barra de escala y Norte

# rm(list=ls())
# dev.off()

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

# Division administrativa ----
setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/DivisionPoliticaAdministrativa2019/')

list.files(pattern = '.shp')

lim <- readOGR('.', 'DivisionPoliticaAdministrativa2019')

head(lim@data)
str(lim@data)

levels(lim@data$CUT_REG)

id.reg <- which(lim@data$CUT_REG == '09')
lim.09 <- lim[id.reg,]
head(lim.09@data)

plot(lim.09)

crs(lim.09)
lim.09.utm18 <- spTransform(lim.09, utm18)
crs(lim.09.utm18)

# fin ---




# Red vial ----

setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/Red_Vial_Chile_05_05_2020/')

list.files()

red.vial <- readOGR(dsn = 'Red_Vial_Chile_05_05_2020.gdb')
head(red.vial@data)
str(red.vial@data)

red.vial@data$REGION <- as.character(red.vial@data$REGION)
unique(red.vial@data$REGION)

red.vial@data$REGION <- gsub('RegiÃ³n', 'Region', red.vial@data$REGION)
unique(red.vial@data$REGION)

red.vial@data$REGION[1]
red.vial@data$REGION <- gsub(red.vial@data$REGION[1], 'Region de La Araucania', red.vial@data$REGION)
unique(red.vial@data$REGION)


red.vial@data$REGION <- gsub('Ã¡', 'a', red.vial@data$REGION)
unique(red.vial@data$REGION)

red.vial@data$REGION <- gsub('Ã‘', 'N', red.vial@data$REGION)
unique(red.vial@data$REGION)

red.vial@data$REGION <- gsub('Ã±', 'n', red.vial@data$REGION)
unique(red.vial@data$REGION)

red.vial@data$REGION <- gsub('Ã', 'i', red.vial@data$REGION)
unique(red.vial@data$REGION)

red.vial@data$REGION <- gsub('i©', 'e', red.vial@data$REGION)
unique(red.vial@data$REGION)


id.reg <- which(red.vial@data$REGION=='Region de La Araucania')
red.vial.09 <- red.vial[id.reg,]
head(red.vial.09@data)

crs(red.vial.09)
plot(red.vial.09, axes=TRUE)

red.vial.09.utm18 <- spTransform(red.vial.09, utm18)
crs(red.vial.09.utm18)
plot(red.vial.09.utm18, axes=TRUE)

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

crs(hospitales.09)
plot(hospitales.09, axes=TRUE)

hospitales.09.utm18 <- spTransform(hospitales.09, utm18)
crs(hospitales.09.utm18)
plot(hospitales.09.utm18, axes=TRUE)

# fin ---



# mapa ----

plot(lim.09.utm18, axes=TRUE)
plot(red.vial.09.utm18, lty=2, col='red', add=TRUE)
plot(hospitales.09.utm18, pch=16, add=TRUE)

legend('bottomright', lty = c(NA, 2, NA), pch = c(0, NA, 16), col = c('black', 'red', 'black'), 
       legend = c('Comunas', 'Red vial', 'Hospitales'))


area.region <- round( gArea(lim.09.utm18)/1000000, 2) ; area.region
n.comunas <- nrow(lim.09.utm18@data) ; n.comunas
longitud.red.vial <- round(gLength(red.vial.09.utm18)/1000, 2) ; longitud.red.vial
n.hospitales.region <- nrow(hospitales.09.utm18@data) ; n.hospitales.region

leyenda.area.region <- paste('Area (km^2)', '=', area.region, sep = ' ') ; leyenda.area.region
leyenda.n.comunas <- paste('N° comunas', '=', n.comunas, sep = ' ') ; leyenda.n.comunas
leyenda.longitud.red.vial <- paste('Longitud caminos (km)', '=', longitud.red.vial, sep = ' ') ; leyenda.longitud.red.vial
leyenda.n.hospitales.region <- paste('N° hospitales', '=', n.hospitales.region, sep = ' ') ; leyenda.n.hospitales.region

legend('topright', legend = c(leyenda.area.region, leyenda.n.comunas, 
                              leyenda.longitud.red.vial, leyenda.n.hospitales.region), 
       bty = 'n', cex = 0.8)

addscalebar(label.col = 'black', plotepsg = 32718, style = 'ticks') 
addnortharrow(pos = "topleft", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.6) 

# fin ---





# Depuracion de datos ----
class(lim.09.utm18@data$COMUNA)
class(hospitales.09.utm18@data$NOM_COMUNA)

lim.09.utm18@data$COMUNA <- as.character(lim.09.utm18@data$COMUNA)
hospitales.09.utm18@data$NOM_COMUNA <- as.character(hospitales.09.utm18@data$NOM_COMUNA)

lim.i <- sort( unique(lim.09.utm18@data$COMUNA) ) ; lim.i
hosp.i <- sort( unique(hospitales.09.utm18@data$NOM_COMUNA) ) ; hosp.i

hosp.i[5]
hosp.i[13]
hosp.i[14]
hosp.i[15]
hosp.i[18]
hosp.i[19]
hosp.i[21]

lim.09.utm18@data$COMUNA[ lim.09.utm18@data$COMUNA == hosp.i[5] ] <- 'Curacautin'
lim.09.utm18@data$COMUNA[ lim.09.utm18@data$COMUNA == hosp.i[13] ] <- 'Pitrufquen'
lim.09.utm18@data$COMUNA[ lim.09.utm18@data$COMUNA == hosp.i[14] ] <- 'Pucon'
lim.09.utm18@data$COMUNA[ lim.09.utm18@data$COMUNA == hosp.i[15] ] <- 'Puren'
lim.09.utm18@data$COMUNA[ lim.09.utm18@data$COMUNA == hosp.i[18] ] <- 'Tolten'
lim.09.utm18@data$COMUNA[ lim.09.utm18@data$COMUNA == hosp.i[19] ] <- 'Traiguen'
lim.09.utm18@data$COMUNA[ lim.09.utm18@data$COMUNA == hosp.i[21] ] <- 'Vilcun'

hospitales.09.utm18@data$NOM_COMUNA[ hospitales.09.utm18@data$NOM_COMUNA == hosp.i[5] ] <- 'Curacautin'
hospitales.09.utm18@data$NOM_COMUNA[ hospitales.09.utm18@data$NOM_COMUNA == hosp.i[13] ] <- 'Pitrufquen'
hospitales.09.utm18@data$NOM_COMUNA[ hospitales.09.utm18@data$NOM_COMUNA == hosp.i[14] ] <- 'Pucon'
hospitales.09.utm18@data$NOM_COMUNA[ hospitales.09.utm18@data$NOM_COMUNA == hosp.i[15] ] <- 'Puren'
hospitales.09.utm18@data$NOM_COMUNA[ hospitales.09.utm18@data$NOM_COMUNA == hosp.i[18] ] <- 'Tolten'
hospitales.09.utm18@data$NOM_COMUNA[ hospitales.09.utm18@data$NOM_COMUNA == hosp.i[19] ] <- 'Traiguen'
hospitales.09.utm18@data$NOM_COMUNA[ hospitales.09.utm18@data$NOM_COMUNA == hosp.i[21] ] <- 'Vilcun'

lim.i <- sort( unique(lim.09.utm18@data$COMUNA) ) ; lim.i
hosp.i <- sort( unique(hospitales.09.utm18@data$NOM_COMUNA) ) ; hosp.i

# fin ---




# Division administrativa (comuna de interes) ----

head(lim.09.utm18@data)
str(lim.09.utm18@data)

unique(lim.09.utm18@data$COMUNA)

comuna.de.interes <- 'Temuco'
id.comuna<- which(lim.09.utm18@data$COMUNA==comuna.de.interes)
lim.comuna.utm18 <- lim.09.utm18[id.comuna,]

plot(lim.comuna.utm18, axes=TRUE)

# fin ---


# Red vial (comuna de interes) ----

head(red.vial.09.utm18@data)

red.vial.comuna.utm18 <- crop(red.vial.09.utm18, lim.comuna.utm18)

plot(lim.comuna.utm18, axes=TRUE)
plot(red.vial.comuna.utm18, lty = 2, col = 'red', add=TRUE)

# fin ---


# Hospitales (comuna de interes) ----

head(hospitales.09.utm18)
str(hospitales.09.utm18@data)

id.comuna <- which(hospitales.09.utm18@data$NOM_COMUNA==comuna.de.interes)
hospitales.comuna.utm18 <- hospitales.09.utm18[id.comuna,]
head(hospitales.comuna.utm18@data)

etiqueta.hospital <- hospitales.comuna.utm18@data$NOMBRE ; etiqueta.hospital
class(etiqueta.hospital)

etiqueta.hospital <- as.character(etiqueta.hospital)
class(etiqueta.hospital)

plot(lim.comuna.utm18, axes=TRUE)
plot(red.vial.comuna.utm18, lty = 2, col = 'red', add=TRUE)
plot(hospitales.comuna.utm18, pch = 16, add = TRUE)
text(hospitales.comuna.utm18, pos = 3, labels = etiqueta.hospital, 
     col = 'black', cex = 0.7, halo = TRUE, hc='white', hw=0.1)

area.comuna <- round( gArea(lim.comuna.utm18)/1000000, 2) ; area.comuna
longitud.red.vial.comuna <- round(gLength(red.vial.comuna.utm18)/1000, 2) ; longitud.red.vial.comuna
n.hospitales.comuna <- nrow(hospitales.comuna.utm18@data) ; n.hospitales.comuna

leyenda.area.comuna <- paste('Area (km^2)', '=', area.comuna, sep = ' ') ; leyenda.area.comuna
leyenda.longitud.red.vial.comuna <- paste('Longitud caminos (km)', '=', longitud.red.vial.comuna, sep = ' ') ; leyenda.longitud.red.vial.comuna
leyenda.n.hospitales.comuna <- paste('N° hospitales', '=', n.hospitales.comuna, sep = ' ') ; leyenda.n.hospitales.comuna

legend('topright', legend = c(leyenda.area.comuna, leyenda.longitud.red.vial.comuna, 
                              leyenda.n.hospitales.comuna), 
       bty = 'n', cex = 0.8)

addscalebar(pos = 'bottomright', label.col = 'black', plotepsg = 32718, style = 'ticks') 
addnortharrow(pos = "topleft", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.6) 

# fin ---