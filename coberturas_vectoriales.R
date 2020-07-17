# install.packages('raster')
# install.packages('rgdal')
# install.packages('rgeos')
# install.packages('prettymapr')

library(raster) # lectura y herramientas para coberturas raster
library(rgdal) # lectura de coberturas vectoriales 
library(rgeos) # herramientas SIG
library(prettymapr) # barra de escala y Norte
library(ggplot2) # Para elaboracion de graficos

# rm(list=ls())
# dev.off()

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84


# Division administrativa ----

# Lectura de capa

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/DivisionPoliticaAdministrativa2019/')

list.files()
list.files(pattern = '.shp')

lim <- readOGR(dsn = '.', layer = 'DivisionPoliticaAdministrativa2019')

head(lim@data)
str(lim@data)


# Filtro

unique(lim@data$CUT_REG)

id.reg <- which(lim@data$CUT_REG == '09')
lim.09 <- lim[id.reg,]
head(lim.09@data)

plot(lim.09, axes = TRUE)

crs(lim.09)
lim.09.utm18 <- spTransform(lim.09, utm18)
crs(lim.09.utm18)

plot(lim.09.utm18, axes = TRUE)


# Guardar capa

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/coberturas_finales/')
# 
# writeOGR(lim.09.utm18, ".", "poligono_09_utm18s", driver="ESRI Shapefile",
#         overwrite_layer = TRUE)

# fin ---




# Red vial ----

# Lectura de capa

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/Red_Vial_Chile_05_05_2020/')
list.files()

red.vial <- readOGR(dsn = 'Red_Vial_Chile_05_05_2020.gdb')
head(red.vial@data)
str(red.vial@data)


# Depuracion

nombres.regiones.unicos <- unique(red.vial@data$REGION)
nombres.regiones.unicos

red.vial@data$REGION <- gsub('RegiÃ³n', 'Region', red.vial@data$REGION)
unique(red.vial@data$REGION)

nombres.regiones.unicos <- unique(red.vial@data$REGION)
nombres.regiones.unicos

nombres.regiones.unicos[1]
red.vial@data$REGION <- gsub(nombres.regiones.unicos[1], 'Region de La Araucania', red.vial@data$REGION)
unique(red.vial@data$REGION)

nombres.regiones.unicos[3]
red.vial@data$REGION <- gsub(nombres.regiones.unicos[3], 'Region de Tarapaca', red.vial@data$REGION)
unique(red.vial@data$REGION)

nombres.regiones.unicos[5]
red.vial@data$REGION <- gsub(nombres.regiones.unicos[5], 'Region de Los Rios', red.vial@data$REGION)
unique(red.vial@data$REGION)

nombres.regiones.unicos[7]
red.vial@data$REGION <- gsub(nombres.regiones.unicos[7], 'Region de Valparaiso', red.vial@data$REGION)
unique(red.vial@data$REGION)

nombres.regiones.unicos[9]
red.vial@data$REGION <- gsub(nombres.regiones.unicos[9], 'Region de Nuble', red.vial@data$REGION)
unique(red.vial@data$REGION)

nombres.regiones.unicos[10]
red.vial@data$REGION <- gsub(nombres.regiones.unicos[10], 'Region del Biobio', red.vial@data$REGION)
unique(red.vial@data$REGION)

nombres.regiones.unicos[12]
red.vial@data$REGION <- gsub(nombres.regiones.unicos[12], 'Region de Magallanes y de la Antartica Chilena', red.vial@data$REGION)
unique(red.vial@data$REGION)

nombres.regiones.unicos[16]
red.vial@data$REGION <- gsub(nombres.regiones.unicos[16], 'Region Aysen del General Carlos Ibanez del Campo', red.vial@data$REGION)
unique(red.vial@data$REGION)

head(red.vial@data)


# Visualizacion de datos

tapply(red.vial@data$Shape_Length, red.vial@data$REGION, sum)

suma.longitud <- tapply(red.vial@data$Shape_Length, red.vial@data$REGION, sum)
suma.longitud

db.suma.longitud <- as.data.frame(suma.longitud)
db.suma.longitud
dim(db.suma.longitud)

nombre.region <- rownames(db.suma.longitud)
rownames(db.suma.longitud) <- 1:nrow(db.suma.longitud)
db.suma.longitud

db.suma.longitud$nombre.region <- nombre.region
db.suma.longitud
dim(db.suma.longitud)

longitud.en.porcentaje0 <- (db.suma.longitud$suma.longitud*100)/sum(db.suma.longitud$suma.longitud)
longitud.en.porcentaje0

longitud.en.porcentaje <- round(longitud.en.porcentaje0, 2)
longitud.en.porcentaje

db.suma.longitud$porc <- longitud.en.porcentaje
db.suma.longitud

db.suma.longitud$leyenda <- paste(db.suma.longitud$nombre.region, ' ', '(', db.suma.longitud$porc, 
                                  '%', ')', sep = '')
db.suma.longitud

# link de colores HTML: https://htmlcolorcodes.com/es/
colores.i <- c('#C0392B', '#8E44AD', '#2980B9', '#27AE60', '#F1C40F',
               '#F39C12', '#D35400', '#BDC3C7', '#34495E', '#E74C3C',
               '#78281F', '#4A235A', '#196F3D', '#7B7D7D', '#E59866',
               '#7DCEA0')

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/plots/')
# png('distribucion_de_longitud_de_caminos_por_region.png', width = 600, height = 480, units = "px")

ggplot(db.suma.longitud, aes(x="", y=porc, fill = leyenda)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(name = 'Regiones', values = colores.i) +
  labs(title = 'Distribución de la longitud de caminos por región', 
       y = NULL,
       x = NULL)

# dev.off()


# Filtro

head(red.vial@data)

id.reg <- which(red.vial@data$REGION=='Region de La Araucania')
red.vial.09 <- red.vial[id.reg,]
unique(red.vial.09@data$REGION)
head(red.vial.09@data)

red.vial.09@data <- red.vial.09@data[, c('ROL', 'ROL_LABEL', 'NOMBRE', 'ENROLADO', 'CONCESIONADO', 'Shape_Length')]
head(red.vial.09@data)

plot(red.vial.09, axes = TRUE)
compareCRS(red.vial.09, lim.09.utm18)

red.vial.09.utm18 <- spTransform(red.vial.09, utm18)
crs(red.vial.09.utm18)

plot(red.vial.09.utm18, axes=TRUE)


# Guardar capa

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/coberturas_finales/')
# 
# writeOGR(red.vial.09.utm18, ".", "linea_09_red_vial_utm18s", driver="ESRI Shapefile",
#         overwrite_layer = TRUE)

# fin ---




# Hospitales ----

# Lectura de capa

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/hospitales/')
list.files(pattern = '.shp')

hospitales <- readOGR('.', 'HOSPITALES_SNSS_24102018')
head(hospitales@data)
str(hospitales@data)


# Filtro

hospitales@data <- hospitales@data[, c('NOMBRE', 'COD_REGION', 'NOM_REGION', 'SERVIC_SAL', 'NOM_COMUNA')]
head(hospitales@data)

table(hospitales@data$NOM_REGION)
unique(hospitales@data$COD_REGION)

id.reg <- which(hospitales@data$COD_REGION=='9')
hospitales.09 <- hospitales[id.reg,]

plot(hospitales.09, axes=TRUE)
compareCRS(hospitales.09, lim.09.utm18)

hospitales.09.utm18 <- spTransform(hospitales.09, utm18)
crs(hospitales.09.utm18)

plot(lim.09.utm18, axes = TRUE)
plot(hospitales.09.utm18, pch = 16, add = TRUE)


# Guardar capa

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/coberturas_finales/')
# 
# writeOGR(hospitales.09.utm18, ".", "punto_09_hospitales_utm18s", driver="ESRI Shapefile",
#         overwrite_layer = TRUE)

# fin ---




# mapa a nivel regional ----

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/plots/')
# png('mapa_09_con_red_vial_y_hospitales.png', width = 600, height = 600, units = "px")

plot(lim.09.utm18, axes=TRUE)
plot(red.vial.09.utm18, lty=2, col='red', add=TRUE)
plot(hospitales.09.utm18, pch=16, add=TRUE)

legend('bottomright', lty = c(NA, 2, NA), pch = c(0, NA, 16), col = c('black', 'red', 'black'), 
       legend = c('Comunas', 'Red vial', 'Hospitales'))


area.region0 <- gArea(lim.09.utm18)/1000000
area.region <- round(area.region0, 2) ; area.region

n.comunas <- nrow(lim.09.utm18@data) ; n.comunas

longitud.red.vial0 <- gLength(red.vial.09.utm18)/1000 ; longitud.red.vial0
longitud.red.vial <- round(longitud.red.vial0, 2) ; longitud.red.vial

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

# dev.off()

# fin ---





# Depuracion de datos a nivel comunal ----

# Explorando datos

unique(lim.09.utm18@data$COMUNA)
unique(hospitales.09.utm18@data$NOM_COMUNA)

lim.i <- sort( unique(lim.09.utm18@data$COMUNA) ) ; lim.i
hosp.i <- sort( unique(hospitales.09.utm18@data$NOM_COMUNA) ) ; hosp.i

hosp.i[5]
hosp.i[13]
hosp.i[14]
hosp.i[15]
hosp.i[18]
hosp.i[19]
hosp.i[21]


# Depuracion de datos

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

# Explorando datos

head(lim.09.utm18@data)
str(lim.09.utm18@data)

unique(lim.09.utm18@data$COMUNA)


# Filtro

comuna.de.interes <- 'Villarrica'
id.comuna<- which(lim.09.utm18@data$COMUNA==comuna.de.interes)
lim.comuna.utm18 <- lim.09.utm18[id.comuna,]

plot(lim.comuna.utm18, axes=TRUE)


# Guardar capa

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/coberturas_finales/')
# 
# writeOGR(lim.comuna.utm18, ".", "poligono_comuna_Villarica_utm18s", driver="ESRI Shapefile",
#         overwrite_layer = TRUE)

# fin ---




# Red vial (comuna de interes) ----

# Explorando datos

head(red.vial.09.utm18@data)


# Corte de cobertura

red.vial.comuna.utm18 <- crop(red.vial.09.utm18, lim.comuna.utm18)

plot(lim.comuna.utm18, axes=TRUE)
plot(red.vial.comuna.utm18, lty = 2, col = 'red', add=TRUE)


# Guardar capa

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/coberturas_finales/')
# 
# writeOGR(red.vial.comuna.utm18, ".", "linea_comuna_Villarica_red_vial_utm18s", driver="ESRI Shapefile",
#         overwrite_layer = TRUE)

# fin ---




# Hospitales (comuna de interes) ----

# Explorando datos

head(hospitales.09.utm18)
str(hospitales.09.utm18@data)


# Filtro

id.comuna <- which(hospitales.09.utm18@data$NOM_COMUNA==comuna.de.interes)
hospitales.comuna.utm18 <- hospitales.09.utm18[id.comuna,]
head(hospitales.comuna.utm18@data)

etiqueta.hospital <- hospitales.comuna.utm18@data$NOMBRE ; etiqueta.hospital

plot(lim.comuna.utm18, axes=TRUE)
plot(red.vial.comuna.utm18, lty = 2, col = 'red', add=TRUE)
plot(hospitales.comuna.utm18, pch = 16, add = TRUE)
text(hospitales.comuna.utm18, pos = 3, labels = etiqueta.hospital, 
     col = 'black', cex = 0.7, halo = TRUE, hc='white', hw=0.1)


# Guardar capa

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/coberturas_finales/')
# 
# writeOGR(red.vial.comuna.utm18, ".", "punto_comuna_Villarica_hospital_utm18s", driver="ESRI Shapefile",
#         overwrite_layer = TRUE)

# fin ---




# mapa comunal ----

# setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/plots/')
# png('mapa_comuna_Villarrica_con_red_vial_y_hospitales.png', width = 600, height = 600, units = "px")

plot(lim.comuna.utm18, axes=TRUE)
plot(red.vial.comuna.utm18, lty = 2, col = 'red', add=TRUE)
plot(hospitales.comuna.utm18, pch = 16, add = TRUE)
text(hospitales.comuna.utm18, pos = 3, labels = etiqueta.hospital, 
     col = 'black', cex = 1, halo = TRUE, hc='white', hw=0.2)

area.comuna0 <- gArea(lim.comuna.utm18)/1000000 ; area.comuna0
area.comuna <- round(area.comuna0, 2) ; area.comuna

longitud.red.vial.comuna0 <- gLength(red.vial.comuna.utm18)/1000 ; longitud.red.vial.comuna0
longitud.red.vial.comuna <- round(longitud.red.vial.comuna0, 2) ; longitud.red.vial.comuna

n.hospitales.comuna <- nrow(hospitales.comuna.utm18@data) ; n.hospitales.comuna

leyenda.area.comuna <- paste('Area (km^2)', '=', area.comuna, sep = ' ') ; leyenda.area.comuna
leyenda.longitud.red.vial.comuna <- paste('Longitud caminos (km)', '=', longitud.red.vial.comuna, sep = ' ') ; leyenda.longitud.red.vial.comuna
leyenda.n.hospitales.comuna <- paste('N° hospitales', '=', n.hospitales.comuna, sep = ' ') ; leyenda.n.hospitales.comuna

legend('topright', legend = c(leyenda.area.comuna, leyenda.longitud.red.vial.comuna, 
                              leyenda.n.hospitales.comuna), 
       bty = 'n', cex = 0.8)

addscalebar(pos = 'bottomright', label.col = 'black', plotepsg = 32718, style = 'ticks') 
addnortharrow(pos = "topleft", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.6) 

# dev.off()

# fin ---