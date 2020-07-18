# Curso de SIG en R.                                 //
# Bloque 2: capas raster.                           //
# Autor: Francisco Fernando Manquehual Cheuque.    //  
# Fecha de ultima modificacion: 17-07-2020.       //
# /////////////////////////////////////////////////




# Instalar los siguientes paquetes ----

# install.packages('raster')
# install.packages('rgdal')
# install.packages('rasterVis')
# install.packages('ggplot2')
# install.packages('ggspatial')
# install.packages('ggridges')
# install.packages('prettymapr')
# install.packages('cowplot')
# install.packages('xlsx')

# fin ---




# Cargar librerias ----

library(raster) # lectura y herramientas para coberturas raster y vectoriales
library(rgdal) # lectura de coberturas vectoriales 
library(rasterVis) # nuevos metodos de visualizacion de mapas
library(ggplot2) # nuevos metodos de visualizacion de mapas
library(ggspatial) # nuevos metodos de visualizacion de mapas
library(ggridges) # graficos de densidad
library(prettymapr) # barra de escala y Norte
library(cowplot) # multiplots
library(xlsx) # guardar una base de datos en formato xlsx (Excel)

# fin ---




# Sistemas de coordenadas que se utilizaran ----
      
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84

# fin ---




# Historico ----

# Lectura de capas

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/temperatura_maxima_historico/')

list.files()
t.max1 <- raster('wc2.1_2.5m_tmax_01.tif')
t.max1

plot(t.max1, main = 'Enero')

lista.archivos.historicos <- list.files(pattern = '.tif') ; lista.archivos.historicos
t.max.historico <- stack(lista.archivos.historicos)
t.max.historico
names(t.max.historico)


# Cambiando el nombre a las capas

meses.anho <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio',
                            'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')

names(t.max.historico) <- meses.anho
t.max.historico

plot(t.max.historico, 3:6)

# fin ---




# Futuro ----

# Lectura de capas

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/temperatura_maxima_futuro/share/spatial03/worldclim/cmip6/7_fut/2.5m/MIROC6/ssp585/')

t.max.futuro0 <- raster('wc2.1_2.5m_tmax_MIROC6_ssp585_2081-2100.tif')
t.max.futuro0

t.max.futuro <- stack('wc2.1_2.5m_tmax_MIROC6_ssp585_2081-2100.tif')
t.max.futuro


# Cambiando el nombre a las capas

names(t.max.futuro) <- meses.anho
t.max.futuro

plot(t.max.futuro, 3:6)

# fin ---




# Division administrativa ----

# Lectura de capa

setwd('C:/Users/Usuario/Documents/Francisco/curso_SIG_en_R/DivisionPoliticaAdministrativa2019/')

lim <- readOGR('.', 'DivisionPoliticaAdministrativa2019')


# Filtro

id.reg <- which(lim@data$CUT_REG == '09')
lim.09 <- lim[id.reg,]
head(lim.09@data)

marco <- spTransform(lim.09, wgs84)
plot(marco, axes = TRUE)

# fin ---




# Recorte de capas raster ----

# historico

t.max.historico.clip0 <- crop(t.max.historico, marco)
plot(t.max.historico.clip0, 3:6)

t.max.historico.clip <- mask(t.max.historico.clip0, marco)
plot(t.max.historico.clip, 3:6)


# Futuro 

t.max.futuro.clip0 <- crop(t.max.futuro, marco)
t.max.futuro.clip <- mask(t.max.futuro.clip0, marco)
plot(t.max.futuro.clip, 3:6)

# fin ---




# Ejemplo reprojeccion ----


t.max.historico.clip.utm18 <- projectRaster(t.max.historico.clip, crs = utm18)

crs(t.max.historico.clip)
crs(t.max.historico.clip.utm18)

par(mfrow = c(1, 2))
plot(t.max.historico.clip, main = 'Enero - WGS84', 1)
plot(t.max.historico.clip.utm18, main = 'Enero - WGS84 UTM 18S', 1)


# fin ---




# Diferentes tipos de mapas ----

# configuraciones previas 

paleta.colores <- heat.colors(100)
myTheme <- rasterTheme(region = paleta.colores)
intervalo.leyenda <- seq(0, 40, by=1)
unidad.variable <- '°C'
nombre.eje.y <- 'Latitud'
nombre.eje.x <- 'Longitud'


# Mapa con ggplot

t.max.futuro.clip1 <- t.max.futuro.clip[[1]]

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/plots/')
# png('tmax_futuro_enero_09_ggplot.png', width = 500, height = 500, units = "px")

ggplot() +
  layer_spatial(t.max.futuro.clip1) +
  labs(y=nombre.eje.y, x=nombre.eje.x) +
  scale_fill_gradientn(name = unidad.variable, colours = paleta.colores, na.value = 'transparent') +
  coord_sf(crs=4326, expand = FALSE) +
  theme_bw() +
  annotation_scale(location = "bl", text_col = 'black', style = 'ticks', line_col = 'black') + 
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm"))

# dev.off()


# Mapa con levelplot

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/plots/')
# png('tmax_futuro_enero_09_levelplot.png', width = 500, height = 500, units = "px")

levelplot(t.max.futuro.clip, layer = 1, margin = list(FUN = median, axis = TRUE, 
                                           scales = list(x=c(0, 40),
                                                         y=c(0, 40))), 
          par.settings=myTheme, colorkey=TRUE, xlab = nombre.eje.x, ylab = nombre.eje.y,
          main='Enero', at=intervalo.leyenda)

# dev.off()


# Multiples mapas con levelplot

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/plots/')
# png('tmax_futuro_mensual_09_levelplot.png', width = 720, height = 720, units = "px")

levelplot(t.max.futuro.clip, par.settings=myTheme, colorkey=list(space='right'), 
          main = 'Temperatura máxima esperada a fines del siglo XXI',
          xlab = nombre.eje.x, ylab = nombre.eje.y, at=intervalo.leyenda)

# dev.off()

# fin ---




# Raster a data frame (Historico)----

# Raster a data frame

db.t.max.historico0 <- as.data.frame(t.max.historico.clip, na.rm = TRUE, xy = TRUE)
head(db.t.max.historico0)
dim(db.t.max.historico0)

db.t.max.historico <- round(db.t.max.historico0, 3)
head(db.t.max.historico)
dim(db.t.max.historico0)


# Guardar base de datos

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/base_de_datos_creados/')
# 
# write.xlsx(db.t.max.historico, file = 'datos_historicos_promedio_mensual_tmax_09.xlsx',
#            sheetName = 'temperatura_maxima', row.names = FALSE)
# 
# write.csv(db.t.max.historico, file = 'datos_historicos_promedio_mensual_tmax_09.csv',
#           row.names = FALSE)

# fin ---




# Raster a data frame (Futuro)----

db.t.max.futuro0 <- as.data.frame(t.max.futuro.clip, na.rm = TRUE, xy = TRUE)
head(db.t.max.futuro0)
dim(db.t.max.futuro0)

db.t.max.futuro <- round(db.t.max.futuro0, 3)
head(db.t.max.futuro)
dim(db.t.max.futuro0)

# Guardar base de datos

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/base_de_datos_creados/')
# 
# write.xlsx(db.t.max.futuro, file = 'datos_futuro_promedio_mensual_tmax_09.xlsx',
#            sheetName = 'temperatura_maxima', row.names = FALSE)
# 
# write.csv(db.t.max.futuro, file = 'datos_futuro_promedio_mensual_tmax_09.csv',
#           row.names = FALSE)

# fin ---




# Boxplots Historico vs Futuro ----

# preparacion de db's

mar.i <- 0
db.t.max.historico <- db.t.max.historico[, -c(1, 2)]
db.t.max.futuro <- db.t.max.futuro[, -c(1, 2)]


# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/plots/')
# png('boxplot_tmax_historico_vs_futuro.png', width = 720, height = 480, units = "px")

par(mfrow=c(1,2), oma=c(6,4,1,1), mar=c(mar.i,mar.i,mar.i,mar.i))

boxplot(db.t.max.historico, ylim=c(0, 40), las=2)
mtext(unidad.variable, side=2, line = 3)
mtext('Historico', side=3, line = -2)

boxplot(db.t.max.futuro, yaxt='n', las=2)
mtext('2081-2100', side=3, line = -2)

# dev.off()

# fin ---




# Graficos de densidad historico ----

# Enero

nombre.mes.i <- colnames(db.t.max.historico)[1] ; nombre.mes.i
valor.mes.i <- db.t.max.historico[,1] ; head(valor.mes.i)

db.t.max.historico.1 <- data.frame(mes = nombre.mes.i, valor = valor.mes.i)
head(db.t.max.historico.1)


# Febrero

nombre.mes.i <- colnames(db.t.max.historico)[2] ; nombre.mes.i
valor.mes.i <- db.t.max.historico[,2] ; head(valor.mes.i)

db.t.max.historico.2 <- data.frame(mes = nombre.mes.i, valor = valor.mes.i)
head(db.t.max.historico.2)


# Union de base de datos

db.t.max.historico.1.2 <- rbind(db.t.max.historico.1, db.t.max.historico.2) 
head(db.t.max.historico.1.2)
table(db.t.max.historico.1.2$mes)


# Loop que permite repetir lo anterior 'i' (marzo a diciembre) veces

db.t.max.historico.full0 <- c()
for (i in 3:12) {
  
  # mes i
  nombre.mes.i <- colnames(db.t.max.historico)[i]
  valor.mes.i <- db.t.max.historico[,i]
  
  db.t.max.historico.i <- data.frame(mes = nombre.mes.i, valor = valor.mes.i)
  
  # union de base de datos desde marzo a diciembre
  db.t.max.historico.full0 <- rbind(db.t.max.historico.full0, db.t.max.historico.i)
  
}

db.t.max.historico.full0
table(db.t.max.historico.full0$mes)


# Union de base de datos final

db.t.max.historico.full <- rbind(db.t.max.historico.1.2, db.t.max.historico.full0)
table(db.t.max.historico.full$mes)

unique(db.t.max.historico.full$mes)


# Preparacion de base de datos

db.t.max.historico.full$mes <- factor( db.t.max.historico.full$mes, levels = rev(meses.anho) )
levels(db.t.max.historico.full$mes)


# Grafico de densidad 

ggplot(db.t.max.historico.full, aes(x = valor, y = mes, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradientn(unidad.variable, colours = paleta.colores) +
  labs(title = 'Historico', x = unidad.variable, y = NULL) +
  theme_bw() 

# fin ---




# Graficos de densidad futuro ----

# Enero

nombre.mes.j <- colnames(db.t.max.futuro)[1] ; nombre.mes.j
valor.mes.j <- db.t.max.futuro[,1] ; head(valor.mes.j)

db.t.max.futuro.1 <- data.frame(mes = nombre.mes.j, valor = valor.mes.j)
head(db.t.max.futuro.1)


# Loop que permite repetir lo anterior 'j' (marzo a diciembre) veces

db.t.max.futuro.full0 <- c()
for (j in 2:12) {
  
  # mes j
  nombre.mes.j <- colnames(db.t.max.futuro)[j]
  valor.mes.j <- db.t.max.futuro[,j]
  
  db.t.max.futuro.j <- data.frame(mes = nombre.mes.j, valor = valor.mes.j)
  
  # union de base de datos desde marzo a diciembre
  db.t.max.futuro.full0 <- rbind(db.t.max.futuro.full0, db.t.max.futuro.j)
  
}


# Union de base de datos final

db.t.max.futuro.full <- rbind(db.t.max.futuro.1, db.t.max.futuro.full0)
head(db.t.max.futuro.full)
table(db.t.max.historico.full$mes)


# Preparacion de base de datos

db.t.max.futuro.full$mes <- factor( db.t.max.futuro.full$mes, levels = rev(meses.anho) )
levels(db.t.max.futuro.full$mes)


# Grafico de densidad 
ggplot(db.t.max.futuro.full, aes(x = valor, y=mes, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradientn(unidad.variable, colours = paleta.colores) +
  labs(title = 'Futuro (1981-2100)', x = unidad.variable, y = NULL) +
  theme_bw() 

# fin ---




# Distribucion de tmax historico vs futuro ---- 

# Union de base de datos

db.t.max.historico.full$periodo <- 'Historico'
db.t.max.futuro.full$periodo <- 'Futuro'

db.t.max.full <- rbind(db.t.max.historico.full, db.t.max.futuro.full)
head(db.t.max.full)
table(db.t.max.full$periodo)


# Preparacion de base de datos

db.t.max.full$periodo <- factor( db.t.max.full$periodo, levels = c('Historico', 'Futuro') )
levels(db.t.max.full$periodo)


# Grafico de densidad

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/plots/')
# png('distribucion_tmax_historico_vs_futuro.png', width = 720, height = 720, units = "px")

ggplot(db.t.max.full, aes(x = valor, y=mes, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradientn(unidad.variable, colours = paleta.colores) +
  labs(x = unidad.variable, y = NULL) +
  theme_dark() +
  facet_wrap(~periodo, nrow = 2)

# dev.off()

# fin ---




# Temperatura anual historica vs esperada (2081-2100) ----

# Calculo para capas del periodo historico

t.max.historico.anual <- mean(t.max.historico.clip)
plot(t.max.historico.anual)

# Guardar capa

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/coberturas_finales/')
# 
# writeRaster(t.max.historico.anual, filename="tmax_media_anual_historico.tif", 
#             format="GTiff", overwrite=TRUE)


# Calculo para capas del periodo futuro

t.max.futuro.anual <- mean(t.max.futuro.clip)
plot(t.max.futuro.anual)

# Guardar capa

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/coberturas_finales/')
# 
# writeRaster(t.max.futuro.anual, filename="tmax_media_anual_futuro.tif", 
#             format="GTiff", overwrite=TRUE)


# Preparacion de capas

t.max.hist.futu <- stack(t.max.historico.anual, t.max.futuro.anual)
names(t.max.hist.futu) <- c('Historico', 'Futuro')


# Mapa periodo historico vs futuro

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/plots/')
# png('mapa_tmax_anual_historico_vs_futuro.png', width = 600, height = 300, units = "px")

levelplot(t.max.hist.futu, par.settings=myTheme, colorkey=list(space='right'), 
          xlab = nombre.eje.x, ylab = nombre.eje.y, at=intervalo.leyenda)

# dev.off()

# fin ---




# Diferencia de temperatura entre periodo futuro e historico ----

# Calculando la diferencia
t.max.hist.futu.diferencia <- overlay(t.max.historico.anual, t.max.futuro.anual, 
                                      fun = function(r1, r2) { return( r2-r1 ) })

minValue(t.max.hist.futu.diferencia)
maxValue(t.max.hist.futu.diferencia)

# Guardar capa

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/coberturas_finales/')
# 
# writeRaster(t.max.hist.futu.diferencia, filename="diferencia_tmax_anual_historico_y_futuro.tif", 
#             format="GTiff", overwrite=TRUE)


# Mapa

# setwd('C:/Users/Usuario/Documents/Francisco/Curso_SIG_en_R/plots/')
# png('mapa_diferencia_tmax_anual_historico_y_futuro.png', width = 600, height = 600, units = "px")

levelplot(t.max.hist.futu.diferencia, margin = list(FUN = median, axis = TRUE, 
                                                      scales = list(x=c(0, 5),
                                                                    y=c(0, 5))), 
          par.settings=myTheme, colorkey=TRUE, xlab = nombre.eje.x, ylab = nombre.eje.y,
          main='Diferencia entre temperatura historica y esperada (2081-2100)')

# dev.off()

# fin ---