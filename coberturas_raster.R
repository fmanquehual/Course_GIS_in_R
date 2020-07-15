# install.packages('raster')
# install.packages('rgdal')
# install.packages('rasterVis')
# install.packages('ggplot2')
# install.packages('ggspatial')
# install.packages('ggridges')
# install.packages('prettymapr')

library(raster) # lectura y herramientas para coberturas raster
library(rgdal) # lectura de coberturas vectoriales 
library(rasterVis) # nuevos metodos de visualizacion de mapas
library(ggplot2) # nuevos metodos de visualizacion de mapas
library(ggspatial) # nuevos metodos de visualizacion de mapas
library(ggridges) # graficos de densidad
library(prettymapr) # barra de escala y Norte

rm(list=ls())
dev.off()
      
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

# Historico ----
setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/temperatura_maxima_historico/')

list.files()
t.max1 <- raster('wc2.1_2.5m_tmax_01.tif')
t.max1

plot(t.max1, main = 'Enero')

lista.archivos.historicos <- list.files(pattern = '.tif') ; lista.archivos.historicos
t.max.historico <- stack(lista.archivos.historicos)
t.max.historico

meses.anho <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio',
                            'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')

names(t.max.historico) <- meses.anho
t.max.historico

plot(t.max.historico, 3:6)

# fin ---



# Futuro ----

setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/temperatura_maxima_futuro/share/spatial03/worldclim/cmip6/7_fut/2.5m/MIROC6/ssp585/')
t.max.futuro0 <- raster('wc2.1_2.5m_tmax_MIROC6_ssp585_2081-2100.tif')
t.max.futuro0

t.max.futuro <- stack('wc2.1_2.5m_tmax_MIROC6_ssp585_2081-2100.tif')
t.max.futuro

names(t.max.futuro) <- meses.anho
t.max.futuro

plot(t.max.futuro, 3:6)

# fin ---



# Division administrativa ----
setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/coberturas/DivisionPoliticaAdministrativa2019/')

lim <- readOGR('.', 'DivisionPoliticaAdministrativa2019')

id.reg <- which(lim@data$CUT_REG == '09')
lim.09 <- lim[id.reg,]
head(lim.09@data)

marco <- spTransform(lim.09, wgs84)
plot(marco, axes = TRUE)

# fin ---



# Recorte ----

t.max.historico.clip0 <- crop(t.max.historico, marco)
plot(t.max.historico.clip0, 3:6)

t.max.historico.clip <- mask(t.max.historico.clip0, marco)
plot(t.max.historico.clip, 3:6)


t.max.futuro.clip0 <- crop(t.max.futuro, marco)
t.max.futuro.clip <- mask(t.max.futuro.clip0, marco)
plot(t.max.futuro.clip, 3:6)

# fin ---


# Mapas ----

myPal <- heat.colors(100)
myTheme <- rasterTheme(region = myPal)
my.at <- seq(0, 40, by=1) # breaks para la leyenda
unidad.variable <- '°C'

# setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/plots/')
# png('tmax_futuro_levelplot_720px.png', width = 720, height = 720, units = "px")

levelplot(t.max.futuro.clip, par.settings=myTheme, colorkey=list(space='right'), 
          main = 'Temperatura máxima esperada a fines del siglo XXI',
          xlab = '', ylab = unidad.variable, at=my.at)

# dev.off()

levelplot(t.max.futuro.clip, layer = 1, margin = list(FUN = median, axis = TRUE, 
                                           scales = list(x=c(0, 40),
                                                         y=c(0, 40))), 
          par.settings=myTheme, colorkey=TRUE, xlab = '', ylab = unidad.variable,
          main='Enero', at=my.at)


t.max.futuro.clip1 <- t.max.futuro.clip[[1]]

ggplot() +
layer_spatial(t.max.futuro.clip1) +
labs(y=NULL, x=NULL) +
scale_fill_gradientn(unidad.variable, colours = myPal, na.value = 'white') +
coord_sf(crs=4326, expand = FALSE) +
theme_bw() +
annotation_scale(location = "br", text_col = 'black', style = 'ticks', line_col = 'black') + 
annotation_north_arrow(location = "tl", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
  
# fin ---



# Raster a data frame ----

db.t.max.historico <- round( as.data.frame(t.max.historico.clip, na.rm = TRUE), 1)
head(db.t.max.historico)

db.t.max.futuro <- as.data.frame(t.max.futuro.clip, na.rm = TRUE)
head(db.t.max.futuro)

# fin ---



# boxplot ----
mar.i <- 0

# setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/plots/')
# pdf('boxplots_tmax.pdf')

par(mfrow=c(1,2), oma=c(6,4,1,1), mar=c(mar.i,mar.i,mar.i,mar.i))

boxplot(db.t.max.historico, ylim=c(0, 40), las=2)
mtext(unidad.variable, side=2, line = 3)
mtext('Historico', side=3, line = -2)

boxplot(db.t.max.futuro, yaxt='n', las=2)
mtext('2081-2100', side=3, line = -2)

# dev.off()

# fin ---



# graficos de densidad historico ----

# enero
db.t.max.historico.1 <- round( as.data.frame(t.max.historico.clip[[1]], na.rm = TRUE), 1)
head(db.t.max.historico.1)

db.t.max.historico.1$mes <- colnames(db.t.max.historico.1)
head(db.t.max.historico.1)

colnames(db.t.max.historico.1)[1] <- 'valor'
head(db.t.max.historico.1)

# febrero
db.t.max.historico.2 <- round( as.data.frame(t.max.historico.clip[[2]], na.rm = TRUE), 1)
head(db.t.max.historico.2)

db.t.max.historico.2$mes <- colnames(db.t.max.historico.2)
head(db.t.max.historico.2)

colnames(db.t.max.historico.2)[1] <- 'valor'
head(db.t.max.historico.2)

# union de data frames
db.t.max.historico.1.2 <- rbind(db.t.max.historico.1, db.t.max.historico.2) 
head(db.t.max.historico.1.2)
table(db.t.max.historico.1.2$mes)

db.t.max.historico.full0 <- c()
for (i in 3:12) {
  db.t.max.historico.i <- round( as.data.frame(t.max.historico.clip[[i]], na.rm = TRUE), 1)
  db.t.max.historico.i$mes <- colnames(db.t.max.historico.i)
  colnames(db.t.max.historico.i)[1] <- 'valor'  
  
  db.t.max.historico.full0 <- rbind(db.t.max.historico.full0, db.t.max.historico.i)
}

db.t.max.historico.full0
table(db.t.max.historico.full0$mes)

db.t.max.historico.full <- rbind(db.t.max.historico.1.2, db.t.max.historico.full0)
table(db.t.max.historico.full$mes)

unique(db.t.max.historico.full$mes)

db.t.max.historico.full$mes <- factor( db.t.max.historico.full$mes, levels = rev(meses.anho) )
levels(db.t.max.historico.full$mes)

ggplot(db.t.max.historico.full, aes(x = valor, y=mes, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradientn(unidad.variable, colours = myPal) +
  xlim(0, 40) +
  labs(title = 'Historico', x = unidad.variable, y = NULL) +
  theme_bw() 

# fin ---



# graficos de densidad futuro ----

# enero
db.t.max.futuro.1 <- round( as.data.frame(t.max.futuro.clip[[1]], na.rm = TRUE), 1)
head(db.t.max.futuro.1)

db.t.max.futuro.1$mes <- colnames(db.t.max.futuro.1)
head(db.t.max.futuro.1)

colnames(db.t.max.futuro.1)[1] <- 'valor'
head(db.t.max.futuro.1)


db.t.max.futuro.full0 <- c()
for (i in 2:12) {
  db.t.max.futuro.i <- round( as.data.frame(t.max.futuro.clip[[i]], na.rm = TRUE), 1)
  db.t.max.futuro.i$mes <- colnames(db.t.max.futuro.i)
  colnames(db.t.max.futuro.i)[1] <- 'valor'  
  
  db.t.max.futuro.full0 <- rbind(db.t.max.futuro.full0, db.t.max.futuro.i)
}

db.t.max.futuro.full <- rbind(db.t.max.futuro.1, db.t.max.futuro.full0)
db.t.max.futuro.full$mes <- factor( db.t.max.futuro.full$mes, levels = rev(meses.anho) )
levels(db.t.max.futuro.full$mes)

ggplot(db.t.max.futuro.full, aes(x = valor, y=mes, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradientn(unidad.variable, colours = myPal) +
  labs(title = 'Futuro (1981-2100)', x = unidad.variable, y = NULL) +
  theme_bw() 

# fin ---



# Distribucion de tmax historico vs futuro ---- 
db.t.max.historico.full$periodo <- 'Historico'
db.t.max.futuro.full$periodo <- 'Futuro'

db.t.max.full <- rbind(db.t.max.historico.full, db.t.max.futuro.full)
head(db.t.max.full)
table(db.t.max.full$periodo)

db.t.max.full$periodo <- factor( db.t.max.full$periodo, levels = c('Historico', 'Futuro') )
levels(db.t.max.full$periodo)

distribucion.hist.futu <- ggplot(db.t.max.full, aes(x = valor, y=mes, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradientn(unidad.variable, colours = myPal) +
  labs(x = unidad.variable, y = NULL) +
  theme_dark() +
  facet_wrap(~periodo, nrow = 2)

# setwd('C:/Users/Francisco/Documents/Curso_SIG_en_R/plots/')
# png('distribucion_tmax_historico_vs_futuro_480px.png', width = 480, height = 480, units = "px")
# pdf('distribucion_tmax_historico_vs_futuro.pdf')

distribucion.hist.futu

# dev.off()

# fin ---



# Diferencia entre temperatura historica y esperada (2081-2100) ----

t.max.historico.anual <- mean(t.max.historico.clip)
plot(t.max.historico.anual)

t.max.futuro.anual <- mean(t.max.futuro.clip)
plot(t.max.futuro.anual)

t.max.hist.futu <- stack(t.max.historico.anual, t.max.futuro.anual)
names(t.max.hist.futu) <- c('Historico', 'Futuro')

levelplot(t.max.hist.futu, par.settings=myTheme, colorkey=list(space='right'), 
          xlab = '', ylab = unidad.variable, at=my.at)



t.max.hist.futu.diferencia <- overlay(t.max.historico.anual, t.max.futuro.anual, 
                                      fun = function(r1, r2) { return( r2-r1 ) })

minValue(t.max.hist.futu.diferencia)
maxValue(t.max.hist.futu.diferencia)

levelplot(t.max.hist.futu.diferencia, margin = list(FUN = median, axis = TRUE, 
                                                      scales = list(x=c(0, 5),
                                                                    y=c(0, 5))), 
          par.settings=myTheme, colorkey=TRUE, xlab = '', ylab = unidad.variable,
          main='Diferencia entre temperatura historica y esperada (2081-2100)')

# fin ---

