#### TP 2 - 1a Parte
#### Guille Solovey

#### 0. Cargo los datos de Properati y me quedo con CABA, venta y USD ####
d.all  <- read.csv("ar_properties.csv")

filtro <- is.element(d.all$l2, "Capital Federal") &
          is.element(d.all$operation_type, "Venta") &
          is.element(d.all$currency, "USD") 

d.caba.venta.dolares <- d.all[filtro,]

# filtro <- d.all$l2 == "Capital Federal" & d.all$operation_type == "Venta" & d.all$currency == "USD"
# d.caba.venta.dolares <- d.all[filtro,]
# unique(d.caba.venta.dolares$l2)


#### 1. ¿Es el precio por metro cuadrado igual en toda el dataset? ####
#### ¿Cómo varía? Cree precio por metro cuadrado (precio por m2). 
#### Caracterice esta variable usando las herramientas de estadística 
#### descriptiva vistas.
  
summary(d.caba.venta.dolares$surface_total)
summary(d.caba.venta.dolares$price)

# me voy a quedar con los que tienen superficie y precio positivo
filtro <- !is.element(d.caba.venta.dolares$surface_total, c(NA, 0)) &
          !is.element(d.caba.venta.dolares$surface_covered, c(NA, 0)) &
          !is.element(d.caba.venta.dolares$price, c(NA, 0))
  
d1 <- d.caba.venta.dolares[filtro,]

# verifico que esté razonable
summary(d1$surface_total)
summary(d1$surface_covered)
summary(d1$price) 

# defino el precio por metro cuadrado
d1$precioxm2 <- d1$price / d1$surface_total

hist(d1$precioxm2, breaks = 1000)


# margen del plot:
# par(mar=c(bottom, left, top, right)) - default: c(5.1, 4.1, 4.1, 2.1)
mar.default <- c(5.1, 4.1, 4.1, 2.1)
par(mar=c(5.1, 4.1, 2.1, 2.1))
hist(d1$precioxm2, 
     breaks = 1000, 
     xlim = c(0,10000), 
     xlab = "precio por m2", 
     main = "")
par(new=T)
boxplot(d1$precioxm2, outline = F, horizontal = T, 
        axes = F, ylim = c(0,10000), xlab = "precio por m2",
        col = rgb(1, 1, 1, alpha = 0.5), xlim=c(0,8))

# que % de propiedades tiene mas de USD10000 por m2?
100*mean(d1$precioxm2>10000) 

#### 2. ¿Cuales propiedades son caras, promedio o baratas?  #####
#### Construya una partición que separe el precio por metro cuadrado
#### en tres categorías.
  
# me quedo con los que tienen precioxm2 menor a 10000
100*mean(d1$precioxm2>10000) # 0.23 %

d2 <- d1[d1$precioxm2<10000,]

# Separo el precio x m2 en 3 cat
umbral1 <- quantile(d2$precioxm2,0.25)
umbral2 <- quantile(d2$precioxm2,0.75)

cat1 <- d2$precioxm2 <= umbral1
cat2 <- d2$precioxm2 > umbral1 &
        d2$precioxm2 <= umbral2
cat3 <- d2$precioxm2 > umbral2 

d2$precioxm2.cat[cat1] <- 1
d2$precioxm2.cat[cat2] <- 2
d2$precioxm2.cat[cat3] <- 3




# https://colorbrewer2.org/
mybreaks <- seq(0,10000,100) 
mycolors <- rep(NA, length(mybreaks))
mycolors[mybreaks < umbral1] <- rgb(229/255,245/255,249/255)
mycolors[mybreaks >= umbral1 & mybreaks < umbral2] <- rgb(153/255,216/255,201/255)
mycolors[mybreaks >= umbral2] <- rgb(44/255,162/255,95/255)

# https://statisticsglobe.com/draw-histogram-with-different-colors-in-r
hist(d2$precioxm2, 
     breaks = mybreaks, 
     col = mycolors,
     main = "Numero de prop según precios por m2 \n CABA|USD|VENTA",
     xlab = "precio por m2 (USD)",
     ylab = "",
     border = F)
legend("topright", legend = c("bajo","medio","alto"), 
       fill = c(rgb(229/255,245/255,249/255),
                rgb(153/255,216/255,201/255),
                rgb(44/255,162/255,95/255)),
       box.lwd = 0)

# Otra idea: highest density region (minima region que contiene el alpha % 
# de los casos)
# https://github.com/robjhyndman/hdrcde
library(hdrcde)
hdr <- hdr(d2$precioxm2,prob=89)$hdr
lines(hdr,c(0,0), lwd = 4, col = "red")
legend(6000,1000, legend = "HDR", box.lwd = 0, col = "red", lty = 1, lwd = 4)


#### 3. ¿Cómo cambia el precio por cantidad de habitaciones? ####

barplot(table(d2$rooms))
f  <- is.element(d2$rooms, 1:6) & !is.element(d2$rooms, NA)
d3 <- d2[f,]

d3$price.x1000 <- d3$price/1000

barplot(table(d3$rooms))

boxplot(price.x1000 ~ rooms, outline = F, data = d3)

table(d3$property_type, d3$rooms)

# miro por tipo de propiedad, pero solo Casa, Departamento, PH, Casa
par(mfrow=c(1,3))
f <- is.element(d3$property_type, "Departamento") 
boxplot(price.x1000 ~ rooms, outline = F, data = d3[f,], main = "Departamento")

f <- is.element(d3$property_type, "PH") 
boxplot(price.x1000 ~ rooms, outline = F, data = d3[f,], main = "PH")

f <- is.element(d3$property_type, "Casa") 
boxplot(price.x1000 ~ rooms, outline = F, data = d3[f,], main = "Casa")


# Distintos barrios
sort(table(d3$l3))

par(mfrow=c(1,3))
f <- is.element(d3$property_type, c('Casa','PH','Departamento')) & 
  is.element(d3$l3, "Palermo")
boxplot(price.x1000 ~ rooms, outline = F, data = d3[f,], main = "Palermo")

f <- is.element(d3$property_type, c('Casa','PH','Departamento')) & 
  is.element(d3$l3, "Caballito")
boxplot(price.x1000 ~ rooms, outline = F, data = d3[f,], main = "Caballito")

f <- is.element(d3$property_type, c('Casa','PH','Departamento')) & 
  is.element(d3$l3, "Flores")
boxplot(price.x1000 ~ rooms, outline = F, data = d3[f,], main = "Flores")


#### 3.1 ¿Cuales son los barrios más caros y los mas baratos? ####
par(mfrow=c(1,1))
b <- boxplot(price.x1000 ~ l3, data = d3, outline = F)
d3$l3 <- factor(d3$l3)
d3$l3 <- factor(d3$l3, levels = levels(d3$l3)[order(b$stats[3,])])

par(mfrow=c(1,1))
N <- 12
masbaratos <- levels(d3$l3)[1:N]
dtemp <- d3[is.element(d3$l3, masbaratos),]
dtemp$l3 <- factor(dtemp$l3, masbaratos)
par(mar = c(4,10,2,5))
boxplot(price.x1000 ~ l3, data = dtemp, horizontal = T, 
        outline = F, las = 2,
        ylab = "", main = "barrios más baratos")


par(mfrow=c(1,1))
N <- 12
mascaros <- rev(levels(d3$l3))[1:N]
dtemp <- d3[is.element(d3$l3, mascaros),]
dtemp$l3 <- factor(dtemp$l3, mascaros)
par(mar = c(4,10,2,5))
boxplot(price.x1000 ~ l3, data = dtemp, horizontal = T, 
        outline = F, las = 2,
        ylab = "", main = "barrios más caros")

#### 4. ¿Qué considera que describe mejor el precio de una propiedad? ####
#### ¿Número de habitaciones (*rooms*) o su superficie cubierta? 
f <- d3$surface_covered<600
hist(d3$surface_covered[f], breaks = 30)

f <- d3$surface_total<600
hist(d3$surface_total[f], breaks = 30)

# me quedo con las que tienen superficie menor a 600 m2
f <- d3$surface_covered<600 & d3$surface_total<600
d4 <- d3[f,]

par(mfrow=c(1,1), mar = mar.default)
boxplot(price.x1000 ~ rooms, data = d4, outline = F, ylim = c(0,1800))

plot(price.x1000 ~ surface_covered, data = d4)

plot(price.x1000 ~ surface_covered, data = d4, pch = 16, cex =0.1, 
     ylim = c(0,1800),
     xlim = c(0,500))
lf <- lm(price.x1000 ~ surface_covered, data = d4)
abline(lf$coefficients[1], lf$coefficients[2], col = "red")


#### 5. ¿Cómo cambia la superficie cubierta por rooms en el dataset? ####
#### ¿Cambia al considerar distintos tipos de propiedad? ¿Y distintos 
#### barrios? Caracterice metro cuadrado por habitación. 
par(mfrow=c(1,1))
boxplot(surface_covered ~ rooms, outline = F, data = d4)

par(mfrow=c(1,3))
f <- is.element(d4$property_type,"Casa")
boxplot(surface_covered ~ rooms, outline = F, data = d4[f,], main = "Casa")
f <- is.element(d4$property_type,"Departamento")
boxplot(surface_covered ~ rooms, outline = F, data = d4[f,], main = "Departamento")
f <- is.element(d4$property_type,"PH")
boxplot(surface_covered ~ rooms, outline = F, data = d4[f,], main = "PH")

f <- is.element(d4$l3,"Palermo")
boxplot(surface_covered ~ rooms, outline = F, data = d4[f,], main = "Palermo")
f <- is.element(d4$l3,"Caballito")
boxplot(surface_covered ~ rooms, outline = F, data = d4[f,], main = "Caballito")
f <- is.element(d4$l3,"Flores")
boxplot(surface_covered ~ rooms, outline = F, data = d4[f,], main = "Flores")


# metro cuadrado por habitacion
d4$metroxhab <- d4$surface_covered / d4$rooms
par(mar = mar.default, mfrow = c(1,1) )
hist(d4$metroxhab, breaks = 100, main = "", xlim = c(0,200))


#### 6. ¿Cuál es la relación entre la sup total y la sup cubierta, ####
#### al considerar distintos tipos de propiedades?

par(mfrow=c(1,3))
f <- is.element(d4$property_type,"Casa")
plot(surface_total ~ surface_covered, data = d4[f,], pch = 19, cex = 0.1, xlim = c(0,600), ylim = c(0,600), main = "Casa")
abline(0,1,lwd=4,col="red")

f <- is.element(d4$property_type,"Departamento")
plot(surface_total ~ surface_covered, data = d4[f,], pch = 19, cex = 0.1, xlim = c(0,600), ylim = c(0,600), main = "Departamento")
abline(0,1,lwd=4,col="red")

f <- is.element(d4$property_type,"PH")
plot(surface_total ~ surface_covered, data = d4[f,], pch = 19, cex = 0.1, xlim = c(0,600), ylim = c(0,600), main = "PH")
abline(0,1,lwd=4,col="red")

#### 7. ¿Cómo se relaciona la cantidad de habitaciones (rooms) con la cantidad de baños? ####
####  Cómo cambia según el tipo de propiedad?¿ Y según el barrio?
f <- is.element(d4$property_type, c("Casa", "Departamento", "PH"))
d5 <- d4[f,]
table(d5$rooms, d5$bathrooms)

f <- d5$bathrooms < 8
d5 <- d5[f,]


par(mfrow=c(1,1))

f <- is.element(d5$property_type, "Casa")
tbl <- prop.table(table(d5$rooms[f], d5$bathrooms[f]), margin = 1)
rownames(tbl) <- 1:6
colnames(tbl) <- c("1 baño", "2 baños", "3 baños", "4 baños", "5 baños", "6 baños", "7 baños")
par(mar = c(5, 5, 5, 8))
barplot(t(tbl), border="white", xlab="rooms", legend = T, 
        args.legend = list(x = "topright", inset = c(-0.4, 0)), main = "Casa")

f <- is.element(d5$property_type, "Departamento")
tbl <- prop.table(table(d5$rooms[f], d5$bathrooms[f]), margin = 1)
rownames(tbl) <- 1:6
colnames(tbl) <- c("1 baño", "2 baños", "3 baños", "4 baños", "5 baños", "6 baños")
par(mar = c(5, 5, 5, 8))
barplot(t(tbl), border="white", xlab="rooms", main = "Departamento", legend = T, 
        args.legend = list(x = "topright", inset = c(-0.4, 0)))

f <- is.element(d5$property_type, "PH")
tbl <- prop.table(table(d5$rooms[f], d5$bathrooms[f]), margin = 1)
rownames(tbl) <- 1:5
colnames(tbl) <- c("1 baño", "2 baños", "3 baños", "4 baños", "5 baños")
par(mar = c(5, 5, 5, 8))
barplot(t(tbl), border="white", xlab="rooms", main = "PH", legend = T, 
        args.legend = list(x = "topright", inset = c(-0.4, 0)))



#### 8. ¿Cuánto tiempo duran los anuncios en departamentos en CABA? ####
#### ¿Nota algo extraño? 
#### Para calcular la cantidad de días entre dos fechas, puede restarlas si 
#### están en formato Date.

par(mfrow=c(1,1))
par(mar = c(5, 5, 5, 5))
# vuelvo a d1
d1$start_date <- as.Date(d1$start_date)
d1$end_date   <- as.Date(d1$end_date)
dt <- d1$end_date - d1$start_date

hist(as.numeric(dt))
summary(as.numeric(dt))

hist(as.numeric(dt[dt<(5*365)]))

# hay varias que tienen año 9999. deben ser las que siguen activas, 23%
mean(format(d1$end_date, format="%Y")==9999)
f <- !is.element(format(d1$end_date, format="%Y"), 9999)
hist(as.numeric(dt[f]))


#### 9. ¿Cómo cambia la cantidad de anuncios publicados en los distintos días de #### 
#### la semana? La función weekdays permite identificar a qué día corresponde 
#### el anuncio publicado.
par(mar = c(7, 5, 3, 5))
dias <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday")
wd <- weekdays( d1$start_date) 
tbl <- table(wd)
tbl <- tbl[match(dias, rownames(tbl))]
barplot(tbl, las=2, main = "Dia de publicación de un aviso")



