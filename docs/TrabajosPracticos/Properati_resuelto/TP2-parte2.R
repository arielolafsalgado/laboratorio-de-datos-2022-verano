#### TP 2 - 2da Parte
#### Guille Solovey

#### 0. Amplíe el dataset para considerar todas las propiedades de argentina, ####
#### con tipo de operación Venta o Alquiler (y cualquier moneda). 
d.all  <- read.csv("ar_properties.csv")

filtro <- is.element(d.all$l1, "Argentina") &
          is.element(d.all$operation_type, c("Venta", "Alquiler"))

d.arg  <- d.all[filtro,]

#### 1. ¿Cuál es la relación entre región (`l2`) y tipo de operación (Venta o Alquiler)? ####
#### ¿Cambia con el tipo de propiedad?
  
tbl <- table(d.arg$l2, d.arg$operation_type)
tbl <- prop.table(tbl,1)
par(mar = c(9, 4, 2, 2))
barplot(t(tbl), las=2, beside = T, cex.names = 0.7, legend = T,
        args.legend = list(x = 25, y = 1.1))


# 
# 
# d   <- data.frame(loc = rownames(tbl),
#                   alq = as.numeric(as.matrix(tbl)[,1]),
#                   ven = as.numeric(as.matrix(tbl)[,2]))
# 
# # par(mar=c(bottom, left, top, right)) - default: c(5.1, 4.1, 4.1, 2.1)
# # mar.default <- c(5.1, 4.1, 4.1, 2.1)
# par(mar = c(7, 4, 2, 2))
# barplot(d$alq, names.arg = d$loc, las = 2, ylim = c(0,1), cex.names = 0.5)


  
#### 2. ¿Qué diferencia existe en el tipo de operación (Venta o Alquiler) entre ####
#### Catamarca y La Rioja? ¿Y entre Chaco y Santa Cruz?
f <- is.element(d.arg$l2, c("Catamarca", "La Rioja"))
tbl <- table(d.arg$l2[f], d.arg$operation_type[f])
tbl <- prop.table(tbl,1)
tbl

f <- is.element(d.arg$l2, c("Chaco", "Santa Cruz"))
tbl <- table(d.arg$l2[f], d.arg$operation_type[f])
tbl <- prop.table(tbl,1)
tbl

#### 3. ¿Cómo describiría la relación entre la variable `l2` (región) y `l3` (barrio)? ####
#### ¿Están igual de particionadas todas las regiones?
#### Construya una lista con tantos elementos como regiones (categorías de `l2`) 
#### y en cada elemento incluya las subregiones asociadas (categorías de `l3`). 
#### Construya un vector representando la cantidad de subregiones para cada región 
#### y resuma este vector.

d.arg <- d.arg[!is.na(d.arg$l2),]
d.arg <- d.arg[!is.na(d.arg$l3),]

# cuantos barrios (subregiones) tiene cada region
reg.nombres <- unique(d.arg$l2)
bar.nombres <- unique(d.arg$l3)
reg.n <- length(reg.nombres)
bar.n <- length(bar.nombres)

barxreg   <- vector(mode = "list", length = reg.n)
barxreg.n <- rep(NA, reg.n)
for (i in 1:reg.n){
  f <- is.element(d.arg$l2, reg.nombres[i])
  barxreg[[i]] <- unique(d.arg$l3[f])
  barxreg.n[i] <- length(barxreg[[i]])
}

barplot(barxreg.n, names.arg = reg.nombres, las = 2, cex.names = 0.5)


ord <- order(barxreg.n, decreasing = T)
barplot(barxreg.n[ord], names.arg = reg.nombres[ord], las = 2, cex.names = 0.5)

length( unique(d.arg$l3[d.arg$l2=="Santa Fe"]) )

#### 4. Caracterice el *fondo* de las propiedades (superficie total - superficie ####
#### cubierta) para distintas regiones del país.
  
d.arg$superficie_fondo <- d.arg$surface_total - d.arg$surface_covered
summary(d.arg$superficie_fondo)

f <- !is.na(d.arg$superficie_fondo) & d.arg$superficie_fondo>0
d.arg <- d.arg[f,]

hist(d.arg$superficie_fondo)
mean(d.arg$superficie_fondo>1000)

f <- d.arg$superficie_fondo<1000
d.arg <- d.arg[f,]
hist(d.arg$superficie_fondo, breaks = 100, xlim = c(0,200))
     
# ¿En qué lugar los *fondos* son más grandes?
b <- boxplot(superficie_fondo ~ l2, data = d.arg, outline = F, 
             las = 2, cex.axis = 0.5)

or <- order( b$stats[3,] )
d.arg$l2 <- factor(d.arg$l2, levels = b$names[or])
b <- boxplot(superficie_fondo ~ l2, data = d.arg, outline = F, 
             las = 2, cex.axis = 0.5)

# ¿Cambia según el tipo de propiedad?
b <- boxplot(superficie_fondo ~ property_type, data = d.arg, 
             outline = F, las = 2, cex.axis = 0.5, xlab = "")

or <- order( b$stats[3,] )
d.arg$property_type <- factor(d.arg$property_type, levels = b$names[or])
b <- boxplot(superficie_fondo ~ property_type, data = d.arg, outline = F, 
             las = 2, cex.axis = 0.5, xlab = "")


