# PRÁCTICA 1 ESTADÍSTICA

# Comenzamos instalando y cargando el paquete HSAUR y su librería. 

install.packages("HSAUR")
library("HSAUR")

# Cargamos el data frame que contiene el objeto Forbes2000.

data("Forbes2000",package = "HSAUR")


#######################################################################
##                            EJERCICIO 1                            ##
#######################################################################


# Creamos una tabla en la que solo aparecen las empresas de España junto a todas 
# sus variables.

SPAIN = Forbes2000[Forbes2000$country == "Spain" ,]

# Calculamos la media y mediana de ventas de cada empresa usando los comandos
# 'mean' y 'median'.
# Accedemos al valor de ventas de cada empresa mediante 
# 'SPAIN$sales'.

mean(SPAIN$sales)  # media
median(SPAIN$sales)  # mediana

# Mediante el mismo procedimiento creamos una tabla de las compañías que
# pertenecen al grupo de países de Francia, UK y Alemania. Para acceder a las
# compañías usamos '%in%' junto al vector que contiene el nombre de los países.

UKFG = Forbes2000[ Forbes2000$country %in% c("United Kingdom","France",
                                             "Germany"),]
# Calculamos medias y medianas de las ventas.

mean(UKFG$sales)  # media
median(UKFG$sales)  # mediana


#######################################################################
##                            EJERCICIO 2                            ##
#######################################################################


# Extraemos en otra tabla el nombre de las empresas españolas que tienen 
# beneficios negativos junto a sus respectivos beneficios

SPAIN[ SPAIN$profits <= 0, c("name" , "profits") ]


#######################################################################
##                            EJERCICIO 3                            ##
#######################################################################


# Creamos una tabla que nos indica cuantas empresas se dedican a cada categoría, 
# eliminando aquellas categorías que no incluyen a ninguna.

Cat_SPAIN = table( droplevels (SPAIN$category) )

Cat_SPAIN[ Cat_SPAIN == max(Cat_SPAIN) ]  # Calculamos el sector mayoritario en España.

# Otra forma

Spain_=subset(Forbes2000, country=="Spain")   # Nos quedamos con las observaciones de Spain.
rev(sort(summary(Spain_$category)))[1]        # Dentro de las categorías de "Spain_" las ordenamos por orden
                                              # creciente de frecuencia y despues invertimos ese orden para 
                                              # quedarnos con la categoría que haya en primer lugar


#######################################################################
##                            EJERCICIO 4                            ##
#######################################################################

# Ordenamos los valores de la variable sales en Forbes2000 de forma descendente.
# Y nos quedamos con las 50 primeras.

ranking = Forbes2000 [rev(order(Forbes2000$sales)), ][1:50,]

# Creamos el gráfico.

plot(log(assets)~log(marketvalue), ranking, col = rgb(0,0,1,1), pch = 19)
text(log(assets)~log(marketvalue), data = ranking, labels = abbreviate(ranking$country, 
                              minlength = 2), pos = 4, cex = 0.6, col = "red")


#######################################################################
##                            EJERCICIO 5                            ##
#######################################################################


# Calculamos la media, mediana, desviación típica y rango intercuartílico de las 
# ventas en cada país usando 'tapply'

tapply(Forbes2000$sales, Forbes2000$country, median, na.rm=TRUE)  # media

tapply(Forbes2000$sales, Forbes2000$country, median, na.rm=TRUE)  # mediana

tapply(Forbes2000$sales, Forbes2000$country, sd, na.rm=TRUE)  # desv. típica

tapply(Forbes2000$sales, Forbes2000$country, IQR, na.rm=TRUE)  # rango interc.

# Como en el dataframe Forbes2000 los valores de profits estan guardados de la 
# forma (x * 10^9) bastará ver qué valores son mayores que 5.

mayor_profit = Forbes2000[Forbes2000$profits > 5, c("country")]

# Eliminamos los países que no tengan ninguna empresa cuyos beneficios superen
# los 5 * 10^9 US$

table(droplevels(mayor_profit))
