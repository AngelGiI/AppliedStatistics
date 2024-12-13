# PR�CTICA 1 ESTAD�STICA

# Comenzamos instalando y cargando el paquete HSAUR y su librer�a. 

install.packages("HSAUR")
library("HSAUR")

# Cargamos el data frame que contiene el objeto Forbes2000.

data("Forbes2000",package = "HSAUR")


#######################################################################
##                            EJERCICIO 1                            ##
#######################################################################


# Creamos una tabla en la que solo aparecen las empresas de Espa�a junto a todas 
# sus variables.

SPAIN = Forbes2000[Forbes2000$country == "Spain" ,]

# Calculamos la media y mediana de ventas de cada empresa usando los comandos
# 'mean' y 'median'.
# Accedemos al valor de ventas de cada empresa mediante 
# 'SPAIN$sales'.

mean(SPAIN$sales)  # media
median(SPAIN$sales)  # mediana

# Mediante el mismo procedimiento creamos una tabla de las compa��as que
# pertenecen al grupo de pa�ses de Francia, UK y Alemania. Para acceder a las
# compa��as usamos '%in%' junto al vector que contiene el nombre de los pa�ses.

UKFG = Forbes2000[ Forbes2000$country %in% c("United Kingdom","France",
                                             "Germany"),]
# Calculamos medias y medianas de las ventas.

mean(UKFG$sales)  # media
median(UKFG$sales)  # mediana


#######################################################################
##                            EJERCICIO 2                            ##
#######################################################################


# Extraemos en otra tabla el nombre de las empresas espa�olas que tienen 
# beneficios negativos junto a sus respectivos beneficios

SPAIN[ SPAIN$profits <= 0, c("name" , "profits") ]


#######################################################################
##                            EJERCICIO 3                            ##
#######################################################################


# Creamos una tabla que nos indica cuantas empresas se dedican a cada categor�a, 
# eliminando aquellas categor�as que no incluyen a ninguna.

Cat_SPAIN = table( droplevels (SPAIN$category) )

Cat_SPAIN[ Cat_SPAIN == max(Cat_SPAIN) ]  # Calculamos el sector mayoritario en Espa�a.

# Otra forma

Spain_=subset(Forbes2000, country=="Spain")   # Nos quedamos con las observaciones de Spain.
rev(sort(summary(Spain_$category)))[1]        # Dentro de las categor�as de "Spain_" las ordenamos por orden
                                              # creciente de frecuencia y despues invertimos ese orden para 
                                              # quedarnos con la categor�a que haya en primer lugar


#######################################################################
##                            EJERCICIO 4                            ##
#######################################################################

# Ordenamos los valores de la variable sales en Forbes2000 de forma descendente.
# Y nos quedamos con las 50 primeras.

ranking = Forbes2000 [rev(order(Forbes2000$sales)), ][1:50,]

# Creamos el gr�fico.

plot(log(assets)~log(marketvalue), ranking, col = rgb(0,0,1,1), pch = 19)
text(log(assets)~log(marketvalue), data = ranking, labels = abbreviate(ranking$country, 
                              minlength = 2), pos = 4, cex = 0.6, col = "red")


#######################################################################
##                            EJERCICIO 5                            ##
#######################################################################


# Calculamos la media, mediana, desviaci�n t�pica y rango intercuart�lico de las 
# ventas en cada pa�s usando 'tapply'

tapply(Forbes2000$sales, Forbes2000$country, median, na.rm=TRUE)  # media

tapply(Forbes2000$sales, Forbes2000$country, median, na.rm=TRUE)  # mediana

tapply(Forbes2000$sales, Forbes2000$country, sd, na.rm=TRUE)  # desv. t�pica

tapply(Forbes2000$sales, Forbes2000$country, IQR, na.rm=TRUE)  # rango interc.

# Como en el dataframe Forbes2000 los valores de profits estan guardados de la 
# forma (x * 10^9) bastar� ver qu� valores son mayores que 5.

mayor_profit = Forbes2000[Forbes2000$profits > 5, c("country")]

# Eliminamos los pa�ses que no tengan ninguna empresa cuyos beneficios superen
# los 5 * 10^9 US$

table(droplevels(mayor_profit))
