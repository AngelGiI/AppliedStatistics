################################################################################
######                                                                    ######
######                AN�LISIS DE EXPERIMENTOS CON R (1)                  ######
######                                                                    ######
######                       ESTAD�STICA APLICADA                         ######
######               GRADO EN INGENIER�A MATEM�TICA 2020/2021             ######
######                                                                    ######
################################################################################

#        ESTE SCRIPT HA SIDO REALIZADO POR TINGUARO RODR�GUEZ (2020)           #

# En primer lugar, vamos a ver c�mo cargar datos en R para poder realizar el 
# an�lisis estad�stico de un experimento.
#
# Como sabemos, el formato con que trabaja R para almacenar datos es el 
# data.frame. Adem�s, de cara al an�lisis posterior, es necesario que este 
# data frame est� estructurado en t�rminos de un factor y la variable respuesta 
# de nuestro problema. Esto es, el data frame debe contener una variable de tipo
# factor con los diferentes niveles, repetidos tantas veces como observaciones
# de la respuesta haya en cada nivel. Y asociado a cada nivel del factor, deben
# estar las correspondientes respuestas de cada nivel. Hay diversas maneras de 
# conseguir un data frame con esta configuraci�n.
#
# La primera forma es introducirlos directamente en este formato a partir de los
# vectores correspondientes usando la funci�n data.frame(). Veamos como hacerlo 
# para los datos del ejercicio 4 de la Hoja de problemas 1. # 

region=rep(c("Este","Norte","Oeste","Sur"),each=5)
region

# Como vemos, la funci�n rep() repite cada uno de los elementos del vector que
# recibe como primer argumento tantas veces como se indique en el par�metro
# "each". 
#
# Las observaciones de la respuesta, en tanto normalmente no contendr�n 
# repeticiones al tratarse de una variable continua, hay que introducirlas en
# otro vector una a una CUIDANDO QUE SE INTRODUZCAN EN EL ORDEN ADECUADO, esto
# es, cada respuesta debe quedar asociada al nivel en que se ha observado. #

incremento=c(10.4,12.8,15.6,9.2,8.7,
             12.8,14.2,16.3,10.1,12.0,
             11.2,9.8,10.7,6.3,12.4,
             13.9,14.2,12.8,15.0,13.7) 
incremento

# Una vez tenemos los vectores correspondientes, solo hay que juntarlos en un
# objeto data.frame con la funci�n data.frame(). #

ventas=data.frame(region,incremento)
str(ventas)

# Como vemos, la funci�n data.frame() asocia directamente un vector de tipo 
# texto a un factor, con tantos niveles como cadenas de texto diferentes 
# contenga el vector. 
#
# Otra manera de obtener el mismo data.frame es introducir las observaciones
# de la respuesta en diversos vectores con el nombre del nivel asociado, crear
# el data frame con estos vectores (todos num�ricos) y luego usar la funci�n 
# melt() del package "reshape2" para que proporcione el formato deseado. #

Este=c(10.4,12.8,15.6,9.2,8.7) 
Norte=c(12.8,14.2,16.3,10.1,12.0) 
Oeste=c(11.2,9.8,10.7,6.3,12.4) 
Sur=c(13.9,14.2,12.8,15.0,13.7)

ventas=data.frame(Este,Norte,Oeste,Sur)
ventas
str(ventas)

# Como vemos, este data frame no tiene la estructura correcta, ya que no tiene
# una variable factor que proporcione los niveles en que se ha observado cada
# respuesta. Pero podemos transformarlo f�cilmente al formato deseado usando
# la funci�n melt() para reestructurarlo de la manera apropiada. #

install.packages("reshape2")
library("reshape2")
ventas=melt(ventas,variable.name="region",value.name = "incremento")
str(ventas)

# Como vemos, la funci�n melt() b�sicamente transpone el data frame que se le
# suministra como argumento, de manera que los nombres de las variables pasan
# a ser los valores de una variable factor nombrada seg�n el par�metro
# variable.name, mientras que los valores de cada una de esas variables
# pasan a organizarse en un vector num�rico nombrado seg�n el par�metro 
# value.name. 
#
# M�s adelante veremos c�mo crear el data frame cuando nuestros datos est�n en
# un archivo.
#
# Una vez que tenemos nuestro data frame correctamente estructurado, podemos 
# usar la funci�n attach() para que los nombres de variable de este data frame
# sean accesibles por R sin necesidad de anteponer el nombre del data frame. #

rm(region,incremento) # eliminamos region e incremento del environment

names(ventas)
incremento # devuelve un error: el vector incremento no est� disponible
attach(ventas)
incremento # ya no devuelve el error, ahora incremento s� est� disponible

# Como vemos, ya podemos acceder a los vectores del data frame ventas sin tener
# que preespecificar el nombre de este data frame. 
#
# Vamos a comenzar el an�lisis de estos datos. Como siempre, lo primero que 
# conviene realizar es un an�lisis descriptivo de los datos. En este caso, los
# diagramas de cajas pueden no ser una buena opci�n en tanto se tienen pocas
# observaciones (solo 5) en cada nivel del factor "region". Una alternativa es
# el llamado diagrama de "tiras" (stripchart), que representa cada una de las
# observaciones separadas por los niveles del factor. #

layout(matrix(1:3,ncol=3))
plot(incremento ~ region)
stripchart(incremento ~ region,vertical=T)
plot.design(ventas,ylim=c(min(incremento),max(incremento)))
tapply(incremento,region,mean)
tapply(incremento,region,sd)

# En este caso, los gr�ficos nos indican que podr�a existir una diferencia 
# al menos entre los niveles Oeste y Sur. Adem�s, vemos que la dispersi�n en 
# este �ltimo nivel parece menor que la dispersi�n de las observaciones en el 
# resto de niveles. Esto podr�a afectar a la validez del ANOVA en tanto se 
# podr�a incumplir el supuesto de homocedasticidad o igualdad de varianzas 
# en los diferentes niveles.
#
# Por este motivo, es aconsejable realizar siempre un contraste de igualdad de
# varianzas entre los diferentes grupos. El contraste adecuado cuando se tienen
# m�s de dos grupos es el llamado contraste de Levene, que contrasta la 
# hip�tesis nula de que todos los niveles o grupos tienen la misma varianza. #

install.packages("lawstat")
library(lawstat)
levene.test(incremento,region,"median")

# En este caso, el p-valor del contraste es relativamente grande, con lo que 
# no podemos descartar que las diferentes regiones tengan una varianza similar
# y que las diferencias de varianza que observamos sean simplemente debidas al
# azar. Por lo tanto, no hay en principio inconveniente en realizar el an�lisis 
# de varianza.
#
# R proporciona diferentes funciones capaces de implementar el an�lisis de 
# varianza o ANOVA. Aqu� vamos a usar la funci�n aov() contenida en la 
# distribuci�n R base, aunque no viene mal saber que diferentes packages 
# proporcionan otras versiones que incorporan funciones con alg�n a�adido. 
# 
# La funci�n aov() ha de recibir como argumento una f�rmula de R indicando el
# an�lisis que se quiere realizar, esto es, cu�l es la variable respuesta y 
# cu�l la variable factor que proporciona la divisi�n en grupos o niveles. Como
# es habitual en R, la f�rmula ha de especificarse usando el operador ~, de 
# modo que la variable que aparezca antes de ~ es la variable respuesta, y la
# variable que aparece a continuaci�n es el factor.
#
# La funci�n aov() proporciona un objeto con diversas componentes, como por  
# ejemplo las estimaciones de los par�metros, los valores ajustados o los 
# residuos del modelo. Y aunque no tiene una componente espec�fica con el
# an�lisis de varianza o tabla ANOVA, podemos obtener esta f�cilmente usando
# summary() sobre el objeto devuelto por aov().

resultado=aov(incremento ~ region)
summary(resultado)

# Obs�rvese que la tabla ANOVA que se obtiene tiene una fila para el efecto
# del factor, en este caso "region", y otra fila para el error, dada por 
# "Residuals", pero no contiene la fila habitual para la variabilidad total.
# Esto es as� ya que esta �ltima fila la podemos obtener como la suma de las
# anteriores. 
# 
# Esta tabla ANOVA nos indica, como ya vimos, que a nivel de 
# significaci�n alfa=0.05 no hay evidencia suficiente para descartar la 
# igualdad de las medias de la respuesta en los diferentes niveles, aunque el 
# p-valor tan pr�ximo a 0.05 nos debe alertar de que tampoco es totalmente
# descartable que estas diferencias existan.
#
# Antes de estudiar m�s a fondo esta cuesti�n, veamos algunas de las 
# componentes del objeto devuelto por aov(). 
#
# En primer lugar, obtengamos las estimaciones de los par�metros del modelo que
# ha sido ajustado por aov(). Por defecto, aov() ajusta el modelo de las medias,
# con lo que los par�metros que se estiman son las medias (poblacionales) de 
# cada nivel, y como sabemos estas se estiman mediante las correspondientes 
# medias muestrales. Estas estimaciones se almacenan en la componente 
# "coefficients" del objeto devuelto por aov(). Podemos acceder a esta 
# componente de varias maneras. #

resultado$coefficients
coef(resultado)

# Observando estos valores, vemos que el primer elemento bajo la etiqueta 
# "(Intercept)" parece corresponder a la media muestral del nivel o regi�n 
# "Este". Pero el valor de los restantes elementos no parecen coincidir con las 
# medias muestrales de las regiones indicadas en las etiquetas. �Qu� est� 
# pasando aqu�?
#
# La clave es que la funci�n aov() devuelve bajo (Intercept) la media muestral
# del primer nivel (en orden alfab�tico), mientras que bajo las otras etiquetas 
# proporciona la diferencia entre la media muestral del nivel se�alado en la 
# etiqueta y la del primer nivel. Por ejemplo, en el caso de la regi�n Norte,
# se tiene que 1.74 = 13.08 - 11.34. Comprob�moslo con R. #

resultado$coefficients[2]
resultado$coefficients[[2]]
mean(incremento[region=="Norte"])
mean(incremento[region=="Este"])
mean(incremento[region=="Norte"])-mean(incremento[region=="Este"])

# Por tanto, podr�amos obtener las medias muestrales de cada regi�n a partir
# de estas estimaciones o coeficientes, aunque obviamente es m�s c�modo hacerlo
# usando tapply() como ya hemos hecho m�s arriba.
#
# Pasemos ahora a obtener los valores ajustados o predichos por el modelo para 
# cada observaci�n. Sabemos que este valor ajustado viene dado por la media
# del nivel correspondiente, esto es, todas las observaciones de un nivel
# se predicen mediante la media de ese nivel. Estos valores ajustados para las
# 20 observaciones est�n en la componente "fitted.values" del objeto devuelto 
# por aov(). Como antes, podemos acceder a ella de varias maneras. #

resultado$fitted.values
fitted(resultado)
tapply(incremento,region,mean)

# Veamos ahora c�mo obtener los residuos del modelo, que como sabemos son la 
# diferencia entre los valores observados y los ajustados o predichos. Los 
# residuos se encuentran en la componente "residuals" del objeto aov(), y 
# tambi�n hay al menos 2 maneras de acceder a ellos. #

resultado$residuals
resid(resultado)

# Como vemos, hay un residuo por cada observaci�n original. Comprobemos que los 
# residuos de cada grupo suman 0. #

tapply(resultado$residuals,region,sum)

# Ya que podemos acceder a las observaciones originales y a los valores 
# ajustados, comprobemos que estos residuos est�n bien calculados. #

resultado$residuals
incremento-resultado$fitted.values

# Tambi�n podemos acceder al p-valor del contraste ANOVA a trav�s
# del objeto devuelto por la funci�n summary() que construye la tabla ANOVA
# a partir de aov(). #

summary(resultado)
summary(resultado)[[1]][["Pr(>F)"]][[1]]

# E igualmente, a trav�s del summary anterior podemos acceder a la estimaci�n 
# de la varianza sigma^2, que como sabemos corresponde a la MCR. La ra�z 
# cuadrada (sqrt) del MCR corresponde entonces a la estimaci�n de sigma. #

summary(resultado)[[1]][[3]][2]
MCR=summary(resultado)[[1]][[3]][2]
est_sigma=sqrt(MCR)

# Finalmente, el objeto devuelto por aov() incluye algunos m�todos particulares
# para funciones gen�ricas como plot(). En particular, al aplicar esta funci�n
# plot() al objeto devuelto por aov() se obtienen diversos gr�ficos �tiles para
# realizar la diagnosis del modelo. Recordemos (ver Secci�n 1.2.7 de los 
# apuntes del Tema 1) que la diagnosis del modelo consiste en un an�lisis del
# cumplimiento de los supuestos b�sicos del ANOVA en el problema concreto bajo
# estudio. Suele ser necesario comprobar siempre que se cumple: i) la 
# homocedasticidad de los residuos; ii) la normalidad de los residuos; ; y iii) 
# la independencia de los residuos. 
#
# La comprobaci�n de la homocedasticidad se puede realizar gr�ficamente 
# representando los residuos frente a los valores ajustados, y viendo que la
# dispersi�n de los residuos se mantiene relativamente constante y que en 
# particular no se encuentra relacionada con los valores predichos. El primer 
# gr�fico devuelto por plot() al aplicarlo al objeto devuelto por aov() es
# precisamente este tipo de gr�fico. Tambi�n puede valer a este respecto 
# representar las desviaciones t�picas de cada grupo de residuos frente a los
# valores predichos. M�s formalmente, suele aconsejarse realizar un contraste
# de igualdad de varianzas para varios grupos, como el test de Levene ya 
# aplicado m�s arriba. #

layout(matrix(1:4,nrow = 2,ncol=2, byrow = T))
plot(resultado)
layout(1)
plot(tapply(fitted(resultado),region,mean),
     tapply(resultado$residuals,region,sd))
levene.test(incremento,region,"median")

# En este caso, aunque la dispersi�n de las observaciones en la regi�n Sur
# es algo menor que la de las otras regiones, no parece que el supuesto de 
# homocedasticidad se est� incumpliendo de manera grave. 
#
# La comprobaci�n de la normalidad de los residuos tambi�n puede llevarse a 
# cabo gr�ficamente representando los residuos en un gr�fico de probabilidad
# normal. El segundo gr�fico devuelto por plot() al aplicarlo al objeto 
# devuelto por aov() es precisamente este tipo de gr�fico. M�s formalmente,
# suele aconsejarse tambi�n realizar un contraste de normalidad de los residuos,
# como el test de Shapiro-Wilk visto en clases anteriores. #

layout(matrix(1:4,nrow = 2,ncol=2, byrow = T))
plot(resultado)
shapiro.test(resultado$residuals)

# A primera vista todo apunta a que en este caso no hay indicios de que se 
# incumpla el supuesto de normalidad. Fij�ndonos con m�s atenci�n, no obstante,
# en el gr�fico de probabilidad normal vemos que hay algunas observaciones que 
# aparecen indicadas con un n�mero (3,8,14), y que en cierto modo son las que 
# presentan una mayor desviaci�n respecto al comportamiento esperado de 
# normalidad. Tambi�n vemos en el primer gr�fico que esas tres observaciones son 
# las que tienen residuos de mayor tama�o. En los gr�ficos tercero y cuarto  
# (que respectivamente representan los residuos estandarizados vs valores 
# ajustados y por grupos), tambi�n vemos que los puntos asociados a estas 3
# observaciones son las que presentan un comportamiento m�s extremo.
#
# Estos gr�ficos est�n se�alando la posible presencia de observaciones
# influyentes. Esto quiere decir que es posible que estas 3 observaciones est�n
# teniendo una influencia importante en el ajuste del modelo, y en particular
# que su presencia puede estar influyendo en el resultado de manera relevante. 
# Esta influencia es debida a que las observaciones se�aladas son en cierto modo
# at�picas dentro de sus grupos, esto es, podr�an ser outliers, lo que podr�a
# estar sesgando en cierto modo las medias obtenidas en los respectivos grupos y
# afectar al resultado global del ANOVA.
# 
# Antes de ver qu� hacer a partir de estos indicios de presencia de 
# observaciones influyentes, estudiemos c�mo comprobar el �ltimo supuesto de
# independencia de los residuos. Este supuesto suele analizarse solo de manera
# gr�fica, mediante un gr�fico de los residuos frente al orden de recogida de
# las correspondientes observaciones. #

layout(1)
plot(resultado$residuals)

# En este caso, al no observar un patr�n caracter�stico en los datos (que de
# todas formas tampoco sabemos en qu� orden han sido recogidos), podemos
# concluir que no existe evidencia de que se viole el supuesto de independencia.
#
# En resumen, podr�amos decir que en este caso los supuestos de homocedasticidad
# e independencia no parecen bajo sospecha, y que en relaci�n al supuesto de 
# normalidad la �nica duda es la posible presencia de observaciones influyentes.
# 
# Para determinar si las observaciones se�aladas en los gr�ficos anteriores son
# efectivamente influyentes, lo mejor es ajustar de nuevo todo el modelo sin
# esas observaciones. #

obs2=(1:20 !=3 & 1:20 !=8 & 1:20 !=14)
resultado2=aov(incremento ~ region,subset=obs2)
summary(resultado2)
summary(resultado)

# Como vemos, el resultado del ANOVA al dejar fuera esas 3 observaciones es
# ahora significativo, esto es, ahora ser�a posible concluir que realmente
# existe alguna diferencia entre las medias de incrementos de ventas de las
# 4 regiones. Por tanto, estas 3 observaciones eran efectivamente influyentes.
#
# En cualquier caso, para decidir con qu� resultado nos quedamos lo m�s 
# aconsejable es intentar obtener m�s informaci�n sobre las 3 observaciones 
# eliminadas, para entender si presentan alguna caracter�stica que explique su 
# atipicidad. Si no se encuentra ninguna raz�n por la que estas observaciones
# tuvieran que ser excluidas (como provenir de una poblaci�n distinta al resto
# de observaciones, o presentar un error en la transcripci�n de los datos, etc.)
# se tendr�an que mantener en el modelo.
#
# En nuestro caso, al no poder acceder a m�s informaci�n, supondremos que estas
# 3 observaciones son v�lidas, y continuaremos el an�lisis de las diferencias 
# entre los grupos a partir del primer modelo ajustado, esto es, en el que 
# se usaron todas las observaciones para su ajuste.
#
# Veamos entonces c�mo realizar las comparaciones a pares entre las diferentes
# regiones. Esto es, ahora vamos a analizar si existen diferencias entre las
# ventas de las regiones 2 a 2. En este caso, como hay a=4 regiones habr�a
# que realizar 4 sobre 2 comparaciones, esto es, 6 comparaciones. Podemos
# calcular n�meros combinatorios en R con la funci�n choose(). #

choose(4,2) 

# Hagamos entonces las comparaciones a pares, primero sin ning�n m�todo de 
# ajuste de los p-valores o niveles de significaci�n. #

pairwise.t.test(incremento,region,"none")

# Como vemos, obtenemos dos comparaciones significativas, la de Norte vs Oeste
# y la de Sur vs Oeste. Sin embargo, ya podemos intuir que estas comparaciones
# no ser�an significativas en caso de ajustar alfa (o equivalentemente los
# p-valores) mediante el m�todo de Bonferroni. El nivel de significaci�n 
# ajustado ser�a entonces el siguiente: #

0.05/choose(4,2)

# Vemos que con este nivel de significaci�n ninguna de las comparaciones 
# anteriores ser�a significativa. Hagamos ahora las comparaciones especificando
# que se lleve a cabo el ajuste de Bonferroni. #

pairwise.t.test(incremento,region,"bonferroni")

# Vemos que, b�sicamente, lo que hace R es multiplicar los p-valores que se 
# obtuvieron con method="none" por el n�mero de comparaciones, de manera que
# en vez de ajustar el nivel de significaci�n lo que R hace es ajustar los 
# p- valores, y lo que entonces habr�a que hacer es comparar los p-valores 
# ajustados con el nivel de significaci�n habitual. De este modo, en este caso
# concluir�amos que no podemos rechazar la igualdad de medias entre las 
# regiones.
#
# Es importante observar que parece existir una contradicci�n entre el resultado
# del ANOVA (no hay diferencias entre los grupos) y el resultado de las 
# comparaciones a pares sin ajustar por Bonferroni (existe diferencia entre
# las regiones Norte y Oeste y entre Sur y Oeste). Esto es, el primer an�lisis
# nos dice que no hay diferencias significativas entre medias pero el segundo s�
# las encuentra. �Qu� ocurre aqu�?
#
# La clave aqu� es que, cuando existen m�s de 2 grupos, los contrastes ANOVA y t
# realmente realizan comparaciones diferentes. Si es as�, �con qu� resultado nos
# quedamos? Esto es, �nos fiamos de lo que nos dice el ANOVA (no hay diferencias) 
# o de lo que dicen los contrastes t (hay diferencias)? 
#
# La regla que se suele aplicar es que solo hay que llevar a cabo comparaciones
# a pares cuando el ANOVA es significativo. Esto es, si el ANOVA dice que no
# hay diferencias entonces no estudiar�amos las posibles diferencias entre pares
# de grupos. Por contra, cuando el ANOVA dice que s� hay diferencias, entonces
# s� que tiene sentido estudiar entre qu� grupos se dan esas diferencias.
#
# En este sentido, cuando el ANOVA sale no significativo (p-valor > 0.05) pero 
# su p-valor es relativamente peque�o, esto es, est� pr�ximo a 0.05, es posible
# que se d� la aparente contradicci�n entre el ANOVA y los contrastes t. Esta 
# contradicci�n sin embargo no suele ocurrir cuando el ANOVA es significativo. 
#
# Adem�s, como se advert�a al ejecutar la funci�n aov() (l�neas 167-171), cuando
# el p-valor del ANOVA es no significativo pero pr�ximo a 0.05 es posible que
# realmente existan algunas diferencias entre grupos, pero que estas est�n
# de alg�n modo "enmascaradas" por c�mo se han dividido las observaciones entre
# grupos. Para ilustrar esta idea, vamos a rehacer los grupos, de manera que
# ahora las observaciones de las regiones Este y Oeste se asignar�n a un �nico 
# grupo. 
#
# Para ello, en primer lugar copiamos la estructura de factor de la variable 
# region en una nueva variable. #

region2=factor(region)

# Esta nueva variable region2 tiene los mismos 4 niveles o grupos que la anterior
# variable region. #

levels(region2)

# Vamos a rehacer la separaci�n entre grupos, de manera que ahora las 
# observaciones de las regiones Este y Oeste se asignan a un �nico grupo, que
# denominaremos Este+Oeste. #

levels(region2)[c(1,3)]="Este+Oeste"
levels(region2)

# Como vemos, ahora el factor region2 tiene solo 3 niveles. Llevemos a cabo el 
# an�lisis de la varianza para comparar las medias de la respuesta en estos
# 3 grupos. #

resultado2=aov(incremento ~ region2)
summary(resultado2)

# As� pues, ahora el ANOVA s� sale significativo, esto es, podr�amos concluir que
# s� existen diferencias entre estos grupos. Por tanto, tendr�a sentido aplicar
# ahora las comparaciones m�ltiples para estudiar entre qu� grupos se dan esas
# diferencias. #

pairwise.t.test(incremento,region2,"none")

# En este caso, habr�a diferencia entre las ventas en la regi�n Sur y las ventas
# conjuntas de las regiones Este y Oeste. Esta �nica diferencia significativa
# de hecho se mantiene al aplicar el ajuste de Bonferroni. #

pairwise.t.test(incremento,region2,"bonferroni")

# Antes de concluir, vamos a ver que cuando solo se tienen 2 grupos, el ANOVA y 
# el contraste t son equivalentes, esto es, en ese caso son el mismo contraste. 
# Para ello, vamos a reorganizar los datos originales en 2 grupos, Norte+Sur y 
# Este+Oeste. #

region3=factor(region)
levels(region3)
levels(region3)[c(2,4)]="Norte+Sur"
levels(region3)
levels(region3)[c(1,3)]="Este+Oeste"
levels(region3)

# As�, el nuevo factor region3 tiene ahora solo 2 niveles. Realizemos el ANOVA
# para comparar las medias de ambos grupos. #

resultado3=aov(incremento ~ region3)
summary(resultado3)

# De nuevo, el ANOVA nos lleva a la conclusi�n de que hay diferencia entre las 
# ventas conjuntas de las regiones Norte y Sur y las de las regiones Este y 
# Oeste. Veamos que el p-valor asociado al contraste t de estos dos grupos 
# (asumiendo varianzas iguales) es el mismo que acabamos de obtener con el
# ANOVA. #

t.test(incremento ~ region3, var.equal=T)

# De hecho, podemos comprobar que las varianzas usadas en ambos contrastes 
# son las mismas. Como sabemos, la varianza que estima el ANOVA es la dada por
# el MCR, y la estimaci�n de sigma es su ra�z cuadrada. #

summary(resultado3)[[1]][[3]][2]
sqrt(summary(resultado3)[[1]][[3]][2])

# En el contraste t para dos poblaciones con la misma varianza, se estima esta
# varianza combinando las cuasivarianzas muestrales de las dos muestras, 
# mediante la f�rmula que aparece en el pdf "Contrastes de hip�tesis para la
# comparaci�n de dos poblaciones". #

s2=(9*sd(incremento[region3=="Este+Oeste"])^2+
      9*sd(incremento[region3=="Norte+Sur"])^2)/(10+10-2)
s2
s=sqrt(s2)
s

# Tambi�n podemos obtener s a partir del valor t0 del estad�stico de contraste,
# despejando s de la f�rmula para t0. Para ello, podemos acceder al valor de
# t0 a partir del objeto devuelto por la funci�n t.test(). #

t0=t.test(incremento ~ region3, var.equal=T)[[1]]
t0

s=(mean(incremento[region3=="Este+Oeste"])-
     mean(incremento[region3=="Norte+Sur"]))/
  (t0*sqrt(2/10))
s

# Para finalizar, es importante resaltar que, en el caso de las comparaciones 
# m�ltiples, los contrastes t utilizan la estimaci�n de varianza dada por el 
# ANOVA, esto es, en ese caso se tendr�a s2=MCR. Para comprobarlo, volvamos
# al modelo con 4 grupos, correspondientes a las 4 regiones del ejemplo.

resultado=aov(incremento ~ region)
summary(resultado)

# La estimaci�n de varianza del ANOVA es como sabemos el MCR, y la estimaci�n
# de sigma es su ra�z cuadrada. #

s2_anova=summary(resultado)[[1]][[3]][2]
s2_anova
s_anova=sqrt(s2_anova)
s_anova

# Recordemos tambi�n los resultados obtenidos al llevar a cabo sobre estos datos
# comparaciones m�ltiples mediante contrastes t. #

pairwise.t.test(incremento,region,"none")

# Vamos entonces a replicar el contraste para la comparaci�n entre por ejemplo 
# las regiones Sur y Este usando la metodolog�a de comparaciones m�ltiples. 
#
# Siguiendo la f�rmula en los apuntes del TEMA 1, el estad�stico de contraste 
# para esta comparaci�n se obtiene como sigue. #

t0=(mean(incremento[region=="Este"])-mean(incremento[region=="Sur"]))/
  sqrt(s2_anova*(2/5))
t0

# Obtengamos el p-valor asociado a este estad�stico de contraste. #

2*pt(abs(t0),4*5-4,lower.tail = F)

# Como vemos, este es el mismo p-valor que se obtuvo con pairwise.t.test() para
# la comparaci�n Este vs. Sur. As� pues, los contrastes t que se aplican en el 
# procedimiento de comparaciones m�ltiples usan la estimaci�n de varianza del 
# ANOVA en lugar de la estimaci�n obtenida combinando las estimaciones de 
# varianza de ambos grupos. Esto no contradice la equivalencia vista antes ya 
# que en el caso de 2 grupos ambos estimadores (ANOVA y combinado) coinciden.
#
# Como hemos visto, estos contrastes t se obtienen a partir de la diferencia de
# medias de los grupos que se comparan. Por otro lado, antes hemos visto (l�neas
# 176-207) que los coeficientes estimados por aov() son precisamente las 
# diferencias entre las medias de cada grupo y un grupo de referencia. A menos 
# que se especifique de otro modo, aov() toma como grupo de referencia el del
# primer nivel por orden alfab�tico. 
#
# Estos contrastes t usando la varianza del ANOVA para comparar respecto a un
# grupo de referencia se pueden obtener directamente sin necesidad de usar
# comparaciones m�ltiples con la funci�n pairwise.t.test(). Esto se realiza
# usando la funci�n summary.lm() sobre el objeto devuelto por aov(). #

summary.lm(resultado)
pairwise.t.test(incremento,region,"none")

# Como se puede observar, los valores t y los p-valores que se obtienen se 
# corresponden con los obtenidos mediante pairwise.t.test() al comparar contra
# la regi�n Este. La regi�n Este es la referencia en este caso ya que es el 
# primer nivel por orden alfab�tico.
# 
# Podemos cambiar la referencia que toma el ANOVA como sigue. #

region_refOeste=factor(region)
levels(region_refOeste)
region_refOeste=relevel(region_refOeste,ref="Oeste")
levels(region_refOeste)

# Al realizar el ANOVA sobre este nuevo factor reordenado, los contrastes que
# devuelve summary.lm() se realizan contra la nueva referencia. #

resultado4=aov(incremento ~ region_refOeste)
summary(resultado4)
summary.lm(resultado4)
pairwise.t.test(incremento,region_refOeste,"none")

# Como ya se advirti� antes, debemos tratar con cautela los casos en que 
# alguno de estos contrastes sea significativo cuando el ANOVA no lo es. #
