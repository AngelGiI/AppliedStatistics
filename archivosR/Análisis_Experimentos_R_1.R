################################################################################
######                                                                    ######
######                ANÁLISIS DE EXPERIMENTOS CON R (1)                  ######
######                                                                    ######
######                       ESTADÍSTICA APLICADA                         ######
######               GRADO EN INGENIERÍA MATEMÁTICA 2020/2021             ######
######                                                                    ######
################################################################################

#        ESTE SCRIPT HA SIDO REALIZADO POR TINGUARO RODRÍGUEZ (2020)           #

# En primer lugar, vamos a ver cómo cargar datos en R para poder realizar el 
# análisis estadístico de un experimento.
#
# Como sabemos, el formato con que trabaja R para almacenar datos es el 
# data.frame. Además, de cara al análisis posterior, es necesario que este 
# data frame esté estructurado en términos de un factor y la variable respuesta 
# de nuestro problema. Esto es, el data frame debe contener una variable de tipo
# factor con los diferentes niveles, repetidos tantas veces como observaciones
# de la respuesta haya en cada nivel. Y asociado a cada nivel del factor, deben
# estar las correspondientes respuestas de cada nivel. Hay diversas maneras de 
# conseguir un data frame con esta configuración.
#
# La primera forma es introducirlos directamente en este formato a partir de los
# vectores correspondientes usando la función data.frame(). Veamos como hacerlo 
# para los datos del ejercicio 4 de la Hoja de problemas 1. # 

region=rep(c("Este","Norte","Oeste","Sur"),each=5)
region

# Como vemos, la función rep() repite cada uno de los elementos del vector que
# recibe como primer argumento tantas veces como se indique en el parámetro
# "each". 
#
# Las observaciones de la respuesta, en tanto normalmente no contendrán 
# repeticiones al tratarse de una variable continua, hay que introducirlas en
# otro vector una a una CUIDANDO QUE SE INTRODUZCAN EN EL ORDEN ADECUADO, esto
# es, cada respuesta debe quedar asociada al nivel en que se ha observado. #

incremento=c(10.4,12.8,15.6,9.2,8.7,
             12.8,14.2,16.3,10.1,12.0,
             11.2,9.8,10.7,6.3,12.4,
             13.9,14.2,12.8,15.0,13.7) 
incremento

# Una vez tenemos los vectores correspondientes, solo hay que juntarlos en un
# objeto data.frame con la función data.frame(). #

ventas=data.frame(region,incremento)
str(ventas)

# Como vemos, la función data.frame() asocia directamente un vector de tipo 
# texto a un factor, con tantos niveles como cadenas de texto diferentes 
# contenga el vector. 
#
# Otra manera de obtener el mismo data.frame es introducir las observaciones
# de la respuesta en diversos vectores con el nombre del nivel asociado, crear
# el data frame con estos vectores (todos numéricos) y luego usar la función 
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
# respuesta. Pero podemos transformarlo fácilmente al formato deseado usando
# la función melt() para reestructurarlo de la manera apropiada. #

install.packages("reshape2")
library("reshape2")
ventas=melt(ventas,variable.name="region",value.name = "incremento")
str(ventas)

# Como vemos, la función melt() básicamente transpone el data frame que se le
# suministra como argumento, de manera que los nombres de las variables pasan
# a ser los valores de una variable factor nombrada según el parámetro
# variable.name, mientras que los valores de cada una de esas variables
# pasan a organizarse en un vector numérico nombrado según el parámetro 
# value.name. 
#
# Más adelante veremos cómo crear el data frame cuando nuestros datos están en
# un archivo.
#
# Una vez que tenemos nuestro data frame correctamente estructurado, podemos 
# usar la función attach() para que los nombres de variable de este data frame
# sean accesibles por R sin necesidad de anteponer el nombre del data frame. #

rm(region,incremento) # eliminamos region e incremento del environment

names(ventas)
incremento # devuelve un error: el vector incremento no está disponible
attach(ventas)
incremento # ya no devuelve el error, ahora incremento sí está disponible

# Como vemos, ya podemos acceder a los vectores del data frame ventas sin tener
# que preespecificar el nombre de este data frame. 
#
# Vamos a comenzar el análisis de estos datos. Como siempre, lo primero que 
# conviene realizar es un análisis descriptivo de los datos. En este caso, los
# diagramas de cajas pueden no ser una buena opción en tanto se tienen pocas
# observaciones (solo 5) en cada nivel del factor "region". Una alternativa es
# el llamado diagrama de "tiras" (stripchart), que representa cada una de las
# observaciones separadas por los niveles del factor. #

layout(matrix(1:3,ncol=3))
plot(incremento ~ region)
stripchart(incremento ~ region,vertical=T)
plot.design(ventas,ylim=c(min(incremento),max(incremento)))
tapply(incremento,region,mean)
tapply(incremento,region,sd)

# En este caso, los gráficos nos indican que podría existir una diferencia 
# al menos entre los niveles Oeste y Sur. Además, vemos que la dispersión en 
# este último nivel parece menor que la dispersión de las observaciones en el 
# resto de niveles. Esto podría afectar a la validez del ANOVA en tanto se 
# podría incumplir el supuesto de homocedasticidad o igualdad de varianzas 
# en los diferentes niveles.
#
# Por este motivo, es aconsejable realizar siempre un contraste de igualdad de
# varianzas entre los diferentes grupos. El contraste adecuado cuando se tienen
# más de dos grupos es el llamado contraste de Levene, que contrasta la 
# hipótesis nula de que todos los niveles o grupos tienen la misma varianza. #

install.packages("lawstat")
library(lawstat)
levene.test(incremento,region,"median")

# En este caso, el p-valor del contraste es relativamente grande, con lo que 
# no podemos descartar que las diferentes regiones tengan una varianza similar
# y que las diferencias de varianza que observamos sean simplemente debidas al
# azar. Por lo tanto, no hay en principio inconveniente en realizar el análisis 
# de varianza.
#
# R proporciona diferentes funciones capaces de implementar el análisis de 
# varianza o ANOVA. Aquí vamos a usar la función aov() contenida en la 
# distribución R base, aunque no viene mal saber que diferentes packages 
# proporcionan otras versiones que incorporan funciones con algún añadido. 
# 
# La función aov() ha de recibir como argumento una fórmula de R indicando el
# análisis que se quiere realizar, esto es, cuál es la variable respuesta y 
# cuál la variable factor que proporciona la división en grupos o niveles. Como
# es habitual en R, la fórmula ha de especificarse usando el operador ~, de 
# modo que la variable que aparezca antes de ~ es la variable respuesta, y la
# variable que aparece a continuación es el factor.
#
# La función aov() proporciona un objeto con diversas componentes, como por  
# ejemplo las estimaciones de los parámetros, los valores ajustados o los 
# residuos del modelo. Y aunque no tiene una componente específica con el
# análisis de varianza o tabla ANOVA, podemos obtener esta fácilmente usando
# summary() sobre el objeto devuelto por aov().

resultado=aov(incremento ~ region)
summary(resultado)

# Obsérvese que la tabla ANOVA que se obtiene tiene una fila para el efecto
# del factor, en este caso "region", y otra fila para el error, dada por 
# "Residuals", pero no contiene la fila habitual para la variabilidad total.
# Esto es así ya que esta última fila la podemos obtener como la suma de las
# anteriores. 
# 
# Esta tabla ANOVA nos indica, como ya vimos, que a nivel de 
# significación alfa=0.05 no hay evidencia suficiente para descartar la 
# igualdad de las medias de la respuesta en los diferentes niveles, aunque el 
# p-valor tan próximo a 0.05 nos debe alertar de que tampoco es totalmente
# descartable que estas diferencias existan.
#
# Antes de estudiar más a fondo esta cuestión, veamos algunas de las 
# componentes del objeto devuelto por aov(). 
#
# En primer lugar, obtengamos las estimaciones de los parámetros del modelo que
# ha sido ajustado por aov(). Por defecto, aov() ajusta el modelo de las medias,
# con lo que los parámetros que se estiman son las medias (poblacionales) de 
# cada nivel, y como sabemos estas se estiman mediante las correspondientes 
# medias muestrales. Estas estimaciones se almacenan en la componente 
# "coefficients" del objeto devuelto por aov(). Podemos acceder a esta 
# componente de varias maneras. #

resultado$coefficients
coef(resultado)

# Observando estos valores, vemos que el primer elemento bajo la etiqueta 
# "(Intercept)" parece corresponder a la media muestral del nivel o región 
# "Este". Pero el valor de los restantes elementos no parecen coincidir con las 
# medias muestrales de las regiones indicadas en las etiquetas. ¿Qué está 
# pasando aquí?
#
# La clave es que la función aov() devuelve bajo (Intercept) la media muestral
# del primer nivel (en orden alfabético), mientras que bajo las otras etiquetas 
# proporciona la diferencia entre la media muestral del nivel señalado en la 
# etiqueta y la del primer nivel. Por ejemplo, en el caso de la región Norte,
# se tiene que 1.74 = 13.08 - 11.34. Comprobémoslo con R. #

resultado$coefficients[2]
resultado$coefficients[[2]]
mean(incremento[region=="Norte"])
mean(incremento[region=="Este"])
mean(incremento[region=="Norte"])-mean(incremento[region=="Este"])

# Por tanto, podríamos obtener las medias muestrales de cada región a partir
# de estas estimaciones o coeficientes, aunque obviamente es más cómodo hacerlo
# usando tapply() como ya hemos hecho más arriba.
#
# Pasemos ahora a obtener los valores ajustados o predichos por el modelo para 
# cada observación. Sabemos que este valor ajustado viene dado por la media
# del nivel correspondiente, esto es, todas las observaciones de un nivel
# se predicen mediante la media de ese nivel. Estos valores ajustados para las
# 20 observaciones están en la componente "fitted.values" del objeto devuelto 
# por aov(). Como antes, podemos acceder a ella de varias maneras. #

resultado$fitted.values
fitted(resultado)
tapply(incremento,region,mean)

# Veamos ahora cómo obtener los residuos del modelo, que como sabemos son la 
# diferencia entre los valores observados y los ajustados o predichos. Los 
# residuos se encuentran en la componente "residuals" del objeto aov(), y 
# también hay al menos 2 maneras de acceder a ellos. #

resultado$residuals
resid(resultado)

# Como vemos, hay un residuo por cada observación original. Comprobemos que los 
# residuos de cada grupo suman 0. #

tapply(resultado$residuals,region,sum)

# Ya que podemos acceder a las observaciones originales y a los valores 
# ajustados, comprobemos que estos residuos están bien calculados. #

resultado$residuals
incremento-resultado$fitted.values

# También podemos acceder al p-valor del contraste ANOVA a través
# del objeto devuelto por la función summary() que construye la tabla ANOVA
# a partir de aov(). #

summary(resultado)
summary(resultado)[[1]][["Pr(>F)"]][[1]]

# E igualmente, a través del summary anterior podemos acceder a la estimación 
# de la varianza sigma^2, que como sabemos corresponde a la MCR. La raíz 
# cuadrada (sqrt) del MCR corresponde entonces a la estimación de sigma. #

summary(resultado)[[1]][[3]][2]
MCR=summary(resultado)[[1]][[3]][2]
est_sigma=sqrt(MCR)

# Finalmente, el objeto devuelto por aov() incluye algunos métodos particulares
# para funciones genéricas como plot(). En particular, al aplicar esta función
# plot() al objeto devuelto por aov() se obtienen diversos gráficos útiles para
# realizar la diagnosis del modelo. Recordemos (ver Sección 1.2.7 de los 
# apuntes del Tema 1) que la diagnosis del modelo consiste en un análisis del
# cumplimiento de los supuestos básicos del ANOVA en el problema concreto bajo
# estudio. Suele ser necesario comprobar siempre que se cumple: i) la 
# homocedasticidad de los residuos; ii) la normalidad de los residuos; ; y iii) 
# la independencia de los residuos. 
#
# La comprobación de la homocedasticidad se puede realizar gráficamente 
# representando los residuos frente a los valores ajustados, y viendo que la
# dispersión de los residuos se mantiene relativamente constante y que en 
# particular no se encuentra relacionada con los valores predichos. El primer 
# gráfico devuelto por plot() al aplicarlo al objeto devuelto por aov() es
# precisamente este tipo de gráfico. También puede valer a este respecto 
# representar las desviaciones típicas de cada grupo de residuos frente a los
# valores predichos. Más formalmente, suele aconsejarse realizar un contraste
# de igualdad de varianzas para varios grupos, como el test de Levene ya 
# aplicado más arriba. #

layout(matrix(1:4,nrow = 2,ncol=2, byrow = T))
plot(resultado)
layout(1)
plot(tapply(fitted(resultado),region,mean),
     tapply(resultado$residuals,region,sd))
levene.test(incremento,region,"median")

# En este caso, aunque la dispersión de las observaciones en la región Sur
# es algo menor que la de las otras regiones, no parece que el supuesto de 
# homocedasticidad se esté incumpliendo de manera grave. 
#
# La comprobación de la normalidad de los residuos también puede llevarse a 
# cabo gráficamente representando los residuos en un gráfico de probabilidad
# normal. El segundo gráfico devuelto por plot() al aplicarlo al objeto 
# devuelto por aov() es precisamente este tipo de gráfico. Más formalmente,
# suele aconsejarse también realizar un contraste de normalidad de los residuos,
# como el test de Shapiro-Wilk visto en clases anteriores. #

layout(matrix(1:4,nrow = 2,ncol=2, byrow = T))
plot(resultado)
shapiro.test(resultado$residuals)

# A primera vista todo apunta a que en este caso no hay indicios de que se 
# incumpla el supuesto de normalidad. Fijándonos con más atención, no obstante,
# en el gráfico de probabilidad normal vemos que hay algunas observaciones que 
# aparecen indicadas con un número (3,8,14), y que en cierto modo son las que 
# presentan una mayor desviación respecto al comportamiento esperado de 
# normalidad. También vemos en el primer gráfico que esas tres observaciones son 
# las que tienen residuos de mayor tamaño. En los gráficos tercero y cuarto  
# (que respectivamente representan los residuos estandarizados vs valores 
# ajustados y por grupos), también vemos que los puntos asociados a estas 3
# observaciones son las que presentan un comportamiento más extremo.
#
# Estos gráficos están señalando la posible presencia de observaciones
# influyentes. Esto quiere decir que es posible que estas 3 observaciones estén
# teniendo una influencia importante en el ajuste del modelo, y en particular
# que su presencia puede estar influyendo en el resultado de manera relevante. 
# Esta influencia es debida a que las observaciones señaladas son en cierto modo
# atípicas dentro de sus grupos, esto es, podrían ser outliers, lo que podría
# estar sesgando en cierto modo las medias obtenidas en los respectivos grupos y
# afectar al resultado global del ANOVA.
# 
# Antes de ver qué hacer a partir de estos indicios de presencia de 
# observaciones influyentes, estudiemos cómo comprobar el último supuesto de
# independencia de los residuos. Este supuesto suele analizarse solo de manera
# gráfica, mediante un gráfico de los residuos frente al orden de recogida de
# las correspondientes observaciones. #

layout(1)
plot(resultado$residuals)

# En este caso, al no observar un patrón característico en los datos (que de
# todas formas tampoco sabemos en qué orden han sido recogidos), podemos
# concluir que no existe evidencia de que se viole el supuesto de independencia.
#
# En resumen, podríamos decir que en este caso los supuestos de homocedasticidad
# e independencia no parecen bajo sospecha, y que en relación al supuesto de 
# normalidad la única duda es la posible presencia de observaciones influyentes.
# 
# Para determinar si las observaciones señaladas en los gráficos anteriores son
# efectivamente influyentes, lo mejor es ajustar de nuevo todo el modelo sin
# esas observaciones. #

obs2=(1:20 !=3 & 1:20 !=8 & 1:20 !=14)
resultado2=aov(incremento ~ region,subset=obs2)
summary(resultado2)
summary(resultado)

# Como vemos, el resultado del ANOVA al dejar fuera esas 3 observaciones es
# ahora significativo, esto es, ahora sería posible concluir que realmente
# existe alguna diferencia entre las medias de incrementos de ventas de las
# 4 regiones. Por tanto, estas 3 observaciones eran efectivamente influyentes.
#
# En cualquier caso, para decidir con qué resultado nos quedamos lo más 
# aconsejable es intentar obtener más información sobre las 3 observaciones 
# eliminadas, para entender si presentan alguna característica que explique su 
# atipicidad. Si no se encuentra ninguna razón por la que estas observaciones
# tuvieran que ser excluidas (como provenir de una población distinta al resto
# de observaciones, o presentar un error en la transcripción de los datos, etc.)
# se tendrían que mantener en el modelo.
#
# En nuestro caso, al no poder acceder a más información, supondremos que estas
# 3 observaciones son válidas, y continuaremos el análisis de las diferencias 
# entre los grupos a partir del primer modelo ajustado, esto es, en el que 
# se usaron todas las observaciones para su ajuste.
#
# Veamos entonces cómo realizar las comparaciones a pares entre las diferentes
# regiones. Esto es, ahora vamos a analizar si existen diferencias entre las
# ventas de las regiones 2 a 2. En este caso, como hay a=4 regiones habría
# que realizar 4 sobre 2 comparaciones, esto es, 6 comparaciones. Podemos
# calcular números combinatorios en R con la función choose(). #

choose(4,2) 

# Hagamos entonces las comparaciones a pares, primero sin ningún método de 
# ajuste de los p-valores o niveles de significación. #

pairwise.t.test(incremento,region,"none")

# Como vemos, obtenemos dos comparaciones significativas, la de Norte vs Oeste
# y la de Sur vs Oeste. Sin embargo, ya podemos intuir que estas comparaciones
# no serían significativas en caso de ajustar alfa (o equivalentemente los
# p-valores) mediante el método de Bonferroni. El nivel de significación 
# ajustado sería entonces el siguiente: #

0.05/choose(4,2)

# Vemos que con este nivel de significación ninguna de las comparaciones 
# anteriores sería significativa. Hagamos ahora las comparaciones especificando
# que se lleve a cabo el ajuste de Bonferroni. #

pairwise.t.test(incremento,region,"bonferroni")

# Vemos que, básicamente, lo que hace R es multiplicar los p-valores que se 
# obtuvieron con method="none" por el número de comparaciones, de manera que
# en vez de ajustar el nivel de significación lo que R hace es ajustar los 
# p- valores, y lo que entonces habría que hacer es comparar los p-valores 
# ajustados con el nivel de significación habitual. De este modo, en este caso
# concluiríamos que no podemos rechazar la igualdad de medias entre las 
# regiones.
#
# Es importante observar que parece existir una contradicción entre el resultado
# del ANOVA (no hay diferencias entre los grupos) y el resultado de las 
# comparaciones a pares sin ajustar por Bonferroni (existe diferencia entre
# las regiones Norte y Oeste y entre Sur y Oeste). Esto es, el primer análisis
# nos dice que no hay diferencias significativas entre medias pero el segundo sí
# las encuentra. ¿Qué ocurre aquí?
#
# La clave aquí es que, cuando existen más de 2 grupos, los contrastes ANOVA y t
# realmente realizan comparaciones diferentes. Si es así, ¿con qué resultado nos
# quedamos? Esto es, ¿nos fiamos de lo que nos dice el ANOVA (no hay diferencias) 
# o de lo que dicen los contrastes t (hay diferencias)? 
#
# La regla que se suele aplicar es que solo hay que llevar a cabo comparaciones
# a pares cuando el ANOVA es significativo. Esto es, si el ANOVA dice que no
# hay diferencias entonces no estudiaríamos las posibles diferencias entre pares
# de grupos. Por contra, cuando el ANOVA dice que sí hay diferencias, entonces
# sí que tiene sentido estudiar entre qué grupos se dan esas diferencias.
#
# En este sentido, cuando el ANOVA sale no significativo (p-valor > 0.05) pero 
# su p-valor es relativamente pequeño, esto es, está próximo a 0.05, es posible
# que se dé la aparente contradicción entre el ANOVA y los contrastes t. Esta 
# contradicción sin embargo no suele ocurrir cuando el ANOVA es significativo. 
#
# Además, como se advertía al ejecutar la función aov() (líneas 167-171), cuando
# el p-valor del ANOVA es no significativo pero próximo a 0.05 es posible que
# realmente existan algunas diferencias entre grupos, pero que estas estén
# de algún modo "enmascaradas" por cómo se han dividido las observaciones entre
# grupos. Para ilustrar esta idea, vamos a rehacer los grupos, de manera que
# ahora las observaciones de las regiones Este y Oeste se asignarán a un único 
# grupo. 
#
# Para ello, en primer lugar copiamos la estructura de factor de la variable 
# region en una nueva variable. #

region2=factor(region)

# Esta nueva variable region2 tiene los mismos 4 niveles o grupos que la anterior
# variable region. #

levels(region2)

# Vamos a rehacer la separación entre grupos, de manera que ahora las 
# observaciones de las regiones Este y Oeste se asignan a un único grupo, que
# denominaremos Este+Oeste. #

levels(region2)[c(1,3)]="Este+Oeste"
levels(region2)

# Como vemos, ahora el factor region2 tiene solo 3 niveles. Llevemos a cabo el 
# análisis de la varianza para comparar las medias de la respuesta en estos
# 3 grupos. #

resultado2=aov(incremento ~ region2)
summary(resultado2)

# Así pues, ahora el ANOVA sí sale significativo, esto es, podríamos concluir que
# sí existen diferencias entre estos grupos. Por tanto, tendría sentido aplicar
# ahora las comparaciones múltiples para estudiar entre qué grupos se dan esas
# diferencias. #

pairwise.t.test(incremento,region2,"none")

# En este caso, habría diferencia entre las ventas en la región Sur y las ventas
# conjuntas de las regiones Este y Oeste. Esta única diferencia significativa
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

# Así, el nuevo factor region3 tiene ahora solo 2 niveles. Realizemos el ANOVA
# para comparar las medias de ambos grupos. #

resultado3=aov(incremento ~ region3)
summary(resultado3)

# De nuevo, el ANOVA nos lleva a la conclusión de que hay diferencia entre las 
# ventas conjuntas de las regiones Norte y Sur y las de las regiones Este y 
# Oeste. Veamos que el p-valor asociado al contraste t de estos dos grupos 
# (asumiendo varianzas iguales) es el mismo que acabamos de obtener con el
# ANOVA. #

t.test(incremento ~ region3, var.equal=T)

# De hecho, podemos comprobar que las varianzas usadas en ambos contrastes 
# son las mismas. Como sabemos, la varianza que estima el ANOVA es la dada por
# el MCR, y la estimación de sigma es su raíz cuadrada. #

summary(resultado3)[[1]][[3]][2]
sqrt(summary(resultado3)[[1]][[3]][2])

# En el contraste t para dos poblaciones con la misma varianza, se estima esta
# varianza combinando las cuasivarianzas muestrales de las dos muestras, 
# mediante la fórmula que aparece en el pdf "Contrastes de hipótesis para la
# comparación de dos poblaciones". #

s2=(9*sd(incremento[region3=="Este+Oeste"])^2+
      9*sd(incremento[region3=="Norte+Sur"])^2)/(10+10-2)
s2
s=sqrt(s2)
s

# También podemos obtener s a partir del valor t0 del estadístico de contraste,
# despejando s de la fórmula para t0. Para ello, podemos acceder al valor de
# t0 a partir del objeto devuelto por la función t.test(). #

t0=t.test(incremento ~ region3, var.equal=T)[[1]]
t0

s=(mean(incremento[region3=="Este+Oeste"])-
     mean(incremento[region3=="Norte+Sur"]))/
  (t0*sqrt(2/10))
s

# Para finalizar, es importante resaltar que, en el caso de las comparaciones 
# múltiples, los contrastes t utilizan la estimación de varianza dada por el 
# ANOVA, esto es, en ese caso se tendría s2=MCR. Para comprobarlo, volvamos
# al modelo con 4 grupos, correspondientes a las 4 regiones del ejemplo.

resultado=aov(incremento ~ region)
summary(resultado)

# La estimación de varianza del ANOVA es como sabemos el MCR, y la estimación
# de sigma es su raíz cuadrada. #

s2_anova=summary(resultado)[[1]][[3]][2]
s2_anova
s_anova=sqrt(s2_anova)
s_anova

# Recordemos también los resultados obtenidos al llevar a cabo sobre estos datos
# comparaciones múltiples mediante contrastes t. #

pairwise.t.test(incremento,region,"none")

# Vamos entonces a replicar el contraste para la comparación entre por ejemplo 
# las regiones Sur y Este usando la metodología de comparaciones múltiples. 
#
# Siguiendo la fórmula en los apuntes del TEMA 1, el estadístico de contraste 
# para esta comparación se obtiene como sigue. #

t0=(mean(incremento[region=="Este"])-mean(incremento[region=="Sur"]))/
  sqrt(s2_anova*(2/5))
t0

# Obtengamos el p-valor asociado a este estadístico de contraste. #

2*pt(abs(t0),4*5-4,lower.tail = F)

# Como vemos, este es el mismo p-valor que se obtuvo con pairwise.t.test() para
# la comparación Este vs. Sur. Así pues, los contrastes t que se aplican en el 
# procedimiento de comparaciones múltiples usan la estimación de varianza del 
# ANOVA en lugar de la estimación obtenida combinando las estimaciones de 
# varianza de ambos grupos. Esto no contradice la equivalencia vista antes ya 
# que en el caso de 2 grupos ambos estimadores (ANOVA y combinado) coinciden.
#
# Como hemos visto, estos contrastes t se obtienen a partir de la diferencia de
# medias de los grupos que se comparan. Por otro lado, antes hemos visto (líneas
# 176-207) que los coeficientes estimados por aov() son precisamente las 
# diferencias entre las medias de cada grupo y un grupo de referencia. A menos 
# que se especifique de otro modo, aov() toma como grupo de referencia el del
# primer nivel por orden alfabético. 
#
# Estos contrastes t usando la varianza del ANOVA para comparar respecto a un
# grupo de referencia se pueden obtener directamente sin necesidad de usar
# comparaciones múltiples con la función pairwise.t.test(). Esto se realiza
# usando la función summary.lm() sobre el objeto devuelto por aov(). #

summary.lm(resultado)
pairwise.t.test(incremento,region,"none")

# Como se puede observar, los valores t y los p-valores que se obtienen se 
# corresponden con los obtenidos mediante pairwise.t.test() al comparar contra
# la región Este. La región Este es la referencia en este caso ya que es el 
# primer nivel por orden alfabético.
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

# Como ya se advirtió antes, debemos tratar con cautela los casos en que 
# alguno de estos contrastes sea significativo cuando el ANOVA no lo es. #
