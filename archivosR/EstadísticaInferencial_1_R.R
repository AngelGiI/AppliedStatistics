################################################################################
######                                                                    ######
######                ESTADÍSTICA INFERENCIAL CON R (1)                   ######
######                                                                    ######
######                       ESTADÍSTICA APLICADA                         ######
######               GRADO EN INGENIERÍA MATEMÁTICA 2020/2021             ######
######                                                                    ######
################################################################################

#               ESTE SCRIPT ESTÁ COMPLETAMENTE BASADO EN EL LIBRO            
# Brian S. Everitt, Thorsten Hothorn, A Handbook of Statistical Analyses 
# Using R      #

vignette("Ch_simple_inference",package="HSAUR")

# En este script vamos a estudiar cómo llevar a cabo análisis de tipo inferencial 
# usando R.
#
# A diferencia de la estadística descriptiva, que se orienta a la confección de 
# resúmenes adecuados de un conjunto de observaciones, el objetivo de la 
# estadística inferencial es emplear esas observaciones para obtener conocimiento 
# sobre el proceso que las ha generado. En otras palabras, se trata de obtener
# información de la o las poblaciones de interés en base a una muestra extraída 
# de esa población. Es decir, queremos utilizar la información obtenida en un 
# subconjunto de esa población, la muestra, para conocer (inferir) cómo es esa 
# población o conjunto completo. Se trata pues de intentar conocer el todo a
# través de la observación de una parte de ese todo.
#
# Las herramientas fundamentales de la estadística inferencial son los contrastes
# de hipótesis y los intervalos de confianza. La clave de la utilidad de estas 
# herramientas es que nos permiten dilucidar cuestiones sobre características
# relevantes de la población (su media, su varianza, su mediana, etc.) teniendo
# en cuenta la incertidumbre introducida por el hecho de observar solo una parte 
# de la población, esto es, teniendo en cuenta que existe una variabilidad en 
# lo que observamos a causa de la aleatoriedad en la selección de la muestra, y 
# que esta variabilidad puede afectar a la calidad de la inferencia realizada 
# desde la parte hacia el todo. 
#
# Este manejo de la incertidumbre se realiza a través del uso de distribuciones
# de probabilidad. Bajo ciertos supuestos, como el que las características de la 
# población bajo estudio son de una determinada manera, será más probable observar 
# algunas muestras que otras. Por tanto, la aparición de una muestra altamente 
# improbable bajo esos supuestos o hipótesis puede tomarse como una evidencia de
# de que la población realmente no tiene las características supuestas. 
#
# Vamos a comenzar a ilustrar estas nociones con el conjunto de datos "roomwidth".
# En este conjunto de datos, dos grupos de estudiantes de una universidad de
# Australia estiman el ancho de un aula, uno de los grupos en metros y el otro en 
# pies, poco después de introducirse oficialmente el sistema métrico decimal en
# ese país. El interés está en comparar si ambos grupos de estudiantes proveen
# una estimación similar del ancho del aula en unidades diferentes. #

install.packages("HSAUR2")
library(HSAUR2)
data("roomwidth")
?roomwidth
table(roomwidth$unit)

# De cara a investigar si la anchura estimada por el primer grupo es similar a la 
# estimada por el segundo grupo, tenemos que convertir las mediciones en pies a 
# metros. #

convert = ifelse(roomwidth$unit == "feet", 1/3.28, 1)

# Veamos un resumen de las distribuciones de medidas de cada grupo expresadas en 
# metros. #

tapply(roomwidth$width * convert, roomwidth$unit, summary)
tapply(roomwidth$width * convert, roomwidth$unit, sd)

# Parece que las estimaciones realizadas en metros son algo mayores que las 
# realizadas en pies, y que las primeras presentan casi el doble de variabilidad
# que las segundas. 
# 
# Realicemos también una representación gráfica de ambas distribuciones. Para ello,
# vamos a dividir el dispositivo gráfico en cuatro partes, organizadas en dos filas
# y dos columnas, pero de manera que la primera fila corresponda a un único 
# gráfico. # 

layout(matrix(c(1,2,1,3), nrow = 2, ncol = 2, byrow = FALSE))
boxplot(I(width * convert) ~ unit, data = roomwidth,
          ylab = "Anchura estimada (m)", xlab="",
          varwidth = TRUE, names = c("Estimación en pies (convertidos a metros)",
          "Estimación en metros"))

# Un gráfico útil para evaluar la normalidad de ambas distribuciones son los 
# llamados gráficos Q-Q o gráficos de probabilidad normal. En este se representan
# las observaciones en el eje Y contra los cuantiles teóricos de una distribución
# normal en el eje X, y se superpone una línea que representa cómo se distribuirían
# las observaciones en caso de ser perfectamente normales. Nótese que se utiliza
# una condición lógica para seleccionar las observaciones realizadas en pies, y 
# luego se aplica el operador negación o NOT, dado por el símbolo "!", para 
# invertir esta selección. #

pies = roomwidth$unit == "feet"
qqnorm(roomwidth$width[pies],
            ylab = "Anchura estimada (pies)")
qqline(roomwidth$width[pies])
qqnorm(roomwidth$width[!pies],
             ylab = "Anchura estimada (m)")
qqline(roomwidth$width[!pies])

# Los diagramas de caja indican que ambas distribuciones presentan varias 
# observaciones extremas u outliers, y que las estimaciones en metros presentan 
# una mayor asimetría y variabilidad que las estimaciones en pies, como ya habíamos
# advertido con las desviaciones típicas antes calculadas. Los gráficos de 
# probabilidad normal sugieren que ambas distribuciones se separan del patrón 
# normal. 
#
# La presencia de outliers, las diferencias en variabilidad y la aparente no 
# normalidad de los datos sugiere que es necesario ser prudentes al aplicar un 
# contraste t para comparar si ambas distribuciones presentan una media similar.
# No obstante, vamos a aplicar en primer lugar la versión estándar del contraste
# de medias.

t.test(I(width*convert)~unit, data=roomwidth, var.equal = TRUE)

# El resultado del contraste indica que existe una evidencia significativa de que
# ambos conjuntos de mediciones difieren en localización, siendo en promedio 
# las estimaciones en pies menores que las realizadas en metros por una diferencia
# de entre 4 y 0.5 metros.  
#
# El contraste anterior se realiza bajo el supuesto de normalidad de ambas 
# distribuciones y de igualdad de sus varianzas. Como hemos visto, este último 
# supuesto podría no cumplirse, lo que llevaría a realizar la comparación anterior
# mediante el contraste de Welch, que no asume varianzas iguales. Esto se implementa
# con la opción var.equal = FALSE en la función t.test() anterior. #

t.test(I(width*convert)~unit, data=roomwidth, var.equal = FALSE)

# El resultado de este contraste de Welch indica de nuevo la significatividad
# de las diferencias entre ambos conjuntos de mediciones.
# 
# Una última objeción a estas conclusiones es que el supuesto de normalidad
# de ambas distribuciones podría no estarse cumpliendo. Esto puede comprobarse
# más formalmente usando un contraste de normalidad, como el contraste de
# Shapiro-Wilk, que contrasta la hipótesis nula de que un conjunto de datos proviene
# de una distribución normal. #

tapply(roomwidth$width*convert, roomwidth$unit, shapiro.test)

# Estos contrastes rechazan la hipótesis nula de normalidad para ambas 
# distribuciones. Esto conduce a poner en duda los resultados de los contrastes t
# anteriores, y a la conveniencia de aplicar un contraste no paramétrico como 
# el de Wilcoxon. #

wilcox.test(I(width*convert)~unit, data=roomwidth, conf.int=TRUE)

# El resultado de este contraste indica de nuevo que ambas distribuciones difieren
# en localización, en este caso que sus medianas no coinciden, proporcionando  
# además el intervalo [-2.85,-0.24] para la diferencia en metros entre ambas 
# medianas (nótese que este intervalo es sensiblemente más pequeño que los 
# reportados anteriormente por los contrastes t para la diferencia de medias). #

# Veamos ahora un ejemplo en el que se tienen datos de tipo pareado. Se trata del
# conjunto de datos "waves", que registra la tensión de estiramiento 
# que sufre un sistema para generación de electricidad a partir de la energía de 
# las olas marítimas al sujetarse mediante dos diferentes métodos de anclado, uno 
# de los cuales es sensiblemente más barato que el otro. Las diferentes 
# observaciones corresponden a diferentes condiciones del estado del mar, bajo las 
# cuales se midió en el mismo sistema la tensión generada por ambos métodos de
# anclado. La cuestión de interés es si la tensión de estiramiento es la misma
# para ambos métodos de anclado. #

data("waves")

# El contraste apropiado para dilucidar esta cuestión es el contraste t para datos
# pareados. En tanto este test se basa en la normalidad de la distribución de  
# diferencias entre observaciones pareadas, conviene representar gráficamente 
# lo datos en un diagrama de cajas y un gráfico Q-Q para evaluar este supuesto.
# Aplicar el test de Shapiro-Wilk permite también dilucidar la cuestión de 
# normalidad más formalmente. #

difanclado <- waves$method1 - waves$method2
layout(matrix(1:2, ncol = 2))
boxplot(difanclado, ylab = "Diferencias",
        main = "Diagrama de cajas")
abline(h = 0, lty = 2)
qqnorm(difanclado, ylab = "Diferencias")
qqline(difanclado)

shapiro.test(difanclado)

# En este caso, no parece que tenga lugar una desviación importante de la
# normalidad, aunque el escaso número de observaciones tampoco permite garantizar
# esta cuestión. No obstante, parece perfectamente viable aplicar el contraste t
# para datos pareados. #

t.test(difanclado)

# El resultado de este contraste indica que no existe evidencia para concluir 
# que ambos métodos de anclado difieran en la tensión que generan sobre el sistema
# de generación eléctrica. Por prudencia, repitamos este contraste usando ahora 
# una metodología no paramétrica. #

wilcox.test(difanclado)

# El resultado de este test refuerza las conclusiones obtenidas mediante el anterior
# contraste t. Por tanto, parece entonces que es posible emplear el método más 
# barato sin empeorar la protección del sistema olamotriz. #

# Pasemos ahora a ilustrar un test útil para contrastar la presencia de una 
# relación lineal entre dos variables numéricas. Se usará para ello el conjunto de
# datos "water", que registra tasas medias de mortalidad por 100.000 hombres en el 
# periodo 1958-1964 junto a las concentraciones de calcio en el agua corriente en
# 61 localidades del Reino Unido. La ubicación de estas localidades dentro del país
# viene separada en dos grupos, "South" y "North", atendiendo a su latitud. Una
# cuestión de interés podría ser si las tasas de mortalidad están relacionadas con 
# la dureza del agua, y si esta relación varía con el emplazamiento de las 
# ciudades. #

data("water")

# Para visualizar la posible relación entre mortalidad y dureza del agua, vamos 
# a representar las observaciones en un diagrama de dispersión, en el que se
# añadirá una línea de ajuste de tipo regresión. Además, para estudiar las 
# distribuciones marginales de ambas variables representaremos también junto a 
# este diagrama bivariado un histograma de la dureza y un diagrama de cajas de la 
# mortalidad. 
#
# Nótese que en la llamada a layout() para especificar la estructura
# del dispositivo gráfico, se utilizarán los parámetros "widths" y "heights", 
# que permiten especificar respectivamente la anchura de las columnas del 
# dispositivo y la altura de sus filas. En particular, la primera columna será
# el doble de ancha que la segunda, y la segunda fila el doble de alta que la 
# primera. El último parámetro en layout() es "respect", que controla si se usa
# la misma unidad para estas alturas y anchuras antes especificadas. #

layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE), c(2, 1), c(1, 2), TRUE)
psymb = as.numeric(water$location)
plot(mortality ~ hardness, data = water, pch = psymb)
abline(lm(mortality ~ hardness, data = water))
legend("topright", legend = levels(water$location), pch = c(1,2), bty = "n")
hist(water$hardness)
boxplot(water$mortality)

# El diagrama de dispersión parace indicar que la mortalidad decrece al aumentar
# la dureza del agua, aunque esta relación podría variar ligeramente entre las
# ciudades del norte y del sur del país. Además, el histograma para la dureza
# parece indicar una distribución marginal asimétrica de esta variable, mientras
# la mortalidad parece tener una distribución mucho más equilibrada.
#
# Obtengamos ahora el coeficiente de correlación de Pearson de las muestras de
# mortalidad y dureza. #

cor(water$mortality,water$hardness)

# Este valor -0.655 de la correlación describe una posible relación inversa entre 
# dureza y mortalidad, al igual que se observaba en el diagrama de dispersión. 
# Contrastemos ahora que este valor de la correlación es significativamente
# diferente de 0, esto es, que la relación observada no está causada por
# la variabilidad de la muestra. #

cor.test(~ mortality + hardness, data = water)

# El resultado del test indica que existe evidencia para rechazar la hipótesis
# nula de correlación cero, lo que puede tomarse como prueba de que existe una
# relación como la observada entre mortalidad y dureza del agua. #

# Pasemos ahora a ilustrar un test para contrastar la existencia de relación
# entre dos variables categóricas. Usaremos para esto el conjunto de datos 
# "pistonrings", que contiene la tabla de contingencia con la frecuencia de fallos
# en las anillas de los pistones situados en tres localizaciones ("legs") de 
# cuatro compresores de un motor de vapor. Las variables son entonces el compresor
# y las localizaciones, y el interés se centra en estudiar si hay independencia
# entre ellas, esto es, si los fallos ocurren siguiendo el mismo patrón de 
# localización en todos los compresores. #

data("pistonrings")

# Para estudiar la independencia entre ambas variables, empleamos el contraste
# chi-cuadrado para tablas de contingencia, disponible mediante la función 
# chisq.test(). #

chisq.test(pistonrings)

# El resultado de este contraste indica que la evidencia en contra de la hipótesis
# nula de independencia no es muy elevada, aunque el p-valor de 0.068 tampoco 
# está muy alejado del nivel de significación habitual de 0.05. En cierto modo,
# podríamos decir que tampoco hay una evidencia fuerte para descartar que la
# independencia se incumpla. 
#
# Una manera de llevar el análisis un poco más allá para analizar en qué
# combinaciones de compresor y localización se alejan más las frecuencias 
# observadas de las esperadas en caso de independencia es mediante el análisis
# de los residuos estandarizados, que se obtienen como la diferencia entre 
# la frecuencia esperada y la observada partida por la frecuencia esperada.  
# 
# Podemos acceder a estos residuos estandarizados mediante el elemento 
# "residuals" del objeto que devuelve la funcion chisq.test(). #

chisq.test(pistonrings)$residuals

# Podemos visualizar estos residuos mediante la función assoc() del package
# vcd.

install.packages("vcd")
library(vcd)
assoc(pistonrings)

# Este gráfico indica que las desviaciones de la independencia más 
# pronunciadas se dan en las localizaciones centro y sur de los compresores
# 1 y 4. #

# Para concluir, ilustremos el contraste de independencia para variables
# categóricas pareadas mediante el conjunto de datos "rearrest". Estos datos
# se refieren a pares de delicuentes juveniles en el estado de Florida, 
# emparejados en 1987 mediante criterios como la edad o el número de condenas 
# anteriores, de manera que uno de los integrantes de cada par fue enviado 
# a juicio en un tribunal de menores y el otro en un tribunal de adultos. 
# A final de 1988, se registró si alguno de los jóvenes volvieron a 
# delinquir (rearrest). Cada par se clasifica entonces en base a si el 
# joven enviado al tribunal juvenil o al adulto, ambos o ninguno, fue
# procesado de nuevo. La cuestión de interés es si esta reincidencia puede
# estar relacionada con el tipo de tribunal al que fueron enviados. # 

data("rearrests")

# Para estudiar esta cuestión se aplicará el contraste de McNemar, disponible
# mediante la función mcnemar.test(). #

mcnemar.test(rearrests, correct = FALSE)

# En este caso, existe una fuerte evidencia de que el tipo de tribunal y 
# la reincidencia están relacionados. De hecho, es posible ver que los 
# jóvenes juzgados en tribunales juveniles tienen menor probabilidad de 
# reincidir que los juzgados en tribuanles adultos. 
#
# 
