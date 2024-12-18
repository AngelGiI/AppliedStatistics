################################################################################
######                                                                    ######
######                ESTAD�STICA INFERENCIAL CON R (1)                   ######
######                                                                    ######
######                       ESTAD�STICA APLICADA                         ######
######               GRADO EN INGENIER�A MATEM�TICA 2020/2021             ######
######                                                                    ######
################################################################################

#               ESTE SCRIPT EST� COMPLETAMENTE BASADO EN EL LIBRO            
# Brian S. Everitt, Thorsten Hothorn, A Handbook of Statistical Analyses 
# Using R      #

vignette("Ch_simple_inference",package="HSAUR")

# En este script vamos a estudiar c�mo llevar a cabo an�lisis de tipo inferencial 
# usando R.
#
# A diferencia de la estad�stica descriptiva, que se orienta a la confecci�n de 
# res�menes adecuados de un conjunto de observaciones, el objetivo de la 
# estad�stica inferencial es emplear esas observaciones para obtener conocimiento 
# sobre el proceso que las ha generado. En otras palabras, se trata de obtener
# informaci�n de la o las poblaciones de inter�s en base a una muestra extra�da 
# de esa poblaci�n. Es decir, queremos utilizar la informaci�n obtenida en un 
# subconjunto de esa poblaci�n, la muestra, para conocer (inferir) c�mo es esa 
# poblaci�n o conjunto completo. Se trata pues de intentar conocer el todo a
# trav�s de la observaci�n de una parte de ese todo.
#
# Las herramientas fundamentales de la estad�stica inferencial son los contrastes
# de hip�tesis y los intervalos de confianza. La clave de la utilidad de estas 
# herramientas es que nos permiten dilucidar cuestiones sobre caracter�sticas
# relevantes de la poblaci�n (su media, su varianza, su mediana, etc.) teniendo
# en cuenta la incertidumbre introducida por el hecho de observar solo una parte 
# de la poblaci�n, esto es, teniendo en cuenta que existe una variabilidad en 
# lo que observamos a causa de la aleatoriedad en la selecci�n de la muestra, y 
# que esta variabilidad puede afectar a la calidad de la inferencia realizada 
# desde la parte hacia el todo. 
#
# Este manejo de la incertidumbre se realiza a trav�s del uso de distribuciones
# de probabilidad. Bajo ciertos supuestos, como el que las caracter�sticas de la 
# poblaci�n bajo estudio son de una determinada manera, ser� m�s probable observar 
# algunas muestras que otras. Por tanto, la aparici�n de una muestra altamente 
# improbable bajo esos supuestos o hip�tesis puede tomarse como una evidencia de
# de que la poblaci�n realmente no tiene las caracter�sticas supuestas. 
#
# Vamos a comenzar a ilustrar estas nociones con el conjunto de datos "roomwidth".
# En este conjunto de datos, dos grupos de estudiantes de una universidad de
# Australia estiman el ancho de un aula, uno de los grupos en metros y el otro en 
# pies, poco despu�s de introducirse oficialmente el sistema m�trico decimal en
# ese pa�s. El inter�s est� en comparar si ambos grupos de estudiantes proveen
# una estimaci�n similar del ancho del aula en unidades diferentes. #

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
# Realicemos tambi�n una representaci�n gr�fica de ambas distribuciones. Para ello,
# vamos a dividir el dispositivo gr�fico en cuatro partes, organizadas en dos filas
# y dos columnas, pero de manera que la primera fila corresponda a un �nico 
# gr�fico. # 

layout(matrix(c(1,2,1,3), nrow = 2, ncol = 2, byrow = FALSE))
boxplot(I(width * convert) ~ unit, data = roomwidth,
          ylab = "Anchura estimada (m)", xlab="",
          varwidth = TRUE, names = c("Estimaci�n en pies (convertidos a metros)",
          "Estimaci�n en metros"))

# Un gr�fico �til para evaluar la normalidad de ambas distribuciones son los 
# llamados gr�ficos Q-Q o gr�ficos de probabilidad normal. En este se representan
# las observaciones en el eje Y contra los cuantiles te�ricos de una distribuci�n
# normal en el eje X, y se superpone una l�nea que representa c�mo se distribuir�an
# las observaciones en caso de ser perfectamente normales. N�tese que se utiliza
# una condici�n l�gica para seleccionar las observaciones realizadas en pies, y 
# luego se aplica el operador negaci�n o NOT, dado por el s�mbolo "!", para 
# invertir esta selecci�n. #

pies = roomwidth$unit == "feet"
qqnorm(roomwidth$width[pies],
            ylab = "Anchura estimada (pies)")
qqline(roomwidth$width[pies])
qqnorm(roomwidth$width[!pies],
             ylab = "Anchura estimada (m)")
qqline(roomwidth$width[!pies])

# Los diagramas de caja indican que ambas distribuciones presentan varias 
# observaciones extremas u outliers, y que las estimaciones en metros presentan 
# una mayor asimetr�a y variabilidad que las estimaciones en pies, como ya hab�amos
# advertido con las desviaciones t�picas antes calculadas. Los gr�ficos de 
# probabilidad normal sugieren que ambas distribuciones se separan del patr�n 
# normal. 
#
# La presencia de outliers, las diferencias en variabilidad y la aparente no 
# normalidad de los datos sugiere que es necesario ser prudentes al aplicar un 
# contraste t para comparar si ambas distribuciones presentan una media similar.
# No obstante, vamos a aplicar en primer lugar la versi�n est�ndar del contraste
# de medias.

t.test(I(width*convert)~unit, data=roomwidth, var.equal = TRUE)

# El resultado del contraste indica que existe una evidencia significativa de que
# ambos conjuntos de mediciones difieren en localizaci�n, siendo en promedio 
# las estimaciones en pies menores que las realizadas en metros por una diferencia
# de entre 4 y 0.5 metros.  
#
# El contraste anterior se realiza bajo el supuesto de normalidad de ambas 
# distribuciones y de igualdad de sus varianzas. Como hemos visto, este �ltimo 
# supuesto podr�a no cumplirse, lo que llevar�a a realizar la comparaci�n anterior
# mediante el contraste de Welch, que no asume varianzas iguales. Esto se implementa
# con la opci�n var.equal = FALSE en la funci�n t.test() anterior. #

t.test(I(width*convert)~unit, data=roomwidth, var.equal = FALSE)

# El resultado de este contraste de Welch indica de nuevo la significatividad
# de las diferencias entre ambos conjuntos de mediciones.
# 
# Una �ltima objeci�n a estas conclusiones es que el supuesto de normalidad
# de ambas distribuciones podr�a no estarse cumpliendo. Esto puede comprobarse
# m�s formalmente usando un contraste de normalidad, como el contraste de
# Shapiro-Wilk, que contrasta la hip�tesis nula de que un conjunto de datos proviene
# de una distribuci�n normal. #

tapply(roomwidth$width*convert, roomwidth$unit, shapiro.test)

# Estos contrastes rechazan la hip�tesis nula de normalidad para ambas 
# distribuciones. Esto conduce a poner en duda los resultados de los contrastes t
# anteriores, y a la conveniencia de aplicar un contraste no param�trico como 
# el de Wilcoxon. #

wilcox.test(I(width*convert)~unit, data=roomwidth, conf.int=TRUE)

# El resultado de este contraste indica de nuevo que ambas distribuciones difieren
# en localizaci�n, en este caso que sus medianas no coinciden, proporcionando  
# adem�s el intervalo [-2.85,-0.24] para la diferencia en metros entre ambas 
# medianas (n�tese que este intervalo es sensiblemente m�s peque�o que los 
# reportados anteriormente por los contrastes t para la diferencia de medias). #

# Veamos ahora un ejemplo en el que se tienen datos de tipo pareado. Se trata del
# conjunto de datos "waves", que registra la tensi�n de estiramiento 
# que sufre un sistema para generaci�n de electricidad a partir de la energ�a de 
# las olas mar�timas al sujetarse mediante dos diferentes m�todos de anclado, uno 
# de los cuales es sensiblemente m�s barato que el otro. Las diferentes 
# observaciones corresponden a diferentes condiciones del estado del mar, bajo las 
# cuales se midi� en el mismo sistema la tensi�n generada por ambos m�todos de
# anclado. La cuesti�n de inter�s es si la tensi�n de estiramiento es la misma
# para ambos m�todos de anclado. #

data("waves")

# El contraste apropiado para dilucidar esta cuesti�n es el contraste t para datos
# pareados. En tanto este test se basa en la normalidad de la distribuci�n de  
# diferencias entre observaciones pareadas, conviene representar gr�ficamente 
# lo datos en un diagrama de cajas y un gr�fico Q-Q para evaluar este supuesto.
# Aplicar el test de Shapiro-Wilk permite tambi�n dilucidar la cuesti�n de 
# normalidad m�s formalmente. #

difanclado <- waves$method1 - waves$method2
layout(matrix(1:2, ncol = 2))
boxplot(difanclado, ylab = "Diferencias",
        main = "Diagrama de cajas")
abline(h = 0, lty = 2)
qqnorm(difanclado, ylab = "Diferencias")
qqline(difanclado)

shapiro.test(difanclado)

# En este caso, no parece que tenga lugar una desviaci�n importante de la
# normalidad, aunque el escaso n�mero de observaciones tampoco permite garantizar
# esta cuesti�n. No obstante, parece perfectamente viable aplicar el contraste t
# para datos pareados. #

t.test(difanclado)

# El resultado de este contraste indica que no existe evidencia para concluir 
# que ambos m�todos de anclado difieran en la tensi�n que generan sobre el sistema
# de generaci�n el�ctrica. Por prudencia, repitamos este contraste usando ahora 
# una metodolog�a no param�trica. #

wilcox.test(difanclado)

# El resultado de este test refuerza las conclusiones obtenidas mediante el anterior
# contraste t. Por tanto, parece entonces que es posible emplear el m�todo m�s 
# barato sin empeorar la protecci�n del sistema olamotriz. #

# Pasemos ahora a ilustrar un test �til para contrastar la presencia de una 
# relaci�n lineal entre dos variables num�ricas. Se usar� para ello el conjunto de
# datos "water", que registra tasas medias de mortalidad por 100.000 hombres en el 
# periodo 1958-1964 junto a las concentraciones de calcio en el agua corriente en
# 61 localidades del Reino Unido. La ubicaci�n de estas localidades dentro del pa�s
# viene separada en dos grupos, "South" y "North", atendiendo a su latitud. Una
# cuesti�n de inter�s podr�a ser si las tasas de mortalidad est�n relacionadas con 
# la dureza del agua, y si esta relaci�n var�a con el emplazamiento de las 
# ciudades. #

data("water")

# Para visualizar la posible relaci�n entre mortalidad y dureza del agua, vamos 
# a representar las observaciones en un diagrama de dispersi�n, en el que se
# a�adir� una l�nea de ajuste de tipo regresi�n. Adem�s, para estudiar las 
# distribuciones marginales de ambas variables representaremos tambi�n junto a 
# este diagrama bivariado un histograma de la dureza y un diagrama de cajas de la 
# mortalidad. 
#
# N�tese que en la llamada a layout() para especificar la estructura
# del dispositivo gr�fico, se utilizar�n los par�metros "widths" y "heights", 
# que permiten especificar respectivamente la anchura de las columnas del 
# dispositivo y la altura de sus filas. En particular, la primera columna ser�
# el doble de ancha que la segunda, y la segunda fila el doble de alta que la 
# primera. El �ltimo par�metro en layout() es "respect", que controla si se usa
# la misma unidad para estas alturas y anchuras antes especificadas. #

layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE), c(2, 1), c(1, 2), TRUE)
psymb = as.numeric(water$location)
plot(mortality ~ hardness, data = water, pch = psymb)
abline(lm(mortality ~ hardness, data = water))
legend("topright", legend = levels(water$location), pch = c(1,2), bty = "n")
hist(water$hardness)
boxplot(water$mortality)

# El diagrama de dispersi�n parace indicar que la mortalidad decrece al aumentar
# la dureza del agua, aunque esta relaci�n podr�a variar ligeramente entre las
# ciudades del norte y del sur del pa�s. Adem�s, el histograma para la dureza
# parece indicar una distribuci�n marginal asim�trica de esta variable, mientras
# la mortalidad parece tener una distribuci�n mucho m�s equilibrada.
#
# Obtengamos ahora el coeficiente de correlaci�n de Pearson de las muestras de
# mortalidad y dureza. #

cor(water$mortality,water$hardness)

# Este valor -0.655 de la correlaci�n describe una posible relaci�n inversa entre 
# dureza y mortalidad, al igual que se observaba en el diagrama de dispersi�n. 
# Contrastemos ahora que este valor de la correlaci�n es significativamente
# diferente de 0, esto es, que la relaci�n observada no est� causada por
# la variabilidad de la muestra. #

cor.test(~ mortality + hardness, data = water)

# El resultado del test indica que existe evidencia para rechazar la hip�tesis
# nula de correlaci�n cero, lo que puede tomarse como prueba de que existe una
# relaci�n como la observada entre mortalidad y dureza del agua. #

# Pasemos ahora a ilustrar un test para contrastar la existencia de relaci�n
# entre dos variables categ�ricas. Usaremos para esto el conjunto de datos 
# "pistonrings", que contiene la tabla de contingencia con la frecuencia de fallos
# en las anillas de los pistones situados en tres localizaciones ("legs") de 
# cuatro compresores de un motor de vapor. Las variables son entonces el compresor
# y las localizaciones, y el inter�s se centra en estudiar si hay independencia
# entre ellas, esto es, si los fallos ocurren siguiendo el mismo patr�n de 
# localizaci�n en todos los compresores. #

data("pistonrings")

# Para estudiar la independencia entre ambas variables, empleamos el contraste
# chi-cuadrado para tablas de contingencia, disponible mediante la funci�n 
# chisq.test(). #

chisq.test(pistonrings)

# El resultado de este contraste indica que la evidencia en contra de la hip�tesis
# nula de independencia no es muy elevada, aunque el p-valor de 0.068 tampoco 
# est� muy alejado del nivel de significaci�n habitual de 0.05. En cierto modo,
# podr�amos decir que tampoco hay una evidencia fuerte para descartar que la
# independencia se incumpla. 
#
# Una manera de llevar el an�lisis un poco m�s all� para analizar en qu�
# combinaciones de compresor y localizaci�n se alejan m�s las frecuencias 
# observadas de las esperadas en caso de independencia es mediante el an�lisis
# de los residuos estandarizados, que se obtienen como la diferencia entre 
# la frecuencia esperada y la observada partida por la frecuencia esperada.  
# 
# Podemos acceder a estos residuos estandarizados mediante el elemento 
# "residuals" del objeto que devuelve la funcion chisq.test(). #

chisq.test(pistonrings)$residuals

# Podemos visualizar estos residuos mediante la funci�n assoc() del package
# vcd.

install.packages("vcd")
library(vcd)
assoc(pistonrings)

# Este gr�fico indica que las desviaciones de la independencia m�s 
# pronunciadas se dan en las localizaciones centro y sur de los compresores
# 1 y 4. #

# Para concluir, ilustremos el contraste de independencia para variables
# categ�ricas pareadas mediante el conjunto de datos "rearrest". Estos datos
# se refieren a pares de delicuentes juveniles en el estado de Florida, 
# emparejados en 1987 mediante criterios como la edad o el n�mero de condenas 
# anteriores, de manera que uno de los integrantes de cada par fue enviado 
# a juicio en un tribunal de menores y el otro en un tribunal de adultos. 
# A final de 1988, se registr� si alguno de los j�venes volvieron a 
# delinquir (rearrest). Cada par se clasifica entonces en base a si el 
# joven enviado al tribunal juvenil o al adulto, ambos o ninguno, fue
# procesado de nuevo. La cuesti�n de inter�s es si esta reincidencia puede
# estar relacionada con el tipo de tribunal al que fueron enviados. # 

data("rearrests")

# Para estudiar esta cuesti�n se aplicar� el contraste de McNemar, disponible
# mediante la funci�n mcnemar.test(). #

mcnemar.test(rearrests, correct = FALSE)

# En este caso, existe una fuerte evidencia de que el tipo de tribunal y 
# la reincidencia est�n relacionados. De hecho, es posible ver que los 
# j�venes juzgados en tribunales juveniles tienen menor probabilidad de 
# reincidir que los juzgados en tribuanles adultos. 
#
# 
