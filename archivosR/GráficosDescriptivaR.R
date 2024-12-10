################################################################################
######                                                                    ######
######                 ANÁLISIS DESCRIPTIVO GRÁFICO CON R                 ######
######                                                                    ######
######                       ESTADÍSTICA APLICADA                         ######
######               GRADO EN INGENIERÍA MATEMÁTICA 2020/2021             ######
######                                                                    ######
################################################################################

#               ESTE SCRIPT ESTÁ COMPLETAMENTE BASADO EN EL LIBRO            
# Brian S. Everitt, Thorsten Hothorn, A Handbook of Statistical Analyses 
# Using R      #

# En este script vamos a estudiar de manera más detallada el uso de  
# representaciones gráficas para el análisis descriptivo de datos. 
# 
# Se suele considerar que no hay una herramienta estadística de mayor utilidad
# que un gráfico bien escogido. De algún modo, en este contexto es válido
# el dicho de que "una imagen vale más que mil palabras". En general, el análisis
# de casi cualquier conjunto de datos debe iniciarse siempre mediante el empleo 
# de gráficos de cara a intentar entender las características generales de los 
# datos.
#
# Las ventajas de la utilización de gráficos estadísticos incluyen las siguientes:
# - En comparación con otros tipos de información, los gráficos bien elegidos y 
#   bien diseñados son más efectivos para crear interés y atraer la atención del
#   lector.
# - Las relaciones visuales que captan los gráficos suelen ser más facilmente 
#   entendidas y recordadas.
# - El uso de gráficos puede ahorrar tiempo y esfuerzo, ya que el significado
#   esencial de diversas medidas estadísticas puede ser visualizado fácilmente.
# - Los gráficos proveen una descripción global de un problema que permite una
#   comprensión más completa y equilibrada que la presentación en tablas o texto.
# - Los gráficos pueden permitir encontrar relaciones ocultas o no esperadas, y
#   estimular o al menos ayudar al pensamiento analítico y la investigación.
# 
# No obstante, también hay que tener presente que, aunque los humanos somos muy
# capaces a la hora de discernir visualmente patrones existentes en los datos, 
# también somos muy capaces de imaginarlos cuando no están presentes.
# 
# En cualquier caso, hoy en día los ordenadores facilitan enormemente la 
# realización de muy distintos tipos de gráficos, por lo que la cuestión ya no es 
# si debemos realizar gráficos sino qué gráficos debemos realizar.
#
# Para comenzar este estudio sobre la utilidad de los gráficos estadísticos 
# utilizaremos el conjunto de datos "USmelanoma". Este presenta tasas de mortalidad
# debida al melanoma maligno de la piel en varones blancos en los estados 
# continentales de EEUU en el periodo 1950-1969. 
#
# Carguemos los datos, que se encuentran disponibles en el paquete HSAUR2 de R. #

install.packages("HSAUR2")
library(HSAUR2)
data("USmelanoma")

# Podemos ver la descripción de este conjunto de datos en la ayuda: #

?USmelanoma

# En concreto, este conjunto de datos contiene las siguientes variables:
# - mortality: número de muertes por melanoma por cada millón de habitantes. 
# - latitude: latitud del centro geográfico de cada estado.
# - longitude: longitud del centro geográfico de cada estado.
# - ocean: una variable binaria indicando contigüidad del estado al océano.

# Algunas cuestiones de interés en relación a estos datos son las siguientes:
# - ¿Hay diferencias entre las tasas de mortalidad de los estados costeros y no
#   costeros?
# - ¿Afecta la situación geográfica (longitud y latitud) a las tasas de 
#   mortalidad? 
#
# Comencemos el estudio de estos datos obteniendo un histograma y un gráfico de 
# cajas de todas las tasas de mortalidad. Para esto, debemos usar las funciones
# hist() y boxplot() que se introdujeron en clases anteriores. Además,
# vamos a representar ambos gráficos conjuntamente, en una misma figura. Para ello,
# conviene fijar el mismo rango en el eje x de ambos gráficos, lo que se hará más 
# tarde con la opción "xlim" o "ylim" en las funciones correspondientes. El rango
# a usar podemos especificarlo como sigue. #

rangox=range(USmelanoma$mortality) * c(0.9,1.1)
rangox

# Una vez definido el rango, debemos especificar que la figura a crear contendrá
# dos gráficos. Para ello usamos la función layout(), que indica la división
# de la figura en regiones. A continuación, se usa la función boxplot() para
# crear el gráfico de caja, y la función hist() para el histograma. Usaremos
# diversas opciones de estas funciones para especificar el rango de los ejes,
# la aparición de títulos, etc. El comando axis() se emplea para especificar 
# que se grafique el eje x en la zona inferior de la figura, ya que en la función
# hist() se ha especificado que no se representen los ejes. #

layout(matrix(1:2,nrow=2))
boxplot(USmelanoma$mortality, ylim=rangox, horizontal = TRUE, xlab="Mortalidad")
hist(USmelanoma$mortality, xlim = rangox, xlab = "", main = "", axes = FALSE,
     ylab = "")
axis(1)

# Estos gráficos indican una cierta asimetría de la distribución de las tasas de 
# mortalidad. Observar esta distribución de todas las tasas de mortalidad es un
# primer paso útil, pero para profundizar en las cuestiones de interés es preciso
# comparar las tasas de mortalidad en estados costeros y no costeros, por ejemplo. 
# Esto nos lleva a producir una figura con dos gráficos de cajas o dos histogramas
# (uno para cada tipo de estado). 
# 
# Comenzando por los gráficos de cajas, para los que es posible especificar que
# se representen de manera paralela en un mismo gráfico utilizando la notación
# de fórmulas que se introdujo en clases anteriores dentro de la función 
# boxplot(). De esta manera, podemos representar la distribución condicional 
# de las tasas de mortalidad (en general, de una variable numérica) en los
# grupos determinados por la variable "coast" (en general una variable categórica).
# El código para crear esta figura es el siguiente. #

layout(matrix(1))
boxplot(mortality ~ ocean, data = USmelanoma, xlab = "Estados costeros",
     ylab = "Mortalidad")
plot(mortality ~ ocean, data = USmelanoma, xlab = "Estados costeros",
     ylab = "Mortalidad")

# Estos gráficos dan la impresión de que las tasas de mortalidad debida al  
# melanoma se incrementan en los estados de ambas costas de EEUU en comparación
# a las de los estados interiores.
# 
# Los correspondientes histogramas producen una impresión similar. Para producirlos
# es necesario separar primero los datos de los estados costeros e interiores
# en dos data frames separados. #

ecosta=subset(USmelanoma,ocean=="yes")
eint=subset(USmelanoma,ocean=="no")
layout(matrix(1:2,nrow=2))
hist(ecosta$mortality, xlim = rangox, xlab = "Estados costeros", 
     main = "Mortalidad", axes = FALSE, ylab = "")
axis(1)
hist(eint$mortality, xlim = rangox, xlab = "Estados interiores", 
     main = "", axes = FALSE, ylab = "")
axis(1)

# Aunque estos dos histogramas reflejan de manera bastante adecuada la densidad de
# la distribución de la mortalidad por melanoma condicionada por la cercanía
# a la costa, es importante saber que en general los histogramas pueden dar lugar
# a problemas por su dependencia del número de intervalos que se representan.
#
# La mejor alternativa es usar estimaciones más formales de la función de densidad 
# de las variables condicionadas respectivas. Esto se puede realizar con la 
# función density(), cuyo resultado se envía al procedimiento plot(). Esto lleva
# a cabo el gráfico de la estimación del primer grupo de datos (estados costeros),
# y para añadir en este gráfico la estimación de densidad del segundo grupo 
# (estados interiores) usamos la función lines(). Conviene notar el uso del 
# parámetro lty para especificar el tipo de línea con que se representa la 
# estimación de cada grupo. Además, es posble añadir una leyenda al gráfico 
# usando la función legend(). #

layout(matrix(1))
plot(density(ecosta$mortality), lty = 1, xlim = rangox, main = "", 
     ylim =c(0,0.02))
lines(density(eint$mortality), lty = 2)
legend("topleft", lty=1:2, legend = c("Estados costeros","Estados interiores"),
       bty="n")

# De nuevo, estas estimaciones de densidad dan lugar a la misma impresión de que
# la mortalidad por melanoma aumenta en los estados costeros en comparación con
# los estados interiores.
#
# Pasemos ahora a estudiar cómo se relaciona la tasa de mortalidad por melanoma 
# con la localización geográfica de los diferentes estados dada en términos de la 
# latitud y longitud de su punto central. Nótese que esto conlleva estudiar  
# la relación entre dos variables numéricas. Para este tipo de relaciones, el 
# gráfico más indicado es el diagrama de dispersión 
#
# El siguiente código genera los diagramas de dispersión de mortalidad vs.
# latitud y vs. longitud. Nótese que ahora la función layout() se usa para
# dividir la figura a crear en dos columnas, en las que se representarán los 
# scatterplots usando la función plot() con la correspondiente fórmula. #

layout(matrix(1:2,ncol=2))
plot(mortality ~ latitude, data=USmelanoma)
plot(mortality ~ longitude, data=USmelanoma)

# Claramente, la tasa de mortalidad parece relacionarse solo con la latitud. Para
# investigar esta relación en términos de la proximidad al mar, a continuación
# podríamos realizar un diagrama de dispersión de la mortalidad vs. latitud para 
# cada conjunto de estados (costeros e interiores). Otra opción sin embargo es 
# hacer un único scatter utilizando diferentes símbolos para cada tipo de estados.
#
# Esto se puede conseguir a través del parámetro "pch" de la función plot(),
# que admite que se le suministre un vector indicando el tipo de símbolo a usar 
# para cada una de las observaciones que se representarán. Una manera sencilla
# de llevar a cabo esto es convertir la variable de tipo factor "ocean" en una  
# variable entera, con valor 1 para los estados interiores y valor 2 para los 
# costeros. Para ello, se puede usar la familia de funciones "as", que fuerzan  
# a que una variable se convierta a una clase determinada. En este caso, usaremos
# la función as.integer(). #

layout(matrix(1))
plot(mortality ~ latitude, data=USmelanoma, pch=as.integer(USmelanoma$ocean))
legend("topright", legend = c("Estados interiores", "Estados costeros"),
       pch=1:2, bty="n")

# Este diagrama de dispersión parece indicar que las menores tasas de mortalidad
# se dan en los estados norteños interiores. En general, para una misma latitud 
# los estados costeros muestran una mayor tasa de mortalidad que los interiores. 
# De hecho, los estados costeros de la costa sur de EEUU, con latitud menor a 
# 34 grados, son los que presentan una mayor tasa de mortalidad. #

subset(USmelanoma, latitude < 34)

# En el ejemplo anterior, la variable de interés o respuesta, la tasa de 
# mortalidad, es de naturaleza continua. Pasemos ahora a estudiar el caso de una
# variable respuesta categórica con otro ejemplo. 
# 
# El conjunto de datos "CHFLS" contiene los resultados de un estudio realizado 
# a principios de siglo en 60 localidades chinas seleccionadas para representar
# todo el abanico geográfico y socioeconómico de la China actual. En particular,
# este conjunto de datos contiene las respuestas de las mujeres con pareja
# masculina sin información missing, lo que conduce a una muestra de 1534 mujeres 
# con las variables que es posible consultar en la ayuda. #

?CHFLS
data(CHFLS)

xtabs(~R_happy, data=CHFLS)
barplot(xtabs(~R_happy, data=CHFLS))

# Vamos a centrarnos en primer lugar en la percepción del estado de salud físico
# y mental, que se correspondían con las siguientes preguntas del cuestionario:
#
# 1) De modo general, ¿considera que su estado de salud es malo, no muy bueno, 
#    regular, bueno o excelente? (R_health)
# 2) De modo general, ¿cuán feliz ha sido en los últimos 12 meses? (R_happy)  
# 
# Ambas variables son factores con niveles ordenados, esto es, con naturaleza
# ordinal. Podemos estudiar la distribución de este tipo de variables  
# mediante una tabla de frecuencias o un diagrama de barras (barplot). #

table(CHFLS$R_health)
xtabs(~R_health, data=CHFLS)
barplot(xtabs(~R_health, data=CHFLS))

table(CHFLS$R_happy)
xtabs(~R_happy, data=CHFLS)
barplot(xtabs(~R_happy, data=CHFLS))

# Una herramienta para visualizar la relación entre 2 variables categóricas  
# como las anteriores viene dado por el gráfico de columnas. Antes de ver
# este gráfico, hagamos una tabla de frecuencia bidimensional, más 
# habitualmente conocida como tabla de contingencia, de las variables 
# R_happy y R_health. #

xtabs(~ R_happy + R_health, data = CHFLS)

# Observando esta tabla ya es posible intuir que las categorías de R_happy
# no ocurren con la misma frecuencia relativa en todas los niveles de 
# R_health. Esto es, el nivel percibido de felicidad parece variar de
# algún modo con el nivel percibido de salud. Una representación gráfica
# como el diagrama de columnas facilita el comprobar esta impresión. #  

plot(R_happy ~ R_health, data=CHFLS)

# Este gráfico consiste en una colección de rectángulos que representan  
# las celdas de la tabla de contingencia anterior. El área de cada
# rectángulo es proporcional al número de observaciones o frecuencia de
# la celda. 
# 
# Así, por ejemplo, el rectángulo de la esquina inferior 
# derecha del gráfico está asociado a las 150 mujeres muy felices y con
# un estado de salud excelente. La anchura de toda la columna de la derecha 
# se corresponde con la frecuencia de mujeres con estado de salud excelente.
# La longitud del citado rectángulo de la esquina inferior derecha a su vez 
# se corresponde con la frecuencia condicional de mujeres muy felices dado
# que su estado de salud es excelente. Al multiplicar estas dos cantidades 
# se obtiene el área del rectángulo referido, que como hemos dicho se 
# corresponde con la frecuencia de mujeres muy felices y con salud excelente.
# Moviéndonos de izquierda a derecha desde salud mala a excelente podemos ver 
# que la frecuencia condicional de mujeres muy felices aumenta con el estado  
# de salud, mientras que la frecuencia condicional de mujeres muy infelices o
# no muy felices decrece. 
# 
# De cara a estudiar la asociación de una variable categórica y una variable
# continua, en el ejemplo anterior vimos que se pueden emplear gráficos de 
# caja paralelos cuando entendemos que la variable de interes o respuesta es
# la variable continua y la categórica toma el papel de variable independiente. 
# Pero si queremos que la variable categórica, por ejemplo el nivel de 
# felicidad percibida, sea la respuesta y estudiar cómo varía en función de 
# una variable continua, por ejemplo los ingresos económicos, estos gráficos 
# de caja paralelos nos informarían sobre la distribución condicionada de la 
# variable continua dada la categórica, cuando lo que nos interesa en realidad
# es la distribución condicionada inversa, esto es, de los niveles de felicidad
# dados los ingresos.
# 
# Así pues, un gráfico útil para estudiar esta relación entre una respuesta
# categórica y una variable explicativa continua es el llamado columnograma 
# (spinogram), para el que primero se ha de categorizar la variable continua,
# y en cada una de estas categorías se representa mediante columnas la
# distribución condicionada de la variable categórica, de manera similar al 
# diagrama de columnas (spineplot) anterior. #

layout(matrix(1))
plot(R_happy ~ log(R_income+1),data=CHFLS)

# Al estudiar la dependencia del nivel de felicidad percibida respecto de los
# ingresos, este gráfico aporta la impresión de que la proporción de muejeres
# infelices o no muy felices decrece al aumentar los ingresos (que se han 
# transformado mediante el logaritmo por su distribución asimétrica), mientras
# que la proporción de mujeres muy felices parece ser más o menos constante y no
# depender del nivel de ingresos.
#
# Un gráfico similar pero que no categoriza la variable continua es el llamado
# gráfico de densidad condicional, en el que se realiza una estimación de la 
# densidad de la distribución condicional de la variable categórica para cada
# posible valor de la variable explicativa continua. #

cdplot(R_happy ~ log(R_income+1),data=CHFLS)

# Una última cuestión de interés puede ser la de estudiar la relación entre   
# los ingresos de las mujeres y los de sus parejas, atendiendo a la vez 
# al nivel de estudios de las mujeres. Esto es, queremos estudiar la relación 
# entre dos variables continuas (ingresos de la mujer y de su pareja) 
# condicionando por los diferentes niveles de estudio de las mujeres.
# 
# En tanto la relación que nos interesa se da entre dos variables continuas,
# el gráfico apropiado será el diagrama de dispersión. Pero al condicionar
# por una variable categórica, hay que obtener un scatter por cada nivel
# de esa variable. Esto se puede llevar a cabo mediante la función xyplot()
# del package "lattice", que debemos instalar y cargar antes de 
# poder ejectuar xyplot(). #

install.packages("lattice")
library("lattice")
xyplot(log(A_income+0.5) ~ log(R_income+0.5) | R_edu, data=CHFLS)

# Antes de comentar este resultado, es conveniente tener en cuenta que, a pesar 
# de ser continuas, las variables de ingresos R_income y A_income presentan  
# numerosos valores repetidos, ya que en realidad para muchas observaciones
# estas variables fueron imputadas a grosso modo a partir de otras respuestas.  
# Por esto, es muy probable que haya numerosos casos en que el par de ingresos 
# sean exactamente los mismos, lo que conduce a puntos solapados en el diagrama
# de dispersión. Este solapamiento a menudo impide observar correctamente la
# densidad de observaciones en algunas zonas del gráfico.
# 
# Una manera de resolver este problema es "alterar" (jitter) un poco los datos,
# esto es, añadirle una pequeña cantidad de ruido de manera que las observaciones
# no se desplacen de manera significativa, pero sí lo suficiente como para evitar 
# el solapamiento referido. Esto se puede realiza con la función jitter(). #

xyplot(jitter(log(A_income+0.5)) ~ jitter(log(R_income+0.5)) | R_edu, data=CHFLS)

# Este gráfico revela algunas cuestiones interesantes. Por ejemplo, parecen darse
# cuatro situaciones entre los pares de ingresos, que se corresponden con grupos 
# de observaciones en los diagramas de dispersión:
#
# 1) Ambos miembros de las pareja no tienen ingresos;
# 2) El compañero no tiene ingresos; 
# 3) La mujer no tiene ingresos;
# 4) Ambos tienen ingresos.
# 
# En algunos casos, también parece apreciarse que diversas observaciones caen en
# una línea recta con pendiente 1, aunque esto probablemente sea un efecto de la
# imputación que se ha comentado antes. 
# 
# Nótese que para parejas en que la mujer tiene título universitario, el ingreso
# de ambos miembros de la pareja tiende a ser relativamente alto, excepto por un
# par de casos en el que el compañero no tiene ingresos. 
# 
# Hay algunos casos de ex-alumnas universitarias que viven en parejas en que solo
# el hombre tiene ingresos, y para el resto de parejas en este caso se aprecia una
# muy ligera correlación positiva entre los ingresos de ambos miembros.
#
# Para mujeres de niveles educativos más bajos se dan las cuatro situaciones
# mencionadas, con una proporción algo mayor de parejas en que solo el hombre 
# tiene ingresos frente a parejas en que solo la mujer los tiene. En estos casos,
# si se ignoran las parejas en la línea recta, apenas parece haber correlación
# entre los dos niveles de ingresos. #
