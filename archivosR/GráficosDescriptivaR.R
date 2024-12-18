################################################################################
######                                                                    ######
######                 AN�LISIS DESCRIPTIVO GR�FICO CON R                 ######
######                                                                    ######
######                       ESTAD�STICA APLICADA                         ######
######               GRADO EN INGENIER�A MATEM�TICA 2020/2021             ######
######                                                                    ######
################################################################################

#               ESTE SCRIPT EST� COMPLETAMENTE BASADO EN EL LIBRO            
# Brian S. Everitt, Thorsten Hothorn, A Handbook of Statistical Analyses 
# Using R      #

# En este script vamos a estudiar de manera m�s detallada el uso de  
# representaciones gr�ficas para el an�lisis descriptivo de datos. 
# 
# Se suele considerar que no hay una herramienta estad�stica de mayor utilidad
# que un gr�fico bien escogido. De alg�n modo, en este contexto es v�lido
# el dicho de que "una imagen vale m�s que mil palabras". En general, el an�lisis
# de casi cualquier conjunto de datos debe iniciarse siempre mediante el empleo 
# de gr�ficos de cara a intentar entender las caracter�sticas generales de los 
# datos.
#
# Las ventajas de la utilizaci�n de gr�ficos estad�sticos incluyen las siguientes:
# - En comparaci�n con otros tipos de informaci�n, los gr�ficos bien elegidos y 
#   bien dise�ados son m�s efectivos para crear inter�s y atraer la atenci�n del
#   lector.
# - Las relaciones visuales que captan los gr�ficos suelen ser m�s facilmente 
#   entendidas y recordadas.
# - El uso de gr�ficos puede ahorrar tiempo y esfuerzo, ya que el significado
#   esencial de diversas medidas estad�sticas puede ser visualizado f�cilmente.
# - Los gr�ficos proveen una descripci�n global de un problema que permite una
#   comprensi�n m�s completa y equilibrada que la presentaci�n en tablas o texto.
# - Los gr�ficos pueden permitir encontrar relaciones ocultas o no esperadas, y
#   estimular o al menos ayudar al pensamiento anal�tico y la investigaci�n.
# 
# No obstante, tambi�n hay que tener presente que, aunque los humanos somos muy
# capaces a la hora de discernir visualmente patrones existentes en los datos, 
# tambi�n somos muy capaces de imaginarlos cuando no est�n presentes.
# 
# En cualquier caso, hoy en d�a los ordenadores facilitan enormemente la 
# realizaci�n de muy distintos tipos de gr�ficos, por lo que la cuesti�n ya no es 
# si debemos realizar gr�ficos sino qu� gr�ficos debemos realizar.
#
# Para comenzar este estudio sobre la utilidad de los gr�ficos estad�sticos 
# utilizaremos el conjunto de datos "USmelanoma". Este presenta tasas de mortalidad
# debida al melanoma maligno de la piel en varones blancos en los estados 
# continentales de EEUU en el periodo 1950-1969. 
#
# Carguemos los datos, que se encuentran disponibles en el paquete HSAUR2 de R. #

install.packages("HSAUR2")
library(HSAUR2)
data("USmelanoma")

# Podemos ver la descripci�n de este conjunto de datos en la ayuda: #

?USmelanoma

# En concreto, este conjunto de datos contiene las siguientes variables:
# - mortality: n�mero de muertes por melanoma por cada mill�n de habitantes. 
# - latitude: latitud del centro geogr�fico de cada estado.
# - longitude: longitud del centro geogr�fico de cada estado.
# - ocean: una variable binaria indicando contig�idad del estado al oc�ano.

# Algunas cuestiones de inter�s en relaci�n a estos datos son las siguientes:
# - �Hay diferencias entre las tasas de mortalidad de los estados costeros y no
#   costeros?
# - �Afecta la situaci�n geogr�fica (longitud y latitud) a las tasas de 
#   mortalidad? 
#
# Comencemos el estudio de estos datos obteniendo un histograma y un gr�fico de 
# cajas de todas las tasas de mortalidad. Para esto, debemos usar las funciones
# hist() y boxplot() que se introdujeron en clases anteriores. Adem�s,
# vamos a representar ambos gr�ficos conjuntamente, en una misma figura. Para ello,
# conviene fijar el mismo rango en el eje x de ambos gr�ficos, lo que se har� m�s 
# tarde con la opci�n "xlim" o "ylim" en las funciones correspondientes. El rango
# a usar podemos especificarlo como sigue. #

rangox=range(USmelanoma$mortality) * c(0.9,1.1)
rangox

# Una vez definido el rango, debemos especificar que la figura a crear contendr�
# dos gr�ficos. Para ello usamos la funci�n layout(), que indica la divisi�n
# de la figura en regiones. A continuaci�n, se usa la funci�n boxplot() para
# crear el gr�fico de caja, y la funci�n hist() para el histograma. Usaremos
# diversas opciones de estas funciones para especificar el rango de los ejes,
# la aparici�n de t�tulos, etc. El comando axis() se emplea para especificar 
# que se grafique el eje x en la zona inferior de la figura, ya que en la funci�n
# hist() se ha especificado que no se representen los ejes. #

layout(matrix(1:2,nrow=2))
boxplot(USmelanoma$mortality, ylim=rangox, horizontal = TRUE, xlab="Mortalidad")
hist(USmelanoma$mortality, xlim = rangox, xlab = "", main = "", axes = FALSE,
     ylab = "")
axis(1)

# Estos gr�ficos indican una cierta asimetr�a de la distribuci�n de las tasas de 
# mortalidad. Observar esta distribuci�n de todas las tasas de mortalidad es un
# primer paso �til, pero para profundizar en las cuestiones de inter�s es preciso
# comparar las tasas de mortalidad en estados costeros y no costeros, por ejemplo. 
# Esto nos lleva a producir una figura con dos gr�ficos de cajas o dos histogramas
# (uno para cada tipo de estado). 
# 
# Comenzando por los gr�ficos de cajas, para los que es posible especificar que
# se representen de manera paralela en un mismo gr�fico utilizando la notaci�n
# de f�rmulas que se introdujo en clases anteriores dentro de la funci�n 
# boxplot(). De esta manera, podemos representar la distribuci�n condicional 
# de las tasas de mortalidad (en general, de una variable num�rica) en los
# grupos determinados por la variable "coast" (en general una variable categ�rica).
# El c�digo para crear esta figura es el siguiente. #

layout(matrix(1))
boxplot(mortality ~ ocean, data = USmelanoma, xlab = "Estados costeros",
     ylab = "Mortalidad")
plot(mortality ~ ocean, data = USmelanoma, xlab = "Estados costeros",
     ylab = "Mortalidad")

# Estos gr�ficos dan la impresi�n de que las tasas de mortalidad debida al  
# melanoma se incrementan en los estados de ambas costas de EEUU en comparaci�n
# a las de los estados interiores.
# 
# Los correspondientes histogramas producen una impresi�n similar. Para producirlos
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
# la distribuci�n de la mortalidad por melanoma condicionada por la cercan�a
# a la costa, es importante saber que en general los histogramas pueden dar lugar
# a problemas por su dependencia del n�mero de intervalos que se representan.
#
# La mejor alternativa es usar estimaciones m�s formales de la funci�n de densidad 
# de las variables condicionadas respectivas. Esto se puede realizar con la 
# funci�n density(), cuyo resultado se env�a al procedimiento plot(). Esto lleva
# a cabo el gr�fico de la estimaci�n del primer grupo de datos (estados costeros),
# y para a�adir en este gr�fico la estimaci�n de densidad del segundo grupo 
# (estados interiores) usamos la funci�n lines(). Conviene notar el uso del 
# par�metro lty para especificar el tipo de l�nea con que se representa la 
# estimaci�n de cada grupo. Adem�s, es posble a�adir una leyenda al gr�fico 
# usando la funci�n legend(). #

layout(matrix(1))
plot(density(ecosta$mortality), lty = 1, xlim = rangox, main = "", 
     ylim =c(0,0.02))
lines(density(eint$mortality), lty = 2)
legend("topleft", lty=1:2, legend = c("Estados costeros","Estados interiores"),
       bty="n")

# De nuevo, estas estimaciones de densidad dan lugar a la misma impresi�n de que
# la mortalidad por melanoma aumenta en los estados costeros en comparaci�n con
# los estados interiores.
#
# Pasemos ahora a estudiar c�mo se relaciona la tasa de mortalidad por melanoma 
# con la localizaci�n geogr�fica de los diferentes estados dada en t�rminos de la 
# latitud y longitud de su punto central. N�tese que esto conlleva estudiar  
# la relaci�n entre dos variables num�ricas. Para este tipo de relaciones, el 
# gr�fico m�s indicado es el diagrama de dispersi�n 
#
# El siguiente c�digo genera los diagramas de dispersi�n de mortalidad vs.
# latitud y vs. longitud. N�tese que ahora la funci�n layout() se usa para
# dividir la figura a crear en dos columnas, en las que se representar�n los 
# scatterplots usando la funci�n plot() con la correspondiente f�rmula. #

layout(matrix(1:2,ncol=2))
plot(mortality ~ latitude, data=USmelanoma)
plot(mortality ~ longitude, data=USmelanoma)

# Claramente, la tasa de mortalidad parece relacionarse solo con la latitud. Para
# investigar esta relaci�n en t�rminos de la proximidad al mar, a continuaci�n
# podr�amos realizar un diagrama de dispersi�n de la mortalidad vs. latitud para 
# cada conjunto de estados (costeros e interiores). Otra opci�n sin embargo es 
# hacer un �nico scatter utilizando diferentes s�mbolos para cada tipo de estados.
#
# Esto se puede conseguir a trav�s del par�metro "pch" de la funci�n plot(),
# que admite que se le suministre un vector indicando el tipo de s�mbolo a usar 
# para cada una de las observaciones que se representar�n. Una manera sencilla
# de llevar a cabo esto es convertir la variable de tipo factor "ocean" en una  
# variable entera, con valor 1 para los estados interiores y valor 2 para los 
# costeros. Para ello, se puede usar la familia de funciones "as", que fuerzan  
# a que una variable se convierta a una clase determinada. En este caso, usaremos
# la funci�n as.integer(). #

layout(matrix(1))
plot(mortality ~ latitude, data=USmelanoma, pch=as.integer(USmelanoma$ocean))
legend("topright", legend = c("Estados interiores", "Estados costeros"),
       pch=1:2, bty="n")

# Este diagrama de dispersi�n parece indicar que las menores tasas de mortalidad
# se dan en los estados norte�os interiores. En general, para una misma latitud 
# los estados costeros muestran una mayor tasa de mortalidad que los interiores. 
# De hecho, los estados costeros de la costa sur de EEUU, con latitud menor a 
# 34 grados, son los que presentan una mayor tasa de mortalidad. #

subset(USmelanoma, latitude < 34)

# En el ejemplo anterior, la variable de inter�s o respuesta, la tasa de 
# mortalidad, es de naturaleza continua. Pasemos ahora a estudiar el caso de una
# variable respuesta categ�rica con otro ejemplo. 
# 
# El conjunto de datos "CHFLS" contiene los resultados de un estudio realizado 
# a principios de siglo en 60 localidades chinas seleccionadas para representar
# todo el abanico geogr�fico y socioecon�mico de la China actual. En particular,
# este conjunto de datos contiene las respuestas de las mujeres con pareja
# masculina sin informaci�n missing, lo que conduce a una muestra de 1534 mujeres 
# con las variables que es posible consultar en la ayuda. #

?CHFLS
data(CHFLS)

xtabs(~R_happy, data=CHFLS)
barplot(xtabs(~R_happy, data=CHFLS))

# Vamos a centrarnos en primer lugar en la percepci�n del estado de salud f�sico
# y mental, que se correspond�an con las siguientes preguntas del cuestionario:
#
# 1) De modo general, �considera que su estado de salud es malo, no muy bueno, 
#    regular, bueno o excelente? (R_health)
# 2) De modo general, �cu�n feliz ha sido en los �ltimos 12 meses? (R_happy)  
# 
# Ambas variables son factores con niveles ordenados, esto es, con naturaleza
# ordinal. Podemos estudiar la distribuci�n de este tipo de variables  
# mediante una tabla de frecuencias o un diagrama de barras (barplot). #

table(CHFLS$R_health)
xtabs(~R_health, data=CHFLS)
barplot(xtabs(~R_health, data=CHFLS))

table(CHFLS$R_happy)
xtabs(~R_happy, data=CHFLS)
barplot(xtabs(~R_happy, data=CHFLS))

# Una herramienta para visualizar la relaci�n entre 2 variables categ�ricas  
# como las anteriores viene dado por el gr�fico de columnas. Antes de ver
# este gr�fico, hagamos una tabla de frecuencia bidimensional, m�s 
# habitualmente conocida como tabla de contingencia, de las variables 
# R_happy y R_health. #

xtabs(~ R_happy + R_health, data = CHFLS)

# Observando esta tabla ya es posible intuir que las categor�as de R_happy
# no ocurren con la misma frecuencia relativa en todas los niveles de 
# R_health. Esto es, el nivel percibido de felicidad parece variar de
# alg�n modo con el nivel percibido de salud. Una representaci�n gr�fica
# como el diagrama de columnas facilita el comprobar esta impresi�n. #  

plot(R_happy ~ R_health, data=CHFLS)

# Este gr�fico consiste en una colecci�n de rect�ngulos que representan  
# las celdas de la tabla de contingencia anterior. El �rea de cada
# rect�ngulo es proporcional al n�mero de observaciones o frecuencia de
# la celda. 
# 
# As�, por ejemplo, el rect�ngulo de la esquina inferior 
# derecha del gr�fico est� asociado a las 150 mujeres muy felices y con
# un estado de salud excelente. La anchura de toda la columna de la derecha 
# se corresponde con la frecuencia de mujeres con estado de salud excelente.
# La longitud del citado rect�ngulo de la esquina inferior derecha a su vez 
# se corresponde con la frecuencia condicional de mujeres muy felices dado
# que su estado de salud es excelente. Al multiplicar estas dos cantidades 
# se obtiene el �rea del rect�ngulo referido, que como hemos dicho se 
# corresponde con la frecuencia de mujeres muy felices y con salud excelente.
# Movi�ndonos de izquierda a derecha desde salud mala a excelente podemos ver 
# que la frecuencia condicional de mujeres muy felices aumenta con el estado  
# de salud, mientras que la frecuencia condicional de mujeres muy infelices o
# no muy felices decrece. 
# 
# De cara a estudiar la asociaci�n de una variable categ�rica y una variable
# continua, en el ejemplo anterior vimos que se pueden emplear gr�ficos de 
# caja paralelos cuando entendemos que la variable de interes o respuesta es
# la variable continua y la categ�rica toma el papel de variable independiente. 
# Pero si queremos que la variable categ�rica, por ejemplo el nivel de 
# felicidad percibida, sea la respuesta y estudiar c�mo var�a en funci�n de 
# una variable continua, por ejemplo los ingresos econ�micos, estos gr�ficos 
# de caja paralelos nos informar�an sobre la distribuci�n condicionada de la 
# variable continua dada la categ�rica, cuando lo que nos interesa en realidad
# es la distribuci�n condicionada inversa, esto es, de los niveles de felicidad
# dados los ingresos.
# 
# As� pues, un gr�fico �til para estudiar esta relaci�n entre una respuesta
# categ�rica y una variable explicativa continua es el llamado columnograma 
# (spinogram), para el que primero se ha de categorizar la variable continua,
# y en cada una de estas categor�as se representa mediante columnas la
# distribuci�n condicionada de la variable categ�rica, de manera similar al 
# diagrama de columnas (spineplot) anterior. #

layout(matrix(1))
plot(R_happy ~ log(R_income+1),data=CHFLS)

# Al estudiar la dependencia del nivel de felicidad percibida respecto de los
# ingresos, este gr�fico aporta la impresi�n de que la proporci�n de muejeres
# infelices o no muy felices decrece al aumentar los ingresos (que se han 
# transformado mediante el logaritmo por su distribuci�n asim�trica), mientras
# que la proporci�n de mujeres muy felices parece ser m�s o menos constante y no
# depender del nivel de ingresos.
#
# Un gr�fico similar pero que no categoriza la variable continua es el llamado
# gr�fico de densidad condicional, en el que se realiza una estimaci�n de la 
# densidad de la distribuci�n condicional de la variable categ�rica para cada
# posible valor de la variable explicativa continua. #

cdplot(R_happy ~ log(R_income+1),data=CHFLS)

# Una �ltima cuesti�n de inter�s puede ser la de estudiar la relaci�n entre   
# los ingresos de las mujeres y los de sus parejas, atendiendo a la vez 
# al nivel de estudios de las mujeres. Esto es, queremos estudiar la relaci�n 
# entre dos variables continuas (ingresos de la mujer y de su pareja) 
# condicionando por los diferentes niveles de estudio de las mujeres.
# 
# En tanto la relaci�n que nos interesa se da entre dos variables continuas,
# el gr�fico apropiado ser� el diagrama de dispersi�n. Pero al condicionar
# por una variable categ�rica, hay que obtener un scatter por cada nivel
# de esa variable. Esto se puede llevar a cabo mediante la funci�n xyplot()
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
# de dispersi�n. Este solapamiento a menudo impide observar correctamente la
# densidad de observaciones en algunas zonas del gr�fico.
# 
# Una manera de resolver este problema es "alterar" (jitter) un poco los datos,
# esto es, a�adirle una peque�a cantidad de ruido de manera que las observaciones
# no se desplacen de manera significativa, pero s� lo suficiente como para evitar 
# el solapamiento referido. Esto se puede realiza con la funci�n jitter(). #

xyplot(jitter(log(A_income+0.5)) ~ jitter(log(R_income+0.5)) | R_edu, data=CHFLS)

# Este gr�fico revela algunas cuestiones interesantes. Por ejemplo, parecen darse
# cuatro situaciones entre los pares de ingresos, que se corresponden con grupos 
# de observaciones en los diagramas de dispersi�n:
#
# 1) Ambos miembros de las pareja no tienen ingresos;
# 2) El compa�ero no tiene ingresos; 
# 3) La mujer no tiene ingresos;
# 4) Ambos tienen ingresos.
# 
# En algunos casos, tambi�n parece apreciarse que diversas observaciones caen en
# una l�nea recta con pendiente 1, aunque esto probablemente sea un efecto de la
# imputaci�n que se ha comentado antes. 
# 
# N�tese que para parejas en que la mujer tiene t�tulo universitario, el ingreso
# de ambos miembros de la pareja tiende a ser relativamente alto, excepto por un
# par de casos en el que el compa�ero no tiene ingresos. 
# 
# Hay algunos casos de ex-alumnas universitarias que viven en parejas en que solo
# el hombre tiene ingresos, y para el resto de parejas en este caso se aprecia una
# muy ligera correlaci�n positiva entre los ingresos de ambos miembros.
#
# Para mujeres de niveles educativos m�s bajos se dan las cuatro situaciones
# mencionadas, con una proporci�n algo mayor de parejas en que solo el hombre 
# tiene ingresos frente a parejas en que solo la mujer los tiene. En estos casos,
# si se ignoran las parejas en la l�nea recta, apenas parece haber correlaci�n
# entre los dos niveles de ingresos. #
