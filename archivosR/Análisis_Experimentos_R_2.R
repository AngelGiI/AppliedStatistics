################################################################################
######                                                                    ######
######                ANÁLISIS DE EXPERIMENTOS CON R (2)                  ######
######                                                                    ######
######                       ESTADÍSTICA APLICADA                         ######
######               GRADO EN INGENIERÍA MATEMÁTICA 2020/2021             ######
######                                                                    ######
################################################################################

#        ESTE SCRIPT HA SIDO REALIZADO POR TINGUARO RODRÍGUEZ (2020)           #

# Vamos a resolver el ejercicio 14 de la Hoja 1 usando la función aov(). Este 
# ejercicio aplica sobre los mismos datos de dureza de una aleación tres modelos
# distintos: i) unifactorial completamente aleatorizado, con el factor "método de
# forjado", con niveles A y B; ii) unifactorial aleatorizado por bloques, con el
# mismo factor anterior y una variable bloque, "operador", con niveles O y P; y
# iii) un modelo bifactorial con las variables anteriores, que permite considerar 
# además la posible interacción entre los 2 factores.
#
# Comencemos por cargar los datos. Siguiendo el modo en que se llevó a cabo esta 
# carga en el script anterior, es posible definir 3 vectores de longitud 8, que 
# es el número de observaciones de la respuesta, conteniendo respectivamente
# el nivel del factor "método", del bloque/factor "operador" que definen cada
# tratamiento, así como las correspondientes observaciones de la respuesta en 
# cada tratamiento. #

metodo=as.factor(c("A","A","A","A","B","B","B","B"))
operador=as.factor(c("O","O","P","P","O","O","P","P"))
dureza=c(4.8, 5, 5.2, 5.1, 4.9, 5, 6.8, 7.4)

# Es directo entonces generar un data.frame agrupando todos estos vectores. #

datos=data.frame(metodo,operador,dureza)

# En tanto que habitualmente los datos a analizar estarán contenidos en un 
# archivo, normalmente un .txt o un .csv, es conveniente saber cómo cargarlos en 
# R desde esa ubicación. Esto se puede llevar a cabo mediante varias funciones
# de R. En este caso, como los datos vienen en un formato .txt separado por 
# tabulaciones, una función apropiada es read.table(), aunque también podríamos
# conseguirlo con otras funciones como read.csv() o read.delim(). 
# 
# La función read.table() tiene numerosas opciones para adaptarse a diferentes
# formatos y características de los archivos con los datos "en crudo". En nuestro
# caso, la importación de los datos se puede realizar de manera bastante directa,
# simplemente indicando a R que el archivo a importar, datosEj14H1.txt, contiene
# los nombres de las variables en la primera fila con la opción header=T. 
# Como primer argumento de la llamada a read.table() se ha de especificar la
# ruta completa del archivo, incluyendo la extensión de este. Un detalle a tener
# en cuenta al especificar la ruta es que el caracter / que sirve para indicar
# las subcarpetas en la ruta es un carácter reservado de R, por lo que ha de 
# añadirse otro carácter / antes de cada / que aparezca en la ruta del archivo.
#
# Antes de proceder a la importación, vamos a eliminar todos los objetos que
# hemos creado hasta ahora mediante la función rm(). A esta función podemos 
# pasarle los nombres de los objetos a eliminar, uno a uno, aunque en este caso,
# en que queremos eliminar todos los objetos, es posible también llevarlo a cabo
# como sigue. #

rm(list=ls())

# Por supuesto, esto es equivalente a la siguiente sentencia. #

rm(datos,metodo,operador,dureza)

# Procedamos entonces a la importación de los datos desde el fichero 
# datosEj14H1.txt. #

ruta="C:\\(copiar aquí la ruta de carpetas)\\datosEj14H1.txt"
datos=read.table(ruta,header=T)
str(datos)

# Obsérvese que esteñ objeto "datos" devuelto por read.table() es de nuevo un
# data.frame, al igual que el objeto "datos" creado antes a partir de los 
# vectores que introdujimos, aunque hay un problema. ¿Cuál es? 
















# El problema es que los valores de la variable respuesta "dureza", que han de
# ser numéricos, han sido leidos como caracteres, lo que ha llevado a que
# "dureza" sea interpretado como un factor, con tantos niveles como valores
# diferentes en las observaciones de esta variable. Esto ha sucedido porque
# en el archivo datosEj14H1.txt los valores de esta variable usan el separador
# decimal ",", que es el habitual en España, mientras que en R, como en todo
# el software de origen anglosajón, el separador por defecto es ".". Por esto,
# al realizar la importación, cuando read.table() lee la columna de "dureza"
# y encuentra caracteres ",", no lo reconoce como un separador decimal sino 
# como un caracter de texto, y de ahí que "dureza" se haya importado como un
# factor.
#
# Para resolver este problema, se ha de especificar en read.table() que el 
# separador decimal viene dado por "," en lugar de ".", que es el valor por 
# defecto. Esto se hace especificando dec="," en los argumentos de 
# read.table(). #

datos=read.table(ruta,header=T,dec=",")
str(datos)

# Ahora ya tenemos los datos correctamente almacenados en el data.frame "datos".
#
# Para poder acceder a los vectores que componen "datos" sin necesidad de 
# anteponer el nombre del data frame, usemos la función attach(). # 

attach(datos)

# Comencemos con el análisis de los datos mediante el modelo especificado en el
# apartado a del ejercicio, esto es, mediente un modelo unifactorial 
# completamente aleatorizado, en el que las variaciones de la respuesta (dureza)
# se intentan explicar mediante las variaciones en el factor "método".
#
# Como siempre, conviene comenzar el análisis mediante la exploración de los 
# datos usando técnicas descriptivas. En primer lugar, realicemos gráficos
# de la respuesta en los diferentes niveles del factor considerado. #

layout(matrix(1:3,ncol=3))
plot(dureza ~ metodo)
stripchart(dureza ~ metodo,vertical=T)
plot.design(dureza ~ metodo,ylim=c(min(dureza),max(dureza)))

# La primera impresión que dan estos datos es que, a pesar de que la media de la
# respuesta con el método B es superior a la media con el método A, esta 
# diferencia no va a ser significativa atendiendo a la dispersión que presentan
# las observaciones. Esto es, la diferencia entre las medias no es 
# suficientemente grande en comparación con el ruido o error experimental bajo
# este diseño unifactorial. 
# 
# Obtengamos ahora las medias de cada nivel y su dispersión. #

tapply(dureza,metodo,mean)
tapply(dureza,metodo,sd)

# Como vemos, la dispersión en el nivel B es claramente superior a la del nivel 
# A, lo cual puede conllevar problemas de heterocedasticidad. Apliquemos el 
# contraste de Levene para dilucidar esta cuestión. #

library(lawstat)
levene.test(dureza,metodo,"median")

# Este test otorga un p-valor muy bajo a la hipótesis nula de igualdad de 
# varianzas, lo que conllevaría rechazarla y por tanto concluir que no se cumple
# el supuesto de homocedasticidad. No obstante, el reducido número de 
# observaciones (8 en total) hace que el resultado de este test haya de ser
# tomado con precaución. En cualquier caso, a continuación se llevará a cabo el 
# análisis del modelo sin prestar más atención al cumplimiento de los supuestos,
# ya que en particular puede ser que alguno de los modelos de los apartados 
# siguientes sea realmente más apropiado. 
#
# Procedemos entonces a obtener la tabla ANOVA para el modelo unifactorial
# simple de este apartado. #

resultado=aov(dureza ~ metodo)
summary(resultado)
summary.lm(resultado)

# La tabla ANOVA indica que, como ya intuíamos, el factor "método" por sí solo 
# no es capaz de explicar adecuadamente las variaciones observadas de la 
# respuesta. Sin más información sobre las condiciones de realización de los 
# experimentos tendríamos que concluir que la evidencia disponible apunta a que 
# el factor "método" no tiene influencia sobre la dureza de las aleaciones. 
#


# Pasemos ahora al apartado b, en que se pide ajustar un modelo unifactorial 
# aleatorizado por bloques, en el que el factor sigue siendo el método de 
# forjado, pero al que se añade una variable bloque referente al operador que 
# realizó las aleaciones mediante cada método.
#
# En primer lugar, echemos un vistazo a los datos al organizarlos por los 
# niveles de la variable bloque. #

plot(dureza ~ operador)
stripchart(dureza ~ operador,vertical=T)
plot.design(dureza ~ operador,ylim=c(min(dureza),max(dureza)))

# Se observa un comportamiento similar al anterior. Hay diferencias entre las
# medias de dureza de las aleaciones producidas por cada operador, más incluso 
# que en relación a los métodos de forjado, pero esta diferencia no parece que 
# sea claramente mayor que las variaciones observadas dentro de cada nivel.
#
# Obtengamos ahora las medias de cada nivel del bloque y su dispersión. #

tapply(dureza,operador,mean)
tapply(dureza,operador,sd)

# De nuevo observamos diferencias importantes de dispersión entre ambos niveles,
# lo cual de nuevo podría significar que el supuesto de homocedasticidad no se
# cumple para los niveles de la variable "operador". Aplicamos el test de Levene
# para obtener una respuesta cuantitativa. #

levene.test(dureza,operador,"median")

# El p-valor es de nuevo bastante bajo, lo que indicaría que tampoco se cumple la
# homocedasticidad en relación a los niveles de la variable bloque. Como antes, 
# sin embargo,  proseguiremos el análisis sin prestar más atención a estos 
# supuestos por ahora. 
#
# Continuemos entonces con el análisis descriptivo de los datos
# considerando conjuntamente el factor "método" y el bloque "operador". Como 
# cada variable tiene 2 niveles, en conjunto se tienen 2 x 2 = 4 tratamientos,
# que corresponden a las diferentes combinaciones de niveles de ambas variables.
# Para añadir el bloque "operador" al factor "metodo" en la fórmula de R que 
# usamos antes en las funciones plot() y aov(), usamos el signo +. Esto es, 
# la fórmula para el nuevo modelo será "dureza ~ metodo+operador".

layout(matrix(1:4,ncol=4))
plot(dureza ~ metodo+operador)
stripchart(dureza ~ metodo+operador,vertical=T)
plot.design(dureza ~ metodo+operador,ylim=c(min(dureza),max(dureza)))

# Como vemos, estos gráficos informan simultáneamente sobre el comportamiento de 
# la respuesta ante cada variable en el modelo, aunque esta información se
# presenta separada, esto es, hay un gráfico de cajas para cada variable y en el
# gráfico de medias también hay una línea para cada variable. La excepción es el
# stripchart, que nos presenta las observaciones de la respuesta en cada 
# tratamiento o combinación de niveles de ambas variables. 
#
# La impresión de los gráficos anteriores, salvando el stripchart, es que, como 
# ya habíamos notado antes, las diferencias de medias son relativamente pequeñas 
# en comparación con la dispersión que se observa. El stripchart en cambio nos
# indica que la combinación del método B con el operador P parece producir 
# aleaciones de mucha mayor dureza. 

# Es posible realizar gráficos que estudien el efecto de la respuesta en cada
# tratamiento, en lugar del efecto de cada variable por separado. Para ello, se 
# crea primero una nueva variable que combine las dos anteriores, de modo que 
# los valores de la nueva variable sean los 4 diferentes tratamientos. El 
# siguiente comando genera esta variable "tratamientos" y la incluye 
# directamente en el data frame "datos" antes creado. #

datos$tratamientos=with(datos,interaction(metodo,operador))

datos[,c("metodo","operador","tratamientos")]

# Como vemos, los valores que toma esta variable "tratamientos" corresponden
# a la combinación de niveles de las variables "metodo" y "operador", 
# concatenados usando "." como separador. 
#
# Ahora es directo obtener los correspondientes gráficos para cada tratamiento
# usando esta nueva variable. #

attach(datos)

layout(matrix(1:3,ncol=3))
plot(dureza ~ tratamientos)
stripchart(dureza ~ tratamientos,vertical=T)
plot.design(dureza ~ tratamientos,ylim=c(min(dureza),max(dureza)))

# En estos gráficos ya se puede intuir que lo que ocurre en el tratamiento B.P
# tiene que ver con un cambio de tendencia en relación a lo observado en los 
# demás tratamientos. Quedémonos por ahora con esta idea, y continuemos viendo 
# los estadísticos descriptivos desglosados por tratamiento. Aquí es posible 
# usar la función tapply() de dos maneras diferentes. #

tapply(dureza,list(metodo,operador),mean)
tapply(dureza,list(metodo,operador),sd)

tapply(dureza,tratamientos,mean)
tapply(dureza,tratamientos,sd)

# Procedemos entonces a obtener la tabla ANOVA para el modelo unifactorial
# con bloque de este apartado. #

resultado2=aov(dureza ~ metodo + operador)
summary(resultado2)
summary.lm(resultado2)

# La tabla ANOVA ahora contiene una fila adicional para estudiar la 
# significatividad del efecto del bloque "operador". Vemos que el efecto de esta 
# variable bloque es significativo para alfa=0.05, y que el efecto del factor "método" 
# sigue sin serlo, aunque se observa una diferencia no despreciable respecto a la 
# evidencia obtenida para este factor en el modelo anterior. Ahora el p-valor del
# método de forjado es un 50% menor, a pesar de que la suma de cuadrados asociada
# toma el mismo valor. Esto es así ya que en este caso la media de cuadrados del
# error MCR es ahora más pequeña, ya que parte de la variabilidad de la 
# respuesta que antes quedaba sin explicar es ahora recogida y explicada a 
# través de la variable bloque. 

# Pasemos entonces al apartado c, que pide ajustar un modelo bifactorial a los
# mismos datos. La diferencia fundamental entre el anterior modelo unifactorial
# con bloque y el bifactorial reside en que este último va a permitir considerar
# la posible interacción entre los factores, en este caso entre las variables 
# "metodo", que es el factor propiamente dicho, en el sentido de que es la 
# variable de la que interesa estudiar su efecto en la respuesta, y "operador",
# que interviene más como un bloque, en el sentido de ser una variable que nos
# ayuda a controlar mejor la variabilidad del experimento.
#
# Esta interacción, como ya se ha adelantado, consiste en que el efecto de la 
# variación de un factor puede modificarse sensiblemente al variar conjuntamente
# con el otro factor. En un modelo lineal, en el que cada factor interviene de 
# forma separada y aditiva, el efecto estimado de variar el nivel de un factor es 
# el mismo independientemente de qué nivel tome el resto de factores. Sin embargo,
# en un modelo con interacción, esta variación puede ser diferente para cada nivel
# del resto de factores. 
# 
# En el ejercicio que nos ocupa, la existencia de interacción implicaría que el 
# efecto de variar el nivel del factor "método", por ejemplo pasando del método A
# al método B, será diferente para cada operador. Esto es, la diferencia entre
# la dureza media obtenida con los métodos A y B cuando el experimento lo realiza
# el operador O variará en relación a la misma diferencia cuando el experimento
# lo ejecuta el otro operador, P. 
#
# Para estudiar gráficamente la presencia de interacción, R proporciona la función
# interaction.plot(), que nos permite estudiar las tendencias anteriores de la
# respuesta ante variaciones de un factor para diferentes niveles de otro factor.
# El siguiente comando requiere la creación de este tipo de gráfico. #

layout(1)
interaction.plot(metodo,operador,dureza)

# Como vemos, cuando el operador O realiza las aleaciones, el efecto de pasar del
# método A al B es un ligero aumento de la dureza de las aleaciones. Sin embargo,
# cuando es el operador P el que lleva a cabo las aleaciones, el efecto del cambio
# de método es un aumento muy pronunciado de la dureza. En otras palabras, hay un 
# cambio en la tendencia del efecto del factor "método" en función del operador. 
# De hecho, el método B parece mucho más efectivo que el A cuando es el operador P 
# el que lo realiza, mientras que ambos métodos tienen una eficacia similar (si 
# acaso algo menor la del B) cuando los opera O. 
#
# En general, siempre que las líneas observadas en el gráfico de interacción 
# no sean paralelas, se tendrá que existe un cierto efecto de interacción. La 
# significatividad de este efecto, sin embargo, dependerá del tamaño del cambio
# de tendencia en comparación con el tamaño del error experimental, como es 
# habitual en la metodología ANOVA.
#
# Obtengamos la tabla ANOVA del modelo bifactorial, lo que nos permitirá estudiar
# la significatividad de la interacción de manera conjunta a la de los factores. 
# Para especificar este tipo de modelo, en el objeto fórmula se ha de usar el
# símbolo "*" en lugar de "+", que era el empleado para especificar un modelo
# unifactorial con bloque. #

resultado3=aov(dureza ~ metodo * operador)
summary(resultado3)
summary.lm(resultado3)

# Como se puede observar en la tabla ANOVA obtenida, ahora los dos factores, tanto
# el método de forjado como el operador, son significativos a nivel 0.01. La 
# explicación para este cambio en el resultado respecto al obtenido con el modelo
# unifactorial con bloque es que buena parte de la variabilidad que quedaba
# sin explicar en este último es ahora explicada a través de la interacción. Es
# directo comprobar que la suma de cuadrados residual del modelo anterior con 
# bloque es la suma de las sumas de cuadrados residual y de interacción del 
# modelo bifactorial. Además, el R^2 de este modelo es 96.95%, lo que implica
# que ahora es posible explicar un 26% más de la variabilidad de la dureza.
#
# En concreto, la suma de cuadrados residual se ha reducido desde 2.015 a 0.21, 
# a cambio de perder un grado de libertad en la estimación del error, que ahora se
# destina a la estimación de la interacción. En consecuencia, la MCR es ahora
# un 13% de la MCR anterior, esto es, el error experimental se ha reducido en un
# 87% al tener en cuenta en el modelo la variabilidad debida a la interacción.
#
# Por esto, las medias de cuadrados de ambos factores son ahora mucho mayores que
# el error experimental, lo que explica que ahora el factor sí sea significativo
# en tanto los correspondientes estadísticos F toman valores mucho más altos. 
#
# Además, la tabla ANOVA ahora contiene una línea adicional para el efecto de la
# interacción, que vemos que también es significativo a nivel 0.01. 
#
# Así pues, los resultados de este modelo nos permitirían afirmar que existe 
# evidencia suficiente para concluir que tanto el método de forjado como el 
# operador que lo aplica tienen influencia en la dureza de las aleaciones 
# producidas. Aun más, tendríamos evidencia suficiente para considerar que existe 
# un cambio importante de tendencia en el efecto del método de forjado en función 
# del operador que realice la aleación. En otras palabras, concluiríamos que no 
# solo el método B es mejor que el A, sino también que la mejora que produce puede
# variar en función del operador que lo aplique.
#
# Vamos ahora a estudiar la estimación de los parámetros a partir de los 
# coeficientes que devuelve aov(). Comparemos estos coeficientes con las
# medias de cada tratamiento. #

resultado3$coefficients
tapply(dureza,list(metodo,operador),mean)

# No es difícil apreciar que el coeficiente de Intercept es ahora la media
# de las observaciones en el tratamiento de referencia, A.O. Y que el resto de 
# coeficientes se corresponde con la diferencia entre la media de cada uno de los 
# otros tratamientos y la de ese tratamiento de referencia. Por ejemplo, veamos
# que el segundo coeficiente se corresponde con la diferencia de medias entre
# los tratamientos A.O y B.O. #

resultado3$coefficients[2]
mean(dureza[tratamientos=="B.O"])-mean(dureza[tratamientos=="A.O"])

# Lo mismo sucede para el resto de coeficientes. #

resultado3$coefficients[3]
mean(dureza[tratamientos=="A.P"])-mean(dureza[tratamientos=="A.O"])

resultado3$coefficients[4]+resultado3$coefficients[2]+resultado3$coefficients[3]
mean(dureza[tratamientos=="B.P"])-mean(dureza[tratamientos=="A.O"])

# Nótese que esta manera de presentar el valor de los coeficientes es similar al 
# del modelos unifactorial (que vimos en el script anterior), de modo que el 
# coeficiente Intercept toma en general el valor predicho por cada modelo en el 
# tratamiento (o nivel) de referencia, y el de resto de coeficientes muestra la 
# diferencia entre los valores predichos en los demás tratamientos (o niveles) y 
# el de referencia. En este sentido, recuérdese que el valor predicho en cada 
# tratamiento por el modelo bifactorial es precisamente la media de las 
# observaciones en cada tratamiento.

# Por otro lado, recordemos que un modelo bifactorial con dos niveles en cada
# factor tiene un total de 9 parámetros, de los que solamente 4 son informativos 
# (sin contar la varianza). Teniendo en cuenta cómo se definían los estimadores
# de los parámetros es sencillo observar que estos estimadores se pueden 
# obtener como sigue: #

mu=mean(dureza)
alfa_A=mean(dureza[metodo=="A"])-mean(dureza)
alfa_B=mean(dureza[metodo=="B"])-mean(dureza)
beta_O=mean(dureza[operador=="O"])-mean(dureza)
beta_P=mean(dureza[operador=="P"])-mean(dureza)
ab_AO=mean(dureza[metodo=="A" & operador=="O"])-mean(dureza[metodo=="A"])-mean(dureza[operador=="O"])+mean(dureza)
ab_AP=mean(dureza[metodo=="A" & operador=="P"])-mean(dureza[metodo=="A"])-mean(dureza[operador=="P"])+mean(dureza)
ab_BO=mean(dureza[metodo=="B" & operador=="O"])-mean(dureza[metodo=="B"])-mean(dureza[operador=="O"])+mean(dureza)
ab_BP=mean(dureza[metodo=="B" & operador=="P"])-mean(dureza[metodo=="B"])-mean(dureza[operador=="P"])+mean(dureza)

print(c(mu,alfa_A,alfa_B,beta_O,beta_P,ab_AO,ab_AP,ab_BO,ab_BP))

# Una vez que se ha visto que el modelo bifactorial produce un ajuste 
# satisfactorio de los datos, queda comprobar que los supuestos básicos del 
# modelo se verifican en el caso concreto de este problema. A este respecto, 
# ya hemos visto que los contrastes de Levene rechazaban la igualdad de varianzas
# entre los grupos asociados a los niveles de ambos factores. Completemos el 
# diagnóstico del modelo de manera gráfica. #

layout(matrix(1:4,ncol=4))
plot(resultado3)

# Estos gráficos muestran que, como ya veníamos sospechando, el supuesto de 
# homocedasticidad no parece cumplirse, puesto que la magnitud de los residuos 
# no parece semejante en los diferentes grupos, y se aprecia cierta tendencia a 
# que esta magnitud crezca al aumentar la respuesta predicha. Además, en el 
# gráfico de probabilidad normal se aprecian dos puntos muy alejados de la 
# normalidad en los extremos de la distribución. Esto nos lleva a la posibilidad
# de que los supuestos básicos de homocedasticidad y normalidad no se cumplan en 
# nuestro caso, lo que pondría en duda la validez de los resultados y conclusiones
# obtenidos.
# 
# Para tratar esta cuestión, la mejor alternativa es la transformación de los 
# datos de la respuesta, dureza. Como se explica en los apuntes, cuando la 
# varianza de los grupos es una función creciente de la respuesta predicha, la
# transformación logarítmica tiende a solucionar el problema.
#
# Así pues, vamos a repetir el ajuste del modelo bifactorial, esta vez usando 
# como respuesta el logaritmo de la dureza. #

resultado4=aov(log(dureza) ~ metodo * operador)
summary(resultado4)
summary.lm(resultado4)

# Estos resultados muestran que ambos factores y su interacción siguen siendo 
# altamente significativos con la respuesta transformada. De hecho, este modelo
# explica todavía un poco más de la variabilidad de la dureza, hasta un 97.49%.
#
# Veamos ahora el diagnóstico de este modelo. #

plot(resultado4)

# Se observa ahora que la magnitud de los residuos ya no es tan extrema en el 
# tratamiento B.P, y que el problema de falta de normalidad también se ha  
# suavizado. De hecho, si calculamos las desviaciones típicas de los diferentes 
# tratamientos en este modelo transformado, observaremos que ahora las diferencias
# sí entran dentro de límites razonables. #

tapply(log(dureza),list(metodo,operador),mean)
tapply(log(dureza),list(metodo,operador),sd)

# Así pues, en definitiva, podemos aceptar sin demasiado recelo las conclusiones 
# anteriores sobre la significatividad del efecto de ambos factores y de su 
# interacción. ##
