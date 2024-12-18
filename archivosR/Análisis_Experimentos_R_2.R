################################################################################
######                                                                    ######
######                AN�LISIS DE EXPERIMENTOS CON R (2)                  ######
######                                                                    ######
######                       ESTAD�STICA APLICADA                         ######
######               GRADO EN INGENIER�A MATEM�TICA 2020/2021             ######
######                                                                    ######
################################################################################

#        ESTE SCRIPT HA SIDO REALIZADO POR TINGUARO RODR�GUEZ (2020)           #

# Vamos a resolver el ejercicio 14 de la Hoja 1 usando la funci�n aov(). Este 
# ejercicio aplica sobre los mismos datos de dureza de una aleaci�n tres modelos
# distintos: i) unifactorial completamente aleatorizado, con el factor "m�todo de
# forjado", con niveles A y B; ii) unifactorial aleatorizado por bloques, con el
# mismo factor anterior y una variable bloque, "operador", con niveles O y P; y
# iii) un modelo bifactorial con las variables anteriores, que permite considerar 
# adem�s la posible interacci�n entre los 2 factores.
#
# Comencemos por cargar los datos. Siguiendo el modo en que se llev� a cabo esta 
# carga en el script anterior, es posible definir 3 vectores de longitud 8, que 
# es el n�mero de observaciones de la respuesta, conteniendo respectivamente
# el nivel del factor "m�todo", del bloque/factor "operador" que definen cada
# tratamiento, as� como las correspondientes observaciones de la respuesta en 
# cada tratamiento. #

metodo=as.factor(c("A","A","A","A","B","B","B","B"))
operador=as.factor(c("O","O","P","P","O","O","P","P"))
dureza=c(4.8, 5, 5.2, 5.1, 4.9, 5, 6.8, 7.4)

# Es directo entonces generar un data.frame agrupando todos estos vectores. #

datos=data.frame(metodo,operador,dureza)

# En tanto que habitualmente los datos a analizar estar�n contenidos en un 
# archivo, normalmente un .txt o un .csv, es conveniente saber c�mo cargarlos en 
# R desde esa ubicaci�n. Esto se puede llevar a cabo mediante varias funciones
# de R. En este caso, como los datos vienen en un formato .txt separado por 
# tabulaciones, una funci�n apropiada es read.table(), aunque tambi�n podr�amos
# conseguirlo con otras funciones como read.csv() o read.delim(). 
# 
# La funci�n read.table() tiene numerosas opciones para adaptarse a diferentes
# formatos y caracter�sticas de los archivos con los datos "en crudo". En nuestro
# caso, la importaci�n de los datos se puede realizar de manera bastante directa,
# simplemente indicando a R que el archivo a importar, datosEj14H1.txt, contiene
# los nombres de las variables en la primera fila con la opci�n header=T. 
# Como primer argumento de la llamada a read.table() se ha de especificar la
# ruta completa del archivo, incluyendo la extensi�n de este. Un detalle a tener
# en cuenta al especificar la ruta es que el caracter / que sirve para indicar
# las subcarpetas en la ruta es un car�cter reservado de R, por lo que ha de 
# a�adirse otro car�cter / antes de cada / que aparezca en la ruta del archivo.
#
# Antes de proceder a la importaci�n, vamos a eliminar todos los objetos que
# hemos creado hasta ahora mediante la funci�n rm(). A esta funci�n podemos 
# pasarle los nombres de los objetos a eliminar, uno a uno, aunque en este caso,
# en que queremos eliminar todos los objetos, es posible tambi�n llevarlo a cabo
# como sigue. #

rm(list=ls())

# Por supuesto, esto es equivalente a la siguiente sentencia. #

rm(datos,metodo,operador,dureza)

# Procedamos entonces a la importaci�n de los datos desde el fichero 
# datosEj14H1.txt. #

ruta="C:\\(copiar aqu� la ruta de carpetas)\\datosEj14H1.txt"
datos=read.table(ruta,header=T)
str(datos)

# Obs�rvese que este� objeto "datos" devuelto por read.table() es de nuevo un
# data.frame, al igual que el objeto "datos" creado antes a partir de los 
# vectores que introdujimos, aunque hay un problema. �Cu�l es? 
















# El problema es que los valores de la variable respuesta "dureza", que han de
# ser num�ricos, han sido leidos como caracteres, lo que ha llevado a que
# "dureza" sea interpretado como un factor, con tantos niveles como valores
# diferentes en las observaciones de esta variable. Esto ha sucedido porque
# en el archivo datosEj14H1.txt los valores de esta variable usan el separador
# decimal ",", que es el habitual en Espa�a, mientras que en R, como en todo
# el software de origen anglosaj�n, el separador por defecto es ".". Por esto,
# al realizar la importaci�n, cuando read.table() lee la columna de "dureza"
# y encuentra caracteres ",", no lo reconoce como un separador decimal sino 
# como un caracter de texto, y de ah� que "dureza" se haya importado como un
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
# anteponer el nombre del data frame, usemos la funci�n attach(). # 

attach(datos)

# Comencemos con el an�lisis de los datos mediante el modelo especificado en el
# apartado a del ejercicio, esto es, mediente un modelo unifactorial 
# completamente aleatorizado, en el que las variaciones de la respuesta (dureza)
# se intentan explicar mediante las variaciones en el factor "m�todo".
#
# Como siempre, conviene comenzar el an�lisis mediante la exploraci�n de los 
# datos usando t�cnicas descriptivas. En primer lugar, realicemos gr�ficos
# de la respuesta en los diferentes niveles del factor considerado. #

layout(matrix(1:3,ncol=3))
plot(dureza ~ metodo)
stripchart(dureza ~ metodo,vertical=T)
plot.design(dureza ~ metodo,ylim=c(min(dureza),max(dureza)))

# La primera impresi�n que dan estos datos es que, a pesar de que la media de la
# respuesta con el m�todo B es superior a la media con el m�todo A, esta 
# diferencia no va a ser significativa atendiendo a la dispersi�n que presentan
# las observaciones. Esto es, la diferencia entre las medias no es 
# suficientemente grande en comparaci�n con el ruido o error experimental bajo
# este dise�o unifactorial. 
# 
# Obtengamos ahora las medias de cada nivel y su dispersi�n. #

tapply(dureza,metodo,mean)
tapply(dureza,metodo,sd)

# Como vemos, la dispersi�n en el nivel B es claramente superior a la del nivel 
# A, lo cual puede conllevar problemas de heterocedasticidad. Apliquemos el 
# contraste de Levene para dilucidar esta cuesti�n. #

library(lawstat)
levene.test(dureza,metodo,"median")

# Este test otorga un p-valor muy bajo a la hip�tesis nula de igualdad de 
# varianzas, lo que conllevar�a rechazarla y por tanto concluir que no se cumple
# el supuesto de homocedasticidad. No obstante, el reducido n�mero de 
# observaciones (8 en total) hace que el resultado de este test haya de ser
# tomado con precauci�n. En cualquier caso, a continuaci�n se llevar� a cabo el 
# an�lisis del modelo sin prestar m�s atenci�n al cumplimiento de los supuestos,
# ya que en particular puede ser que alguno de los modelos de los apartados 
# siguientes sea realmente m�s apropiado. 
#
# Procedemos entonces a obtener la tabla ANOVA para el modelo unifactorial
# simple de este apartado. #

resultado=aov(dureza ~ metodo)
summary(resultado)
summary.lm(resultado)

# La tabla ANOVA indica que, como ya intu�amos, el factor "m�todo" por s� solo 
# no es capaz de explicar adecuadamente las variaciones observadas de la 
# respuesta. Sin m�s informaci�n sobre las condiciones de realizaci�n de los 
# experimentos tendr�amos que concluir que la evidencia disponible apunta a que 
# el factor "m�todo" no tiene influencia sobre la dureza de las aleaciones. 
#


# Pasemos ahora al apartado b, en que se pide ajustar un modelo unifactorial 
# aleatorizado por bloques, en el que el factor sigue siendo el m�todo de 
# forjado, pero al que se a�ade una variable bloque referente al operador que 
# realiz� las aleaciones mediante cada m�todo.
#
# En primer lugar, echemos un vistazo a los datos al organizarlos por los 
# niveles de la variable bloque. #

plot(dureza ~ operador)
stripchart(dureza ~ operador,vertical=T)
plot.design(dureza ~ operador,ylim=c(min(dureza),max(dureza)))

# Se observa un comportamiento similar al anterior. Hay diferencias entre las
# medias de dureza de las aleaciones producidas por cada operador, m�s incluso 
# que en relaci�n a los m�todos de forjado, pero esta diferencia no parece que 
# sea claramente mayor que las variaciones observadas dentro de cada nivel.
#
# Obtengamos ahora las medias de cada nivel del bloque y su dispersi�n. #

tapply(dureza,operador,mean)
tapply(dureza,operador,sd)

# De nuevo observamos diferencias importantes de dispersi�n entre ambos niveles,
# lo cual de nuevo podr�a significar que el supuesto de homocedasticidad no se
# cumple para los niveles de la variable "operador". Aplicamos el test de Levene
# para obtener una respuesta cuantitativa. #

levene.test(dureza,operador,"median")

# El p-valor es de nuevo bastante bajo, lo que indicar�a que tampoco se cumple la
# homocedasticidad en relaci�n a los niveles de la variable bloque. Como antes, 
# sin embargo,  proseguiremos el an�lisis sin prestar m�s atenci�n a estos 
# supuestos por ahora. 
#
# Continuemos entonces con el an�lisis descriptivo de los datos
# considerando conjuntamente el factor "m�todo" y el bloque "operador". Como 
# cada variable tiene 2 niveles, en conjunto se tienen 2 x 2 = 4 tratamientos,
# que corresponden a las diferentes combinaciones de niveles de ambas variables.
# Para a�adir el bloque "operador" al factor "metodo" en la f�rmula de R que 
# usamos antes en las funciones plot() y aov(), usamos el signo +. Esto es, 
# la f�rmula para el nuevo modelo ser� "dureza ~ metodo+operador".

layout(matrix(1:4,ncol=4))
plot(dureza ~ metodo+operador)
stripchart(dureza ~ metodo+operador,vertical=T)
plot.design(dureza ~ metodo+operador,ylim=c(min(dureza),max(dureza)))

# Como vemos, estos gr�ficos informan simult�neamente sobre el comportamiento de 
# la respuesta ante cada variable en el modelo, aunque esta informaci�n se
# presenta separada, esto es, hay un gr�fico de cajas para cada variable y en el
# gr�fico de medias tambi�n hay una l�nea para cada variable. La excepci�n es el
# stripchart, que nos presenta las observaciones de la respuesta en cada 
# tratamiento o combinaci�n de niveles de ambas variables. 
#
# La impresi�n de los gr�ficos anteriores, salvando el stripchart, es que, como 
# ya hab�amos notado antes, las diferencias de medias son relativamente peque�as 
# en comparaci�n con la dispersi�n que se observa. El stripchart en cambio nos
# indica que la combinaci�n del m�todo B con el operador P parece producir 
# aleaciones de mucha mayor dureza. 

# Es posible realizar gr�ficos que estudien el efecto de la respuesta en cada
# tratamiento, en lugar del efecto de cada variable por separado. Para ello, se 
# crea primero una nueva variable que combine las dos anteriores, de modo que 
# los valores de la nueva variable sean los 4 diferentes tratamientos. El 
# siguiente comando genera esta variable "tratamientos" y la incluye 
# directamente en el data frame "datos" antes creado. #

datos$tratamientos=with(datos,interaction(metodo,operador))

datos[,c("metodo","operador","tratamientos")]

# Como vemos, los valores que toma esta variable "tratamientos" corresponden
# a la combinaci�n de niveles de las variables "metodo" y "operador", 
# concatenados usando "." como separador. 
#
# Ahora es directo obtener los correspondientes gr�ficos para cada tratamiento
# usando esta nueva variable. #

attach(datos)

layout(matrix(1:3,ncol=3))
plot(dureza ~ tratamientos)
stripchart(dureza ~ tratamientos,vertical=T)
plot.design(dureza ~ tratamientos,ylim=c(min(dureza),max(dureza)))

# En estos gr�ficos ya se puede intuir que lo que ocurre en el tratamiento B.P
# tiene que ver con un cambio de tendencia en relaci�n a lo observado en los 
# dem�s tratamientos. Qued�monos por ahora con esta idea, y continuemos viendo 
# los estad�sticos descriptivos desglosados por tratamiento. Aqu� es posible 
# usar la funci�n tapply() de dos maneras diferentes. #

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
# variable bloque es significativo para alfa=0.05, y que el efecto del factor "m�todo" 
# sigue sin serlo, aunque se observa una diferencia no despreciable respecto a la 
# evidencia obtenida para este factor en el modelo anterior. Ahora el p-valor del
# m�todo de forjado es un 50% menor, a pesar de que la suma de cuadrados asociada
# toma el mismo valor. Esto es as� ya que en este caso la media de cuadrados del
# error MCR es ahora m�s peque�a, ya que parte de la variabilidad de la 
# respuesta que antes quedaba sin explicar es ahora recogida y explicada a 
# trav�s de la variable bloque. 

# Pasemos entonces al apartado c, que pide ajustar un modelo bifactorial a los
# mismos datos. La diferencia fundamental entre el anterior modelo unifactorial
# con bloque y el bifactorial reside en que este �ltimo va a permitir considerar
# la posible interacci�n entre los factores, en este caso entre las variables 
# "metodo", que es el factor propiamente dicho, en el sentido de que es la 
# variable de la que interesa estudiar su efecto en la respuesta, y "operador",
# que interviene m�s como un bloque, en el sentido de ser una variable que nos
# ayuda a controlar mejor la variabilidad del experimento.
#
# Esta interacci�n, como ya se ha adelantado, consiste en que el efecto de la 
# variaci�n de un factor puede modificarse sensiblemente al variar conjuntamente
# con el otro factor. En un modelo lineal, en el que cada factor interviene de 
# forma separada y aditiva, el efecto estimado de variar el nivel de un factor es 
# el mismo independientemente de qu� nivel tome el resto de factores. Sin embargo,
# en un modelo con interacci�n, esta variaci�n puede ser diferente para cada nivel
# del resto de factores. 
# 
# En el ejercicio que nos ocupa, la existencia de interacci�n implicar�a que el 
# efecto de variar el nivel del factor "m�todo", por ejemplo pasando del m�todo A
# al m�todo B, ser� diferente para cada operador. Esto es, la diferencia entre
# la dureza media obtenida con los m�todos A y B cuando el experimento lo realiza
# el operador O variar� en relaci�n a la misma diferencia cuando el experimento
# lo ejecuta el otro operador, P. 
#
# Para estudiar gr�ficamente la presencia de interacci�n, R proporciona la funci�n
# interaction.plot(), que nos permite estudiar las tendencias anteriores de la
# respuesta ante variaciones de un factor para diferentes niveles de otro factor.
# El siguiente comando requiere la creaci�n de este tipo de gr�fico. #

layout(1)
interaction.plot(metodo,operador,dureza)

# Como vemos, cuando el operador O realiza las aleaciones, el efecto de pasar del
# m�todo A al B es un ligero aumento de la dureza de las aleaciones. Sin embargo,
# cuando es el operador P el que lleva a cabo las aleaciones, el efecto del cambio
# de m�todo es un aumento muy pronunciado de la dureza. En otras palabras, hay un 
# cambio en la tendencia del efecto del factor "m�todo" en funci�n del operador. 
# De hecho, el m�todo B parece mucho m�s efectivo que el A cuando es el operador P 
# el que lo realiza, mientras que ambos m�todos tienen una eficacia similar (si 
# acaso algo menor la del B) cuando los opera O. 
#
# En general, siempre que las l�neas observadas en el gr�fico de interacci�n 
# no sean paralelas, se tendr� que existe un cierto efecto de interacci�n. La 
# significatividad de este efecto, sin embargo, depender� del tama�o del cambio
# de tendencia en comparaci�n con el tama�o del error experimental, como es 
# habitual en la metodolog�a ANOVA.
#
# Obtengamos la tabla ANOVA del modelo bifactorial, lo que nos permitir� estudiar
# la significatividad de la interacci�n de manera conjunta a la de los factores. 
# Para especificar este tipo de modelo, en el objeto f�rmula se ha de usar el
# s�mbolo "*" en lugar de "+", que era el empleado para especificar un modelo
# unifactorial con bloque. #

resultado3=aov(dureza ~ metodo * operador)
summary(resultado3)
summary.lm(resultado3)

# Como se puede observar en la tabla ANOVA obtenida, ahora los dos factores, tanto
# el m�todo de forjado como el operador, son significativos a nivel 0.01. La 
# explicaci�n para este cambio en el resultado respecto al obtenido con el modelo
# unifactorial con bloque es que buena parte de la variabilidad que quedaba
# sin explicar en este �ltimo es ahora explicada a trav�s de la interacci�n. Es
# directo comprobar que la suma de cuadrados residual del modelo anterior con 
# bloque es la suma de las sumas de cuadrados residual y de interacci�n del 
# modelo bifactorial. Adem�s, el R^2 de este modelo es 96.95%, lo que implica
# que ahora es posible explicar un 26% m�s de la variabilidad de la dureza.
#
# En concreto, la suma de cuadrados residual se ha reducido desde 2.015 a 0.21, 
# a cambio de perder un grado de libertad en la estimaci�n del error, que ahora se
# destina a la estimaci�n de la interacci�n. En consecuencia, la MCR es ahora
# un 13% de la MCR anterior, esto es, el error experimental se ha reducido en un
# 87% al tener en cuenta en el modelo la variabilidad debida a la interacci�n.
#
# Por esto, las medias de cuadrados de ambos factores son ahora mucho mayores que
# el error experimental, lo que explica que ahora el factor s� sea significativo
# en tanto los correspondientes estad�sticos F toman valores mucho m�s altos. 
#
# Adem�s, la tabla ANOVA ahora contiene una l�nea adicional para el efecto de la
# interacci�n, que vemos que tambi�n es significativo a nivel 0.01. 
#
# As� pues, los resultados de este modelo nos permitir�an afirmar que existe 
# evidencia suficiente para concluir que tanto el m�todo de forjado como el 
# operador que lo aplica tienen influencia en la dureza de las aleaciones 
# producidas. Aun m�s, tendr�amos evidencia suficiente para considerar que existe 
# un cambio importante de tendencia en el efecto del m�todo de forjado en funci�n 
# del operador que realice la aleaci�n. En otras palabras, concluir�amos que no 
# solo el m�todo B es mejor que el A, sino tambi�n que la mejora que produce puede
# variar en funci�n del operador que lo aplique.
#
# Vamos ahora a estudiar la estimaci�n de los par�metros a partir de los 
# coeficientes que devuelve aov(). Comparemos estos coeficientes con las
# medias de cada tratamiento. #

resultado3$coefficients
tapply(dureza,list(metodo,operador),mean)

# No es dif�cil apreciar que el coeficiente de Intercept es ahora la media
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

# N�tese que esta manera de presentar el valor de los coeficientes es similar al 
# del modelos unifactorial (que vimos en el script anterior), de modo que el 
# coeficiente Intercept toma en general el valor predicho por cada modelo en el 
# tratamiento (o nivel) de referencia, y el de resto de coeficientes muestra la 
# diferencia entre los valores predichos en los dem�s tratamientos (o niveles) y 
# el de referencia. En este sentido, recu�rdese que el valor predicho en cada 
# tratamiento por el modelo bifactorial es precisamente la media de las 
# observaciones en cada tratamiento.

# Por otro lado, recordemos que un modelo bifactorial con dos niveles en cada
# factor tiene un total de 9 par�metros, de los que solamente 4 son informativos 
# (sin contar la varianza). Teniendo en cuenta c�mo se defin�an los estimadores
# de los par�metros es sencillo observar que estos estimadores se pueden 
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
# satisfactorio de los datos, queda comprobar que los supuestos b�sicos del 
# modelo se verifican en el caso concreto de este problema. A este respecto, 
# ya hemos visto que los contrastes de Levene rechazaban la igualdad de varianzas
# entre los grupos asociados a los niveles de ambos factores. Completemos el 
# diagn�stico del modelo de manera gr�fica. #

layout(matrix(1:4,ncol=4))
plot(resultado3)

# Estos gr�ficos muestran que, como ya ven�amos sospechando, el supuesto de 
# homocedasticidad no parece cumplirse, puesto que la magnitud de los residuos 
# no parece semejante en los diferentes grupos, y se aprecia cierta tendencia a 
# que esta magnitud crezca al aumentar la respuesta predicha. Adem�s, en el 
# gr�fico de probabilidad normal se aprecian dos puntos muy alejados de la 
# normalidad en los extremos de la distribuci�n. Esto nos lleva a la posibilidad
# de que los supuestos b�sicos de homocedasticidad y normalidad no se cumplan en 
# nuestro caso, lo que pondr�a en duda la validez de los resultados y conclusiones
# obtenidos.
# 
# Para tratar esta cuesti�n, la mejor alternativa es la transformaci�n de los 
# datos de la respuesta, dureza. Como se explica en los apuntes, cuando la 
# varianza de los grupos es una funci�n creciente de la respuesta predicha, la
# transformaci�n logar�tmica tiende a solucionar el problema.
#
# As� pues, vamos a repetir el ajuste del modelo bifactorial, esta vez usando 
# como respuesta el logaritmo de la dureza. #

resultado4=aov(log(dureza) ~ metodo * operador)
summary(resultado4)
summary.lm(resultado4)

# Estos resultados muestran que ambos factores y su interacci�n siguen siendo 
# altamente significativos con la respuesta transformada. De hecho, este modelo
# explica todav�a un poco m�s de la variabilidad de la dureza, hasta un 97.49%.
#
# Veamos ahora el diagn�stico de este modelo. #

plot(resultado4)

# Se observa ahora que la magnitud de los residuos ya no es tan extrema en el 
# tratamiento B.P, y que el problema de falta de normalidad tambi�n se ha  
# suavizado. De hecho, si calculamos las desviaciones t�picas de los diferentes 
# tratamientos en este modelo transformado, observaremos que ahora las diferencias
# s� entran dentro de l�mites razonables. #

tapply(log(dureza),list(metodo,operador),mean)
tapply(log(dureza),list(metodo,operador),sd)

# As� pues, en definitiva, podemos aceptar sin demasiado recelo las conclusiones 
# anteriores sobre la significatividad del efecto de ambos factores y de su 
# interacci�n. ##
