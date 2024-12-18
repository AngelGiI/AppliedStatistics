################################################################################
######                                                                    ######
######                         INTRODUCCI�N A R                           ######
######                                                                    ######
######                       ESTAD�STICA APLICADA                         ######
######               GRADO EN INGENIER�A MATEM�TICA 2020/2021             ######
######                                                                    ######
################################################################################

#                  ESTE SCRIPT EST� BASADO EN EL LIBRO            
# Brian S. Everitt, Thorsten Hothorn, A Handbook of Statistical Analyses # Using R  #

#vignette("Ch_introduction_to_R",package="HSAUR")
#edit(vignette("Ch_introduction_to_R",package="HSAUR"))
#vignette(package="HSAUR")

# Como otros programas, R ejecuta comandos, que se han de escribir en el prompt
# de la correspondiente consola, o ser ejecutados mediante RStudio. # 

sqrt(25)+2

# Como otros lenguajes, R opera con variables globales, a las que se ha de 
# asignar valor. Estas variables quedan entonces guardadas en la memoria 
# de la sesi�n. Es posible despu�s acceder a ellas u obtener su valor actual. #

x=sqrt(25)+2
x
print(x)

# R tiene disponible diversa documentaci�n de ayuda sobre el lenguaje 
# y las funciones que utiliza. En la URL http://CRAN.R-project.org/manuals.html
# pueden encontrarse manuales sobre diferentes aspectos del lenguaje R. #

help("mean")
?mean

# Aparte de las funcionalidades b�sicas del lenguaje, R tiene disponibles 
# muy diversos procedimientos estad�sticos en la forma de 'paquetes', que son 
# colecciones de funciones, datasets y otros elementos relacionados. 
# 
# Por ejemplo, el libro mencionado arriba va acompa�ado de un paquete de R 
# que, entre otras cosas, contiene los conjuntos de datos que se usan 
# como ejemplo. Este paquete se llama HSAUR (siglas del t�tulo del libro). 
# Veamos como instalarlo y hacerlo accesible a la sesi�n actual de R. # 

install.packages("HSAUR")
library("HSAUR")

# Una vez que el paquete est� instalado y cargado con la funci�n library(), 
# podemos acceder a los conjuntos de datos que contiene.
#
# En particular, es importante saber que R maneja diferentes formatos de datos,
# siendo uno de los m�s �tiles el llamado 'data frame' por su flexibilidad y 
# compatibilidad con otros formatos y programas. 
#
# Carguemos un data frame del paquete HSAUR. #

data("Forbes2000",package = "HSAUR")

# Mediante el comando anterior, en el entorno global de esta sesi�n de R se 
# habr� creado una nueva entrada, esto es, un nuevo objeto Forbes2000,
# conteniendo el data frame correspondiente.
#
# El siguiente comando ls() permite ver las variables que existen actualmente en 
# tal entorno global. #

ls()

# Podr�amos intentar echar un vistazo al contenido de este data frame usando la
# funci�n print(). Pero esto no ser� realmente muy �til de cara a hacernos una
# idea de c�mo es este data frame. # 

print(Forbes2000)

# Es m�s recomendable comenzar por conocer la estructura del data frame con el 
# siguiente comando. #

str(Forbes2000)

# As�, vemos que el objeto Forbes2000 es un data frame con 2000 observaciones, 
# cada una de las cuales es una compa��a o empresa sobre la que se tiene  
# registrada cierta informaci�n o caracter�sticas, en concreto las siguientes 
# 8 variables:
#
# rank: the ranking of the company,
# name: the name of the company,
# country: the country the company is situated in,
# category: a category describing the products the company produces,
# sales: the amount of sales of the company in billion US dollars,
# profits: the profit of the company in billion US dollars,
# assets: the assets of the company in billion US dollars,
# marketvalue: the market value of the company in billion US dollars.
# 
# Se puede obtener informaci�n algo m�s detallada sobre este data frame usando
# la ayuda. #

?Forbes2000

# R es un lenguaje de programaci�n orientado a objetos, lo que significa que
# todo lo que utiliza son objetos, y que cualquier objeto es una instancia de
# una clase. El nombre de la clase de un objeto se puede obtener con el 
# siguiente comando. #

class(Forbes2000)

# La clase data.frame se usa para almacenar datos en el formato tradicional
# de tabla, como lo har�amos en una hoja de Excel, por ejemplo. Cada fila 
# corresponde con una observaci�n y cada columna con una variable. 
#
# Podemos extraer las dimensiones de esta tabla con la funci�n dim(). #

dim(Forbes2000)

# Alternativamente, podemos obtener el n�mero de filas y de columnas usando
# respectivamente las funciones nrow() y ncol() #

nrow(Forbes2000)
ncol(Forbes2000)

# Es posible acceder a los nombres de las variables del data frame mediante 
# la funci�n names() #

names(Forbes2000)

# Los valores de variables particulares pueden extraerse usando estos nombres. 
# Por ejemplo, podemos saber de qu� clase es la variable que da el ranking de
# las compa��as como sigue. #

class(Forbes2000[,"rank"])

# As� vemos que esta variable rank es de tipo integer, esto es, entero.
#
# Es importante ver que los corchetes [] indican siempre un subconjunto de un
# objeto, como una �nica variable de la tabla completa. La coma dentro de los   
# corchetes es necesaria en tanto que los data frames tienen 2 dimensiones 
# (observaciones y variables), y en este caso estamos accediendo a la segunda.
#
# Extraigamos ahora en un nuevo objeto los rankings de todas las compa��as. #

rankings=Forbes2000[,"rank"]
class(rankings)

# De este modo, el objeto rankings es un vector de enteros. La longitud de este
# vector puede obtenerse con la funcion length(). #

length(rankings)

# B�sicamente, un vector es un conjunto de objetos de la misma clase, y 
# constituye la estructura elemental para el manejo de datos en R.
# 
# Veamos diversas formas de crear vectores num�ricos. #

1:3
c(1,2,3)
seq(from = 1, to = 3, by = 1)

# Tambi�n es posible trabajar con vectores cuyos elementos sean strings o 
# cadenas de texto. #

names=Forbes2000[,"name"]
class(names)
length(names)

# Podemos extraer el primer elemento de este vector como sigue. #

Forbes2000[,"name"][1]
names[1]

# Otra clase de inter�s en R es la clase Factor, que representa caracter�sticas
# de tipo car�cter que, a diferencia de la clase Character, se repiten en 
# diferentes observaciones. La categor�a de las compa��as es una variable de esta
# clase. #

class(Forbes2000[,"category"])

# Adem�s, los objetos de la clase factor se almacenan internamente asociando cada
# valor de tipo car�cter con un valor num�rico, el mismo para cada cadena de
# texto. A estos valores num�ricos diferentes se los conoce como niveles. En este
# caso, la variable category tiene 27 niveles diferentes, que representan 
# diferentes categor�as de negocio. #

nlevels(Forbes2000[,"category"])

# Estos niveles pueden extraerse usando la funci�n levels(). #

levels(Forbes2000[,"category"])

# Es posible obtener un resumen estad�stico de las frecuencias de estos niveles
# en el conjunto Forbes2000 usando la funci�n table(). #

table(Forbes2000[,"category"])

# Las variables sales, assests, profit y market value son de tipo num�rico, que 
# es la clase natural para mediciones continuas. #

class(Forbes2000[,"sales"])

# Para variables de este tipo es posible obtener estad�sticos descriptivos 
# habituales como la media, la mediana o el rango. #

median(Forbes2000[,"sales"])
mean(Forbes2000[,"sales"])
range(Forbes2000[,"sales"])

# Una funci�n �til para obtener una primera impresi�n de la distribuci�n de una
# variable num�rica es la funci�n summary(). #

summary(Forbes2000[,"sales"])

# Veamos ahora algunas nociones b�sicas sobre la manipulaci�n de estas diferentes
# clases de objetos.
#
# Como hemos visto, los data frames proporcionan un tipo de objeto �til para
# almacenar datos de diferente naturaleza. Internamente, un data frame es una
# 'lista' de vectores con una longitud N com�n, el total de observaciones de la
# tabla. Cada uno de esos vectores es un conjunto de mediciones u observaciones
# de una misma caracter�stica y, por tanto, con el mismo tipo o clase.
#
# Por ejemplo, el vector 'names' creado anteriormente contiene los nombres de las
# 2000 compa�ias del conjunto de datos. Podemos acceder a un subconjunto de este
# vector usando corchetes []. #

names[1:3]

# Tambi�n es posible indexar este subconjunto con n�meros negativos. El efecto de
# estos es dejar fuera de la selecci�n los correspondientes elementos. #

names[-(4:2000)]

# Podemos obtener la informaci�n de las variables num�ricas de estas 3 compa��as
# usando las dos dimensiones del data frame, separadas por coma (,) dentro de los 
# corchetes. #

Forbes2000[1:3, c("name", "sales", "profits", "assets")]

# Como vimos antes, podemos usar la segunda dimensi�n con corchetes para extraer
# una variable completa. Una manera algo m�s directa es usar el operador $. #

names=Forbes2000$name

# Esto es equivalente al comando names=Forbes2000[,"name"] usado mas arriba.
#
# Para obtener las mayores compa��as en base a un orden diferente al original, 
# por ejemplo las 3 compa��as con mayores ventas, usamos la funci�n sort(). Esta 
# funci�n devuelve los �ndices de los elementos ordenados del vector 'sales', 
# en orden creciente. #

orden_ventas=order(Forbes2000$sales)
orden_ventas[-(1:1997)]
orden_ventas[c(2000, 1999, 1998)]
Forbes2000[orden_ventas[c(2000, 1999, 1998)],
     c("name", "sales", "profits", "assets")]

# Podemos ordenar en orden decreciente usando un argumento extra en sort. #

orden_ventas=order(Forbes2000$sales, decreasing = TRUE)
orden_ventas[1:3]
Forbes2000[orden_ventas[1:3],
           c("name", "sales", "profits", "assets")]

# Otra manera de seleccionar elementos de un vector es mediante el uso de
# un vector de valores de tipo l�gico (TRUE y FALSE), de manera que se 
# seleccionar�n solo los elementos cuyo �ndice corresponda a un valor TRUE. #

Forbes2000[Forbes2000$assets > 1000,
           c("name", "sales", "profits", "assets")]

# N�tese que la expresi�n Forbes2000$assets > 1000 proporciona un vector l�gico
# de longitud 2000, donde el elemento i-�simo es TRUE si y solo si la compa��a
# i-�sima de Forbes2000 tiene unos activos superiores a 10^12 US$. #

table(Forbes2000$assets > 1000)

# Un aspecto importante siempre que se tratan datos es la presencia de valores
# ausentes -tambi�n llamados perdidos- o missing. En R, los missing se tratan 
# mediante un s�mbolo especial, NA, indicando que la medici�n correspondiente
# no est� disponible (Not Available). 
#
# La funci�n is.na() permite comprobar si un determinado elemento es missing,
# o tambi�n obtener los elementos missing de un vector. #

na_profits=is.na(Forbes2000$profits)
table(na_profits)
Forbes2000[na_profits,
           c("name", "sales", "profits", "assets")]

# Esto es, la funci�n is.na() devuelve TRUE para los elementos de un vector con
# valor NA.
#
# Una manera c�moda de eliminar todas las observaciones de un data frame
# con al menos una variable con valor NA es usando la funci�n complete.cases().
# Esta funci�n toma como argumento un data.frame y devuelve un vector l�gico
# con valor TRUE para las observaciones sin ning�n valor missing. #

table(complete.cases(Forbes2000))

# Otra manera de seleccionar subconjuntos de un data frame es mediante la 
# funci�n subset(), que puede a veces ahorrar escribir algo de c�digo. #

UKcomp=subset(Forbes2000, country == "United Kingdom")
dim(UKcomp)

# Centr�mosnos ahora en algunas herramientas orientadas a obtener informaci�n de
# tipo descriptivo sobre objetos R. Como hemos visto anteriormente, la funci�n
# summary() es �til para obtener una primera impresi�n sobre los datos de un 
# vector, aunque tambi�n es posible aplicarla a un data frame completo. #

summary(Forbes2000)

# As�, a trav�s de la salida de esta funci�n podemos ver que la mayor�a de las 
# compa��as est�n situadas en EEUU (US), y tambi�n que la mayor�a de las compa�ias
# pertenecen al sector bancario. Asimismo, en la informaci�n mostrada podemos 
# observar que existen compa��as con beneficios negativos, esto es, p�rdidas, 
# de hasta 26 x 10^9 US$. 
#
# De hecho, es posible aplicar la funci�n summary() a diferentes clases de 
# objetos R. En este sentido, esta funci�n se puede denominar 'gen�rica', en el 
# sentido de que contiene m�todos aplicables a diferentes clases de cara a 
# proporcionar resultados informativos. Aqu� hemos suministrado un argumento de 
# tipo data frame a summary(), siendo natural aplicar esta funci�n a cada una
# de las variables de ese data frame.  
#   
# En particular, como ya hemos comentado, un data frame es una lista, siendo los
# elementos de esta lista las variables (vectores) que contiene el data frame. 
# Para listas y objetos similares, es posible aplicar la funci�n summary() del
# siguiente modo. #

lapply(Forbes2000, summary)

# Esta funci�n lapply() es un miembro de la familia de funciones 'apply'. Este 
# tipo de funciones facilitan el realizar tareas recurrentes en cada elemento
# de un data frame, una matriz, una lista, o para cada nivel de un factor.
#
# Por ejemplo, podr�amos comparar los beneficios de las compa��as en cada una
# de las 27 categor�as (sectores) contempladas en los datos del siguiente modo. #

mprofits <- tapply(Forbes2000$profits,Forbes2000$category, median, na.rm = TRUE)

# Este comando puede leerse como sigue: para cada nivel del factor 'category', 
# determinar los elementos correspondientes del vector num�rico 'profits', y 
# suministrar estos a la funci�n median() con el argumento adicional na.rm = TRUE.
# Este argumento es necesario en tanto 'profits' contiene valores missing, que 
# conducir�an a un resultado no informativo de la funci�n median(). #

median(Forbes2000$profits)

# A partir de los resultados de tapply(), podemos obtener los tres sectores
# con mayor beneficio mediano, usando la funci�n sort() e invirtiendo la
# ordenaci�n (funci�n rev()). #

rev(sort(mprofits))[1:3]

# Por supuesto, es posible reemplazar la funci�n median() por mean() o por 
# cualquier otra funci�n apropiada en la llamada a mapply(). Sin embargo, en este
# caso la media mean() no es una buena elecci�n (�por qu�?). 

# Aparte de una medida de localizaci�n o centralizaci�n como la mediana, para
# describir num�ricamente la distribuci�n de una variable num�rica normalmente 
# es necesario obtener tambi�n alguna medida de dispersi�n que informe de su
# variabilidad. Una medida de dispersi�n robusta viene dada por el rango 
# intercuart�lico, esto es, la diferencia entre el tercer y primer cuartil 
# de los datos. Esta medida de dispersi�n se puede computar mediante la 
# funci�n IQR(). Apliqu�mosla a los beneficios 'profits' por categor�as. #

tapply(Forbes2000$profits, Forbes2000$category, IQR, na.rm=TRUE)

# Aunque esto resuelve la cuesti�n descriptiva que nos ocupaba, vamos a aprovechar
# este tema para ilustrar algunas posibilidades de R. Para ello, en primer lugar 
# vamos a construir una funci�n propia que replique el funcionamiento de esta 
# funci�n IQR().
#
# En R, una funci�n es un objeto, y todos los objetos se crean del mismo modo. 
# Por tanto, tenemos que asignar un objeto funci�n a una variable. Un objeto
# funci�n consiste en una lista de argumentos, que define los par�metros de la
# funci�n y posiblemente sus valores por defecto, y un cuerpo que define las 
# tareas que la funci�n lleva a cabo. El comienzo y el final del cuerpo de la 
# funci�n se indica mediante llaves {}. Adem�s, en la mayor�a de casos una funci�n
# ha de retornar alg�n valor, por lo que el cuerpo ha de contener uno o m�s 
# comandos 'return()'. 
# 
# Volviendo a nuestro ejemplo, llamaremos a nuestra funci�n 'iqr' (n�tese que R
# es sensible a diferencias entre min�sculas y may�sculas). Esta funci�n iqr ha de
# operar sobre un vector num�rico, por tanto ha de tener un argumento, que 
# denotaremos como x. Este vector ser� enviado a la funci�n quantile() para
# obtener los cuartiles que necesitamos. La diferencia entre ellos ser� entonces
# computada mediante la funci�n diff(). La definici�n de esta funci�n iqr es
# entonces la siguiente. #

iqr = function(x) {
  q = quantile(x,prob=c(0.25,0.75), names=FALSE)
  return(diff(q))
}

# Testeemos el funcionamiento de esta funci�n sobre datos simulados de una 
# distribuci�n normal est�ndar N(0,1), comparando su resultado con el de la
# funci�n original IQR(). #

simdata=rnorm(100)
iqr(simdata)
IQR(simdata)

# As� pues, a primera vista parece que nuestra funci�n iqr reproduce adecuadamente
# el resultado de la funci�n nativa IQR. Sin embargo, si el vector x contiene
# alg�n dato missing nuestra funci�n fallar�a. #

simdata[1]=NA
iqr(simdata)

# Para hacer nuestra funci�n iqr m�s flexible, podr�amos usar los mismos 
# argumentos que utiliza la funci�n quantile(), entre ellos na.rm que, como hemos
# visto, permite quitar los valores missing antes de realizar alguna operaci�n,
# como obtener los cuartiles. Aunque esta opci�n funcionar�a, es m�s flexible y 
# adecuado de cara a evitar posibles errores e inconsistencias (por ejemplo si
# se modifica la funci�n quantile() en versiones posteriores de R) usar un
# argumento 'comod�n', dado por puntos suspensivos, esto es ... #

iqr = function(x, ...) {
  q = quantile(x,prob=c(0.25,0.75), names=FALSE, ...)
  return(diff(q))
}

iqr(simdata, na.rm=TRUE)
IQR(simdata, na.rm=TRUE)

# Una vez tenemos terminada nuestra funci�n iqr, es importante ver que podemos
# usarla exactamente igual que cualquier funci�n nativa de R. Esto es, el lenguaje
# no diferencia entre las funciones escritas por los desarrolladores de R y por
# los usuarios. Por ejemplo, podemos usar nuestra funci�n iqr en lugar de 
# IQR en conjunci�n con la funci�n tapply() para calcular las desviaciones 
# intercuart�licas por categor�as. #

iqr_profits=tapply(Forbes2000$profits, Forbes2000$category, iqr, na.rm=TRUE)

# As�, los sectores con la menor y mayor variabilidad en sus beneficios son los
# siguientes. #

levels(Forbes2000$category)[which.min(iqr_profits)]
levels(Forbes2000$category)[which.max(iqr_profits)]

# Aparte de los descriptores num�ricos como medias, medianas, varianzas o rangos
# intercuart�licos, la herramienta fundamental para el an�lisis descriptivo de 
# una distribuci�n de datos son las representaciones gr�ficas.
# 
# Un gr�fico b�sico que permite obtener una primera aproximaci�n a la forma de 
# la distribuci�n de una variable estad�stica continua es el histograma. El  
# siguiente c�digo produce el histograma de la variable 'profits' y de su 
# logaritmo. #

layout(matrix(1:2, nrow = 2))
hist(Forbes2000$profits)
hist(log(Forbes2000$profits))

# Vemos como la distribuci�n de profits presenta una agrupaci�n una gran mayor�a 
# de compa��as en una franja de beneficios entre -5 y 5 x 10^9 US$, aunque 
# algunas pocas compa��as tienen beneficios mucho m�s extremos. La distribuci�n 
# de su logaritmo tiene en cambio una forma m�s equilibrada, y en particular m�s
# similar a una campana normal.
#
# Este tipo de distribuci�n asim�trica y con datos extremos se puede quiz�s 
# observar mejor en la variable 'marketvalue' a trav�s del correspondiente 
# histograma. #

layout(matrix(1:2, nrow = 2))
hist(Forbes2000$marketvalue)
hist(log(Forbes2000$marketvalue))

# El gr�fico apropiado para estudiar la relaci�n entre dos variables num�ricas es
# el llamado diagrama de dispersi�n (scatterplot). Este tipo de relaciones entre
# variables num�ricas se suelen conocer como 'de regresi�n'. En R, este tipo
# de relaci�n de regresi�n se especifica mediante una 'f�rmula de modelo', que en 
# el caso m�s sencillo en el que solo intervienen dos variables presenta la 
# siguiente forma. #

fm=marketvalue~sales
class(fm)

# En esta expresi�n, la variable dependiente est� en el t�rmino de la izquierda,
# mientras que la variable independiente queda en la derecha. La tilde ~ separa
# ambos t�rminos. Este tipo de f�rmulas se pueden suministrar a diferentes
# funciones de R que construyen modelos estad�sticos, como veremos en temas
# posteriores.
#
# Por ahora, nos quedamos con que estas expresiones de tipo f�rmula pueden 
# usarse para construir diagramas de dispersi�n a trav�s de la funci�n plot(). #

layout(matrix(1:1, nrow = 1))
plot(fm,data=Forbes2000, pch=".")

# En tanto las variables 'marketvalue' y 'sales' son fuertemente asim�tricas,
# es m�s ilustrativo graficar sus logaritmos. #

plot(log(marketvalue) ~ log(sales), data = Forbes2000, pch = ".")

# Una representaci�n que permite calibrar mejor la acumulaci�n de puntos en las
# zonas de mayor densidad se consigue seleccionando un color m�s transparente
# para los puntos graficados. #

plot(log(marketvalue) ~ log(sales), data = Forbes2000,
     col = rgb(0,0,0,0.1), pch = 16)

# Cuando la variable independiente o explicativa es un factor (esto es, una 
# variable categ�rica en vez de numerica), la representaci�n gr�fica m�s adecuada
# es el llamado diagrama de cajas (boxplot). El siguiente c�digo genera un
# diagrama de cajas de la variable 'marketvalue' para un conjunto de 4 pa�ses. #

tmp=subset(Forbes2000, country %in%
             c("United Kingdom","Germany", "India", "Turkey"))
tmp$country=tmp$country[,drop=TRUE]
boxplot(log(marketvalue) ~ country, data = tmp,
        ylab = "log(marketvalue)", varwidth = TRUE)

# La primera sentencia del grupo anterior selecciona los datos que queremos
# comparar, esto es, los de los pa�ses indicados. Sin embargo, en el data frame 
# resultante la variable 'country' sigue siendo un factor con 61 niveles (muchos 
# con frecuencia 0), no solamente con los 4 indicados. Para lograr esto y que en 
# el gr�fico de cajas no aparezcan en el eje horizontal todos estos 61 pa�ses, 
# es necesaria la sentencia de la segunda l�nea, que mediante la opci�n drop=TRUE
# elimina los niveles que no presentan observaciones.
