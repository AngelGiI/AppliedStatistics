################################################################################
######                                                                    ######
######                      REGRESI�N LINEAL CON R                        ######
######                                                                    ######
######                       ESTAD�STICA APLICADA                         ######
######               GRADO EN INGENIER�A MATEM�TICA 2020/2021             ######
######                                                                    ######
################################################################################

#        ESTE SCRIPT HA SIDO REALIZADO POR TINGUARO RODR�GUEZ (2021)           #

# En este script vamos a estudiar c�mo realizar an�lisis de regresi�n lineal
# mediante R. 
#
# Veamos algunos ejemplos de aplicaci�n de modelos de regresi�n lineal simple. 
#
# En primer lugar, trabajaremos con unos datos, Ordenadores.txt, en los que se
# registra el tiempo de reparaci�n en minutos (Minutes) de diferentes n�meros de 
# ordenadores (Units). La idea es obtener una funci�n que relacione 
# cuantitativamente el tiempo que se ha de emplear en la reparaci�n con el 
# n�mero de unidades a reparar. Una vez ajustado, un modelo de este tipo 
# facilita dos tareas: i) explicar la dependencia que tiene el tiempo de 
# reparaci�n del n�mero de unidades, y en particular estimar el tiempo medio que
# se tarda en reparar una unidad as� como el tiempo de "set-up" o de preparaci�n
# antes de iniciar un proceso de reparaci�n; ii) predecir el tiempo que se ha de 
# emplear en la reparaci�n de un n�mero cualquiera de unidades, lo que puede
# ser �til por ejemplo de cara a elaborar un presupuesto del coste de un proceso
# de reparaci�n. #
#
# Comencemos por la carga de los datos a partir del archivo Ordenadores.txt. #

ruta="\\Ordenadores.txt"
datos=read.table(ruta,header=T)
str(datos)
attach(datos)

# Antes del ajuste del modelo, conviene siempre ojear un poco los datos, y en 
# particular representarlos gr�ficamente. As� pues, realicemos un breve
# an�lisis descriptivo de las variables o vectores de datos de este ejemplo. #

boxplot(Minutes)
summary(Minutes)

boxplot(Units)
summary(Units)

# Ambas distribuciones parecen relativamente equilibradas, sin fuertes 
# asimetr�as y sin presencia de datos extremos o at�picos.
#
# Una vez estudiadas las variables por separado, conviene representarlas 
# conjuntamente mediante un diagrama de dispersi�n para comprobar si es realista
# establecer una relaci�n lineal entre ellas.

plot(Minutes ~ Units)

# Este gr�fico muestra que los datos parecen agruparse bastante bien alrededor
# de una recta. Para confirmar este punto, obtengamos la correlaci�n entre
# tiempos de reparaci�n y unidades a reparar. #

cor(Minutes,Units)

# La correlaci�n obtenida es positiva y muy pr�xima a 1, lo que es indicador de 
# una relaci�n lineal directa muy fuerte entre ambas variables. 
# 
# En tanto el an�lisis descriptivo indica que es apropiado establecer un modelo
# lineal para relacionar ambas variables, podemos proceder a su ajuste. #

modelo=lm(Minutes ~ Units)
summary(modelo)

# N�tese que al aplicar summary() al objeto devuelto por lm() se obtiene un
# resultado m�s informativo que antes, en el que se especifica no solo el valor
# de las estimaciones de los par�metros, sino tambi�n el error est�ndar (es 
# decir, la desviaci�n t�pica) de estos estimadores, el estad�stico de 
# contraste t para contrastar la hip�tesis nula de que el par�metro 
# correspondiente vale 0 as� como el p-valor asociado a ese estad�stico. 
#
# En concreto, en este caso el resultado nos informa de que la estimaci�n de
# beta0 es 4.162, y la de beta1 es 15.509. Podemos interpretar estos valores de
# la siguiente manera: 
#
# 1) En tanto beta1 representa la pendiente de la recta, el valor de su 
# estimaci�n da la tasa de intercambio entre unidades de la variable explicativa
# y de la respuesta o, en otras palabras, cu�nto aumenta el tiempo
# de reparaci�n con cada unidad extra que se haya de reparar. En este caso,
# este tiempo se estima en unos 15 minutos por cada ordenador extra a reparar.
#
# 2) Por su parte, beta0 representa la ordenada en el origen de la recta de 
# regresi�n, y como tal puede ser interpretada como el valor a esperar de la 
# respuesta cuando la variable explicativa toma el valor 0, o de manera m�s 
# general como un nivel "base" de la variable respuesta. En nuestro caso, la 
# estimaci�n de beta0 puede interpretarse como un tiempo de set-up o preparaci�n
# para realizar la reparaci�n, el mismo independientemente del n�mero de 
# ordenadores a reparar, que en este caso ser�a de unos 4 minutos.
#
# No obstante, se ha de ser precavido con la interpretaci�n del par�metro beta0, 
# puesto que podr�a no tener sentido que la variable explicativa tomara el 
# valor 0, y el valor de beta0 obtenido podr�a no tener sentido tampoco como
# nivel base de la respuesta. En este sentido, el par�metro beta0 podr�a no ser
# interpretable, m�s all� de proporcionar un mecanismo matem�tico para obtener
# una mejor aproximaci�n de la dependencia entre la respuesta y la variable 
# explicativa.
#
# En relaci�n a los contrastes de hip�tesis que realiza lm(), se lleva a cabo
# un contraste t para cada par�metro estimado, contrastando si el par�metro es
# significativamente distinto de 0. En este caso, el par�metro beta1 asociado a 
# Units obtiene un p-valor muy bajo, por lo que podemos estar bastante seguros
# de que efectivamente el n�mero de ordenadores a reparar influye en el tiempo
# que se ha de emplear en la reparaci�n, con la tasa ya descrita. Es importante
# ver que el contraste sobre este par�metro es el que estudia la 
# significatividad del modelo de regresi�n, en el sentido de si este regresor
# es �til para explicar la respuesta.

# En cambio, el par�metro beta0 no es significativo, lo que debemos interpretar 
# como que no existe certeza en que el tiempo de set-up sea realmente diferente 
# de 0, esto es, podr�a ser que las reparaciones no tuvieran que emplear un 
# tiempo previo de preparaci�n.
#
# Adem�s, lm() realiza tambi�n un contraste F para validar globalmente la 
# significatividad del modelo. En este caso, al tener una �nica variable 
# explicativa, este contraste es equivalente al contraste t del par�metro beta1
# para Units. N�tese que el p-valor asociado a este contraste F es el mismo que
# el obtenido para el coeficiente de Units. Comprobemos adem�s que al elevar
# el estad�stico t al cuadrado se obtiene el valor del estad�stico F. #

summary(modelo)[["fstatistic"]][1] # Valor del estad�stico F
summary(modelo)[["coefficients"]][2,3] # Valor del estad�sto t para beta1
summary(modelo)[["coefficients"]][2,3]^2 # t^2=F

# Una �ltima informaci�n relevante proporcionada por summary() es el R-cuadrado,
# que indica la proporci�n de variabilidad de la respuesta explicada por el
# modelo de regresi�n. En este caso, se obtiene el valor 0.9874, por lo que el
# modelo que hemos ajustado estar�a explicando un 98.74% de la variabilidad de
# los tiempos de reparaci�n, o en otras palabras, el n�mero de ordenadores a 
# reparar explica un 98.74% del tiempo empleado para la reparaci�n.
# 
# Como suced�a con aov(), la funci�n lm() devuelve un objeto con m�ltiples
# componentes, desde la estimaci�n de los par�metros a los valores ajustados
# o los residuos. A estas componentes se accede de manera similar a c�mo se
# hac�a con aov(), esto es, usando el operador $ despu�s del nombre del objeto
# en que se ha guardado el resultado de lm().
#
# Por ejemplo, es posible obtener los tiempos de reparaci�n predichos por el 
# modelo para los n�meros de unidades a reparar observados como sigue. #

matrix(cbind(Units,modelo$fitted.values),byrow = T, nrow=2)

# Como en el caso del ANOVA para an�lisis de experimentos, los modelos de 
# regresi�n lineal se basan en una serie de supuestos que es importante 
# verificar si se cumplen en el caso bajo estudio. La manera m�s extendida de
# realizar este diagn�stico del modelo es mediante el an�lisis gr�fico de los
# residuos obtenidos. R proporciona los gr�ficos necesarios aplicando la funci�n
# gen�rica plot() sobre el objeto devuelto por lm(). #

plot(modelo)

# El primer gr�fico representa los residuos contra los valores predichos. Este
# gr�fico permite estudiar la validez del primer supuesto de linealidad del 
# modelo. Si el modelo es v�lido, los residuos han de presentar un comportamiento 
# aleatorio, sin patrones aparentes. Esto parece cumplirse en este caso. Adem�s,
# este gr�fico tambi�n puede darnos una idea de si se cumple el supuesto de 
# homocedasticidad, esto es, que los errores tengan una dispersi�n similar para
# todos los valores de la respuesta, lo cual tambi�n parece cumplirse en este 
# caso.
#
# El segundo gr�fico, de probabilidad normal, nos permite contrastar el supuesto
# de normalidad de los errores. En este caso parece cumplirse relativamente bien,
# quiz�s con una �nica observaci�n m�s extrema de lo habitual.
#
# El tercer gr�fico estudia la posible dependencia de la magnitud de los 
# residuos con el valor de la respuesta, lo cual permite indagar mejor en la 
# validez del supuesto de homocedasticidad. Reafirmando lo ya observado en el 
# primer gr�fico, no se aprecia ning�n patr�n creciente o decreciente en esta
# magnitud al variar el valor de la respuesta, por lo que podemos descartar el
# incumplimiento de este supuesto.
#
# Finalmente, el cuarto gr�fico permite detectar la posible presencia de 
# observaciones influyentes. Para ello, debemos comprobar si existe alg�n dato
# con una distancia de Cook mayor que 1, lo que no sucede en nuestro caso. Por
# tanto, no se tendr�an observaciones problem�ticas que influyan de manera
# desmedida en el modelo obtenido.
#
# En conclusi�n, el modelo de regresi�n lineal simple que se ha ajustado 
# proporciona una aproximaci�n adecuada a la relaci�n entre tiempos de 
# reparaci�n y unidades a reparar, por lo que puede ser usado con prop�sitos
# predictivos o explicativos.
#
# Por ejemplo, supongamos que se quiere automatizar la realizaci�n de 
# presupuestos para la reparaci�n de cualquier n�mero de unidades, donde el 
# coste de una hora de trabajo del t�cnico es de 50???. As� pues, podr�amos 
# estimar el coste de reparar por ejemplo 7 ordenadores como sigue. #

coste_hora=50
unidades=7
(coste_hora/60)*(modelo$coefficients[1]+modelo$coefficients[2]*unidades)

# Pasemos ahora a otro ejemplo, en que los datos registran las alturas de 
# conyuges de una muestra de matrimonios heterosexuales. La pregunta que surge
# en este contexto es si las alturas de los conyuges est�n relacionadas.
#
# Carguemos los datos, que se encuentran en el archivo Matrimonios.txt. #

ruta="\\Matrimonios.txt"
datos=read.table(ruta,header=T)
str(datos)
attach(datos)

# Realicemos un breve an�lisis descriptivo de las alturas de esposos (Husband)
# y esposas (Wife). #

boxplot(Husband,Wife)
summary(Husband)
summary(Wife)

plot(Husband ~ Wife)
cor(Husband,Wife)

# Este an�lisis descriptivo muestra que ambas alturas parecen estar relacionadas
# de manera directa. Para dilucidar esta cuesti�n de manera cuantitativa, lo
# mejor es ajustar un modelo de regresi�n a los datos. #

modelo=lm(Husband ~ Wife)
summary(modelo)

# Estos resultados muestran que ambas alturas est�n fuertemente relacionadas, y
# en particular que la altura de la esposa es un buen predictor de la altura del
# esposo, ya que se obtiene un p-valor muy pr�ximo a 0 (2e-16) para esa variable.
# La presencia de una constante (Intercept) diferente de 0 en el modelo tambi�n 
# es significativa, aunque con un p-valor no tan peque�o (0.002). El test F
# conduce a la misma conclusi�n de significatividad del modelo, aunque como 
# sabemos este contraste F es equivalente al contraste t para la �nica variable
# en el modelo, por lo que en este caso no aporta realmente m�s informaci�n. 
# Obs�rvese que los p-valores de ambos test son iguales (2e-16) y que el 
# cuadrado del estad�stico t para Wife es igual al valor del estad�stico F. #

summary(modelo)$coefficients[2,3]
summary(modelo)$coefficients[2,3]^2

# Una vez vista la significatividad del modelo, pasemos a su interpretaci�n.
# La estimaci�n del par�metro beta_1, que mide el efecto de la variable
# predictora (Wife) sobre la respuesta (Husband), es de 0.83 cm./cm., de modo 
# que se estima que si una esposa es 1 cm. m�s alta que otra, en media tendr� un
# esposo 0.83 cm. m�s alto. 
#
# Esto podr�a llevar a concluir que, a medida que aumenta la altura de la esposa, 
# menor ser� la diferencia en altura entre ambos c�nyuges, ya que la pendiente
# parece ser menor que 1. Pero, en realidad, estos resultados solo nos aseguran 
# que esta pendiente beta_1 es diferente (y mayor) de 0, pues el valor de 0.83
# es solo una estimaci�n puntual de beta_1, y el contraste que se ha efectuado
# sobre este par�metro a partir de ese estimador es el de si beta_1 es diferente
# de 0. 
#
# Para estudiar la cuesti�n de si la pendiente es menor o, en general, diferente
# de 1, tenemos que realizar otro contraste t, bajo la hip�tesis nula de que 
# beta_1 = 1, y ver qu� ocurre.

n=length(Wife) # tama�o muestral, n�mero de observaciones
alfa=0.05 # nivel de significaci�n
est_beta_1=modelo$coefficients[2] # valor del estimador de beta_1
beta_1_H0=1 # valor de contraste en la hip�tesis nula
se_est_beta_1=summary(modelo)$coefficients[2,2] #error est�ndar del estimador
t0=(est_beta_1-beta_1_H0)/se_est_beta_1 #estad�stico de contraste t
t0 
qt(1-alfa/2,n-2) # valor cr�tico del contraste
2*(1-pt(abs(t0),length(Wife)-2)) # p-valor del contraste

# As� pues, parece que es posible descartar la hip�tesis nula H0:beta1=1,
# y, en tanto que el estimador puntual del par�metro es menor que 1, podemos
# tener cierta certeza en que la pendiente es realmente inferior a 1, por lo
# que las alturas de los c�nyuges tender�an a igualarse al crecer sus alturas.
#
# Para concluir, hagamos un breve diagn�stico del modelo. #

layout(matrix(1:4,ncol=4))
plot(modelo)

# Los gr�ficos muestran que los residuos no presentan ning�n patr�n que denote
# falta de aleatoriedad o de homocedasticidad, y parecen ajustarse muy bien al
# supuesto de normalidad. Tampoco parecen existir observaciones influyentes o
# at�picas. Por tanto, no hay raz�n para dudar del cumplimiento de los supuestos
# del modelo de regresi�n, y por tanto tampoco de las conclusiones anteriores. #

# Veamos un �ltimo ejemplo acerca de c�mo el an�lisis de los residuos puede dar
# pistas acerca de la no-validez del modelo de regresi�n. Los datos de este
# ejemplo registran la evoluci�n de la producci�n mundial de petr�leo desde
# 1880 hasta 1988 en miles de barriles diarios, y se encuentran en el archivo
# Petroleo.txt. 
#
# Carguemos los datos. #

ruta="\\Petroleo.txt"
datos=read.table(ruta,header=T)
str(datos)
attach(datos)

# Hagamos un breve an�lisis descriptivo de la relaci�n entre las variables, Year 
# (a�o) y MBBL (producci�n en miles de barriles diarios). #

layout(1)
plot(MBBL ~ Year)
cor(MBBL,Year)

# Como vemos, la correlaci�n lineal entre ambas variables es bastante fuerte,
# aunque se puede apreciar a la vez una tendencia no lineal, de tipo
# exponencial, al menos en el periodo hasta los a�os 70 del siglo pasado.
#
# No obstante, hagamos un primera prueba suponiendo una relaci�n lineal entre
# ambas variables. #

modelo=lm(MBBL ~ Year)
summary(modelo)

# El modelo es fuertemente significativo, con p-valores muy pr�ximos a 0, y sin
# mayor informaci�n podr�amos estar tentados a concluir que la tendencia de la
# producci�n es lineal, estimando en unos 245.91 Mbbl el aumento de producci�n
# anual. 
#
# Sin embargo, antes de validar las conclusiones es importante realizar el 
# diagn�stico del modelo. Antes de ello, representemos la recta de regresi�n
# que hemos estimado. Usaremos para esto la funci�n abline(), que permite 
# a�adir una recta a un gr�fico ya existente. Esta funci�n tiene un m�todo para
# interpretar la salida de la funci�n lm(), por lo que esta representaci�n es 
# directa. #

plot(MBBL ~ Year)
abline(lm(MBBL ~ Year))

# Como vemos, la recta en realidad no realiza un ajuste demasiado bueno, ya que
# tiende a subestimar la producci�n en los primeros a�os hasta 1910, para luego
# sobreestimarla en el periodo central desde 1920 hasta 1970, y volver a 
# subestimarla a partir de ah�.
#
# Veamos c�mo se refleja esto en los residuos del modelo. #

layout(matrix(1:4,ncol=4))
plot(modelo)

# En el primer gr�fico es posible apreciar claramente que el patr�n que siguen 
# los residuos no es aleatorio, sino que siguen primero una tendencia decreciente
# y luego creciente. Esto es una fuerte indicaci�n de que el error no tiene
# realmente un papel aleatorio en el modelo, sino que se dedica a corregir una
# mala especificaci�n de este. En otras palabras, la relaci�n entre las variables
# no es lineal, y por tanto el primer supuesto, de linealidad, no se estar�a 
# cumpliendo, lo que invalida casi totalmente las conclusiones que podamos 
# extraer del modelo anterior.
#
# Para corregir este problema, introduzcamos un modelo exponencial de la 
# relaci�n entre las variables. Para ello, se ha de ajustar un modelo lineal
# al logaritmo de la variable respuesta, de modo que al tomar la exponencial
# del modelo lineal resultante se obtendr� el modelo exponencial requerido. #

modelo=lm(log(MBBL) ~ Year)
summary(modelo)

# El modelo que se obtiene es m�s significativo aun que el anterior, con 
# p-valores pr�cticamente nulos, y una proporci�n de variabilidad explicada de 
# la producci�n del 98.34%, cuando el modelo lineal anterior alcanzaba a 
# explicar un 79.97%. 
#
# �C�mo obtenemos ahora la estimaci�n del incremento anual de la producci�n?
# N�tese que ahora, como el modelo ajusta el logaritmo de la producci�n,
# el valor del par�metro asociado a Year, que se estima en 0.0616, informa del 
# aumento anual de ese logaritmo. El incremento de la producci�n se estima
# entonces tomando la exponencial de este par�metro. #

exp(modelo$coefficients[2])

# Es importante ver que ahora este incremento interviene de manera 
# multiplicativa, no aditiva. Esto es, se estima que cada a�o la producci�n se
# incrementa en un factor de 1.0635, es decir, un 6.35% respecto a la producci�n
# del a�o anterior.
#
# Visualizemos el ajuste proporcionado por este modelo. Para ello, tenemos que
# a�adir sobre el diagrama de dispersi�n de las observaciones la curva 
# exponencial cuyos par�metros acabamos de estimar. 

layout(1)
plot(MBBL ~ Year)

# Ahora no disponemos de una funci�n que interprete la salida de lm() (que es un
# modelo lineal aunque se haya hecho sobre el logaritmo de la variable respuesta)
# como un curva exponencial en lugar de una recta. Por ello, ser� necesario usar
# el modelo exponencial obtenido para calcular sus valores ajustados en una  
# sucesi�n de a�os en el periodo observado, de cara a aproximar gr�ficamente
# la curva exponencial del modelo mediante la funci�n lines(), que representa
# l�neas entre los pares de puntos que se le suministran sobre un gr�fico 
# anterior. #

years=seq(from=1880, to=1988, by=1) # a�os en que se calcula la exponencial
pred_MBBL=exp(modelo$coefficients[1]+modelo$coefficients[2]*years) # exponencial
lines(years,pred_MBBL) 

# Como vemos, el modelo ahora proporciona un ajuste mucho m�s satisfactorio
# del comportamiento hist�rico de la producci�n, al menos hasta comienzos de la
# decada de 1970.
#
# Realicemos el diagn�stico de este modelo. #

layout(matrix(1:4,ncol=4))
plot(modelo)

# El comportamiento de los residuos sigue ahora un patr�n mucho m�s aleatorio,
# aunque se aprecian algunas tendencias, sobre todo en el periodo final. Y es
# posible apreciar tambi�n un peor ajuste a la normalidad, con algunos puntos
# que parecen desviarse de manera sensible, y que corresponden a la primera
# y �ltimas observaciones. 
#
# Si estudiamos el ajuste exponencial del gr�fico anterior con la curva 
# exponencial, veremos que a partir de mediados de la d�cada de 1970 se produce
# una modificaci�n del patr�n exponencial de la producci�n, que conduce a la 
# aparici�n de varios de los residuos que aparecen se�alados en los gr�ficos de 
# diagn�stico. Si investigamos qu� ocurri� en estos a�os que pudiera afectar de
# ese modo el patr�n de producci�n mundial, encontraremos que una causa m�s que
# plausible se encuentra en la Guerra de Yom Kippur de 1973, que dio lugar a 
# interrupciones y restricciones en la producci�n de varios pa�ses �rabes.
#
# Teniendo esto en cuenta, lo m�s sensato es eliminar las observaciones 
# posteriores a ese a�o, ya que responden a un patr�n de producci�n diferente
# al del resto de observaciones. Estad�sticamente hablando dir�amos que esas 
# observaciones de la producci�n posteriores a 1973 proceden de una poblaci�n
# diferente a las anteriores.
#
# As� pues, procedamos a ajustar el modelo solo con las observaciones hasta
# 1973. #

select=Year <= 1973
modelo=lm(log(MBBL[select]) ~ Year[select])
summary(modelo)

# Este modelo ajusta aun mejor la producci�n en el nuevo periodo, ya que explica
# hasta un 99.55% de su variabilidad. Adem�s, el ajuste de la curva exponencial
# ahora no presenta apenas desviaciones. #

layout(1)
plot(MBBL[select] ~ Year[select])
pred_MBBL=exp(modelo$coefficients[1]+modelo$coefficients[2]*years[select])
lines(years[select],pred_MBBL)

# Procedamos al diagn�stico de este modelo. #

layout(matrix(1:4,ncol=4))
plot(modelo)

# Ahora es m�s dif�cil observar un patr�n claro en el comportamiento de los 
# residuos, y su ajuste a la normalidad parece algo m�s satisfactorio. No 
# obstante, el �ltimo de los gr�ficos de diagn�stico nos advierte que podr�a 
# existir una observaci�n influyente, la primera, del a�o 1880. Al encontrarse
# en un extremo del periodo observado, este a�o est� alejado de la media del 
# periodo, por lo que su capacidad de apalancamiento es alta, y su efecto puede
# ser importante en tanto tambi�n ese a�o est� asociado a un residuo relativamente
# grande.
# 
# Como una �ltima prueba, ajustemos un modelo retirando tambi�n la primera
# observaci�n. #

select=Year <= 1973 & Year > 1880
modelo=lm(log(MBBL[select]) ~ Year[select])
summary(modelo)

# La capacidad explicativa del modelo aumenta hasta un 99.63% #

layout(1)
plot(MBBL[select] ~ Year[select])
pred_MBBL=exp(modelo$coefficients[1]+modelo$coefficients[2]*years[select])
lines(years[select],pred_MBBL)

# El ajuste parece casi perfecto. #

layout(matrix(1:4,ncol=4))
plot(modelo)

# El comportamiento de los residuos ya es casi totalmente satisfactorio, aunque
# algunos no terminen de ajustarse del todo a la normalidad. Estos son en buena
# parte procedentes de la d�cada de 1930, que sufri� los efectos del crack 
# burs�til de 1929. 
#
# Por tanto, podemos confiar en que hemos ajustado un modelo adecuado a los datos,
# y que las conclusiones de este son cre�bles. Para concluir, veamos cual es la
# estimaci�n del incremento anual de la producci�n durante el periodo finalmente 
# ajustado, entre 1890 y 1973. #

exp(modelo$coefficients[2]) # El incremento anual se estima en un 6.73%
