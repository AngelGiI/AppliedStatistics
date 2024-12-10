################################################################################
######                                                                    ######
######                      REGRESIÓN LINEAL CON R                        ######
######                                                                    ######
######                       ESTADÍSTICA APLICADA                         ######
######               GRADO EN INGENIERÍA MATEMÁTICA 2020/2021             ######
######                                                                    ######
################################################################################

#        ESTE SCRIPT HA SIDO REALIZADO POR TINGUARO RODRÍGUEZ (2021)           #

# En este script vamos a estudiar cómo realizar análisis de regresión lineal
# mediante R. 
#
# Veamos algunos ejemplos de aplicación de modelos de regresión lineal simple. 
#
# En primer lugar, trabajaremos con unos datos, Ordenadores.txt, en los que se
# registra el tiempo de reparación en minutos (Minutes) de diferentes números de 
# ordenadores (Units). La idea es obtener una función que relacione 
# cuantitativamente el tiempo que se ha de emplear en la reparación con el 
# número de unidades a reparar. Una vez ajustado, un modelo de este tipo 
# facilita dos tareas: i) explicar la dependencia que tiene el tiempo de 
# reparación del número de unidades, y en particular estimar el tiempo medio que
# se tarda en reparar una unidad así como el tiempo de "set-up" o de preparación
# antes de iniciar un proceso de reparación; ii) predecir el tiempo que se ha de 
# emplear en la reparación de un número cualquiera de unidades, lo que puede
# ser útil por ejemplo de cara a elaborar un presupuesto del coste de un proceso
# de reparación. #
#
# Comencemos por la carga de los datos a partir del archivo Ordenadores.txt. #

ruta="\\Ordenadores.txt"
datos=read.table(ruta,header=T)
str(datos)
attach(datos)

# Antes del ajuste del modelo, conviene siempre ojear un poco los datos, y en 
# particular representarlos gráficamente. Así pues, realicemos un breve
# análisis descriptivo de las variables o vectores de datos de este ejemplo. #

boxplot(Minutes)
summary(Minutes)

boxplot(Units)
summary(Units)

# Ambas distribuciones parecen relativamente equilibradas, sin fuertes 
# asimetrías y sin presencia de datos extremos o atípicos.
#
# Una vez estudiadas las variables por separado, conviene representarlas 
# conjuntamente mediante un diagrama de dispersión para comprobar si es realista
# establecer una relación lineal entre ellas.

plot(Minutes ~ Units)

# Este gráfico muestra que los datos parecen agruparse bastante bien alrededor
# de una recta. Para confirmar este punto, obtengamos la correlación entre
# tiempos de reparación y unidades a reparar. #

cor(Minutes,Units)

# La correlación obtenida es positiva y muy próxima a 1, lo que es indicador de 
# una relación lineal directa muy fuerte entre ambas variables. 
# 
# En tanto el análisis descriptivo indica que es apropiado establecer un modelo
# lineal para relacionar ambas variables, podemos proceder a su ajuste. #

modelo=lm(Minutes ~ Units)
summary(modelo)

# Nótese que al aplicar summary() al objeto devuelto por lm() se obtiene un
# resultado más informativo que antes, en el que se especifica no solo el valor
# de las estimaciones de los parámetros, sino también el error estándar (es 
# decir, la desviación típica) de estos estimadores, el estadístico de 
# contraste t para contrastar la hipótesis nula de que el parámetro 
# correspondiente vale 0 así como el p-valor asociado a ese estadístico. 
#
# En concreto, en este caso el resultado nos informa de que la estimación de
# beta0 es 4.162, y la de beta1 es 15.509. Podemos interpretar estos valores de
# la siguiente manera: 
#
# 1) En tanto beta1 representa la pendiente de la recta, el valor de su 
# estimación da la tasa de intercambio entre unidades de la variable explicativa
# y de la respuesta o, en otras palabras, cuánto aumenta el tiempo
# de reparación con cada unidad extra que se haya de reparar. En este caso,
# este tiempo se estima en unos 15 minutos por cada ordenador extra a reparar.
#
# 2) Por su parte, beta0 representa la ordenada en el origen de la recta de 
# regresión, y como tal puede ser interpretada como el valor a esperar de la 
# respuesta cuando la variable explicativa toma el valor 0, o de manera más 
# general como un nivel "base" de la variable respuesta. En nuestro caso, la 
# estimación de beta0 puede interpretarse como un tiempo de set-up o preparación
# para realizar la reparación, el mismo independientemente del número de 
# ordenadores a reparar, que en este caso sería de unos 4 minutos.
#
# No obstante, se ha de ser precavido con la interpretación del parámetro beta0, 
# puesto que podría no tener sentido que la variable explicativa tomara el 
# valor 0, y el valor de beta0 obtenido podría no tener sentido tampoco como
# nivel base de la respuesta. En este sentido, el parámetro beta0 podría no ser
# interpretable, más allá de proporcionar un mecanismo matemático para obtener
# una mejor aproximación de la dependencia entre la respuesta y la variable 
# explicativa.
#
# En relación a los contrastes de hipótesis que realiza lm(), se lleva a cabo
# un contraste t para cada parámetro estimado, contrastando si el parámetro es
# significativamente distinto de 0. En este caso, el parámetro beta1 asociado a 
# Units obtiene un p-valor muy bajo, por lo que podemos estar bastante seguros
# de que efectivamente el número de ordenadores a reparar influye en el tiempo
# que se ha de emplear en la reparación, con la tasa ya descrita. Es importante
# ver que el contraste sobre este parámetro es el que estudia la 
# significatividad del modelo de regresión, en el sentido de si este regresor
# es útil para explicar la respuesta.

# En cambio, el parámetro beta0 no es significativo, lo que debemos interpretar 
# como que no existe certeza en que el tiempo de set-up sea realmente diferente 
# de 0, esto es, podría ser que las reparaciones no tuvieran que emplear un 
# tiempo previo de preparación.
#
# Además, lm() realiza también un contraste F para validar globalmente la 
# significatividad del modelo. En este caso, al tener una única variable 
# explicativa, este contraste es equivalente al contraste t del parámetro beta1
# para Units. Nótese que el p-valor asociado a este contraste F es el mismo que
# el obtenido para el coeficiente de Units. Comprobemos además que al elevar
# el estadístico t al cuadrado se obtiene el valor del estadístico F. #

summary(modelo)[["fstatistic"]][1] # Valor del estadístico F
summary(modelo)[["coefficients"]][2,3] # Valor del estadísto t para beta1
summary(modelo)[["coefficients"]][2,3]^2 # t^2=F

# Una última información relevante proporcionada por summary() es el R-cuadrado,
# que indica la proporción de variabilidad de la respuesta explicada por el
# modelo de regresión. En este caso, se obtiene el valor 0.9874, por lo que el
# modelo que hemos ajustado estaría explicando un 98.74% de la variabilidad de
# los tiempos de reparación, o en otras palabras, el número de ordenadores a 
# reparar explica un 98.74% del tiempo empleado para la reparación.
# 
# Como sucedía con aov(), la función lm() devuelve un objeto con múltiples
# componentes, desde la estimación de los parámetros a los valores ajustados
# o los residuos. A estas componentes se accede de manera similar a cómo se
# hacía con aov(), esto es, usando el operador $ después del nombre del objeto
# en que se ha guardado el resultado de lm().
#
# Por ejemplo, es posible obtener los tiempos de reparación predichos por el 
# modelo para los números de unidades a reparar observados como sigue. #

matrix(cbind(Units,modelo$fitted.values),byrow = T, nrow=2)

# Como en el caso del ANOVA para análisis de experimentos, los modelos de 
# regresión lineal se basan en una serie de supuestos que es importante 
# verificar si se cumplen en el caso bajo estudio. La manera más extendida de
# realizar este diagnóstico del modelo es mediante el análisis gráfico de los
# residuos obtenidos. R proporciona los gráficos necesarios aplicando la función
# genérica plot() sobre el objeto devuelto por lm(). #

plot(modelo)

# El primer gráfico representa los residuos contra los valores predichos. Este
# gráfico permite estudiar la validez del primer supuesto de linealidad del 
# modelo. Si el modelo es válido, los residuos han de presentar un comportamiento 
# aleatorio, sin patrones aparentes. Esto parece cumplirse en este caso. Además,
# este gráfico también puede darnos una idea de si se cumple el supuesto de 
# homocedasticidad, esto es, que los errores tengan una dispersión similar para
# todos los valores de la respuesta, lo cual también parece cumplirse en este 
# caso.
#
# El segundo gráfico, de probabilidad normal, nos permite contrastar el supuesto
# de normalidad de los errores. En este caso parece cumplirse relativamente bien,
# quizás con una única observación más extrema de lo habitual.
#
# El tercer gráfico estudia la posible dependencia de la magnitud de los 
# residuos con el valor de la respuesta, lo cual permite indagar mejor en la 
# validez del supuesto de homocedasticidad. Reafirmando lo ya observado en el 
# primer gráfico, no se aprecia ningún patrón creciente o decreciente en esta
# magnitud al variar el valor de la respuesta, por lo que podemos descartar el
# incumplimiento de este supuesto.
#
# Finalmente, el cuarto gráfico permite detectar la posible presencia de 
# observaciones influyentes. Para ello, debemos comprobar si existe algún dato
# con una distancia de Cook mayor que 1, lo que no sucede en nuestro caso. Por
# tanto, no se tendrían observaciones problemáticas que influyan de manera
# desmedida en el modelo obtenido.
#
# En conclusión, el modelo de regresión lineal simple que se ha ajustado 
# proporciona una aproximación adecuada a la relación entre tiempos de 
# reparación y unidades a reparar, por lo que puede ser usado con propósitos
# predictivos o explicativos.
#
# Por ejemplo, supongamos que se quiere automatizar la realización de 
# presupuestos para la reparación de cualquier número de unidades, donde el 
# coste de una hora de trabajo del técnico es de 50???. Así pues, podríamos 
# estimar el coste de reparar por ejemplo 7 ordenadores como sigue. #

coste_hora=50
unidades=7
(coste_hora/60)*(modelo$coefficients[1]+modelo$coefficients[2]*unidades)

# Pasemos ahora a otro ejemplo, en que los datos registran las alturas de 
# conyuges de una muestra de matrimonios heterosexuales. La pregunta que surge
# en este contexto es si las alturas de los conyuges están relacionadas.
#
# Carguemos los datos, que se encuentran en el archivo Matrimonios.txt. #

ruta="\\Matrimonios.txt"
datos=read.table(ruta,header=T)
str(datos)
attach(datos)

# Realicemos un breve análisis descriptivo de las alturas de esposos (Husband)
# y esposas (Wife). #

boxplot(Husband,Wife)
summary(Husband)
summary(Wife)

plot(Husband ~ Wife)
cor(Husband,Wife)

# Este análisis descriptivo muestra que ambas alturas parecen estar relacionadas
# de manera directa. Para dilucidar esta cuestión de manera cuantitativa, lo
# mejor es ajustar un modelo de regresión a los datos. #

modelo=lm(Husband ~ Wife)
summary(modelo)

# Estos resultados muestran que ambas alturas están fuertemente relacionadas, y
# en particular que la altura de la esposa es un buen predictor de la altura del
# esposo, ya que se obtiene un p-valor muy próximo a 0 (2e-16) para esa variable.
# La presencia de una constante (Intercept) diferente de 0 en el modelo también 
# es significativa, aunque con un p-valor no tan pequeño (0.002). El test F
# conduce a la misma conclusión de significatividad del modelo, aunque como 
# sabemos este contraste F es equivalente al contraste t para la única variable
# en el modelo, por lo que en este caso no aporta realmente más información. 
# Obsérvese que los p-valores de ambos test son iguales (2e-16) y que el 
# cuadrado del estadístico t para Wife es igual al valor del estadístico F. #

summary(modelo)$coefficients[2,3]
summary(modelo)$coefficients[2,3]^2

# Una vez vista la significatividad del modelo, pasemos a su interpretación.
# La estimación del parámetro beta_1, que mide el efecto de la variable
# predictora (Wife) sobre la respuesta (Husband), es de 0.83 cm./cm., de modo 
# que se estima que si una esposa es 1 cm. más alta que otra, en media tendrá un
# esposo 0.83 cm. más alto. 
#
# Esto podría llevar a concluir que, a medida que aumenta la altura de la esposa, 
# menor será la diferencia en altura entre ambos cónyuges, ya que la pendiente
# parece ser menor que 1. Pero, en realidad, estos resultados solo nos aseguran 
# que esta pendiente beta_1 es diferente (y mayor) de 0, pues el valor de 0.83
# es solo una estimación puntual de beta_1, y el contraste que se ha efectuado
# sobre este parámetro a partir de ese estimador es el de si beta_1 es diferente
# de 0. 
#
# Para estudiar la cuestión de si la pendiente es menor o, en general, diferente
# de 1, tenemos que realizar otro contraste t, bajo la hipótesis nula de que 
# beta_1 = 1, y ver qué ocurre.

n=length(Wife) # tamaño muestral, número de observaciones
alfa=0.05 # nivel de significación
est_beta_1=modelo$coefficients[2] # valor del estimador de beta_1
beta_1_H0=1 # valor de contraste en la hipótesis nula
se_est_beta_1=summary(modelo)$coefficients[2,2] #error estándar del estimador
t0=(est_beta_1-beta_1_H0)/se_est_beta_1 #estadístico de contraste t
t0 
qt(1-alfa/2,n-2) # valor crítico del contraste
2*(1-pt(abs(t0),length(Wife)-2)) # p-valor del contraste

# Así pues, parece que es posible descartar la hipótesis nula H0:beta1=1,
# y, en tanto que el estimador puntual del parámetro es menor que 1, podemos
# tener cierta certeza en que la pendiente es realmente inferior a 1, por lo
# que las alturas de los cónyuges tenderían a igualarse al crecer sus alturas.
#
# Para concluir, hagamos un breve diagnóstico del modelo. #

layout(matrix(1:4,ncol=4))
plot(modelo)

# Los gráficos muestran que los residuos no presentan ningún patrón que denote
# falta de aleatoriedad o de homocedasticidad, y parecen ajustarse muy bien al
# supuesto de normalidad. Tampoco parecen existir observaciones influyentes o
# atípicas. Por tanto, no hay razón para dudar del cumplimiento de los supuestos
# del modelo de regresión, y por tanto tampoco de las conclusiones anteriores. #

# Veamos un último ejemplo acerca de cómo el análisis de los residuos puede dar
# pistas acerca de la no-validez del modelo de regresión. Los datos de este
# ejemplo registran la evolución de la producción mundial de petróleo desde
# 1880 hasta 1988 en miles de barriles diarios, y se encuentran en el archivo
# Petroleo.txt. 
#
# Carguemos los datos. #

ruta="\\Petroleo.txt"
datos=read.table(ruta,header=T)
str(datos)
attach(datos)

# Hagamos un breve análisis descriptivo de la relación entre las variables, Year 
# (año) y MBBL (producción en miles de barriles diarios). #

layout(1)
plot(MBBL ~ Year)
cor(MBBL,Year)

# Como vemos, la correlación lineal entre ambas variables es bastante fuerte,
# aunque se puede apreciar a la vez una tendencia no lineal, de tipo
# exponencial, al menos en el periodo hasta los años 70 del siglo pasado.
#
# No obstante, hagamos un primera prueba suponiendo una relación lineal entre
# ambas variables. #

modelo=lm(MBBL ~ Year)
summary(modelo)

# El modelo es fuertemente significativo, con p-valores muy próximos a 0, y sin
# mayor información podríamos estar tentados a concluir que la tendencia de la
# producción es lineal, estimando en unos 245.91 Mbbl el aumento de producción
# anual. 
#
# Sin embargo, antes de validar las conclusiones es importante realizar el 
# diagnóstico del modelo. Antes de ello, representemos la recta de regresión
# que hemos estimado. Usaremos para esto la función abline(), que permite 
# añadir una recta a un gráfico ya existente. Esta función tiene un método para
# interpretar la salida de la función lm(), por lo que esta representación es 
# directa. #

plot(MBBL ~ Year)
abline(lm(MBBL ~ Year))

# Como vemos, la recta en realidad no realiza un ajuste demasiado bueno, ya que
# tiende a subestimar la producción en los primeros años hasta 1910, para luego
# sobreestimarla en el periodo central desde 1920 hasta 1970, y volver a 
# subestimarla a partir de ahí.
#
# Veamos cómo se refleja esto en los residuos del modelo. #

layout(matrix(1:4,ncol=4))
plot(modelo)

# En el primer gráfico es posible apreciar claramente que el patrón que siguen 
# los residuos no es aleatorio, sino que siguen primero una tendencia decreciente
# y luego creciente. Esto es una fuerte indicación de que el error no tiene
# realmente un papel aleatorio en el modelo, sino que se dedica a corregir una
# mala especificación de este. En otras palabras, la relación entre las variables
# no es lineal, y por tanto el primer supuesto, de linealidad, no se estaría 
# cumpliendo, lo que invalida casi totalmente las conclusiones que podamos 
# extraer del modelo anterior.
#
# Para corregir este problema, introduzcamos un modelo exponencial de la 
# relación entre las variables. Para ello, se ha de ajustar un modelo lineal
# al logaritmo de la variable respuesta, de modo que al tomar la exponencial
# del modelo lineal resultante se obtendrá el modelo exponencial requerido. #

modelo=lm(log(MBBL) ~ Year)
summary(modelo)

# El modelo que se obtiene es más significativo aun que el anterior, con 
# p-valores prácticamente nulos, y una proporción de variabilidad explicada de 
# la producción del 98.34%, cuando el modelo lineal anterior alcanzaba a 
# explicar un 79.97%. 
#
# ¿Cómo obtenemos ahora la estimación del incremento anual de la producción?
# Nótese que ahora, como el modelo ajusta el logaritmo de la producción,
# el valor del parámetro asociado a Year, que se estima en 0.0616, informa del 
# aumento anual de ese logaritmo. El incremento de la producción se estima
# entonces tomando la exponencial de este parámetro. #

exp(modelo$coefficients[2])

# Es importante ver que ahora este incremento interviene de manera 
# multiplicativa, no aditiva. Esto es, se estima que cada año la producción se
# incrementa en un factor de 1.0635, es decir, un 6.35% respecto a la producción
# del año anterior.
#
# Visualizemos el ajuste proporcionado por este modelo. Para ello, tenemos que
# añadir sobre el diagrama de dispersión de las observaciones la curva 
# exponencial cuyos parámetros acabamos de estimar. 

layout(1)
plot(MBBL ~ Year)

# Ahora no disponemos de una función que interprete la salida de lm() (que es un
# modelo lineal aunque se haya hecho sobre el logaritmo de la variable respuesta)
# como un curva exponencial en lugar de una recta. Por ello, será necesario usar
# el modelo exponencial obtenido para calcular sus valores ajustados en una  
# sucesión de años en el periodo observado, de cara a aproximar gráficamente
# la curva exponencial del modelo mediante la función lines(), que representa
# líneas entre los pares de puntos que se le suministran sobre un gráfico 
# anterior. #

years=seq(from=1880, to=1988, by=1) # años en que se calcula la exponencial
pred_MBBL=exp(modelo$coefficients[1]+modelo$coefficients[2]*years) # exponencial
lines(years,pred_MBBL) 

# Como vemos, el modelo ahora proporciona un ajuste mucho más satisfactorio
# del comportamiento histórico de la producción, al menos hasta comienzos de la
# decada de 1970.
#
# Realicemos el diagnóstico de este modelo. #

layout(matrix(1:4,ncol=4))
plot(modelo)

# El comportamiento de los residuos sigue ahora un patrón mucho más aleatorio,
# aunque se aprecian algunas tendencias, sobre todo en el periodo final. Y es
# posible apreciar también un peor ajuste a la normalidad, con algunos puntos
# que parecen desviarse de manera sensible, y que corresponden a la primera
# y últimas observaciones. 
#
# Si estudiamos el ajuste exponencial del gráfico anterior con la curva 
# exponencial, veremos que a partir de mediados de la década de 1970 se produce
# una modificación del patrón exponencial de la producción, que conduce a la 
# aparición de varios de los residuos que aparecen señalados en los gráficos de 
# diagnóstico. Si investigamos qué ocurrió en estos años que pudiera afectar de
# ese modo el patrón de producción mundial, encontraremos que una causa más que
# plausible se encuentra en la Guerra de Yom Kippur de 1973, que dio lugar a 
# interrupciones y restricciones en la producción de varios países árabes.
#
# Teniendo esto en cuenta, lo más sensato es eliminar las observaciones 
# posteriores a ese año, ya que responden a un patrón de producción diferente
# al del resto de observaciones. Estadísticamente hablando diríamos que esas 
# observaciones de la producción posteriores a 1973 proceden de una población
# diferente a las anteriores.
#
# Así pues, procedamos a ajustar el modelo solo con las observaciones hasta
# 1973. #

select=Year <= 1973
modelo=lm(log(MBBL[select]) ~ Year[select])
summary(modelo)

# Este modelo ajusta aun mejor la producción en el nuevo periodo, ya que explica
# hasta un 99.55% de su variabilidad. Además, el ajuste de la curva exponencial
# ahora no presenta apenas desviaciones. #

layout(1)
plot(MBBL[select] ~ Year[select])
pred_MBBL=exp(modelo$coefficients[1]+modelo$coefficients[2]*years[select])
lines(years[select],pred_MBBL)

# Procedamos al diagnóstico de este modelo. #

layout(matrix(1:4,ncol=4))
plot(modelo)

# Ahora es más difícil observar un patrón claro en el comportamiento de los 
# residuos, y su ajuste a la normalidad parece algo más satisfactorio. No 
# obstante, el último de los gráficos de diagnóstico nos advierte que podría 
# existir una observación influyente, la primera, del año 1880. Al encontrarse
# en un extremo del periodo observado, este año está alejado de la media del 
# periodo, por lo que su capacidad de apalancamiento es alta, y su efecto puede
# ser importante en tanto también ese año está asociado a un residuo relativamente
# grande.
# 
# Como una última prueba, ajustemos un modelo retirando también la primera
# observación. #

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
# parte procedentes de la década de 1930, que sufrió los efectos del crack 
# bursátil de 1929. 
#
# Por tanto, podemos confiar en que hemos ajustado un modelo adecuado a los datos,
# y que las conclusiones de este son creíbles. Para concluir, veamos cual es la
# estimación del incremento anual de la producción durante el periodo finalmente 
# ajustado, entre 1890 y 1973. #

exp(modelo$coefficients[2]) # El incremento anual se estima en un 6.73%
