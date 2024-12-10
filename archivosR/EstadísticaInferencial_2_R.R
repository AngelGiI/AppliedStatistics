################################################################################
######                                                                    ######
######                ESTADÍSTICA INFERENCIAL CON R (2)                   ######
######                                                                    ######
######                       ESTADÍSTICA APLICADA                         ######
######               GRADO EN INGENIERÍA MATEMÁTICA 2020/2021             ######
######                                                                    ######
################################################################################

#               SCRIPT REALIZADO POR TINGUARO RODRÍGUEZ                   #     

# El uso práctico de los contrastes de hipótesis conlleva razonar sobre la 
# capacidad del test para detectar diferencias entre el valor de contraste y la
# media real y controlar esta capacidad en base a la elección de un tamaño 
# muestral adecuado. 
#
# En este sentido, además de la probabilidad de error de tipo I, alfa, que es
# fijada por el experimentador o más generalmente por el contexto de aplicación,
# es necesario controlar la probabilidad de error de tipo II, beta, que depende
# i) del tamaño de la diferencia entre la media real y la media contrastada en
# unidades de desviación típica; ii) del tamaño de la muestra con que se realiza
# el contraste; y iii) del riesgo alfa que se desea asumir. 
#
# Una herramienta adecuada para entender y visualizar esta relación entre los
# diversos parámetros que intervienen en un contraste son las llamadas curvas
# de potencia, que reflejan la probabilidad de rechazar la hipótesis nula H0 
# en función del tamaño de la diferencia entre el valor asumido en H0 y el
# valor real del parámetro. Nótese que cuando la diferencia es 0 la hipótesis
# nula se está cumpliendo, y por tanto la probabilidad de rechazar H0 en este 
# caso corresponde con la probabilidad alfa de error de tipo I. Por contra,
# cuando la magnitud de la diferencia es mayor que 0, H0 es falsa, y la 
# probabilidad de rechazarla se conoce entonces como la potencia del contraste,
# que es igual a 1 - beta, esto es, 1 menos la probabilidad de error de tipo II
#
# Por tanto, una curva de potencia grafica la potencia del contraste en función
# de las diferencias entre la media contrastada y la media real.
#
# Vamos a construir una función para dibujar la curva de potencia del contraste
# más sencillo, en el que se contrasta si la media de una población normal, con 
# desviación típica conocida y de la que se está observando una muestra de 
# tamaño n, es igual o no a un valor de contraste, usando un nivel de 
# significación alfa prefijado.
#
# Nótese el uso de la instrucción for para indicar la creación de un bucle,
# de if para indicar ejecución condicional y de la función lines() para dibujar
# una curva sobre un gráfico ya ejecutado. También hay que notar que se usa
# la función pnorm para calcular la función de distribución de una normal y
# qnorm para calcular cuantiles de esa distribución. #

dibujar_curva_potencia_I = function(media_test,n,alfa,desv,grafico,color){
  potencia=vector()
  i=0
  for (dif in seq(-3,+3,0.1)){
    i=i+1
    potencia[i]=1-(pnorm(qnorm(1-(alfa/2))-(abs(dif)/(desv/sqrt(n))))
                   -pnorm(qnorm(alfa/2)-(abs(dif)/(desv/sqrt(n)))))
  }
  if (grafico==0) plot(seq(media_test-3,media_test+3,0.1),potencia,
                      type="l",col=color,xlab="Media real",ylab="Potencia")
  else lines(seq(media_test-3,media_test+3,0.1),potencia,type="l",col=color)
}

# Usemos ahora esta función para dibujar la curva de potencia en una situación
# con varianza 1, media a contrastar 10, tamaño muestral 10 y un nivel de
# significación alfa de 0.05. #

desv=1
n=10
alfa=0.05
media_test=10
grafico=0
color=1
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

# Veamos a continuación el efecto de incrementar o disminuir la varianza
# de la distribución. #

desv=2
grafico=1
color=2
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

desv=0.5
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

# Así pues, una varianza mayor de la información disminuye la capacidad del 
# contraste para detectar una diferencia dada, mientras que una varianza menor
# la aumenta.
#
# Pasemos a observar el efecto del tamaño muestral n. #

desv=1
n=30
color=3
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

n=5
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

# Por tanto, aumentar el tamaño muestral nos permite detectar con mayor 
# probabilidad una diferencia dada, mientras que su disminución hace que la
# potencia decrezca.
#
# Finalmente, veamos el efecto de variar el nivel de significación alfa, esto
# es, veamos cómo la potencia del contraste depende del riesgo que esté
# dispuesto a asumir de rechazar H0 cuando sea cierta, o equivalentemente
# cuando la diferencia entre la media real y la contrastada es 0. #

n=10
alfa=0.01
color=4
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

alfa=0.1
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

# Así pues, al imponer un menor riesgo de error de tipo I se hace más complicado
# detectar una diferencia dada entre medias, mientras que asumir un mayor riesgo
# aumenta la potencia del contraste.

# Vamos ahora a ver cómo aproximar curvas de potencias de contrastes de 
# hipótesis más complejos. En particular, estudiaremos la estimación de la
# potencia de un contraste t mediante técnicas de simulación. Nótese que la
# diferencia entre un contraste normal como el aplicado en los ejemplos 
# anteriores y un contraste t para una única muestra es que el primero procede
# bajo el supuesto de que la varianza o desviacion típica de la población
# es conocida, mientras que en el segundo esta varianza se asume desconocida
# y se emplea en el contraste una estimación. Esto hace que la distribución
# del estadístico de contraste pase de ser normal a ser t de Student con 
# n-1 grados de libertad, donde n es el tamaño de la muestra usado.
#
# Para un contraste t, es posible obtener fórmulas que especifiquen el valor
# exacto de la potencia en función del tamaño muestral, la diferencia entre la 
# media real y la constrastada y la desviación típica real. Sin embargo, esto 
# requiere emplear algunas distribuciones complejas y cierto desarrollo 
# teórico, que además solo será válido para este tipo de contraste. Por ello,
# lo que estudiaremos será como aproximar la curva de potencia usando una
# estrategia de simulación.
#
# Esta estrategia de simulación se basa en simular muestras provenientes de una
# distribución normal con los parámetros requeridos, y obtener una estimación de
# la potencia a partir de la proporción de muestras en que se rechaza la 
# hipótesis nula usando el contraste t correspondiente. 
#
# Construyamos entonces una función que estime la potencia del contraste t
# cuando la distribución de los datos tiene una media dada que puede ser
# diferente del valor que se contrasta, usando un para ello muestras simuladas
# generadas mediante un generador de números aleatorios. #

estimar_prob_rechazar_H0=function(media,desv,n,muestras,alfa,media_test){
  resultado=vector()
  for (i in 1:muestras){
    resultado[i]=ifelse(t.test(rnorm(n,mean=media,sd=desv),
                               mu=media_test)$p.value<alfa,1,0)
  }
  total=sum(resultado)
  prob_rechazar_H0=total/muestras; 
  return(prob_rechazar_H0)
}

# Así, por ejemplo podemos estimar la potencia del test t para rechazar la 
# hipótesis nula cuando se contrasta si la media es igual a 10 a partir de datos
# provinientes de una distribución normal con media 9 y varianza 1, usando 100
# muestras de tamaño 10 y un nivel de significación alfa=0.05. #

media=9
desv=1
media_test=10
muestras=100
n=10
alfa=0.05

estimar_prob_rechazar_H0(media,desv,n,muestras,alfa,media_test)

# ¿Obtenemos todos el mismo resultado?
#
# Aparte, una pregunta que debemos hacernos es la siguiente: si volvemos a 
# ejecutar esta función tal cual, ¿obtendremos el mismo resultado? #

estimar_prob_rechazar_H0(media,desv,n,muestras,alfa,media_test)

# ¿Por qué no se obtiene el mismo valor?













# La clave está en la generación de números aleatorios, que produce muestras
# diferentes al volver a llamar a la función anterior. Para garantizar que
# los resultados sean los mismos, debemos asignar una semilla idéntica para 
# el generador de números aleatorios. Este se realiza mediante la función
# set.seed(). #

semilla=123
set.seed(semilla)
estimar_prob_rechazar_H0(media,desv,n,muestras,alfa,media_test)
set.seed(semilla)
estimar_prob_rechazar_H0(media,desv,n,muestras,alfa,media_test)

# Una vez tenemos una manera de estimar la potencia para un contraste particular
# podemos realizar una función para estimar la curva de potencia del contraste t
# al variar la diferencia entre la media real y la contrastada. #

dibujar_curva_potencia_II = function(media_test,n,alfa,desv,muestras,
                                     grafico,color){
  potencia=vector()
  medias=vector()
  i=0
  for (dif in seq(-3,3,0.1)){
    i=i+1
    medias[i]=media_test+dif
    potencia[i]=estimar_prob_rechazar_H0(medias[i],desv,n,muestras,alfa,
                                           media_test)
  }
  if (grafico==0) plot(medias,potencia,type="l",col=color,
                       xlab="Media real",ylab="Potencia")
  else lines(medias,potencia,type="l",col=color)
}

# Con esta función, podemos entonces realizar una primera curva de potencia
# para una situación como la del ejemplo anterior. #

set.seed(semilla)
grafico=0
color=1
dibujar_curva_potencia_II(media_test,n,alfa,desv,muestras,grafico,color)

# Como vemos, la curva no tiene una forma "perfecta" en el sentido de que es
# solo una aproximación a la curva real. Para aumentar la calidad de la 
# aproximación, debe aumentarse el número de simulaciones empleadas, esto es,
# el número de muestras. Esto tendrá el efecto negativo de emplear mayor tiempo
# de computación, que es el precio a pagar por usar una metodología de 
# simulación exigiendo un grado de aproximación mayor. #

muestras=1000
set.seed(semilla)
grafico=1
color=2
dibujar_curva_potencia_II(media_test,n,alfa,desv,muestras,grafico,color)

# Vamos ahora a comparar la curva de potencia del contraste t con la del
# contraste normal empleado en los ejemplos anteriores. 

desv=1
n=10
alfa=0.05
media_test=10
grafico=0
color=1
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

set.seed(semilla)
grafico=1
color=2
dibujar_curva_potencia_II(media_test,n,alfa,desv,muestras,grafico,color)

# Como se puede apreciar, el precio a pagar por no conocer la varianza real de
# la población es una disminución de la potencia para una diferencia dada. Esto
# es así ya que la distribución t tiene más probabilidad en las colas que la
# distribución normal, lo que exige valores algo mayores del estadístico de
# contraste t para obtener el mismo p-valor que el contraste normal. 
#
# Veamos que, al aumentar el tamaño de la muestra n, la potencia del contraste
# t tiende a ser el mismo que el del contraste normal. #

n=2
grafico=0
color=1
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

n=2
muestras=2500
set.seed(semilla)
grafico=1
color=2
dibujar_curva_potencia_II(media_test,n,alfa,desv,muestras,grafico,color)

n=10
color=3
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

muestras=1000
set.seed(semilla)
color=4
dibujar_curva_potencia_II(media_test,n,alfa,desv,muestras,grafico,color)

n=30
color=5
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

muestras=500
set.seed(semilla)
color=6
dibujar_curva_potencia_II(media_test,n,alfa,desv,muestras,grafico,color)

# Lo que se observa es que, al aumentar n, la diferencia entre la potencia del
# contraste t y la del normal para ese tamaño muestral tiende a decrecer. En 
# cierto modo, para n mayor a 30 la diferencia entre ambos test es reducida. #

# Finalmente, veremos cómo la metodología de simulación que se acaba de emplear
# para aproximar la curva de potencia es aplicable a otras tareas estadísticas.
# En particular, veremos que es posible aproximar el p-valor de un contraste a
# partir de observaciones generadas aleatoriamente. Con estas es entonces posible 
# simular repetidamente el estadístico de contraste, obteniendo una aproximación
# a su distribución a partir de la cual se pueden calcular p-valores aproximados.
# Esto es de utilidad cuando no sea posible conocer la distribución del 
# estadístico de contraste bajo la hipótesis nula, o cuando su aproximación
# mediante una determinada distribución límite pueda generar dudas.

# Para ilustrar esta cuestión, vamos a aproximar el p-valor del contraste t
# con una única muestra, cuando se desea contrastar si la media de una 
# población normal con varianza desconocida es igual a un valor de referencia
# mu_0. Para ello, generaremos diversas muestras de observaciones de esa población 
# normal bajo la hipótesis nula, y con ellas simularemos valores del estadistico de 
# contraste t_0. De este modo, se obtendrá una aproximación de la distribución de este
# estadístico bajo la hipótesis nula, con la que se podrá estimar un p-valor a partir
# del número de valores simulados de esta distribución que son más extremos que el 
# valor del estadístico de contraste obtenido para la muestra de prueba. Además, como 
# sabemos, el estadístico de contraste t_0 correspondiente se distribuye como una t de 
# Student, lo que nos permitirá comparar el p-valor aproximado con el p-valor real.

# Así pues, consideremos la hipótesis nula dada por mu_0, y generemos un número rep
# de muestras normales de tamaño n con media igual a mu_0 y cierta desviación típica
# desv. Usamos una función para obtener las rep simulaciones de t_0 y devolverlas
# en un vector ts.

aproxima_distrib=function(rep,n,mu_0,desv){
  ts=c()
  for (i in 1:rep) {
    x=rnorm(n,mean=mu_0,sd=desv)
    t0_simulado=(mean(x)-mu_0)/sqrt(var(x)/length(x))
    ts=c(ts,t0_simulado)
  }
  return(ts)
}

# Tomemos estos valores para los parámetros de la función.
rep=100
n=15
mu_0=0
desv=1

set.seed(0)

# Generamos la muestra de prueba sobre la que se realizará el contraste, y calculamos
# el valor del estadístico de contraste para el que queremos aproximar el p-valor.
media_real=0.5
y=rnorm(n,mean = media_real,sd=desv)
t0_prueba=(mean(y)-mu_0)/sqrt(var(y)/length(y))

# Comparamos el valor de t0_prueba con la distribución simulada de t0 bajo la hipótesis
# nula. El p-valor aproximado se obtiene entonces como la proporción de simulaciones
# que toma un valor más extremo que t0_prueba.
ts=aproxima_distrib(rep,n,mu_0,desv)
p=sum(abs(ts)>abs(t0_prueba))/rep
p

# Comparemos p con el valor teórico (real) del p-valor.
t.test(y)

# La calidad de la aproximación depende del número de simulaciones de la distribución 
# de t0 así como del tamaño muestral n.
