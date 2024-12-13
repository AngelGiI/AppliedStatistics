################################################################################
######                                                                    ######
######                ESTAD�STICA INFERENCIAL CON R (2)                   ######
######                                                                    ######
######                       ESTAD�STICA APLICADA                         ######
######               GRADO EN INGENIER�A MATEM�TICA 2020/2021             ######
######                                                                    ######
################################################################################

#               SCRIPT REALIZADO POR TINGUARO RODR�GUEZ                   #     

# El uso pr�ctico de los contrastes de hip�tesis conlleva razonar sobre la 
# capacidad del test para detectar diferencias entre el valor de contraste y la
# media real y controlar esta capacidad en base a la elecci�n de un tama�o 
# muestral adecuado. 
#
# En este sentido, adem�s de la probabilidad de error de tipo I, alfa, que es
# fijada por el experimentador o m�s generalmente por el contexto de aplicaci�n,
# es necesario controlar la probabilidad de error de tipo II, beta, que depende
# i) del tama�o de la diferencia entre la media real y la media contrastada en
# unidades de desviaci�n t�pica; ii) del tama�o de la muestra con que se realiza
# el contraste; y iii) del riesgo alfa que se desea asumir. 
#
# Una herramienta adecuada para entender y visualizar esta relaci�n entre los
# diversos par�metros que intervienen en un contraste son las llamadas curvas
# de potencia, que reflejan la probabilidad de rechazar la hip�tesis nula H0 
# en funci�n del tama�o de la diferencia entre el valor asumido en H0 y el
# valor real del par�metro. N�tese que cuando la diferencia es 0 la hip�tesis
# nula se est� cumpliendo, y por tanto la probabilidad de rechazar H0 en este 
# caso corresponde con la probabilidad alfa de error de tipo I. Por contra,
# cuando la magnitud de la diferencia es mayor que 0, H0 es falsa, y la 
# probabilidad de rechazarla se conoce entonces como la potencia del contraste,
# que es igual a 1 - beta, esto es, 1 menos la probabilidad de error de tipo II
#
# Por tanto, una curva de potencia grafica la potencia del contraste en funci�n
# de las diferencias entre la media contrastada y la media real.
#
# Vamos a construir una funci�n para dibujar la curva de potencia del contraste
# m�s sencillo, en el que se contrasta si la media de una poblaci�n normal, con 
# desviaci�n t�pica conocida y de la que se est� observando una muestra de 
# tama�o n, es igual o no a un valor de contraste, usando un nivel de 
# significaci�n alfa prefijado.
#
# N�tese el uso de la instrucci�n for para indicar la creaci�n de un bucle,
# de if para indicar ejecuci�n condicional y de la funci�n lines() para dibujar
# una curva sobre un gr�fico ya ejecutado. Tambi�n hay que notar que se usa
# la funci�n pnorm para calcular la funci�n de distribuci�n de una normal y
# qnorm para calcular cuantiles de esa distribuci�n. #

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

# Usemos ahora esta funci�n para dibujar la curva de potencia en una situaci�n
# con varianza 1, media a contrastar 10, tama�o muestral 10 y un nivel de
# significaci�n alfa de 0.05. #

desv=1
n=10
alfa=0.05
media_test=10
grafico=0
color=1
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

# Veamos a continuaci�n el efecto de incrementar o disminuir la varianza
# de la distribuci�n. #

desv=2
grafico=1
color=2
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

desv=0.5
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

# As� pues, una varianza mayor de la informaci�n disminuye la capacidad del 
# contraste para detectar una diferencia dada, mientras que una varianza menor
# la aumenta.
#
# Pasemos a observar el efecto del tama�o muestral n. #

desv=1
n=30
color=3
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

n=5
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

# Por tanto, aumentar el tama�o muestral nos permite detectar con mayor 
# probabilidad una diferencia dada, mientras que su disminuci�n hace que la
# potencia decrezca.
#
# Finalmente, veamos el efecto de variar el nivel de significaci�n alfa, esto
# es, veamos c�mo la potencia del contraste depende del riesgo que est�
# dispuesto a asumir de rechazar H0 cuando sea cierta, o equivalentemente
# cuando la diferencia entre la media real y la contrastada es 0. #

n=10
alfa=0.01
color=4
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

alfa=0.1
dibujar_curva_potencia_I(media_test,n,alfa,desv,grafico,color)

# As� pues, al imponer un menor riesgo de error de tipo I se hace m�s complicado
# detectar una diferencia dada entre medias, mientras que asumir un mayor riesgo
# aumenta la potencia del contraste.

# Vamos ahora a ver c�mo aproximar curvas de potencias de contrastes de 
# hip�tesis m�s complejos. En particular, estudiaremos la estimaci�n de la
# potencia de un contraste t mediante t�cnicas de simulaci�n. N�tese que la
# diferencia entre un contraste normal como el aplicado en los ejemplos 
# anteriores y un contraste t para una �nica muestra es que el primero procede
# bajo el supuesto de que la varianza o desviacion t�pica de la poblaci�n
# es conocida, mientras que en el segundo esta varianza se asume desconocida
# y se emplea en el contraste una estimaci�n. Esto hace que la distribuci�n
# del estad�stico de contraste pase de ser normal a ser t de Student con 
# n-1 grados de libertad, donde n es el tama�o de la muestra usado.
#
# Para un contraste t, es posible obtener f�rmulas que especifiquen el valor
# exacto de la potencia en funci�n del tama�o muestral, la diferencia entre la 
# media real y la constrastada y la desviaci�n t�pica real. Sin embargo, esto 
# requiere emplear algunas distribuciones complejas y cierto desarrollo 
# te�rico, que adem�s solo ser� v�lido para este tipo de contraste. Por ello,
# lo que estudiaremos ser� como aproximar la curva de potencia usando una
# estrategia de simulaci�n.
#
# Esta estrategia de simulaci�n se basa en simular muestras provenientes de una
# distribuci�n normal con los par�metros requeridos, y obtener una estimaci�n de
# la potencia a partir de la proporci�n de muestras en que se rechaza la 
# hip�tesis nula usando el contraste t correspondiente. 
#
# Construyamos entonces una funci�n que estime la potencia del contraste t
# cuando la distribuci�n de los datos tiene una media dada que puede ser
# diferente del valor que se contrasta, usando un para ello muestras simuladas
# generadas mediante un generador de n�meros aleatorios. #

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

# As�, por ejemplo podemos estimar la potencia del test t para rechazar la 
# hip�tesis nula cuando se contrasta si la media es igual a 10 a partir de datos
# provinientes de una distribuci�n normal con media 9 y varianza 1, usando 100
# muestras de tama�o 10 y un nivel de significaci�n alfa=0.05. #

media=9
desv=1
media_test=10
muestras=100
n=10
alfa=0.05

estimar_prob_rechazar_H0(media,desv,n,muestras,alfa,media_test)

# �Obtenemos todos el mismo resultado?
#
# Aparte, una pregunta que debemos hacernos es la siguiente: si volvemos a 
# ejecutar esta funci�n tal cual, �obtendremos el mismo resultado? #

estimar_prob_rechazar_H0(media,desv,n,muestras,alfa,media_test)

# �Por qu� no se obtiene el mismo valor?













# La clave est� en la generaci�n de n�meros aleatorios, que produce muestras
# diferentes al volver a llamar a la funci�n anterior. Para garantizar que
# los resultados sean los mismos, debemos asignar una semilla id�ntica para 
# el generador de n�meros aleatorios. Este se realiza mediante la funci�n
# set.seed(). #

semilla=123
set.seed(semilla)
estimar_prob_rechazar_H0(media,desv,n,muestras,alfa,media_test)
set.seed(semilla)
estimar_prob_rechazar_H0(media,desv,n,muestras,alfa,media_test)

# Una vez tenemos una manera de estimar la potencia para un contraste particular
# podemos realizar una funci�n para estimar la curva de potencia del contraste t
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

# Con esta funci�n, podemos entonces realizar una primera curva de potencia
# para una situaci�n como la del ejemplo anterior. #

set.seed(semilla)
grafico=0
color=1
dibujar_curva_potencia_II(media_test,n,alfa,desv,muestras,grafico,color)

# Como vemos, la curva no tiene una forma "perfecta" en el sentido de que es
# solo una aproximaci�n a la curva real. Para aumentar la calidad de la 
# aproximaci�n, debe aumentarse el n�mero de simulaciones empleadas, esto es,
# el n�mero de muestras. Esto tendr� el efecto negativo de emplear mayor tiempo
# de computaci�n, que es el precio a pagar por usar una metodolog�a de 
# simulaci�n exigiendo un grado de aproximaci�n mayor. #

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
# la poblaci�n es una disminuci�n de la potencia para una diferencia dada. Esto
# es as� ya que la distribuci�n t tiene m�s probabilidad en las colas que la
# distribuci�n normal, lo que exige valores algo mayores del estad�stico de
# contraste t para obtener el mismo p-valor que el contraste normal. 
#
# Veamos que, al aumentar el tama�o de la muestra n, la potencia del contraste
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
# contraste t y la del normal para ese tama�o muestral tiende a decrecer. En 
# cierto modo, para n mayor a 30 la diferencia entre ambos test es reducida. #

# Finalmente, veremos c�mo la metodolog�a de simulaci�n que se acaba de emplear
# para aproximar la curva de potencia es aplicable a otras tareas estad�sticas.
# En particular, veremos que es posible aproximar el p-valor de un contraste a
# partir de observaciones generadas aleatoriamente. Con estas es entonces posible 
# simular repetidamente el estad�stico de contraste, obteniendo una aproximaci�n
# a su distribuci�n a partir de la cual se pueden calcular p-valores aproximados.
# Esto es de utilidad cuando no sea posible conocer la distribuci�n del 
# estad�stico de contraste bajo la hip�tesis nula, o cuando su aproximaci�n
# mediante una determinada distribuci�n l�mite pueda generar dudas.

# Para ilustrar esta cuesti�n, vamos a aproximar el p-valor del contraste t
# con una �nica muestra, cuando se desea contrastar si la media de una 
# poblaci�n normal con varianza desconocida es igual a un valor de referencia
# mu_0. Para ello, generaremos diversas muestras de observaciones de esa poblaci�n 
# normal bajo la hip�tesis nula, y con ellas simularemos valores del estadistico de 
# contraste t_0. De este modo, se obtendr� una aproximaci�n de la distribuci�n de este
# estad�stico bajo la hip�tesis nula, con la que se podr� estimar un p-valor a partir
# del n�mero de valores simulados de esta distribuci�n que son m�s extremos que el 
# valor del estad�stico de contraste obtenido para la muestra de prueba. Adem�s, como 
# sabemos, el estad�stico de contraste t_0 correspondiente se distribuye como una t de 
# Student, lo que nos permitir� comparar el p-valor aproximado con el p-valor real.

# As� pues, consideremos la hip�tesis nula dada por mu_0, y generemos un n�mero rep
# de muestras normales de tama�o n con media igual a mu_0 y cierta desviaci�n t�pica
# desv. Usamos una funci�n para obtener las rep simulaciones de t_0 y devolverlas
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

# Tomemos estos valores para los par�metros de la funci�n.
rep=100
n=15
mu_0=0
desv=1

set.seed(0)

# Generamos la muestra de prueba sobre la que se realizar� el contraste, y calculamos
# el valor del estad�stico de contraste para el que queremos aproximar el p-valor.
media_real=0.5
y=rnorm(n,mean = media_real,sd=desv)
t0_prueba=(mean(y)-mu_0)/sqrt(var(y)/length(y))

# Comparamos el valor de t0_prueba con la distribuci�n simulada de t0 bajo la hip�tesis
# nula. El p-valor aproximado se obtiene entonces como la proporci�n de simulaciones
# que toma un valor m�s extremo que t0_prueba.
ts=aproxima_distrib(rep,n,mu_0,desv)
p=sum(abs(ts)>abs(t0_prueba))/rep
p

# Comparemos p con el valor te�rico (real) del p-valor.
t.test(y)

# La calidad de la aproximaci�n depende del n�mero de simulaciones de la distribuci�n 
# de t0 as� como del tama�o muestral n.
