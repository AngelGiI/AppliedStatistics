######## Ejercicio 3


# Los datos de este ejercicio registran calificaciones de 17 estudiantes 
# en las asignaturas de Inform�tica (I) y Estad�stica (E). #

I=c(3,4,6,7,5,8,7,3,5,4,8,5,5,8,8,8,5)
E=c(5,5,8,7,7,9,10,4,7,4,10,5,7,9,10,5,7)

# En el apartado a) se nos pide en primer lugar obtener las distribuciones 
# marginales de las calificaciones de cada asignatura. Esto significa obtener 
# una tabla con las frecuencias de cada valor que toman estas variables por 
# separado. 
#
# Para obtener la frecuencia absoluta de cada valor, lo m�s directo es usar la 
# funci�n table(). #

table(I)
table(E)

# No es dif�cil obtener el resto de frecuencias (relativa, acumulada, acumulada
# relativa) a partir de esta tabla, que es un objeto similar a un vector.
#
# Por ejemplo, podr�amos calcular las frecuencias relativas dividiendo las 
# tablas anteriores por el n�mero de observaciones, que es igual a la longitud
# de los vectores I o E. #

table(I)/length(I)
table(E)/length(E)

# Para las frecuencias acumuladas, podemos usar la funci�n cumsum(), que va 
# sumando los valores de un vector de manera acumulativa. #

cumsum(table(I))
cumsum(table(I)/length(I))
cumsum(table(E))
cumsum(table(E)/length(E))

# Podemos agrupar todas las frecuencias obtenidas en una �nica tabla mediante
# la funci�n cbind(). #

cbind(table(I),table(I)/length(I),cumsum(table(I)),cumsum(table(I)/length(I)))
cbind(table(E),table(E)/length(E),cumsum(table(E)),cumsum(table(E)/length(E)))

# Una manera de obtener el mismo resultado pero en formato de data frame y con
# nombres adecuados para las columnas pasa por convertir los vectores I y E en
# factores usando la funcion factor(), convertir la tabla de frecuencias 
# asociada en un data.frame mediante la funci�n as.data.frame() y luego
# transformar este data frame a�adiendo el resto de frecuencias mediante la 
# funci�n transform(). #

factorI=factor(I)
Idf=as.data.frame(table(factorI))
Idf=transform(Idf, Frec_rel = prop.table(Freq), 
              Frec_acum = cumsum(Freq))
Idf=transform(Idf,Frec_ac_rel=cumsum(Frec_rel))
Idf

factorE=factor(E)
Edf=as.data.frame(table(factorE))
Edf=transform(Edf, Frec_rel = prop.table(Freq), 
              Frec_acum = cumsum(Freq))
Edf=transform(Edf,Frec_ac_rel=cumsum(Frec_rel))
Edf

# El apartado a) tambi�n pide obtener medias y varianzas de ambos vectores. #

mean(I)
var(I)
mean(E)
var(E)

# En el apartado b) se pide obtener la covarianza y la correlaci�n entre ambos
# vectores. Para el c�lculo de la covarianza, R dispone de la funci�n cov(). #

cov(I,E)

# Comprobemos que en particular R calcula lo que denominamos "cuasi-covarianza",
# en el sentido de que se divide por n-1, siendo n la longitud de los vectores
# de los que se calcula la covarianza.
#
# Podemos realizar esta comprobaci�n de varias maneras. La m�s directa ser�a
# mediante un bucle mediante el cual implementamos el sumatorio de productos
# cruzados de la f�rmula de la covarianza. #

cprod=0
for (i in 1:length(I)){
  cprod=cprod+(I[i]-mean(I))*(E[i]-mean(E))
}
cprod/(length(I)-1)

# Otra manera es restar primero a cada vector su media y luego realizar el 
# producto elemento a elemento de ambos vectores, que nos devuelve entonces un
# nuevo vector, y finalmente sumar este vector de productos y dividir por n-1. #

I0=I-mean(I)
E0=E-mean(E)
sum(I0*E0)/(length(I)-1)

# N�tese que en la expresi�n anterior * realiza el producto elemento a elemento,
# esto es, I0*E0 devuelve un vector de la misma longitud que I0 y E0 cuyos
# elementos son los respectivos productos de los elementos de I0 y E0.
#
# Para realizar el producto escalar, o en general el producto de matrices, se ha
# de usar el operador %*%, para lo cual hay que tener cuidado de que las
# dimensiones de las matrices encajen. Este operador realiza las transposiciones
# necesarias para que las dimensiones sean conformes, por lo que para obtener un 
# producto escalar no hay que usar la funci�n de transposici�n de matrices t().#

I0%*%E0/(length(I)-1)

# Para la correlaci�n, como ya hemos visto en alguna ocasi�n, R dispone de la 
# funci�n cor(). #

cor(I,E)

# comprobemos que efectivamente esta correlaci�n se calcula como el cociente
# entre la covarianza y el producto de las desviaciones t�picas. #

cov(I,E)/(sd(I)*sd(E))

# En el apartado c) se pide ajustar un modelo de regresi�n para explicar las
# calificaciones de Estad�stica a partir de las de Inform�tica. B�sicamente,
# necesitamos obtener las estimaciones de los par�metros beta0 y beta1, de 
# manera que el modelo ajustado tendr� la forma E = beta0 + beta1 * I.
# 
# Obtengamos estas estimaciones aplicando las f�rmulas de los estimadores. #

beta1=cov(I,E)/var(I)
beta1
beta0=mean(E)-beta1*mean(I)
beta0

######### Ejercicio 6

# De manera m�s general, podemos ajustar un modelo de regresi�n mediante la funci�n lm(),
# cuyo nombre proviene del ingl�s "linear model". 
#
# El manejo de esta funci�n es similar a la funci�n aov() que usamos para 
# ajustar modelos de an�lisis de varianza para experimentos. B�sicamente, se ha
# de proporcionar a lm() una f�rmula de R en la que la variable dependiente o 
# respuesta se encuentre antes del operador ~, y la variable independiente o 
# explicativa a continuaci�n de ~. #

Edad=c(2, 3, 4, 4, 5, 5, 6, 7, 7, 9, 9, 10, 11, 11, 12)
Respuestas=c(11, 12, 10, 13, 11, 9, 10, 7, 12, 8, 7, 3, 6, 5, 5)

lm(Respuestas ~ Edad)

# Como vemos, el valor de la estimaci�n de beta0 se da bajo el nombre de 
# (Intercept), que se podr�a traducir por constante u ordenada en el origen. La 
# estimaci�n del par�metro beta1 aparece bajo el nombre de la variable o vector 
# con el que est� asociado este par�metro, en este caso con las calificaciones
# de inform�tica I.
#
# La tabla ANOVA de este modelo se puede obtener aplicando la funci�n summary() al 
# objeto devuelto por lm.

summary(lm(Respuestas ~ Edad))

# El p-valor obtenido para Edad, 6.79e-05, indica que esta variable es un predictor 
# significativo de las respuestas incorrectas, as� que se tendr�a evidencia para 
# sostener la afirmaci�n de que el n�mero de respuestas incorrectas decrece con la edad.

# Para calcular la predicci�n que se pide en el apartado b), tenemos que usar los 
# coeficientes beta estimados y aplicarlos a la edad de Alberto, 10.5 a�os.

modelo=lm(Respuestas ~ Edad)

prediccion=modelo$coefficients[1]+modelo$coefficients[2]*10.5
prediccion

# As� pues, en promedio se puede esperar que Alberto responda incorrectamente 5.79 
# preguntas.


######### Ejercicio 9

Profundidad=c(15, 20, 30, 40, 50, 60, 70)
Oxigeno=c(6.5, 5.6, 5.4, 6.0, 4.6, 1.4, 0.1)

lm(Oxigeno ~ Profundidad)

# La recta de regresi�n vendr�a dada por Oxigeno = 8.631 - 0.1081 * Profundidad.

# La correlaci�n pedida en el apartado b) se obtendr�a usando cor().

cor(Oxigeno,Profundidad)

# Este valor de correlaci�n indica que existe una relaci�n inversa relativamente 
# fuerte entre ambas variables, como tambi�n se deduce del valor negativo de beta1
# (-0.1081) y del p-valor asociado a este par�metro (0.00635), que se obtiene a 
# continuaci�n.

summary(lm(Oxigeno ~ Profundidad))

# Al utilizar esta recta para predecir la concentraci�n de ox�geno que se encontrar�a
# a una profundidad de 90 metros se obtendr�a el siguiente valor:

modelo=lm(Oxigeno ~ Profundidad)

prediccion=modelo$coefficients[1]+modelo$coefficients[2]*90
prediccion

# Este valor negativo no tiene sentido, pues la concentraci�n de una sustancia ha de 
# ser positiva. Esto es consecuencia de que el modelo ha sido ajustado para valores de
# profundidad entre 15 y 70 metros, por lo que al usar el modelo para predecir la 
# concentraci�n de ox�geno a 90 metros de profunididad realmente se estar�a extrapolando
# la relaci�n observada entre las variables m�s all� del rango de las observaciones. Esta
# pr�ctica de extrapolaci�n puede resultar peligrosa, especialmente cuando se utiliza para
# realizar predicciones en valores relativamente alejados del rango de observaciones usadas
# en el ajuste del modelo.


########### Ejercicio 10

X=c(1,2,3,4)
Y=c(10,100,1000,10000)

# Es f�cil intuir que la relaci�n entre X e Y es de tipo exponencial, esto es, Y=10^X. Para 
# poder ajustar esta relaci�n como un modelo lineal ser� necesario transformar el modelo
# que se pide Y = a*10^{b*X} mediante el logaritmo en base 10, ya que entonces
# log_10 Y = log_10 a + b*X = A + b*X, con A = log_10 a, en el que la variable X entra 
# de manera lineal. Una vez obtenidos los coeficientes de este modelo, podemos obtener 
# el estimador de a tomando la exponencial en base 10 de A.

logY=log10(Y)

modelo=lm(logY ~ X)
modelo

A=modelo$coefficients[1]
a=10^A

# As� pues, se obtiene que A = 0 y b = 1, por lo que el modelo ajustado ser�a
# logY = 0 + 1*X, y deshaciendo la transformaci�n logar�tmica se obtendr�a
# Y = 10^0 * 10^{1*X} = 1*10^{1*X} = 10^X, la relaci�n buscada.
#
# Para el apartado b), la predicci�n obtenida para X=3.5 ser�a la siguiente:

10^3.5

# Para X=5, la predicci�n ser�a:

10^5

# No obstante, obs�rvese que el valor X = 5 cae fuera del rango de valores observados
# de esta variable, por lo que habr�a que tener precauci�n al usar esta predicci�n en
# tanto que el modelo se usa para extrapolar en ese punto.