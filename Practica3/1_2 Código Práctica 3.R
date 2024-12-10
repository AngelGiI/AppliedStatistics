# PRÁCTICA 3 ESTADÍSTICA APLICADA

#######################################################################
##                            EJERCICIO 1                            ##
#######################################################################

semilla = 12
sizeM = 20     # Tamaño muestra

set.seed(semilla)

muestra1 = rnorm(sizeM, mean=0, sd=1)  # Genera 2 muestras normales de tamaño
muestra2 = rnorm(sizeM, mean=0, sd=1)  # 20, media 0 y varianza 1.

# Realizamos un test para ver la diferencia que existe entre la media de dos 
# poblaciones normales.

t.test(muestra1, muestra2, alternative = "two.sided", paired = FALSE,
       var.equal = TRUE) 

# Veamos el procedimiento de simulación para aproximar el p-valor de este 
# contraste.

aproxima_distrib_norm=function(rep,n,mu_0,desv){
  ts=c()
  for (i in 1:rep) {
    x=rnorm(n,mean=mu_0,sd=desv)
    y=rnorm(n,mean=mu_0,sd=desv)
    t0_simulado=(mean(x)-mean(y))/sqrt((var(x)+var(y))/length(x))
    ts=c(ts,t0_simulado)
  }
  return(ts)
}

rep = 10000
n = 20
mu_0 = 0
desv = 1


t0_prueba=(mean(muestra1)-mean(muestra2))/sqrt((var(muestra1)+var(muestra2))/sizeM)
ts=aproxima_distrib_norm(rep,n,mu_0,desv)
p=sum(abs(ts)>abs(t0_prueba))/rep
p

# Otra opción era calcular t0_simulado usando "statistic"
# t0_simulado = t.test(x,y)$statistic
# y por consecuente haríamos lo mismo con t0_prueba
# t0_prueba = t.test(muestra1,muestra2)$statistic

#######################################################################
##                            EJERCICIO 2                            ##
#######################################################################

set.seed(semilla)

muestra3 = runif(sizeM, min=0, max=10)  # Genera 2 muestras uniformes de tamaño
muestra4 = runif(sizeM, min=0, max=10)  # 20, mínimo 0 y máximo 10.

# Con este test podemos observar si las dos muestras uniformes provienen
# de la misma población. 

wilcox.test(muestra3, muestra4) 

# Veamos el procedimiento de simulación para aproximar el p-valor de este
# contraste.

aproxima_distrib_unif=function(rep,n,minn,maxx){
  ts=c()
  for (i in 1:rep) {
    x=runif(n,min=minn,max=maxx)
    y=runif(n,min=minn,max=maxx)
    t0_simulado = wilcox.test(x,y)$statistic
    ts=c(ts,t0_simulado)
  }
  return(ts)
}

rep=1000
n=20
minn=0
maxx=10

t0_prueba = wilcox.test(muestra3, muestra4)$statistic


ts=aproxima_distrib_unif(rep,n,minn,maxx)
p=sum(abs(ts-mean(ts))>abs(t0_prueba-mean(ts)))/rep
p


#######################################################################
##                            EJERCICIO 3                            ##
#######################################################################

# Empleando esta función podemos conocer cuál es el mínimo n para poder detectar
# una diferencia de una unidad entre la media real de una población normal con 
# desviación típica 1.5 y la media de contraste con una probabilidad de al menos
# 0.8, con alfa = 0.05

power.t.test(delta=1, sd=1.5, sig.level=0.05, power=0.8, type = "one.sample",
             alternative = "two.sided")

# La idea de este test, es dar 4 parámetros de los 5 posibles y nos devuelve el
# desconocido, en nuestro caso la n.


#######################################################################
##                            EJERCICIO 4                            ##
#######################################################################

set.seed(semilla)

media = 0
desv = 1.5
media_test = 1
muestras = 100  # Tomamos un numero grande y luego sacamos el mínimo
n = 2
alfa = 0.05

# Con esta función calculamos la probabilidad de rechazar la H0.

estimar_prob_rechazar_H0=function(media,desv,n,muestras,alfa,media_test){  
  resultado = vector()                                                      
  for (i in 1:muestras){                                               
    resultado[i]=ifelse(t.test(rnorm(n,mean=media,sd=desv),
                               mu=media_test)$p.value<alfa,1,0)
  }
  total = sum(resultado)
  prob_rechazar_H0 = total/muestras; 
  return(prob_rechazar_H0)
}

# Usemos un while, cuya condición nos vaya acercando mediante simulación al
# n deseado.

while(estimar_prob_rechazar_H0(media,desv,n,muestras,alfa,media_test) <= 0.8)  {
  n = n+1                                                                       
} 

n # Este es el valor buscado.          


#######################################################################
##                            EJERCICIO 5                            ##
#######################################################################


grafico = 0
color = 2
media1 = 0
desv = 1
media2 = 1
muestras = 500
n = 10
alfa = 0.05

set.seed(semilla)

# Usemos una función parecida a la anterior, pero realizamos las modificaciones
# necesarias para que haga la comparación entre la media de 2 muestras normales.

estimar_prob_rechazar_H0=function(media1,desv,n,muestras,alfa,media2){  
  resultado=vector()                                                    
  for (i in 1:muestras){                                               
    resultado[i] = ifelse(t.test(rnorm(n, mean=media1, sd=desv),
                               rnorm(n, mean=media2, sd=desv),
                               alternative = "two.sided",
                               paired = FALSE,
                               var.equal = TRUE)$p.value<alfa,1,0)
  }
  total = sum(resultado)
  prob_rechazar_H0 = total/muestras; 
  return(prob_rechazar_H0)
}

potencia = vector()
i = 0
for (dif in seq(-3,+3,0.1)){
  i = i+1
  potencia[i] = 1-(pnorm(qnorm(1-(alfa/2))-(abs(dif)/(desv/sqrt(n))))
                 -pnorm(qnorm(alfa/2)-(abs(dif)/(desv/sqrt(n)))))
}

# Por último escribimos la función que nos permite realizar un esbozo de la
# gráfica que hace la comparativa de las diferencias de medias entre las
# poblaciones normales y la potencia del test.

dibujar_curva_potencia_t = function(media2,n,alfa,desv,muestras,grafico,color){                  
  probs = vector()                                                    
  medias = vector()                                                   
  i = 0
  for (dif in seq(-3,3,0.1)){
    i = i+1
    medias[i] = media2+dif
    potencia[i] = estimar_prob_rechazar_H0(medias[i],desv,n,muestras,alfa,
                                         media2)
  }
  if (grafico==0) plot(medias, potencia,type="l", col=color,
                       xlab = "Diferencia de las medias", ylab = "Potencia")
  else lines(medias, potencia, type = "l", col = color)
}

dibujar_curva_potencia_t(media2,n,alfa,desv,muestras,
                         grafico,color)

