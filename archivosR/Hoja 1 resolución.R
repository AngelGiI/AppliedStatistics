# Ejercicio 1

Hombres=c(87,	80,	78,	80,	77,	79,	72,	81,	79,	75,	85,	87,	75,	80,	81,	69)
Mujeres=c(79,	84,	85,	78,	73,	86,	74,	74,	82,	82,	83,	83,	79,	76,	84,	83)

# Tamaños muestrales
n1=length(Hombres)
n2=length(Mujeres)

# Medias muestrales
mediaH=mean(Hombres)
mediaM=mean(Mujeres)

# Cuasi-varianzas muestrales
varH=var(Hombres)
varM=var(Mujeres)

# Nivel de significación
alfa=0.05

### Test t

# Primero lo realizamos paso por paso
s=sqrt(((n1-1)*varH+(n2-1)*varM)/(n1+n2-2))

gl=n1+n2-2

t0=(mediaH-mediaM)/(s*sqrt(1/n1+1/n2))

# Mirar la ayuda para las funciones dt, pt, qt, rt
valor_critico=qt(alfa/2,gl,lower.tail = F)

if (abs(t0) >= valor_critico) {
  print("Se rechaza H0:mu_H=mu_M")
} else {
  print("No se rechaza H0:mu_H=mu_M")
}

p_valor=2*pt(abs(t0),gl,lower.tail=F)

# Usamos ahora la función t.test ya programada

t.test(Hombres,Mujeres,var.equal = T)

### Test de Welch

# Primero paso a paso

t0_Welch=(mediaH-mediaM)/sqrt(varH/n1+varM/n2)

v=(varH/n1+varM/n2)^2/(varH^2/(n1^2*(n1-1))+varM^2/(n2^2*(n2-1)))

valor_critico=qt(alfa/2,v,lower.tail = F)

if (abs(t0_Welch) >= valor_critico) {
  print("Se rechaza H0:mu_H=mu_M")
} else {
  print("No se rechaza H0:mu_H=mu_M")
}

p_valor=2*pt(abs(t0_Welch),v,lower.tail=F)

# Función t.test ya programada

t.test(Hombres,Mujeres)

###############
# Ejercicio 2

Metodo1=c(44, 46, 44, 49, 51, 48, 47, 48, 52, 46)
Metodo2=c(39, 42, 45, 43, 42, 48, 51)

t.test(Metodo1,Metodo2,var.equal = T)
t.test(Metodo1,Metodo2)

###############
# Ejercicio 3

Componente_nuevo=c(16.00, 16.09, 15.74, 16.29, 15.96, 15.70, 16.65, 15.66, 16.22, 16.39)
Componente_antiguo=c(15.80, 16.05, 16.89, 16.11, 15.81, 16.09, 16.13, 15.88, 15.63, 15.85)

t.test(Componente_nuevo,Componente_antiguo,var.equal = T)
t.test(Componente_nuevo,Componente_antiguo)

###############
# Ejercicio 4

region=as.factor(rep(c("Este","Norte","Oeste","Sur"),each=5))
region

# Como vemos, la función rep() repite cada uno de los elementos del vector que
# recibe como primer argumento tantas veces como se indique en el parámetro
# "each". 

# Las observaciones de la respuesta, en tanto normalmente no contendrán 
# repeticiones al tratarse de una variable continua, hay que introducirlas en
# otro vector una a una CUIDANDO QUE SE INTRODUZCAN EN EL ORDEN ADECUADO, esto
# es, cada respuesta debe quedar asociada al nivel en que se ha observado. #

incremento=c(10.4,12.8,15.6,9.2,8.7,
             12.8,14.2,16.3,10.1,12.0,
             11.2,9.8,10.7,6.3,12.4,
             13.9,14.2,12.8,15.0,13.7) 
incremento

# Una vez tenemos los vectores correspondientes, solo hay que juntarlos en un
# objeto data.frame con la función data.frame(). #

ventas=data.frame(region,incremento)
str(ventas)

# Niveles del factor "Región"
a=4

# Replicación en cada nivel
n=5

# Total de observaciones
N=a*n

# Nivel de significación
alfa=0.05

# Medias por región
medias_region=tapply(ventas$incremento,ventas$region,mean)

# Media global
media_global=mean(ventas$incremento)

# Sumas de cuadrados
SCT=sum((ventas$incremento-media_global)^2)

SCF=n*sum((medias_region-media_global)^2)

medias_vector=rep(medias_region,each=n)
SCR=sum((ventas$incremento-medias_vector)^2)

SCT
SCF+SCR

# Medias de cuadrados
MCF=SCF/(a-1)
MCR=SCR/(N-a)

# Estadístico de contraste
F=MCF/MCR

# Valor crítico
valor_critico=qf(1-alfa,a-1,N-a)

# p-valor
pvalor=1-pf(F,a-1,N-a)


##############
# Ejercicio 7

vendedor=as.factor(rep(c(1,2,3,4,5),each=5))

ventas=c(9.34, 8.53, 9.43, 8.37, 9.64,
         6.46, 4.83, 5.89, 5.30, 6.33,
         5.79, 5.13, 6.17, 4.72, 5.60,
         8.37, 7.57, 8.69, 8.06, 7.23,
         4.94, 4.11, 5.45, 5.21, 5.00)

datos=data.frame(vendedor,ventas)

# Niveles del factor "Vendedor"
a=5

# Replicación en cada nivel
n=5

# Total de observaciones
N=a*n

# Nivel de significación
alfa=0.05

# Medias por vendedor
medias_vendedor=as.numeric(tapply(datos$ventas,datos$vendedor,mean))

# Media global
media_global=mean(datos$ventas)

# Sumas de cuadrados
SCT=sum((datos$ventas-media_global)^2)

SCF=n*sum((medias_vendedor-media_global)^2)

medias_vector=rep(medias_vendedor,each=n)
SCR=sum((datos$ventas-medias_vector)^2)

SCT
SCF+SCR

# Medias de cuadrados
MCF=SCF/(a-1)
MCR=SCR/(N-a)

# Estadístico de contraste
F=MCF/MCR

# Valor crítico
valor_critico=qf(1-alfa,a-1,N-a)

# p-valor
pvalor=1-pf(F,a-1,N-a)

# Comparación por pares
# Hacemos una función para automatizar las comparaciones a pares
compara_par=function(medias,i,j,MCR,n,a,alfa){
  t0=(medias_vendedor[i]-medias_vendedor[j])/sqrt(MCR*2/n)
  valor_critico=abs(qt(alfa/2,a*n-a))
  pvalor=2*(1-pt(abs(t0),a*n-a))
  c=choose(a,2)
  valor_critico_ajustado=abs(qt(alfa/(2*c),a*n-a))
  pvalor_ajustado=min(2*(1-pt(abs(t0),a*n-a))*c,1)
  valores=c(valor_critico,valor_critico_ajustado)
  pvalores=c(pvalor,pvalor_ajustado)
  return(c(t0,valores,pvalores))
}

compara_par(medias_vendedor,1,2,MCR,n,a,alfa)
compara_par(medias_vendedor,1,3,MCR,n,a,alfa)
compara_par(medias_vendedor,1,4,MCR,n,a,alfa)
compara_par(medias_vendedor,1,5,MCR,n,a,alfa)
compara_par(medias_vendedor,2,3,MCR,n,a,alfa)
compara_par(medias_vendedor,2,4,MCR,n,a,alfa)
compara_par(medias_vendedor,2,5,MCR,n,a,alfa)
compara_par(medias_vendedor,3,4,MCR,n,a,alfa)
compara_par(medias_vendedor,3,5,MCR,n,a,alfa)
compara_par(medias_vendedor,4,5,MCR,n,a,alfa)


################
# Ejercicios 10 y 11

FondoA=c(1.12, 0.06, 0.08, -0.01) 
FondoB=c(0.43,-1.76, -0.09, -2.21) 
FondoC=c(2.21, -1.98, 0.80, -1.32)

Fondo=as.factor(rep(c("A","B","C"),each=4))

Rentabilidad=c(FondoA,FondoB,FondoC)

datos=data.frame(Fondo,Rentabilidad)

a=3
n=4
N=a*n

alfa=0.05

# Medias por fondo
medias_fondo=as.numeric(tapply(datos$Rentabilidad,datos$Fondo,mean))

# Media global
media_global=mean(datos$Rentabilidad)

# Sumas de cuadrados
SCT=sum((datos$Rentabilidad-media_global)^2)

SCF=n*sum((medias_fondo-media_global)^2)

medias_vector=rep(medias_fondo,each=n)
SCR=sum((datos$Rentabilidad-medias_vector)^2)

SCT
SCF+SCR

# Medias de cuadrados
MCF=SCF/(a-1)
MCR=SCR/(N-a)

# Estadístico de contraste
F=MCF/MCR

# Valor crítico
valor_critico=qf(1-alfa,a-1,N-a)

# p-valor
pvalor=1-pf(F,a-1,N-a)

# Vamos ahora a tratar estos datos considerando que el año forma un bloque (Ej 11)

Year=as.factor(rep(c(1,2,3,4),times=3))

datos=data.frame(datos,Year)

a=3
b=4
N=a*b
alfa=0.05

# Medias por fondo
medias_fondo=as.numeric(tapply(datos$Rentabilidad,datos$Fondo,mean))

# Medias por año
medias_year=as.numeric(tapply(datos$Rentabilidad,datos$Year,mean))

# Media global
media_global=mean(datos$Rentabilidad)

# Sumas de cuadrados
SCT=sum((datos$Rentabilidad-media_global)^2)

SCA=b*sum((medias_fondo-media_global)^2)

SCB=a*sum((medias_year-media_global)^2)

medias_fondo_vector=rep(medias_fondo,each=b)
medias_year_vector=rep(medias_year,times=a)

SCR=sum((datos$Rentabilidad-medias_fondo_vector-medias_year_vector+media_global)^2)

SCA+SCB+SCR
SCT

# Medias de cuadrados
MCA=SCA/(a-1)
MCB=SCB/(b-1)
MCR=SCR/((a-1)*(b-1))

# Estadísticos de contraste
FA=MCA/MCR
FB=MCB/MCR

# Valores crítico
valor_critico_A=qf(1-alfa,a-1,(a-1)*(b-1))
valor_critico_B=qf(1-alfa,b-1,(a-1)*(b-1))

# p-valores
pvalor_A=1-pf(FA,a-1,(a-1)*(b-1))
pvalor_B=1-pf(FB,b-1,(a-1)*(b-1))


##########################
# Ejercicio 12

ZonaA=c(3, 6, 4, 5)
ZonaB=c(8, 9, 7, 9)
ZonaC=c(2, 7, 3, 3)

Satisfaccion=c(ZonaA,ZonaB,ZonaC)
Zona=as.factor(rep(c("A","B","C"),each=4))
Tamanho=as.factor(rep(c("Grande","Pequeño"),each=2,times=3))

datos=data.frame(Zona,Tamanho,Satisfaccion)

a=3
b=2
n=2
N=a*b*n

alfa=0.05

# Medias por línea
medias_zona=as.numeric(tapply(datos$Satisfaccion,datos$Zona,mean))

# Medias por año
medias_tam=as.numeric(tapply(datos$Satisfaccion,datos$Tamanho,mean))

# Media global
media_global=mean(datos$Satisfaccion)

# Sumas de cuadrados
SCT=sum((datos$Satisfaccion-media_global)^2)

SCA=b*n*sum((medias_zona-media_global)^2)

SCB=a*n*sum((medias_tam-media_global)^2)

medias_zona_vector=rep(medias_zona,each=b*n)
medias_tam_vector=rep(medias_tam,each=n,times=a)

SCR=sum((datos$Satisfaccion-medias_zona_vector-medias_tam_vector+media_global)^2)

SCA+SCB+SCR
SCT

# Medias de cuadrados
MCA=SCA/(a-1)
MCB=SCB/(b-1)
MCR=SCR/(N-a-b+1)

# Estadísticos de contraste
FA=MCA/MCR
FB=MCB/MCR

# Valores crítico
valor_critico_A=qf(1-alfa,a-1,N-a-b+1)
valor_critico_B=qf(1-alfa,b-1,N-a-b+1)

# p-valores
pvalor_A=1-pf(FA,a-1,N-a-b+1)
pvalor_B=1-pf(FB,b-1,N-a-b+1)


# Desechemos el bloque (modelo unifactorial simple o totalmente aleatorizado)
SCR2=SCR+SCB
MCR2=SCR2/(N-a)
FA2=MCA/MCR2
valor_critico_A2=qf(1-alfa,a-1,N-a)
pvalor_A2=1-pf(FA2,a-1,N-a)


# Ahora con modelo bifactorial
tratamiento=paste(datos$Zona,datos$Tamanho,sep="_") 
datos=data.frame(datos,tratamiento)

medias_tratamiento=tapply(datos$Satisfaccion,datos$tratamiento,mean)

medias_tratamiento_vector=rep(medias_tratamiento,each=n)

SCAB=sum((medias_tratamiento_vector-medias_zona_vector-medias_tam_vector+media_global)^2)

MCAB=SCAB/((a-1)*(b-1))

SCRbif=sum((datos$Satisfaccion-medias_tratamiento_vector)^2)
MCRbif=SCRbif/(a*b*(n-1))

SCT
SCA+SCB+SCAB+SCRbif

# Estadísticos de contraste
FAbif=MCA/MCRbif
FBbif=MCB/MCRbif
FAB=MCAB/MCRbif

# Valores crítico
valor_critico_Abif=qf(1-alfa,a-1,a*b*(n-1))
valor_critico_Bbif=qf(1-alfa,b-1,a*b*(n-1))
valor_critico_AB=qf(1-alfa,(a-1)*(b-1),a*b*(n-1))

# p-valores
pvalor_Abif=1-pf(FAbif,a-1,a*b*(n-1))
pvalor_Bbif=1-pf(FBbif,b-1,a*b*(n-1))
pvalor_AB=1-pf(FAB,(a-1)*(b-1),a*b*(n-1))

################
# Ejercicio 13

Linea1=c(60, 25, 25, 50)
Linea2=c(33, 18, 36, 42)
Linea3=c(38, 5, 23, 48)

Linea=as.factor(rep(c("L1","L2","L3"),each=4))
Estudiante=as.factor(rep(c("E1","E2","E3","E4"),times=3))

Errores=c(Linea1,Linea2,Linea3)

datos=data.frame(Linea,Estudiante,Errores)

a=3
b=4
N=a*b

alfa=0.05

# Medias por línea
medias_linea=as.numeric(tapply(datos$Errores,datos$Linea,mean))

# Medias por año
medias_est=as.numeric(tapply(datos$Errores,datos$Estudiante,mean))

# Media global
media_global=mean(datos$Errores)

# Sumas de cuadrados
SCT=sum((datos$Errores-media_global)^2)

SCA=b*sum((medias_linea-media_global)^2)

SCB=a*sum((medias_est-media_global)^2)

medias_linea_vector=rep(medias_linea,each=b)
medias_est_vector=rep(medias_est,times=a)

SCR=sum((datos$Errores-medias_linea_vector-medias_est_vector+media_global)^2)

SCA+SCB+SCR
SCT

# Medias de cuadrados
MCA=SCA/(a-1)
MCB=SCB/(b-1)
MCR=SCR/((a-1)*(b-1))

# Estadísticos de contraste
FA=MCA/MCR
FB=MCB/MCR

# Valores crítico
valor_critico_A=qf(1-alfa,a-1,(a-1)*(b-1))
valor_critico_B=qf(1-alfa,b-1,(a-1)*(b-1))

# p-valores
pvalor_A=1-pf(FA,a-1,(a-1)*(b-1))
pvalor_B=1-pf(FB,b-1,(a-1)*(b-1))


####### Ejercicio 8

Oro=c(6.686, 6.681, 6.676, 6.678, 6.679)
Platino=c(6.661, 6.661, 6.667, 6.667, 6.664)
Vidrio=c(6.678, 6.671, 6.675, 6.672, 6.674)

Constante_observada=c(Oro,Platino,Vidrio)
Material=as.factor(rep(c("Oro","Platino","Vidrio"), each=5))

datos=data.frame(Material,Constante_observada)

resultado=aov(Constante_observada ~ Material, data=datos)
summary(resultado)


####### Ejercicio 9

Termometro1=c(63, 63, 62, 65)
Termometro2=c(64, 64, 63, 64)
Termometro3=c(58, 59, 59, 68)
Termometro4=c(61, 62, 61, 60)

Termometro=as.factor(rep(c(1,2,3,4),each=4))
Temperatura=c(Termometro1,Termometro2,Termometro3,Termometro4)

datos=data.frame(Termometro,Temperatura)

resultado=aov(Temperatura ~ Termometro, data=datos)
summary(resultado)

plot(resultado)

datos_sin3=datos[Termometro!=3,]

resultado_sin3=aov(Temperatura ~ Termometro, data=datos_sin3)
summary(resultado_sin3)

plot(resultado_sin3)


####### Ejercicio 14

Metodo=as.factor(rep(c("A","B"),each=4))
Operador=as.factor(rep(c("O","P"),each=2,times=2))
Dureza=c(4.8, 5.3, 5.2, 5.1,
         4.9, 5  , 6.8, 7.4)

datos=data.frame(Metodo,Operador,Dureza)

resultado_unif=aov(Dureza ~ Metodo, data=datos)
summary(resultado_unif)

resultado_bloque=aov(Dureza ~ Metodo + Operador, data=datos)
summary(resultado_bloque)

resultado_bifact=aov(Dureza ~ Metodo * Operador, data=datos)
summary(resultado_bifact)

interaction.plot(Metodo,Operador,Dureza)


######### Ejercicio 15

Luz=as.factor(rep(c("-","+"),each=4))
Abono=as.factor(rep(c("-","+"),each=2,times=2))
Biomasa=c(475,647,721,483,
          642,584,685,745)

datos=data.frame(Luz,Abono,Biomasa)

resultado=aov(Biomasa ~ Luz * Abono)
summary(resultado)
interaction.plot(Luz,Abono,Biomasa)
