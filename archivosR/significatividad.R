set.seed(5)
n=15
media=0
desv=1

y1=rnorm(n,media,desv)
y2=rnorm(n,media,desv)
y1
y2
m1=mean(y1)
m1
m2=mean(y2)
m2
v1=var(y1)
v1
v2=var(y2)
v2

m=1000

funcion=function(n,m,media,desv){
  medias=c()
  varianzas=c()
  for (i in 1:m) {
    y=rnorm(n,media,desv)
    med=mean(y)
    var=var(y)
    medias=c(medias,med)
    varianzas=c(varianzas,var)
  }
  difmedia=max(medias)-min(medias)
  difvar=max(varianzas)/min(varianzas)
  
  rechazos=sum(abs(medias/sqrt(1/n))>qnorm(0.025,media,desv,lower.tail = FALSE))
  prop_rechazos=rechazos/m
  
  return(c(difmedia,difvar,prop_rechazos))
}

set.seed(5)
funcion(15,1000,0.5,1)
