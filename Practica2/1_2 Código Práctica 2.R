# PRÁCTICA 2 ESTADÍSTICA APLICADA

# Comenzamos instalando y cargando el paquete HSAUR2 y su librería.

install.packages("HSAUR2")
library(HSAUR2)

# Cargamos el data frame que contiene el objeto CHFLS.

data("CHFLS",package = "HSAUR2")


# El conjunto de datos "CHFLS" contiene los resultados de un estudio realizado 
# a principios de siglo en 60 localidades chinas seleccionadas para representar
# todo el abanico geográfico y socioeconómico de la China actual. En particular,
# este conjunto de datos contiene las respuestas de las mujeres con pareja
# masculina sin información missing, lo que conduce a una muestra de 1534 mujeres 
# con las variables que es posible consultar en la ayuda. #

?CHFLS


#######################################################################
##                            EJERCICIO 1                            ##
#######################################################################

# Creamos el vector de 1's y 0's con respecto a su nivel educativo

convert = ifelse(CHFLS$R_edu < "Senior high school" , 0 , 1)
R_edu2 =  as.factor(convert)

# Con el nuevo vector de tipo factor, renombramos sus niveles para que resulten
# orientativos.

levels(R_edu2) = c("SIN BACH","CON BACH")

# Creamos el nuevo data frame con R_edu2 añadido

CHFLS2 = cbind(CHFLS , R_edu2)



#######################################################################
##                            EJERCICIO 2                            ##
#######################################################################

# HERRAMIENTAS GRÁFICAS

# Comenzamos creando un diagrama de bigotes para hacernos una idea general.

layout(matrix(1))
boxplot(R_income ~ R_edu2, data = CHFLS , xlab = "Estudios", ylab = "Ingresos")

# Para estudiarlo mejor,  hacemos un cambio de escala del eje y.

boxplot(R_income ~ R_edu2, data = CHFLS , ylim = c(0, 1500) , xlab = "Estudios",
        ylab = "Ingresos")

#HERRAMIENTAS INFERENCIALES

# Usamos el shapiro test para ver si se distribuyen los datos como una normal

tapply(CHFLS2$R_income, CHFLS2$R_edu2, shapiro.test)

# El test rechaza que los datos se distribuyan mediante una normal, para ambos 
# casos. Por tanto, usamos un contraste de hipótesis no paramétrico, con el que
# podremos ver si hay una fuerte relación o no, entre los salarios y sus niveles
# educativos. La hipótesis nula es tener todas las medias iguales.

wilcox.test(R_income~R_edu2, data=CHFLS, conf.int=TRUE)


#######################################################################
##                            EJERCICIO 3                            ##
#######################################################################

# HERRAMIENTAS GRÁFICAS

# Realizamos un diagrama de bigotes para hacernos una idea general, ya que es 
# muy ilustrativo y nos aporta gran información.

layout(matrix(1))

levels(CHFLS2$A_edu) =  c("NAS","ES","JHS","SHS","JC","UNI")
levels(CHFLS2$R_edu) = c("NAS","ES","JHS","SHS","JC","UNI")
boxplot(R_income ~ R_edu, data = CHFLS2, ylim = c(0, 3000), xlab = "Estudios", ylab = "Ingresos")

        
# HERRAMIENTAS INFERENCIALES

xtabs(~ R_edu + R_income, data = CHFLS2)

# La idea sería realizar una tabla ANOVA para poder estudiar los 6 niveles de 
# educación, pero no es posible ya que cada grupo cuenta con número de
# observaciones diferente.

#######################################################################
##                            EJERCICIO 4                            ##
#######################################################################

# Comenzamos estudiando ambas distribuciones con un diagrama de barras para 
# hacernos una idea general.

layout(matrix(1:2,ncol=2))

barplot(xtabs(~R_edu, data=CHFLS))
barplot(xtabs(~A_edu, data=CHFLS))


# A simple vista se aprecia una alta relación. Usemos ahora un gráfico
# de columnas, útil para 2 variables categóricas. Pero previamente veamos la 
# tabla de contingencia.

xtabs(~ A_edu + R_edu, data = CHFLS)

layout(matrix(1))
plot(A_edu ~ R_edu , data = CHFLS2)

# Usando el zoom se aprecia mejor el gráfico.


#######################################################################
##                            EJERCICIO 5                            ##
#######################################################################

# Creamos el dataframe "CHFLS3" que es el incicial con el vector nuevo añadido.
# Seguimos el mismo procedimiento que en el ejercicio 1.

convert2 = ifelse(CHFLS$A_edu < "Senior high school" , 0 , 1)
A_edu2 =  as.factor(convert2)
levels(A_edu2) = c("SIN BACH","CON BACH")
CHFLS3 = cbind(CHFLS , A_edu2)

# Dado que estamos trabajando con 2 variables categóricas, volvemos a recurrir
# al gráfico de columnas y a su tabla de contingencia

tab_edu = xtabs(~ R_edu2 + A_edu2, data = CHFLS3)
tab_edu

plot(A_edu2 ~ R_edu2 , data = CHFLS3)

# Como herramienta inferencial usaremos el contraste de independencia de McNemar
# para la tabla de contingencia, por tratarse de variables categóricas pareadas.
  
mcnemar.test(tab_edu, correct = FALSE)

