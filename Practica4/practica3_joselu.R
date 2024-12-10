#***************************************************************
#********************** PRÁCTICA 3 *****************************
#***************************************************************

# Introducimos los datos del enunciado en los vectores material, temperatura, y duración.
# Así mismo construimos el objeto datos que los recopila todos.

material = rep(c(1, 2, 3), each = 12)
temperatura = rep(c(-10, 20, 50, -10, 20, 50, -10, 20, 50), each = 4)
duracion = c(130,155,74,180,34,40,80,75,20,70,82,58,150,188,159,
             126,136,122,106,115,25,70,58,45,138,110,168,160,174,120,150,139,96,104,82,60)
datos = data.frame(material, temperatura, duracion)

# MODELO UNIFACTORIAL SIMPLE COMPLETAMENTE ALEATORIZADO
#************************************************

# En primer lugar vamos a analizar los datos representándolos gráficamente:

stripchart(duracion ~ material, vertical = T)

# Observamos que para el material uno se acumulan las duraciones entre el 0 y el 100.
# Para el segundo material vemos una distribución homogénea y aparentemente aleatoria.
# Además, para el tercer material los datos se concentran en el intervalo [75, 175].

# Obtengamos ahora las medias de cada nivel y su dispersión:

tapply(duracion, material, mean)
tapply(duracion, material, sd)

# Como vemos, las dispersiones de los materiales 1 y 2 son bastante similares, mientras que la
# la de la 3 es algo más pequeña. Para corroborar que se cumple el supuesto de homocedasticidad,
# empleamos el contraste de levene:

library(lawstat)
levene.test(duracion, material, "median")

# El p-valor es muy alto, con lo que parece cumplirse el supuesto de homocedasticidad.
# Obtenemos la tabla ANOVA para el modelo unifactorial simple:

result1 = aov(duracion ~ material)
summary(result1)

# El p-valor es menor que el valor de significación (0.05), con lo que debemos rechazar
# la hipótesis nula que nos decía que el tipo de material NO influye.
# Por lo que el tipo material SÍ influye en la duración de las baterías.

# MODELO UNIFACTORIAL POR BLOQUES
#*********************************

# Al introducir en el modelo la temperatura como variable bloque, vamos a observar los datos
# con respecto a la temperatura:

stripchart(duracion ~ temperatura, vertical = T)

# Para la temperatura = -10, los datos se acumulan en [125,200], por lo que la duracion es mayor
# a temperatura baja. En cambio, para la temperatura = 50, los datos se acumulan en [0,100]
# concluyendo que a temperatura alta, la duracion de la batería es menor.
# Pero, para la temperatura = 20, la distribución es más uniforme y dispersa, con aspecto
# de ser aleatoria.

# Obtenemos la media y la desviación típica de cada nivel del bloque:

tapply(duracion, temperatura, mean)
tapply(duracion, temperatura, sd)

# Las desviaciones parecen bastante dispares. Recurrimos al test de levene para asegurarnos
# de que existe homocedasticidad:

levene.test(duracion, temperatura, "median")

# El p-valor es suficientemente grande, así que asumimos que hay homocedasticidad.

# Imprimimos un gráfico que tiene en cuenta tanto el tipo de material como la temperatura:

stripchart(duracion ~ material + temperatura, vertical = T)

# Se cumple la influencia del tipo de material y de la temperatura, como se ha visto antes.

result2 = aov(duracion ~ material + temperatura)
summary(result2)

# Al ser los p-valores suficientemente pequeños, podemos decir que tanto el material
# como la temperatura son significativos para la duración de la batería.

# MODELO BIFACTORIAL
#********************

# Al ser un modelo bifactorial, vamos a añadir a los datos los tratamientos, es decir,
# la interacción entre el factor material y el factor temperatura:

tratamientos = interaction(material, temperatura)
datos$tratamientos = with(datos, tratamientos)
datos[, c("material", "temperatura", "tratamientos")]

stripchart(duracion ~ tratamientos, vertical = T)

# Observamos una interacción llamativa entre la temperatura 20 y el material 1,
# al haber una duración inusualmente baja en comparación con la de otros materiales
# a esa temperatura.

# Sacamos la tabla ANOVA del modelo bifactorial:

result3 = aov(duracion ~ material*temperatura)
summary(result3)

# Tanto el p-valor del material como el de la temperatura son lo suficientemente
# pequeños como para asegurar que influyen en la duración. Como el p-valor de la
# interacción entre la temperatura y el material es muy grande, no podemos asegurar
# que realmente exista una interacción entre ambos factores.

#******************************************

# Es lógico pensar que el material más adecuado para conseguir una mayor duración
# de la batería es el 3, puesto que es el mejor con temperatura 20 y 50, a pesar
# de que el 2 le supere ligeramente con temperatura -10.