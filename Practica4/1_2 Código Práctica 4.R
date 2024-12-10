# PRÁCTICA 4 ESTADÍSTICA APLICADA


# Introducimos los datos del enunciado en los vectores mat, temp, y dur.
# Así mismo construimos el data frame tabla que los recopila todos.

mat = rep(c(1, 2, 3), each = 12)
temp = rep(c(-10, 20, 50, -10, 20, 50, -10, 20, 50), each = 4)
dur = c(130,155,74,180,34,40,80,75,20,70,82,58,150,188,159,
             126,136,122,106,115,25,70,58,45,138,110,168,160,174,120,150,139,96,104,82,60)

tabla = data.frame(mat, temp, dur)

# MODELO UNIFACTORIAL SIMPLE COMPLETAMENTE ALEATORIZADO

# Analizamos gráficamente los datos del dataframe.

stripchart(dur ~ mat, vertical = T)

# Calculamos la media y la desviación típica de la duración de los materiales.

tapply(dur, mat, mean)
tapply(dur, mat, sd)

# Utilizamos el contraste de Levene para comprobar que se cumple el supuesto de homocedasticidad.

library(lawstat)
levene.test(dur, mat, "median")

# Obtenemos la tabla ANOVA para el modelo unifactorial simple.

result1 = aov(dur ~ mat)
summary(result1)

###################################
# MODELO UNIFACTORIAL POR BLOQUES #
###################################

# Observamos los datos con respecto a la temperatura (variable bloque).

stripchart(dur ~ temp, vertical = T)

# Obtenemos la media y la desviación típica de cada nivel del bloque.

tapply(dur, temp, mean)
tapply(dur, temp, sd)

# Recurrimos a un contraste de igualdad de varianzas (para varios grupos), como
# el test de Levene, para asegurarnos de que existe homocedasticidad.
# Contrasta la hipótesis nula de que todos los niveles o grupos tienen la misma 
# varianza.

levene.test(dur, temp, "median")

# Hacemos un gráfico que tiene en cuenta tanto el tipo de material como la 
# temperatura.

stripchart(dur ~ mat + temp, vertical = T)

# Obtenemos la tabla ANOVA para el modelo unifactorial por bloques.

result2 = aov(dur ~ mat + temp)
summary(result2)

######################
# MODELO BIFACTORIAL #
######################

# Al ser un modelo bifactorial, vamos a añadir a los datos la interacción entre 
# el factor material y el factor temperatura.

inter = interaction(mat, temp)
tabla$inter = with(tabla, inter)
tabla[, c("mat", "temp", "inter")]

# Creamos dos gráficos para ver de forma más clara los datos.

layout(1)
interaction.plot(temp,mat,dur)

plot(dur ~ inter, vertical = T)

# Obtenemos la tabla ANOVA para el modelo bifactorial.

result3 = aov(dur ~ mat*temp)
summary(result3)
