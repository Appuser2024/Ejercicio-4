
#### PLANTEAMIENTO DE HIPÓTESIS
# H0: p1 - p2 =0  vs Ha: p1-p2 >0 


# Calcular estadísticos básicos
n1 <- 300  # tamaño muestra hombres
n2 <- 400  # tamaño muestra mujeres
x1 <- 120  # éxitos hombres
x2 <- 120  # éxitos mujeres

# Calcular proporciones
p1 <- x1/n1  # proporción hombres
p2 <- x2/n2  # proporción mujeres
p_combinada <- (x1 + x2)/(n1 + n2)  # proporción combinada

# Calcular estadístico Z
z_stat <- (p1 - p2) / sqrt(p_combinada * (1-p_combinada) * (1/n1 + 1/n2))

# Calcular p-valor (prueba unilateral derecha)
p_valor <- 1 - pnorm(z_stat)

# Valor crítico
valor_critico <- qnorm(0.95)

# Imprimir resultados
cat("Resultados del análisis:\n")
cat("------------------------\n")
cat(sprintf("Proporción hombres (p1): %.4f\n", p1))
cat(sprintf("Proporción mujeres (p2): %.4f\n", p2))
cat(sprintf("Estadístico Z: %.4f\n", z_stat))
cat(sprintf("Valor crítico (α=0.05): %.4f\n", valor_critico))
cat(sprintf("p-valor: %.4f\n", p_valor))

# Crear gráfico
# Crear secuencia de valores z para la curva normal
z <- seq(-4, 4, length=1000)
densidad <- dnorm(z)

# Crear el gráfico
plot(z, densidad, type="l", lwd=2, 
     main="Prueba de Hipótesis para Diferencia de Proporciones",
     xlab="Z", ylab="Densidad",
     col="blue")

# Añadir región de rechazo
z_rechazo <- seq(valor_critico, 4, length=100)
densidad_rechazo <- dnorm(z_rechazo)
polygon(c(z_rechazo, rev(z_rechazo)), 
        c(densidad_rechazo, rep(0, length(z_rechazo))), 
        col=rgb(1, 0, 0, 0.3))

# Añadir líneas verticales
abline(v=z_stat, col="green", lwd=2, lty=2)
abline(v=valor_critico, col="black", lwd=2, lty=3)

# Añadir leyenda
legend("topright", 
       legend=c("Distribución Normal", 
                "Región de Rechazo (α=0.05)", 
                "Estadístico Z",
                "Valor crítico"),
       col=c("blue", "red", "green", "black"),
       lty=c(1, 1, 2, 3),
       lwd=c(2, 2, 2, 2),
       fill=c(NA, rgb(1, 0, 0, 0.3), NA, NA))

# Añadir cuadrícula
grid()

# Realizar prueba de proporciones usando función incorporada de R
prop.test(x=c(x1, x2), n=c(n1, n2), 
          alternative="greater",
          correct=FALSE)