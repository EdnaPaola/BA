# Cantidad de muestras predictivas a generar
num_muestras <- 100

# Valores de los pesos p y q
w1<-.6
w2<-.2
w3<-1-w1-w2

#PARAMETERS
n = 40  # Number of trials for binomial distribution
p <- 0.42  # Probability of success for binomial distribution
r = 25  # Size parameter for negative binomial distribution
q <- 0.7  # Probability of success for negative binomial distribution
l <- 6.16  # Rate parameter for Poisson distribution

# Función para generar una muestra predictiva
generar_muestra_predictiva <- function(w1, w2) {
  # Generar una muestra de p y q según sus probabilidades
  distribuciones <- c("binomial", "poisson", "binomial_negativa")
  seleccion <- sample(distribuciones, 1, prob = c(w1, w2, 1 - w1 - w2))
  seleccion
  
  # Muestrear un valor de la distribución seleccionada
  if (seleccion == "binomial") {
    n <- n
    prob <- p
    muestra <- rbinom(1, n, prob)
  } else if (seleccion == "poisson") {
    lambda <- l
    muestra <- rpois(1, lambda)
  } else if (seleccion == "binomial_negativa") {
    size <- 25
    prob <- q
    muestra <- rnbinom(1, size, prob)
  }
  
  return(muestra)
}

# Generar conjunto de muestras predictivas
muestras_predictivas <- replicate(num_muestras, generar_muestra_predictiva(w1, w2))

# Imprimir las muestras predictivas generadas
print(muestras_predictivas)
hist(muestras_predictivas)
ggplot(data.frame(muestras_predictivas), aes(x = muestras_predictivas)) +
  geom_histogram(binwidth = 1, fill = "#87B4DA", color = "white", aes(y = ..density..)) +
  labs(title = "Histograma de muestras predictivas", x = "Valores", y = "Densidad") +
  scale_x_continuous(breaks = seq(0, 30, 2.5))
summary((muestras_predictivas))
