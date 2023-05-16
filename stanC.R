library(rstan)
library(modeest)


# Define the Stan model
stan_code <- "
data {
  int<lower=0> N;  // Number of data points
  int X[N];  // Observed data
}

parameters {
  real<lower=0, upper=1> p;  // Binomial parameter
  real<lower=0> lambda;      // Poisson parameter
  real<lower=0, upper=1> q;  // Negative Binomial parameter
  simplex[3] weights;        // Mixture weights
}

model {
  // Prior distributions
  p ~ beta(1, 1);
  lambda ~ gamma(5, 1);
  q ~ beta(1, 1);
  weights ~ dirichlet(rep_vector(1.5, 3));
  

  // Likelihood
  for (n in 1:N) {
     vector[3] lik;
      lik[1] = weights[1] * binomial_lpmf(X[n] | 40, p);
      lik[2] = weights[2] * poisson_lpmf(X[n] | lambda);
      lik[3] = weights[3] * neg_binomial_lpmf(X[n] | 25, q);
    
      target += log_sum_exp(lik);
  }
}

"

# Generate some example data
#set.seed(1)
#N <- 100
#p_true <- 0.3
#lambda_true <- 5
#q_true <- 0.5
#weights_true <- c(0.2, 0.5, 0.3)

#X <- replicate(N, {
 # component <- sample(1:3, 1, prob = weights_true)
 # if (component == 1) rbinom(1, 40, p_true)
# else if (component == 2) rpois(1, lambda_true)
  #else rnbinom(1, 25, q_true)
#})

#DATA
sample <- c(16,  7,  9, 14, 25, 14,  9, 17, 18, 17, 23,  5, 24, 27, 23,
            18, 22, 30, 18, 14, 18,  6,  5,  9, 24, 19, 19, 16, 17, 17,  5, 18, 18, 23,
            7,  7,  9, 25, 14, 15,  6, 14, 20, 18, 27,  9,  4,  4, 22, 23,  1,
            25, 24,  6,  8, 21, 19, 15, 17,  7, 19, 25, 24,  3,  6, 18, 20,  2,
            24,  6, 25, 15, 19,  5,  5, 28, 19, 20,  4, 13, 23, 19, 25, 11, 14,
            4, 16,  2, 27, 19, 22, 10, 14, 19, 14,  4,  5, 16, 21, 24)
X<-sample
N<-100

# Prepare the data for Stan
stan_data <- list(N = N, X = X)

# Compile the Stan model
stan_model <- stan_model(model_code = stan_code)

# Run MCMC sampling
mcmc_samples <- sampling(stan_model, data = stan_data, chains = 5, iter = 100000)

# Print a summary of the posterior
print(mcmc_samples)

# Extract parameter and weight samples
p_samples <- extract(mcmc_samples)$p
lambda_samples <- extract(mcmc_samples)$lambda
q_samples <- extract(mcmc_samples)$q
weights_samples <- extract(mcmc_samples)$weights

# Calculate posterior means
p_mean <- mean(p_samples)
lambda_mean <- mean(lambda_samples)
q_mean <- mean(q_samples)
weights_mean <- colMeans(weights_samples)

# Print the estimated parameter and weight means
cat("Estimated p:", p_mean, "\n")
cat("Estimated lambda:", lambda_mean, "\n")
cat("Estimated q:", q_mean, "\n")
cat("Estimated weights:", weights_mean, "\n")

#GRAPH
#WEIGHTS
w1<-.6
w2<-.2
w3<-1-w1-w2

#PARAMETERS
n = 40  # Number of trials for binomial distribution
p <- 0.42  # Probability of success for binomial distribution
r = 25  # Size parameter for negative binomial distribution
q <- 0.7  # Probability of success for negative binomial distribution
l <- 6.16  # Rate parameter for Poisson distribution

# Generate x values
x <- 0:30  # Range of x values

mixed_function<-function(x){w1*dbinom(x, size = n, prob = p)+w2*dpois(x, lambda = l)+w3*dnbinom(x, size = r, prob = q)}

curve_values <- mixed_function(x)

ggplot(data.frame(sample), aes(x = sample)) +
  geom_histogram(binwidth = 1, fill = "#87B4DA", color = "white", aes(y = ..density..)) +
  labs(title = "Histograma con curva de ajuste", x = "Valores", y = "Densidad") +
  scale_x_continuous(breaks = seq(0, 30, 2.5))+ 
  geom_line(data = data.frame(x = x, y = curve_values), aes(x = x, y = y), color = "red")
summary(sample)

#PARAMETERS DISTRIBUTIONS
ggplot(data.frame(p_samples), aes(x = p_samples)) +
  geom_histogram(binwidth = .05, fill = "#87B4DA", color = "white") +
  labs(title = "Histograma de estimaciones de p", x = "Valores", y = "Frecuencia") +
  scale_x_continuous(breaks = seq(0, 1, .05))#+ geom_density(color = "red")
  summary(p_sample)

ggplot(data.frame(q_samples), aes(x = q_samples)) +
  geom_histogram(binwidth = .05, fill = "#87B4DA", color = "white") +
  labs(title = "Histograma de estimaciones de q", x = "Valores", y = "Frecuencia") +
  scale_x_continuous(breaks = seq(0, 1, .05))
summary(q_sample)

ggplot(data.frame(lambda_samples), aes(x = lambda_samples)) +
  geom_histogram(binwidth = 1, fill = "#87B4DA", color = "white") +
  labs(title = "Histograma de estimaciones de lambda", x = "Valores", y = "Frecuencia") +
  scale_x_continuous(breaks = seq(0, 22,1))
  summary_stats(lambda_sample)
