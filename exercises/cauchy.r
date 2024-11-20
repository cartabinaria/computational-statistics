n_sim <- 10^3
U <- runif(n_sim)
sigma <- 1
mu <- 0

X <- sigma * tan(pi * (U - 0.5)) + mu
plot(ecdf(X), main = "Empirical CDF of Cauchy-distributed Sample")
