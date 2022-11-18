# cahill 4 November 2022
# a multi-parameter model demo in Stan

# libraries
library(dplyr)
library(ggplot2)
library(rstan)
library(tidybayes)
library(cowplot)
devtools::install_github("ChrisFishCahill/gg-qfc")
library(ggqfc)

# simulate data for Gorbachev's homing mussels

# model form:
# y_i ~ Poisson(lambda_i) # i is survey count
# E(y_i) = lambda_i
# Var(y_i) = lambda_i
# log(lambda_i) = beta_0 + beta_1*x1, where x1 is km
# define some (true) parameters and data for simulation

n_replicates <- 3 # replicates each location along river
survey_locations <- seq(from = 0, to = 30, by = 5)
n_surveys <- length(survey_locations) * n_replicates

lambda <- 2 # average number of mussels normal space
beta_0 <- log(lambda) # mean expected count in log space
beta_1 <- 0.15 # river km effect

# define the linear predictor
mu <- exp(beta_0 + beta_1 * survey_locations)

# generate the "observed" data
set.seed(666) # spooky seazon
y_i <- rpois(n_surveys, mu) # error

data <- data.frame(
  "y_i" = y_i, "km" = rep(survey_locations, n_replicates),
  "survey" = rep(1:n_replicates, each = length(survey_locations))
)

glimpse(data)
# plot the data
data %>%
  ggplot(aes(x = km, y = y_i, colour = as.factor(survey))) +
  geom_point() + # position = position_jitter(w = 0.2, h = 0.1)
  xlab("river km") +
  ylab("number of mussels") +
  labs(colour = "survey") +
  theme_qfc()

data

fit <- glm(data$y_i ~ data$km, family = "poisson")
summary(fit)
confint(fit)
beta_0
beta_1

#----------------------------------------------------------------------
# stan stuff

# detect number of CPUs on current host
options(mc.cores = parallel::detectCores())

# eliminate redundant compilations
rstan::rstan_options(auto_write = TRUE)

m <-
  rstan::stan_model(
    "ch34/src/mussels.stan",
    verbose = T
  )

stan_data <- list(
  "n_data" = nrow(data), 
  "y_i" = data$y_i, 
  "km" = data$km
)

# I always code this as a function:
inits <- function() {
  list(
     "beta0" = jitter(1), 
     "beta1" = jitter(0)
    )
}

inits()

# run the model
fit <-
  rstan::sampling(
    m,
    data = stan_data,
    pars = c("beta0", "beta1"),
    iter = 100,
    warmup = 12,
    chains = 1,
    init = inits
    )
shinystan::launch_shinystan(fit)
# plot it 
p1 <- fit %>%
  spread_draws(beta0) %>%
  ggplot(aes(x=beta0)) +
  geom_histogram(bins = 20)

p2 <- fit %>%
  spread_draws(beta1) %>%
  ggplot(aes(x=beta1)) +
  geom_histogram(bins = 20)

p3 <- plot_grid(p1, p2)
p3
