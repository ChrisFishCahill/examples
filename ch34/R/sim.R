# cahill 4 November 2022
# a multi-parameter model demo in Stan 

# libraries
library(dplyr)
library(ggplot2)
library(rstan)
devtools::install_github("ChrisFishCahill/gg-qfc")
library(ggqfc)

# simulate data for Gorbachev's homing mussels

# model form:
# y_i ~ Poisson(lambda_i) # i is survey count
# E(y_i) = lambda_i
# Var(y_i) = lambda_i
# log(lambda_i) = beta_0 + beta_1*x1, where x1 is km--this is process component
# define some (true) parameters and data for simulation

n_replicates <-  # replicates each location along river
survey_locations <- seq(from = , to = , by = )
n_surveys <- length(survey_locations) * n_replicates

lambda <-  # average number of mussels normal space
beta_0 <- log(lambda) # mean expected count in log space
beta_1 <- # river km effect

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
  geom_point(position = position_jitter(w = 0.2, h = 0.1)) +
  xlab("river km") +
  ylab("number of mussels") +
  labs(colour = "survey") +
  theme_qfc()

#----------------------------------------------------------------------
# stan stuff

# detect number of CPUs on current host
options(mc.cores = parallel::detectCores())

# eliminate redundant compilations
rstan::rstan_options(auto_write = TRUE)

m <-
  rstan::stan_model(
    "src/mussels.stan",
    verbose = T
  )

stan_data <- list(
  
)

# I always code this as a function:
inits <- function() {
  list(
  )
}

#run the model
fit <-
  rstan::sampling(
    m,
    data = ,
    pars =
      c(
      ),
    iter = 1,
    warmup = 1,
    chains = 1,
    init = inits
  )


fit
