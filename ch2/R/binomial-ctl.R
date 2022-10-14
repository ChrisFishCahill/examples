#-------------------------------------------------------------------------------
# A simple binomial estimation example
# Cahill 14 Oct 2022
#-------------------------------------------------------------------------------
# load packages
library(tidyverse)
devtools::install_github("ChrisFishCahill/gg-qfc")
library(ggqfc)
library(rstan)
library(tidybayes)
library(cowplot)

#-------------------------------------------------------------------------------
# simulation
# observation ~ bernoulli(theta)
# where theta is the Pr(observing Kentucky Jaguar Worms  ~~~ @ some study site)
# 1 = jaguar worm observed, 0 = sadness
#-------------------------------------------------------------------------------

# set up the true values/simulation control parameters
set.seed(1) # keep "random" data the same
n_site_visit <- 100
theta_true <- 0.19

# generate the fake data
y <- rbinom(n_site_visit, 1, theta_true)
sim_data <- data.frame(y = y, survey = 1:n_site_visit)

# plot it for a sanity check
sim_data %>%
  ggplot(aes(x = survey, y = y)) +
  geom_point() +
  geom_line() +
  ylab("Jaguar worm observed?") +
  xlab("Site visit") +
  ggtitle("0 = not detected, 1 = worm seen") +
  theme_qfc()

seen <- 0:50
df <- data.frame(x = seen, prob = dbinom(seen, 100, prob = theta_true))
df %>%
  ggplot(aes(x = x, y = prob)) +
  geom_line() +
  xlab("True distribution of Jaguar Worm observations") +
  ylab("Pr(x detections | 100 site visits)") +
  ggtitle(expression(If ~ you ~ visit ~ site ~ `100` ~ times ~ 
                       and ~ theta[true] ~ `=` ~ `0.19`)) +
  theme_qfc()

theta_e <- sum(y) / n_site_visit
message(paste0("Empirical estimate of theta = ", theta_e))

#-------------------------------------------------------------------------------
# detect number of CPUs on current host
options(mc.cores = parallel::detectCores())

# eliminate redundant compilations
rstan::rstan_options(auto_write = TRUE)

# compile the stan model
path <- "ch2/src/binomial.stan"
m <- rstan::stan_model(path, verbose = T)

# set up the data and the initial values
stan_data <-
  list(
    n_data = nrow(sim_data),
    y = sim_data$y
  )
stan_data

inits <- function() {
  list(
    theta = jitter(0.5, amount = 0.1)
  )
}
inits()

fit0 <-
  rstan::sampling(
    m,
    data = stan_data,
    pars = "theta",
    iter = 1e5 # run 10000 simulations
  )

fit0 %>% # take object fit0
  spread_draws(theta) %>% # pluck out the estimates of theta
  head(15) # look at the first 15

nbin <- 25 # controls histogram binning
prior <- fit0 %>%
  spread_draws(theta) %>%
  ggplot(aes(x = theta)) +
  geom_histogram(bins = nbin) +
  theme_qfc() +
  xlab("Pr(jaguar worm detection | study site visit)") +
  ggtitle(expression(Prior ~ predictive ~ distribtion ~ `for` ~ theta))
prior

#-------------------------------------------------------------------------------
# compile the stan model, fit it to data
m <- rstan::stan_model(path, verbose = T)

fit <-
  rstan::sampling(
    m,
    data = stan_data,
    pars = "theta"
    # note! this function will run 2000 iterations x 4 chains by default
    # but you can specify it explicitly (and we will do so later)
    # see ?sampling
  )

posterior <- fit %>%
  spread_draws(theta) %>%
  ggplot(aes(x = theta)) +
  geom_histogram(bins = nbin) +
  theme_qfc() +
  xlab("Pr(jaguar worm detection | study site visit)") +
  ggtitle(expression(Posterior ~ distribtion ~ `for` ~ theta)) +
  geom_vline(xintercept = theta_true, lwd = 1, linetype = 2) # add true theta
posterior

posterior + geom_vline(
  xintercept = theta_e, lwd = 1,
  linetype = 2, color = "blue"
) # add true theta

# can visualize *many* ways, e.g.,
fit %>%
  spread_draws(theta) %>%
  ggplot(aes(y = theta, x = "")) +
  geom_violin() +
  ylab("vlaue") +
  xlab(expression(theta)) +
  theme_qfc()

fit %>%
  spread_draws(theta) %>%
  ggplot(aes(y = theta, x = "")) +
  geom_boxplot() +
  ylab("vlaue") +
  xlab(expression(theta)) +
  theme_qfc()

#-------------------------------------------------------------------------------
posterior <- posterior + xlim(0, 1) # +
# geom_vline(xintercept = theta_e, lwd = 1, linetype = 1) # empirical theta
p1 <- plot_grid(prior, posterior, ncol = 1)
p1

#-------------------------------------------------------------------------------
# exercises / stuff worth thinking about in your copious amounts of free time:

# Question 1:
# compiling code takes time, and time is important for model debugging.
# can you figure out a clever way to quickly change prior families / jump between
# prior distributions when your supervisor requests you demonstrate that your results
# are/are not robust to your prior choice? Hint: if-statements are your friend

# Question 2:
# If you don't like vectorization in the stan code, you can do it by looping instead.
# Try it, and see if you get the same answers.  There are *lots* of instances /
# bespoke models where this sort of thing is very helpful

# Question 3:
# So far we have calculated the prior predictive distribution based on our prior,
# and then (numerically) integrated across the prior and likelihood to generate a posterior
# distribution for the best estimate of theta.

# This is different from a so-called posterior predictive distribution for theta,
# which is the Pr(we see a worm | future site visit).
# Can you figure out how to adapt this code to calculate the posterior predictive
# distribution for this problem? 

# Question 4: 
# Regardless of whether you figured out how to implement question 3, how might you 
# be able to use this information to inform managers/biologists/sane humans who 
# do not live in the Matrix on how best to monitor or survey their beloved Kentucky
# jaguar worms? 
#-------------------------------------------------------------------------------
