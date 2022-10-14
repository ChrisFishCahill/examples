# the Bayesian coin example
# calculate the probability of getting heads given Chris is shady

par(mfrow = c(3, 1))

x <- seq(from = 0, to = 1, length.out = 1000) # range of possible probabilities
prior <- dunif(x, 0, 1) # what does this represent?
# prior <- dnorm(x, 0.5, 0.1)
# prior <- dnorm(x, 0.8, 0.1)
# prior <- dnorm(x, 0.1, 0.5)

plot(x, prior,
     ylab = "prior",
     xlab = "x",
     type = "l",
     col = "red",
     main = "Is chris a dirtbag (i.e., a cheater)?: the prior"
)

p_grid <- seq(from = 0, to = 1, length.out = 1000)
likelihood <- dbinom(4, size = 5, prob = p_grid)
plot(x, likelihood,
     type = "l", col = "blue",
     main = "The likelihood"
)

posterior <- likelihood * prior

plot(x, posterior,
     type = "l",
     col = "purple",
     main = "The posterior (i.e., prior x likelihood)"
)

# Frequentist test of p != 0.5:
binom.test(x = 4, n = 5, p = .5) # two sided
binom.test(x = 4, n = 5, p = .5, alternative = "greater") # greater than

# sample posterior with replacement
samples <- sample(p_grid, prob = posterior, size = 1e5, replace = TRUE)
sum(samples > 0.5) / length(samples) # Bayesian solution

# now update the prior
prior <- posterior
# pick a true value of p
my_prob <- 0.75

# initialize counters for the Frequentist test
x_counter <- 4
n_counter <- 5

set.seed(11)
for (i in 1:995) { # starting with 5 flips
  plot(x, prior,
       ylab = "prior",
       xlab = "x",
       type = "l",
       col = "red",
       lwd = 2,
       main = "The prior"
  )
  trial <- rbinom(n = 1, size = 1, prob = my_prob)
  likelihood <- dbinom(trial, size = 1, prob = p_grid)
  plot(x, likelihood,
       type = "l", col = "blue",
       lwd = 2,
       main = "The likelihood"
  )
  
  posterior <- likelihood * prior
  
  x_counter <- x_counter + trial
  n_counter <- n_counter + 1 # increment by one each time
  frequentist_test <- binom.test(x = x_counter, n = n_counter, p = .5) # two sided
  p_value <- round(frequentist_test$p.value, 4)
  # sample posterior with replacement
  samples <- sample(p_grid, prob = posterior, size = 1e5, replace = TRUE)
  pr <- round(sum(samples > 0.5) / length(samples), 4) # Bayesian solution
  
  plot(x, posterior,
       type = "l",
       col = "purple",
       lwd = 2,
       main = paste0("The posterior (prior x likelihood): coin flip = ", i + 5)
  )
  abline(v = my_prob, lty = 3, lwd = 3, col = "black")
  legend(
    x = "topleft", 
    legend = c("true Pr(heads)", "Estimated Posterior"), 
    lty = c(3, 1), 
    col = c("black", "purple"), 
    lwd = 2, cex = 1
  )
  prior <- posterior
  cat("\n")
  cat(paste0("Trial = ", i + 5))
  cat(paste0(
    "\nBayesian result: Pr(heads > 0.5) = ",
    pr, "\nFrequentist result: p-value = ",
    p_value
  ))
  cat("\n")
  Sys.sleep(0.1)
}
