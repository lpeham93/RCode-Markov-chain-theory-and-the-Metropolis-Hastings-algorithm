library(tidyr)
library(MASS)
library(ggplot2)
library(dplyr)

setwd(".")
source("bayes_lm_functions.R")

set.seed(1234)

#1) simulate data-----------------
n <- 1000
#beta0 <- 1:2
beta0 <- 0.5
k <- 1
sig0 <- 3
X <- matrix(rnorm(n*k, mean = 0, sd = 3), nrow = n, ncol = k)
y <- X %*% beta0 + rnorm(n, 0, sig0) 
ggplot(data.frame(y=y, x=X), aes(x=x, y=y)) +
  geom_point() +
  ggtitle("Scatterplot of simulated Data") +
  theme(plot.title = element_text(hjust = 0.5))
#--------------

#2)
#Tune standard deviation of normal proposal
#Flat prior
tune.flat <- lapply(as.list(seq(0.01, 0.2, length.out = 10)), 
                    FUN = function(x) do.mh.posterior(prop_sd = x, ntot = 2000, 
                                                      nburn = 1000, init = 10, prior = "flat", 
                                                      prior_par = 30, tuning = TRUE))
names(tune.flat) <- seq(0.01, 0.2, length.out = 10)

#Normal prior
tune.normal <- lapply(as.list(seq(0.01, 0.2, length.out = 10)), 
                      FUN = function(x) do.mh.posterior(prop_sd = x, ntot = 2000, 
                                                        nburn = 1000, init = 10, prior = "normal", 
                                                        prior_par = c(3,1), tuning = TRUE))
names(tune.normal) <- seq(0.01, 0.2, length.out = 10)

## -> use 0.05 as proposal standard deviation which gives an acceptance of ~25%

#3)
#Sample posterior for different initialisation values
# Flat prior -------------------------------------------------------
results.flat <- lapply(as.list(seq(-25, 25, by=5)), FUN = do.mh.posterior, 
                       ntot = 12000, nburn = 2000, prior = "flat", 
                       prior_par = 30, prop_sd = 0.05)

accept_rates <- sapply(results.flat, FUN = function(x){nrow(unique(x))/nrow(x)})
mean(accept_rates)
beta.hats.flat <- sapply(results.flat, FUN = function(x){apply(x, 2, mean)})
mean(beta.hats.flat)

res <- results.flat[[1]]
res$id <- paste0("mh", 1)
for(i in 2:length(results.flat)){
  a <- results.flat[[i]]
  a$id <- paste0("mh", i)
  res <- bind_rows(res, a)
}

ggplot(res, aes(x=beta1, colour = id)) +
  geom_density() +
  ggtitle("Posterior densities with Flat prior") +
  xlab("beta") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


# Normal prior -----------------------------------------------------
results.normal <- lapply(as.list(seq(-25, 25, by=5)), FUN = do.mh.posterior, 
                         ntot = 12000, nburn = 2000, prior = "normal", 
                         prior_par = c(3, 1), prop_sd = 0.05)

accept_rates <- sapply(results.normal, FUN = function(x){nrow(unique(x))/nrow(x)})
mean(accept_rates)
beta.hats.normal <- sapply(results.normal, FUN = function(x){apply(x, 2, mean)})
mean(beta.hats.normal)

res <- results.normal[[1]]
res$id <- paste0("mh", 1)
for(i in 2:length(results.normal)){
  a <- results.normal[[i]]
  a$id <- paste0("mh", i)
  res <- bind_rows(res, a)
}

ggplot(res, aes(x=beta1, colour = id)) +
  geom_density() +
  ggtitle("Posterior densities with Normal prior") +
  xlab("beta") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


#Compare the results with OLS -------------------
ols <- lm(y ~ X - 1, data = data.frame(y=y, X=X))
coef(ols)

data.frame(true_beta = beta0, BAYESIAN_flat = mean(beta.hats.flat), 
           BAYESIAN_normal = mean(beta.hats.normal), OLS = coef(ols))

coef(lm(y ~ X, data = data.frame(y=y, X=X)))
