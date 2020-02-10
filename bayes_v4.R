library(tidyr)
library(parallel)
library(MASS)
library(ggplot2)
library(dplyr)

setwd("C:/Users/Lukas/Studium/Mathematik/Bachelorarbeit/Metropolis/code")
source("bayes_functions_v2.R")
n.cores <- detectCores() - 1

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

#2) do several MH-runs with diffrent inital values


#if you work on Mac or Linux, you can use faster parallel programming:
#system.time(
#  results <- mclapply(as.list(seq(-50, 50, by=1)), FUN = do.mh.posterior, 
#                      ntot = 10000, nburn = 1000, mc.cores = n.cores)
#)


#single mh-run
#mh <- do.mh.posterior(ntot = 10000, nburn = 1000, init = 4, prior = "flat", prior_par = 30)

# Flat prior -------------------------------------------------------
results <- lapply(as.list(seq(-25, 25, by=5)), FUN = do.mh.posterior, 
                  ntot = 12000, nburn = 2000, prior = "flat", prior_par = 30)

accept_rates <- sapply(results, FUN = function(x){nrow(unique(x))/nrow(x)})
mean(accept_rates)
beta.hats <- sapply(results, FUN = function(x){apply(x, 2, mean)})
mean(beta.hats)

res <- results[[1]]
res$id <- paste0("mh", 1)
for(i in 2:length(results)){
  a <- results[[i]]
  a$id <- paste0("mh", i)
  res <- bind_rows(res, a)
}

ggplot(res, aes(x=beta1, colour = id)) +
  geom_density() +
  ggtitle("Posterior densities with Flat prior") +
  xlab("beta") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#OLS
ols <- lm(y ~ X - 1, data = data.frame(y=y, X=X))
coef(ols)

# compare results
data.frame(true_beta = beta0, BAYESIAN = mean(beta.hats), OLS = coef(ols))

ols <- lm(y ~ X, data = data.frame(y=y, X=X))
coef(ols)


# Normal prior -----------------------------------------------------
results <- lapply(as.list(seq(-25, 25, by=5)), FUN = do.mh.posterior, 
                  ntot = 12000, nburn = 2000, prior = "normal", prior_par = c(3, 1))

accept_rates <- sapply(results, FUN = function(x){nrow(unique(x))/nrow(x)})
mean(accept_rates)
beta.hats <- sapply(results, FUN = function(x){apply(x, 2, mean)})
mean(beta.hats)

res <- results[[1]]
res$id <- paste0("mh", 1)
for(i in 2:length(results)){
  a <- results[[i]]
  a$id <- paste0("mh", i)
  res <- bind_rows(res, a)
}

ggplot(res, aes(x=beta1, colour = id)) +
  geom_density() +
  ggtitle("Posterior densities with Normal prior") +
  xlab("beta") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

