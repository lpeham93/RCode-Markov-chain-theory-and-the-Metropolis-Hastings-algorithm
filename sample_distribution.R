library(ggplot2)
source("sample_dist_functions.R")

set.seed(1234)

#) Example 1
p <- c(0.3, 0.4, 0.3)
mu <- c(-1, 2, 4)
sd <- c(.7, 1, .4)

mh <- do.mh(ntot = 101000, nburn = 1000,x.init = -10, par = 3)
accept_rate <- length(unique(mh))/length(mh)

#--Nice plot with ggplot-------------------------------
mh <- data.frame(mh)
names(mh) <- "x"
mh$N <- "100k"

k1 <- mh[1:1000, ,drop = F]
k1$N <- "1k"

k10 <- mh[1:10000, ,drop = F]
k10$N <- "10k"

mh <- bind_rows(mh, k1, k10)

ggplot(mh, aes(x = x, colour = N)) +
  #  stat_function(fun = g) +
  geom_density() +
  stat_function(fun = g, n=10000, color = "black") +
  ggtitle("Evolution of MH algorithm") +
  theme(plot.title = element_text(hjust = 0.5))
#------------------------------------------------------

#) Example 2
p <- c(.5, .5)
mu <- c(-5, 5)
sd <- c(.5, .5)

mh <- do.mh(ntot = 101000, nburn = 1000, x.init = -10, par = 2)
accept_rate <- length(unique(mh))/length(mh)


mh <- data.frame(mh)
names(mh) <- "x"
mh$N <- "100k"

k1 <- mh[1:1000, ,drop = F]
k1$N <- "1k"

k10 <- mh[1:10000, ,drop = F]
k10$N <- "10k"

mh <- bind_rows(mh, k1, k10)
ggplot(mh, aes(x = x, colour = N)) +
  #  stat_function(fun = g) +
  geom_density() +
  stat_function(fun = g, n=10000, color = "black") +
  ggtitle("Evolution of MH algorithm") +
  theme(plot.title = element_text(hjust = 0.5))
#------------------------------------------------------
