# RCode: Markov-chain-theory-and-the-Metropolis-Hastings-algorithm

In the file "bayes_lm_functions.R" are the necessary functions to approximate the posterior of a univariate linear regression model using the metropolis hastings algorithm. The sampling of the posterior is done using parallel coding (for Linux and Mac) and without parallel coding. So far the code is only finished for univariate regressions, however the main function do.mh.posterior() has already a silent option to sample multivariate regressions. Further testing is needed before I can publish this option

The files "sample_dist_function.R" and "sample_distribution.R" provide the code to approximate mixed normal distributions.

