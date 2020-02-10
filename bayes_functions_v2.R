#---------------  BAYESIAN INFERENCE -------------------------------------------
#Functions

#1) LogLikelihood
logL <- function(par, yy = y, XX = X){
  sigma <- par[length(par)]
  beta <- par[-length(par)]
  sum(dnorm(yy, mean = XX%*%beta, sd = sigma, log = T))
}


prior <- function(beta, form, par){
  if(form == "flat"){
    if(length(par) != 1 | par[1] < 0){stop("Flat prior needs 1 positive parameter!
                                           For lower and upper boundery of uniform distribution")}
    return(dunif(beta, -par, par, log = TRUE))}
  if(form == "normal"){
    if(length(par) != 2 | par[2] < 0){stop("Normal prior needs 2 parameters. par[1] == mean
                                           par[2] == sd. sd > 0")}
    return(dnorm(beta, par[1], par[2], log = TRUE))}
  if(form == "standard normal"){return(dnorm(beta, 0, 1, log = TRUE))}
}

posterior <- function(beta, sig, yy=y, XX=X, prior.form = "flat", ppar){
  logL(c(beta, sig)) + prior(beta, form = prior.form, par = ppar)
}

#also include prop_par
do.mh.posterior <- function(ntot, nburn, init, prior = "flat", prior_par){
  beta.save <- matrix(0, nrow = ntot, ncol = k)
  sig.save <- matrix(0, nrow = ntot, ncol = 1)
  #alpha.save <- matrix(0, nrow = ntot, ncol = 1)
  beta.save[1, ] <- init
  acceptance <- 0
  for (i in 2:ntot){
    #draw a proposal state for beta;
    #ATTENTIOM: variance was tuned before hand to achieve a proper acceptance rate
    beta.prop <- rnorm(1, beta.save[i-1, ], 0.05) #proposal for beta
    #beta.prop <- rmvnorm(1, beta.save[i-1, ], diag(0.01, k)) #proposal for beta, beta k-dimensional
    #sig.prop <- rexp(1)
    
    alpha <- min(0, 
                 posterior(beta.prop, 1, prior.form = prior, ppar = prior_par) 
                 - posterior(beta.save[i-1, ], 1, prior.form = prior, ppar = prior_par))
    
    #alpha.save[i, ] <-alpha
    
    #accept/reject new state
    if (log(runif(1)) < alpha){
      beta.save[i, ] <- beta.prop
      acceptance <- acceptance + 1
    } else{beta.save[i, ] <- beta.save[i-1, ]}
  }
  #return(list(mh = beta.save[-(1:nburn), ], acc_rate = acceptance/(ntot-nburn)))
  df.beta <- data.frame(beta.save[-(1:nburn), ])
  names(df.beta) <- "beta1"
  return(df.beta)
}
