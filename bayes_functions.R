#---------------  BAYESIAN INFERENCE -------------------------------------------
#Functions

#1) LogLikelihood
logL <- function(par, yy = y, XX = X){
  sigma <- par[length(par)]
  beta <- par[-length(par)]
  sum(dnorm(yy, mean = XX%*%beta, sd = sigma, log = T))
}


prior <- function(beta, sig, form){
  if(form == "flat"){return(dunif(beta, -100, 100, log = TRUE))}
  if(form == "standard normal"){return(dnorm(beta, 0, 1, log = TRUE))}
}

posterior <- function(beta, sig, yy=y, XX=X, prior.form = "flat"){
  logL(c(beta, sig)) + prior(beta, sig, form = prior.form)
}

do.mh.posterior <- function(ntot, nburn, init){
  beta.save <- matrix(0, nrow = ntot, ncol = k)
  sig.save <- matrix(0, nrow = ntot, ncol = 1)
  #alpha.save <- matrix(0, nrow = ntot, ncol = 1)
  beta.save[1, ] <- init
  acceptance <- 0
  for (i in 2:ntot){
    #draw a proposal state for beta;
    #ATTENTIOM: variance was tuned before hand to achieve a proper acceptance rate
    #beta.prop <- rnorm(1, beta.save[i-1, ], 0.05) #proposal for beta
    beta.prop <- rmvnorm(1, beta.save[i-1, ], diag(0.01, k)) #proposal for beta, beta k-dimensional
    #sig.prop <- rexp(1)
    
    #acceptance criterion with Log == TRUE ---------------
    alpha <- min(0, 
                 posterior(beta.prop, 1, prior.form = "standard normal") 
                 - posterior(beta.save[i-1, ], 1, prior.form = "standard normal"))
    
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
