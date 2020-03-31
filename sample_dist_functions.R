#------------------------- Functions -------------------------------------------
#-------------------------------------------------------------------------------
g <- function(x, pp=p, mu.=mu, sd.=sd){
  if(length(unique(sapply(list(p, mu, sd), length))) > 1){
    stop("p, mu and sd must be of same length")
  }
  
  probs <- sapply(x, dnorm, mean=mu., sd=sd.)
  pp%*%probs
}

#mh-algorithm as function; can add more options for proposal densities;
#tp do: if - stop check that par does fit to chosen which.porp
do.mh <- function(ntot, nburn,x.init, which.prop = "normal", par, f=g){
  x.save <- matrix(0, nrow = ntot, ncol = 1)
  x.save[1, ] <- x.init #initialise markov chain
  accept <- 0
  for (i in 2:ntot){
    if(which.prop=="normal"){
      x.prop <- rnorm(1, x.save[i-1, ], sd = par) #prop from normal
    }
    if(which.prop=="unif"){
      x.prop <- runif(1, par[1], par[2]) #prop from uniform
    }
    
    alpha <- min(1, f(x.prop)/f(x.save[i-1,])) #reversability 
    
    #accept/reject new state
    if (runif(1) < alpha){
      x.save[i, ] <- x.prop
      accept <- accept + 1
    } else{x.save[i, ] <- x.save[i-1, ]}
  }
  return(x.save[-(1:nburn), ])
}