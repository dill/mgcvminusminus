# write code for the linear predictor and response distribution
tmbam.lp <- function(resp,family,use.weights,offset=FALSE) {

  iltab <- ## table of inverse link functions
    c("eta","exp(eta)","ilogit(eta)","phi(eta)","1/eta","eta^2")
  names(iltab) <- c("identity","log","logit","probit","inverse","sqrt")
  if (!family$link%in%names(iltab)) stop("sorry link not yet handled")

  # if there is an offset, add that to the linear predictor
  # else just calculate the 
  if (offset){
    lp <- "  vector<Type> eta = mu + X*beta + offset;\n"
  }else{
    lp <- "  vector<Type> eta = mu + X*beta;\n"
  }
  ## code linear predictor and expected response...
  if(family$link!="identity"){
    lp <- paste0(lp, "  eta = ", iltab[family$link], ";\n")
    #lp <- paste0("  for(int i=0; i<y.size(); i++)\n",
    #             "    vector<Type> ",
    #             iltab[family$link],") = mu + X*beta;\n")
  }

  ## code the response given mu and any scale parameter prior...
  #scale <- TRUE ## is scale parameter free?
  ll <- paste0("  for(int i=0; i<", resp,".size(); i++)\n")

  if (family$family=="gaussian") {
    hyperpars <- c("  Type sigma = exp(log_sigma);")
    hyperpars_pars <- c("  PARAMETER(log_sigma);\n")
    if (use.weights){
stop("weights not supported")
#      cat("nll -= dnorm(", resp, "(i), eta(i), tau*w(i))\n")
    }else{
      ll <- paste0(ll,
                   "      nll -= dnorm(", resp, "(i), eta(i), sigma, true);\n")
    }
  } else if (family$family=="poisson") {
   # scale <- FALSE
    hyperpars <- c()
    hyperpars_pars <- c()
    ll <- paste0(ll,
                 "      nll -= dpois(", resp, "(i), eta(i), true);\n")
    if (use.weights) warning("weights ignored") 
    use.weights <- FALSE
  }else if (family$family=="binomial") {
    hyperpars <- c()
    hyperpars_pars <- c()
   # scale <- FALSE
    ll <- paste0(ll,
                 "      nll -= dbinom(", resp, "(i), w(i), eta(i), true);\n")
    use.weights <- TRUE
  } else if (family$family=="Gamma") {
stop("no gamma yet!")
#    if(use.weights){
#      cat(resp,"(i) ~ dgamma(r*w(i),r*w(i)/mu(i)) } ## response \n")
#    }else{
#      cat(resp,"(i) ~ dgamma(r,r/mu(i)) } ## response \n")
#    }
#
#    cat("  r ~ dgamma(.05,.005) ## scale parameter prior \n")
#    cat("  scale <- 1/r ## convert r to standard GLM scale\n")
  }else{
    stop("family not implemented yet")
  }

  return(list(lp=lp,
              ll=ll,
              hyperpars_pars=hyperpars_pars,
              hyperpars=hyperpars))
}

