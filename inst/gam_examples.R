# examples from mgcv::gam
library(mgcv)

set.seed(2) ## simulate some data... 
dat <- gamSim(1,n=400,dist="normal",scale=2)
#b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)


source("../R/TMBam.R")

#tmb <- TMBam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat, file="thingo")
tmb <- TMBam(y~s(x0),data=dat, file="thingo")

library(TMB)
compile("thingo.cpp")

dyn.load(dynlib("thingo"))


# build the model
model <- MakeADFun(data=tmb$jags.data, parameters=tmb$jags.ini,
                   DLL="thingo", random=c("beta"))

# actually do the optimisation
# lots of output from the trace
opt <- nlminb(model$par, model$fn, model$gr, control=list(trace=1))

# calculate standard deviations for the estimated pars
rep <- sdreport(model)


