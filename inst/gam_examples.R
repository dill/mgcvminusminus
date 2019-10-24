# examples from mgcv::gam
library(mgcv)

set.seed(2) ## simulate some data... 
dat <- gamSim(1,n=400,dist="normal",scale=2)

source("../R/TMBam.R")
source("../R/lp.R")

tmb <- TMBam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat, file="thingo")
#tmb <- TMBam(y~s(x0),data=dat, file="thingo")

library(TMB)
compile("thingo.cpp")

dyn.load(dynlib("thingo"))


# build the model
model <- MakeADFun(data=tmb$tmb.data, parameters=tmb$tmb.ini,
                   DLL="thingo", random=c("beta"))

# actually do the optimisation
# lots of output from the trace
opt <- nlminb(model$par, model$fn, model$gr, control=list(trace=1))

# calculate standard deviations for the estimated pars
rep <- sdreport(model)



bb <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat, select=TRUE, method="REML")


# compare
plot(coef(bb), c(rep$par.fixed[1], rep$par.random), xlab="mgcv", ylab="TMB", asp=1)
abline(a=0, b=1)

