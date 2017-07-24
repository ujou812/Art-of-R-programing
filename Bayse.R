library(MCMCpack)

regdata <- list(X = c(-2,-1,0,1,2,3), Y = c(1,3,3,3,5,6))
posterior <- MCMCregress(Y~X, data=regdata, burnin=1000, mcmc=10000, b0 = 0.0,B0 = 0, c0 = 2,d0 = 0.001, verbose = 1000)
plot(posterior)
summary(posterior)

library(survival)
example(tobin)
summary(tfit)
tfit.mcmc <- MCMCtobit(durable~age +quant, data = tobin,
                       mcmc = 20000, b0 = 0.0, B0 = 0, c0 = 0.001, d0 = 0.0001, verbose = 1000)
plot(tfit.mcmc)
summary(tfit.mcmc)
