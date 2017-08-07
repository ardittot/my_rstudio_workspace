setwd("~/workspace/r/bayesian/")
library(rstan)
library(coda)
set.seed(20151204)

# Predictor variables
X <- df[,2:5]
intercept <- rep(1,nrow(X))
X <- cbind(intercept, X)
# Response variables
y <- df$rate

#the model
N <- length(y)
K <- ncol(X)
sigma <- 0.0075
m_norm<-stan(file="abtest201708_v1.rstan",data = list(N=N,K=K,y=y,X=X,sigma=sigma),pars = c("param","sigma"))

#plotting the posterior distribution for the parameters
posterior<-As.mcmc.list(m_norm)
# plot(posterior)

#computing the posterior probability for the slopes to be bigger than 0
apply(extract(m_norm,pars="param")$param,2,function(x) length(which(x>0))/4000)

#plot the correlation between the parameters
pairs(m_norm,pars="param")

#plotting credible intervals for the different betas
plot(m_norm,pars=c("param","sigma"))
