setwd("~/workspace/r/bayesian/")
library(rstan)
library(coda)
set.seed(20151204)

#the explanatory variables
dat<-data.frame(x1=runif(100,-2,2),x2=runif(100,-2,2))
#the model
X<-model.matrix(~x1*x2,dat)
#the regression slopes
betas<-runif(4,-1,1)
#the standard deviation for the simulated data
sigma<-1
#the simulated data
y_norm<-rnorm(100,X%*%betas,sigma)
#a matrix to get the predicted y values
new_X<-model.matrix(~x1*x2,expand.grid(x1=seq(min(dat$x1),max(dat$x1),length=20),x2=c(min(dat$x2),mean(dat$x2),max(dat$x2))))

#the model
# m_norm<-stan(file="ex1.stan",data = list(N=100,N2=60,K=4,y=y_norm,X=X,new_X=new_X),pars = c("beta","sigma","y_pred"))
m_norm<-stan(file="ex1.stan",data = list(N=100,K=4,y=y_norm,X=X),pars = c("beta","sigma"))

#plotting the posterior distribution for the parameters
post_beta<-As.mcmc.list(m_norm,pars="beta")
# plot(post_beta)

#computing the posterior probability for the slopes to be bigger than 0
apply(extract(m_norm,pars="beta")$beta,2,function(x) length(which(x>0))/4000)

#plot the correlation between the parameters
pairs(m_norm,pars="beta")

#plotting credible intervals for the different betas
plot(m_norm,pars=c("beta","sigma"))
