#model 1
trueMean1 <- 7.8
trueSd1 <- 1.3
trueMean2 <- 4.3
trueSd2 <- 0.09
sampleSize <- 100

# create dependent values according to ax + b + N(0,sd)
y1 <- rnorm(n=sampleSize,mean=trueMean1,sd=trueSd1)
y2 <- rnorm(n=sampleSize,mean=trueMean2,sd=trueSd2)
y <- c(y1,y2)

hist(y, main="Data Distribution", nclass = 30)

likelihood <- function(param){
  mu1 = param[1]
  sd1 = param[2]
  mu2 = param[3]
  sd2 = param[4]
  
  singlelikelihoods1 = dnorm(y, mean = mu1, sd = sd1, log = T)
  singlelikelihoods2 = dnorm(y, mean = mu2, sd = sd2, log = T)
  singlelikelihoods <- c()
  for (i in length(y)) { 
    singlelikelihoods <- c(singlelikelihoods, max(singlelikelihoods1[i], singlelikelihoods2[i])) 
  }
  
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

# Prior distribution
prior <- function(param){
  mu1 = param[1]
  sd1 = param[2]
  mu2 = param[3]
  sd2 = param[4]

  muprior1 = dunif(mu1, min=0, max=10, log = T)
  muprior2 = dunif(mu2, min=0, max=10, log = T)
  sdprior1 = dunif(sd1, min=0, max=4, log = T)
  sdprior2 = dunif(sd2, min=0, max=4, log = T)
  
  muprior <- max(muprior1, muprior2)
  if ( muprior1 > muprior2 ) {
    sdprior <- sdprior1
  } else {
    sdprior <- sdprior2
  }
  
  return(muprior+sdprior)
}

#define posterior function
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

#define proposal function
proposalfunction <- function(param){
  #return(rnorm(4,mean = param, sd= c(0.2,0.2,0.2,0.2)))
  res <- rnorm(4,mean = param, sd= c(0.2,0.2,0.2,0.2))
  mu_min <- 0; mu_max <- 10
  sd_min <- 0; sd_max <- 3
  while ((res[1]<mu_min) | (res[3]<mu_min) | (res[1]>mu_max) | (res[3]>mu_max) | (res[2]<sd_min) | (res[4]<sd_min) | (res[2]>sd_max) | (res[4]>sd_max)) {
    res <- rnorm(4,mean = param, sd= c(0.2,0.2,0.2,0.2))
  }
  return(res)
}

#run MCMC algorithm function
run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,4))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

#run MCMC algorithm
startvalue = c(0,1,10,1)
chain = run_metropolis_MCMC(startvalue, 100000)

burnIn = 10000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))

### Summary: #######################

par(mfrow = c(4,1))
hist(chain[-(1:burnIn),1],nclass=20, , main="Posterior of Mean1", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]),col="blue")
abline(v = trueMean1, col="red" )
hist(chain[-(1:burnIn),2],nclass=20, main="Posterior of StdDev1", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]),col="blue")
abline(v = trueSd1, col="red" )
hist(chain[-(1:burnIn),3],nclass=20, , main="Posterior of Mean2", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),3]),col="blue")
abline(v = trueMean2, col="red" )
hist(chain[-(1:burnIn),4],nclass=20, main="Posterior of StdDev2", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),4]),col="blue")
abline(v = trueSd2, col="red" )

# for comparison:
summary(lm(y~x))

