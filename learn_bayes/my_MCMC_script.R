#model 1
trueMean <- 7.8
trueSd <-1.2
sampleSize <- 100

# create dependent values according to ax + b + N(0,sd)
y <-  rnorm(n=sampleSize,mean=trueMean,sd=trueSd)

hist(y, main="Data Distribution")

likelihood <- function(param){
  mu = param[1]
  sd = param[2]
  
  singlelikelihoods = dnorm(y, mean = mu, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

# Prior distribution
prior <- function(param){
  mu = param[1]
  sd = param[2]
  muprior = dunif(mu, min=0, max=10, log = T)
  sdprior = dunif(sd, min=0, max=3, log = T)
  return(muprior+sdprior)
}

#define posterior function
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

#define proposal function
proposalfunction <- function(param){
  return(rnorm(2,mean = param, sd= c(0.2,0.2)))
}

#run MCMC algorithm function
run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,2))
  chain[1,] = startvalue
  for (i in 1:iterations){
    # paste(chain[1,])
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
startvalue = c(3,1)
chain = run_metropolis_MCMC(startvalue, 100000)

burnIn = 10000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))

### Summary: #######################

par(mfrow = c(2,2))
hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of Mean", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]),col="blue")
abline(v = trueMean, col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of StdDev", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]),col="blue")
abline(v = trueSd, col="red" )
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of Mean", )
abline(h = trueA, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of StdDev", )
abline(h = trueB, col="red" )

# for comparison:
summary(lm(y~x))

