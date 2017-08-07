#model 1
x1 <- c(0,0,1,1); x2 <- c(0,1,0,1); success <- c(234,811,278,922); size <- c(1120,1032,987,1087)
trueVar1 <- 0.07
trueVar2 <- 0.57
trueVarC <- 0.21

likelihood <- function(param){
  var1 = param[1]
  var2 = param[2]
  varC = param[3]
  
  pred <- (var1 * x1) + (var2 * x2) + varC
  pred <- ifelse(pred>1, 1, pred)
  singlelikelihoods = dbinom(x = success, size = size, prob = pred, log = TRUE)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

# Prior distribution
prior <- function(param){
  var1 = param[1]
  var2 = param[2]
  varC = param[3]
  var1prior = dunif(var1, min=0, max=1, log = T)
  var2prior = dunif(var2, min=0, max=1, log = T)
  varCprior = dunif(varC, min=0, max=1, log = T)
  return(var1prior+var2prior+varCprior)
}

#define posterior function
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

#define proposal function
proposalfunction <- function(param){
  # return(rnorm(3,mean = param, sd= c(0.1,0.1,0.1)))
  res <- rnorm(3,mean = param, sd= c(0.1,0.1,0.02))
  var1_min <- 0; var1_max <- 1
  var2_min <- 0; var2_max <- 1
  varC_min <- 0; varC_max <- 0.4
  while ((res[1]<var1_min) | (res[1]>var1_max) | (res[2]<var2_min) | (res[2]>var2_max) | (res[3]<varC_min) | (res[3]>varC_max)) {
    res <- rnorm(3,mean = param, sd= c(0.1,0.1,0.02))
  }
  return(res)
}

#run MCMC algorithm function
run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))
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
startvalue = c(0.4,0.4,0.1)
chain = run_metropolis_MCMC(startvalue, 100000)

burnIn = 10000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))

### Summary: #######################

par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of var1", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]),col="blue")
abline(v = trueVar1, col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of var2", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]),col="blue")
abline(v = trueVar2, col="red" )
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of varC", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),3]),col="blue")
abline(v = trueVarC, col="red" )
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of var1", )
abline(h = trueVar1, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of var2", )
abline(h = trueVar2, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of varC", )
abline(h = trueVarC, col="red" )
par(mfrow = c(1,1))

# for comparison:
y <- success / size
fit <- summary(glm(y~x1+x2, family = "binomial", weights = size))
print(fit)
