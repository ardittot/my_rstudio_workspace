# Prepare the data
x1 <- c(-1,-1,1,1)
x2 <- c(-1,1,-1,1)
X <- cbind(x1,x2)
success <- c(234,811,278,922);
size <- c(1120,1032,987,1087)
Y <- cbind(success, size)

# For comparison
y <- success / size
fit <- summary(glm(y~x1+x2+x1:x2, family = "binomial", weights = size))
print(fit)
trueVarC  <- fit$coefficients[1]
trueVar1  <- fit$coefficients[2]
trueVar2  <- fit$coefficients[3]
trueVar12 <- fit$coefficients[4]

## Start MCMC
# You may edit this function as your need
calculateProposedY <- function(param, X){
  var1 <- param[1]
  var2 <- param[2]
  var12 <- param[3]
  varC <- param[4]
  x1 <- X[,1]
  x2 <- X[,2]
  
  y <- (var1 * x1) + (var2 * x2) + (var12 * x1 * x2) + varC
  y <- 1 / (1+exp(-y))
  #y <- ifelse(y>1, 1, y)
  return(y)
}

# You may edit this function as your need
calculateLikelihood <- function(param, X, Y_data, use_log){
  success <- Y_data[,1]
  size <- Y_data[,2]
  Y_proposed <- calculateProposedY(param, X)
  likelihood <- dbinom(x = success, size = size, prob = Y_proposed, log = use_log)
  return(likelihood)
}

likelihood <- function(param, X, Y_data, use_log){
  ll <- calculateLikelihood(param, X, Y_data, use_log)
  res <- ifelse(use_log, sum(ll), prod(ll))
  return(res)
}

# Prior distribution; you may edit this function as your need
prior <- function(param, use_log, limit=NULL){
  pr <- rep(0, length(param))
  pr[1] <- dunif(param[1], min=limit[1], max=limit[2], log = use_log) # prior of x1
  pr[2] <- dunif(param[2], min=limit[3], max=limit[4], log = use_log) # prior of x2
  pr[3] <- dunif(param[3], min=limit[5], max=limit[6], log = use_log) # prior of x1:x2
  pr[4] <- dunif(param[4], min=limit[7], max=limit[8], log = use_log) # prior of Intercept
  res <- ifelse(use_log, sum(pr), prod(pr))
  return(res)
}

posterior <- function(param, X, Y_data, use_log, limit){
  if (use_log) {
    res <- likelihood(param, X, Y_data, use_log) + prior(param, use_log, limit)
  } else {
    res <- likelihood(param, X, Y_data, use_log) * prior(param, use_log, limit)
  }
  return(res)
}

# You may edit this function as your need
proposal_param_orig <- function(param){
  numVar <- length(param)
  res <- rnorm(numVar,mean = param, sd= c(0.15,0.15,0.15,0.15))
  #res <- rnorm(4,mean = param, sd= c(0.3,0.3,0.3,0.3))
  #res <- c(0,0,0,0)
  #res[1:3] <- runif(3,-3,3)
  #res[4] <- runif(1,0,1)
  return(res)
}

proposal_param_limit <- function(param, proposal, limit){
  flag <- FALSE
  res <- proposal
  for (i in seq(1,(length(limit)/2),2)) {
    i_min <- 2*i - 1; i_max <- 2*i
    flag <- flag | (proposal[i] < limit[i_min]) | (proposal[i] > limit[i_max])
  }
  while (flag) {
    res <- proposal_param_orig(param)
    flag <- FALSE
    for (i in seq(1,(length(limit)/2),2)) {
      i_min <- 2*i - 1; i_max <- 2*i
      flag <- flag | (res[i] < limit[i_min]) | (res[i] > limit[i_max])
    }
  }
  return(res)
}

# You may turn on/off the 'proposalfunction_limit' function
proposalParameter <- function(param, limit){
  res <- proposal_param_orig(param)
  res <- proposal_param_limit(param, res, limit)
  return(res)
}

calculateSamplingProbability <- function(X, Y, param_old, param_new, use_log){
  posterior1 <- posterior(param_old, X, Y, use_log, limit)
  posterior2 <- posterior(param_new, X, Y, use_log, limit)
  if (use_log) {
    res <- exp(posterior2 - posterior1)
  } else {
    res <- posterior2 / posterior1
  }
  return(res)
}

# You may edit this function as your need. This one is using Metropolis-Hastings algorithm
samplingNextParam <- function(prob, param_old, param_new){
  probab <- ifelse(prob > 1, 1, prob)
  flag <- rbinom(1,1,probab)
  if (flag==0) {
    res <- param_old
  } else {
    res <- param_new
  }
  return(res)
}

# Run MCMC algorithm function
run_MCMC <- function(startvalue, limit, iterations, use_log, X, Y){
  numVar <- length(startvalue)
  chain <- array(dim = c(iterations+1,numVar))
  chain[1,] <- startvalue
  for (i in 1:iterations){
    #print(paste("Iteration-",i,sep = ""))
    param1 <- chain[i,]
    param2 <- proposalParameter(chain[i,], limit)
    probab <- calculateSamplingProbability(X, Y, param1, param2, use_log)
    param_next <- samplingNextParam(probab, param1, param2)
    chain[i+1,] <- param_next
  }
  return(chain)
}

# Example of how to use this script
startvalue = c(1,1,1,0.4)
var_limit <- c(
  c(-3,3),
  c(-3,3),
  c(-3,3),
  c(0,1)
)
num_iter <- 200000
use_log <- TRUE

trace = run_metropolis_MCMC(startvalue, var_limit, num_iter, use_log, X, Y)

chain <- trace
burnIn = 10000
idx <- seq(1,burnIn)
chain <- chain[-idx,]
idx <- seq(from = 1, to = nrow(chain), by = 10)
chain <- chain[idx,]
acceptance = 1-mean(duplicated(chain))
print(acceptance)
# idx <- which(!(duplicated(chain[1:nrow(chain),])))
# chain <- chain[idx,]
print(nrow(chain))

### Summary: #######################

par(mfrow = c(2,4))
hist(chain[,4],nclass=40, main="Posterior of var(Intercept)", xlab="True value = red line")
abline(v = mean(chain[,4]),col="blue")
abline(v = trueVarC, col="red" )
hist(chain[,1],nclass=40, , main="Posterior of var(x1)", xlab="True value = red line" )
abline(v = mean(chain[,1]),col="blue")
abline(v = trueVar1, col="red" )
hist(chain[,2],nclass=40, main="Posterior of var(x2)", xlab="True value = red line")
abline(v = mean(chain[,2]),col="blue")
abline(v = trueVar2, col="red" )
hist(chain[,3],nclass=40, main="Posterior of var(x1:x2)", xlab="True value = red line")
abline(v = mean(chain[,3]),col="blue")
abline(v = trueVar12, col="red" )

plot(chain[,4], type = "l", xlab="True value = red line" , main = "Trace values of var(Intercept)", )
abline(h = trueVarC, col="red" )
plot(chain[,1], type = "l", xlab="True value = red line" , main = "Trace values of var(x1)", )
abline(h = trueVar1, col="red" )
plot(chain[,2], type = "l", xlab="True value = red line" , main = "Trace values of var(x2)", )
abline(h = trueVar2, col="red" )
plot(chain[,3], type = "l", xlab="True value = red line" , main = "Trace values of var(x1:x2)", )
abline(h = trueVar12, col="red" )

par(mfrow = c(1,1))
