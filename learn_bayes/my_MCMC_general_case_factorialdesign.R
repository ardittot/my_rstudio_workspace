## MCMC Package
# You may edit this function as your need
calculateProposedY <- function(param, X){
  varC <- param[1]
  var1 <- param[2]
  var2 <- param[3]
  var3 <- param[4]
  var4 <- param[5]
  var12 <- param[6]
  var13 <- param[7]
  var14 <- param[8]
  var23 <- param[9]
  var24 <- param[10]
  var34 <- param[11]
  x1 <- X[,1]
  x2 <- X[,2]
  x3 <- X[,3]
  x4 <- X[,4]
  
  y <- (var1 * x1) + (var2 * x2) + (var12 * x1 * x2) + varC
  y <- varC + (var1 * x1) + (var2 * x2) + (var3 * x3) + (var4 * x4) + (var12 * x1 * x2) + (var13 * x1 * x3) + (var14 * x1 * x4) + (var23 * x2 * x3) + (var24 * x2 * x4) + (var34 * x3 * x4)
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
  pr[1] <- dunif(param[1], min=limit[1], max=limit[2], log = use_log) # prior of Intercept
  pr[2] <- dunif(param[2], min=limit[3], max=limit[4], log = use_log) # prior of main_copy
  pr[3] <- dunif(param[3], min=limit[5], max=limit[6], log = use_log) # prior of visual_keluarga
  pr[4] <- dunif(param[4], min=limit[7], max=limit[8], log = use_log) # prior of visual_kamar_hotel
  pr[4] <- dunif(param[5], min=limit[9], max=limit[10], log = use_log) # prior of periode_sk
  pr[4] <- dunif(param[6], min=limit[11], max=limit[12], log = use_log) # prior of main_copy:visual_keluarga
  pr[4] <- dunif(param[7], min=limit[13], max=limit[14], log = use_log) # prior of main_copy:visual_kamar_hotel
  pr[4] <- dunif(param[8], min=limit[15], max=limit[16], log = use_log) # prior of main_copy:periode_sk
  pr[4] <- dunif(param[9], min=limit[17], max=limit[18], log = use_log) # prior of visual_keluarga:visual_kamar_hotel
  pr[4] <- dunif(param[10], min=limit[19], max=limit[20], log = use_log) # prior of visual_keluarga:periode_sk
  pr[4] <- dunif(param[11], min=limit[21], max=limit[22], log = use_log) # prior of visual_keluarga:periode_sk
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
  res <- rnorm(numVar,mean = param, sd= rep(0.005,11))
  #res <- c(0,0,0,0)
  #res[1:3] <- runif(3,-3,3)
  #res[4] <- runif(1,0,1)
  return(res)
}

proposal_param_limit <- function(param, proposal, limit){
  flag <- FALSE
  res <- proposal
  for (i in seq(1,length(param))) {
    i_min <- 2*i - 1; i_max <- 2*i
    flag <- flag | (res[i] < limit[i_min]) | (res[i] > limit[i_max])
  }
  while (flag) {
    res <- proposal_param_orig(param)
    flag <- FALSE
    for (i in seq(1,length(param))) {
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

calculateSamplingProbability <- function(X, Y, param_old, param_new, use_log, limit){
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
  flag <- rbinom(n = 1, size = 1, prob = probab)
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
  chain[1,] = startvalue
  print("Running MCMC Sampling")
  pb = txtProgressBar(min = 0, max = iterations, initial = 0)
  for (i in 1:iterations){
    setTxtProgressBar(pb, i)
    param1 <- chain[i,]
    param2 <- proposalParameter(chain[i,], limit)
    probab <- calculateSamplingProbability(X, Y, param1, param2, use_log, limit)
    param_next <- samplingNextParam(probab, param1, param2)
    chain[i+1,] <- param_next
  }
  return(chain)
}

clean_MCMC <- function(burnIn, numStep, removeDuplicate){
  # Fix initial chain values bias
  chain <- trace
  idx <- seq(1,burnIn)
  chain <- chain[-idx,]
  if (ncol(trace)==1) {
    chain <- matrix(chain, ncol = 1)
  }
  # Fix auto-corr issue from MCMC chain values
  idx <- seq(from = 1, to = nrow(chain), by = numStep)
  chain <- chain[idx,]
  if (ncol(trace)==1) {
    chain <- matrix(chain, ncol = 1)
  }
  # Choose if you want to remove duplicated chain values
  if (removeDuplicate) {
    idx <- which(!(duplicated(chain[1:nrow(chain),])))
    chain <- chain[idx,]
  }
  if (ncol(trace)==1) {
    chain <- matrix(chain, ncol = 1)
  }
  return(chain)
}

plot_MCMC <- function(chain, trueVar, varName=NULL){
  numVar <- ncol(chain)
  par(mfrow = c(2,numVar))
  if (length(varName)==0) {
    var_names <- seq(1,numVar)
  } else {
    var_names <- varName
  }
  for (i in seq(numVar)) {
    title_name <- paste(c("Posterior of var:", var_names[i]), sep=" ")
    hist(chain[,i],nclass=40, main=title_name, xlab="True value = red line")
    abline(v = mean(chain[,i]),col="blue")
    abline(v = trueVar[i], col="red" ) 
  }
  for (i in seq(numVar)) {
    title_name <- paste(c("Trace values of var:", var_names[i]), sep=" ")
    plot(chain[,i], type = "l", xlab="True value = red line" , main = title_name)
    abline(h = trueVar[i], col="red" ) 
  }
  par(mfrow = c(1,1))
}

## Example of how to use this script
# Prepare the data
df <- read.csv("~/Downloads/abtest_promoBanner_1702_summary.csv")
size <- rep(23000, nrow(df))
success <- round(size * df$rate)
# For comparison
y <- success / size
fit <- summary(glm(y~(.)^2, family = "binomial", weights = size, data = X))
print(fit)

# Set the MCMC and data parameters
startvalue = rep(0,11)
var_limit <- rep(c(-10,10),11)
num_iter <- 200000
use_log <- TRUE
X <- df[,seq(2,5)]
Y <- cbind(success, size)
trueVar <- fit$coefficients

# Run the functions
trace <- run_MCMC(startvalue, var_limit, num_iter, use_log, X, Y)
chain <- clean_MCMC(burnIn = 10000, numStep = 10, removeDuplicate = FALSE)
# plot_MCMC(chain, trueVar, varName = rownames(fit$coefficients))
plot_MCMC(chain[,seq(1,4)], trueVar[seq(1,4)], varName = rownames(fit$coefficients)[seq(1,4)])
plot_MCMC(chain[,seq(5,8)], trueVar[seq(5,8)], varName = rownames(fit$coefficients)[seq(5,8)])
plot_MCMC(chain[,seq(9,11)], trueVar[seq(9,11)], varName = rownames(fit$coefficients)[seq(9,11)])
