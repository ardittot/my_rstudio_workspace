# Prepare the data
x1 <- c(-1,-1,1,1);x2 <- c(-1,1,-1,1);
success <- c(234,811,278,922);
size <- c(1120,1032,987,1087)
# For comparison
y <- success / size
fit <- summary(glm(y~x1+x2+x1:x2, family = "binomial", weights = size))
print(fit)
trueVarC  <- fit$coefficients[1]
trueVar1  <- fit$coefficients[2]
trueVar2  <- fit$coefficients[3]
trueVar12 <- fit$coefficients[4]
  
# Start MCMC
likelihood <- function(param){
  var1 = param[1]
  var2 = param[2]
  var12 = param[3]
  varC = param[4]
  
  pred <- (var1 * x1) + (var2 * x2) + (var12 * x1 * x2) + varC
  #pred <- ifelse(pred>1, 0.999, pred)
  #pred <- ifelse(pred<(-1), -0.999, pred)
  #pred <- (pred - (-1)) / (1-(-1))
  pred <- 1 / (1+exp(-pred))
  #print("pred"); print(pred)
  singlelikelihoods = dbinom(x = success, size = size, prob = pred, log = TRUE)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

# Prior distribution
prior <- function(param){
  var1 = param[1]
  var2 = param[2]
  var12 = param[3]
  varC = param[4]
  var1prior = dunif(var1, min=-3, max=3, log = T)
  var2prior = dunif(var2, min=-3, max=3, log = T)
  var12prior = dunif(var12, min=-3, max=3, log = T)
  varCprior = dunif(varC, min=0, max=1, log = T)
  return(var1prior+var2prior+var12prior+varCprior)
}

#define posterior function
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

proposalfunction_orig <- function(param){
  #res <- rnorm(4,mean = param, sd= c(0.15,0.15,0.15,0.15))
  res <- rnorm(4,mean = param, sd= c(0.3,0.3,0.3,0.3))
  #res <- c(0,0,0,0)
  #res[1:3] <- runif(3,-3,3)
  #res[4] <- runif(1,0,1)
  return(res)
}

#define proposal function
proposalfunction <- function(param){
  res <- proposalfunction_orig(param)
  var1_min <- -3; var1_max <- 3
  var2_min <- -3; var2_max <- 3
  var12_min <- -3; var12_max <- 3
  varC_min <- 0; varC_max <- 1
  res <- proposalfunction_orig(param)
  while ((res[1]<var1_min) | (res[1]>var1_max) | (res[2]<var2_min) | (res[2]>var2_max) | (res[3]<var12_min) | (res[3]>var12_max) | (res[4]<varC_min) | (res[4]>varC_max)) {
    res <- proposalfunction_orig(param)
  }
  return(res)
}

#run MCMC algorithm function
run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,4))
  chain[1,] = startvalue
  for (i in 1:iterations){
    #print(paste("Iteration-",i,sep = ""))
    proposal <- proposalfunction(chain[i,])
    probab <- exp(posterior(proposal) - posterior(chain[i,]))
    probab <- ifelse(probab > 1, 1, probab)
    flag <- rbinom(1,1,probab)
    if (flag==0) {
      chain[i+1,] <- chain[i,]
    } else {
      chain[i+1,] <- proposal
    }
  }
  return(chain)
}

#run MCMC algorithm
startvalue = c(1,1,1,0.4)
trace = run_metropolis_MCMC(startvalue, 1000000)

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
