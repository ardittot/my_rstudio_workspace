plot_mcmc <- function(chain) {
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
}

prev <- getwd()
setwd("~/workspace/stats_class/bayesian/plot_mcmc/")
N <- nrow(chain)
for (i in seq(2,N)) {
  tmp <- chain[seq(1,i),]
  filename <- paste("plot_mcmc_",i,".png",sep="")
  png(filename=filename)
  plot_mcmc(chain = tmp)
  dev.off()
}
setwd(prev)

# hist(x = chain[seq(1,10),2], breaks = seq(-3,3,length=90))
