## Required packages
library("MASS")
library("fitdistrplus")

## Function for markov model distribution mathematical computation

mult_dist_beta <- function(x1, x2, sample.size=100){
  dist1 <- rbeta(sample.size,x1[1], x1[2]-x1[1])
  dist2 <- rbeta(sample.size,x2[1], x2[2]-x2[1])
  dist_res <- rep(0, length(dist1)*length(dist2))
  for (i in seq(1,length(dist1))){
    for (j in seq(1,length(dist2))){
      dist_res[(i-1)*length(dist2)+j] <- dist1[i] * dist2[j]
    }
  }
  dist_res <- fitdist(dist_res, "beta")
  y <- c(dist_res$estimate[[1]], (dist_res$estimate[[1]]+dist_res$estimate[[2]]))
  return(y)
}

add_dist_beta <- function(x1, x2, sample.size=100, distribution="beta"){
  dist1 <- rbeta(sample.size,x1[1], x1[2]-x1[1])
  dist2 <- rbeta(sample.size,x2[1], x2[2]-x2[1])
  dist_res <- rep(0, length(dist1)*length(dist2))
  for (i in seq(1,length(dist1))){
    for (j in seq(1,length(dist2))){
      tmp <- dist1[i] + dist2[j]
      # print(paste(((i-1)*length(dist2)+j),tmp,sep = ' '))
      dist_res[(i-1)*length(dist2)+j] <- ifelse(tmp>=1,1,tmp)
    }
  }
  dist_res <- fitdist(dist_res, distribution, "mle")
  y <- c(dist_res$estimate[[1]], (dist_res$estimate[[1]]+dist_res$estimate[[2]]))
  return(y)
}

sum_dist_beta <- function(xlist, sample.size=100, distribution="beta"){
  N <- length(xlist)
  dist_res <- xlist[[1]]
  for (i in seq(2,N)){
    dist_res <- add_dist_beta(dist_res, xlist[[i]])
  }
  return(dist_res)
}

merge_dist_beta <- function(xlist, sample.size=100){
  dist_res <- c()
  N <- length(xlist)
  for (i in seq(1,N)){
    distr <- rbeta(sample.size,xlist[[i]][1], xlist[[i]][2]-xlist[[i]][1])
    dist_res <- c(dist_res, distr)
  }
  dist_res <- fitdist(dist_res, "beta", "mle")
  y <- c(dist_res$estimate[[1]], (dist_res$estimate[[1]]+dist_res$estimate[[2]]))
  return(y)
}

plot_dist_beta <- function(x, sample.size=100){
  y <- rbeta(sample.size, x[1], x[2]-x[1])
  y <- fitdist(y, "beta", "mle")
  plot(y)
}

## Example
x1 <- c(11,54)
x2 <- c(23,44)

y <- mult_dist_beta(x1,x2)
plot_dist_beta(y)
print(y)

hist(rbeta(100,x1[1], x1[2]-x1[1]), seq(0,1,0.05))
hist(rbeta(100,x2[1], x2[2]-x2[1]), seq(0,1,0.05))
hist(rbeta(100,y[1], y[2]-y[1]), seq(0,1,0.05))

## Simple example for attribution model

init_A <- c(3,10)
init_B <- c(7,10)
A_B <- c(76,100)
B_A <- c(24,100)
A_end <- c(48,200)
B_end <- c(152,200)

init_A_end <- mult_dist_beta(init_A, A_end)
init_B_end <- mult_dist_beta(init_B, B_end)
init_A_B <- mult_dist_beta(init_A, A_B)
init_B_A <- mult_dist_beta(init_B, B_A)
init_A_B_end <- mult_dist_beta(init_A_B, B_end)
init_B_A_end <- mult_dist_beta(init_B_A, A_end)

# Calculate P(buy) using order-1
markov_1 <- sum_dist_beta(list(init_A_end, init_B_end))
markov_1 <- add_dist_beta(init_A_end, init_B_end)
print(markov_1)
plot_dist_beta(markov_1)

# Calculate P(buy) using order-2
# markov_2 <- sum_dist_beta(list(init_A_end,init_A_B_end, init_B_end, init_B_A_end))
markov_2 <- add_dist_beta(init_A_end, init_B_end)
markov_2 <- add_dist_beta(markov_2, init_A_B_end, "norm")
markov_2 <- add_dist_beta(markov_2, init_B_A_end)
print(markov_2)
plot_dist_beta(markov_2)


