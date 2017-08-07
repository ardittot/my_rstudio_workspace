N <- 1000
x <- rnorm(N, 11, 3)

write.table(x,
            file = 'example1.data',
            row.names = FALSE,
            col.names = FALSE)
hist(x,20)

jags_cmd <- "
model {
  for (i in 1:N) {
x[i] ~ dnorm(mu, tau)
}
mu ~ dnorm(0, .0001)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)
}
"
jags_file <- textConnection(jags_cmd)

jags <- jags.model(jags_file, data = list('x' = x, 'N' = N), n.chains = 4, n.adapt = 100)
# chain <- jags.samples(jags, variable.names = c('mu', 'tau'), n.iter = 1000)
chain <- coda.samples(jags, variable.names = c('mu', 'tau'), n.iter = 1000, thin = 5)
summary(chain)

plot(chain[[1]][,c(1,2)])

