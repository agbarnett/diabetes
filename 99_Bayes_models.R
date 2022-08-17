# 99_Bayes_models.R
# write the code for the Bayesian models
# August 2022

# version with no random intercept
code_no_intercept <- nimbleCode({
  for (i in 1:N) {
    outcome[i] ~ dnorm(mean = mu[i], tau)
    mu[i] <- inprod(X[i,1:P], alpha[1:P])
  }
  # intercept for each database
  for(j in 1:P){
    alpha[j] ~ dnorm(0, sd = 10000)
  }
  tau ~ dgamma(0.1, 0.1) # overall precision
  sd <- 1/sqrt(tau)
})
# version with random intercept
code <- nimbleCode({
  for (i in 1:N) {
    outcome[i] ~ dnorm(mean = mu[i], tau)
    mu[i] <- inprod(X[i,1:P], alpha[1:P]) + r_int[mother[i]]
  }
  # intercept for each database
  for(j in 1:P){
    alpha[j] ~ dnorm(0, sd = 10000)
  }
  tau ~ dgamma(0.1, 0.1) # overall precision
  sd <- 1/sqrt(tau)
  # random intercept for each mother - no need to centre
  for(j in 1:M){
    r_int[j] ~ dnorm(0, tau.mother)
  }
  tau.mother ~ dgamma(0.1, 0.1)
  sd.mother <- 1/sqrt(tau.mother)
})

