## parameters before the unknown change-point are known
bayes.mod<-function(){
  ## likelihood

  for (i in 1:m){
    x[i,]~dmnorm(ifelse(i<=k,u[1,],u[z+1,]),tau)
  }
  
  z~dcat(pai)
  
  ## prior

  tau[1,1]=1
  tau[1,2]=0
  tau[2,1]=0
  tau[2,2]=1
  u[1,1]=0
  u[1,2]=0
  u[2,1]~dnorm(0,0.001)
  u[2,2]=0
  u[3,1]=0
  u[3,2]~dnorm(0,0.001)
  u[4,1]~dnorm(0,0.001)
  u[4,2]~dnorm(0,0.001)
  for (i in 1:3){
    alpha[i]=1/3
  }
  pai~ddirch(alpha)
  for (i in 1:(m-1)){
    idxprob[i]=1/(m-1)
  }
  k~dcat(idxprob)
}

## parameters before change-point are estimated through the data
bayes.mod<-function(){
  ## likelihood
  
  for (i in 1:m){
    x[i,]~dmnorm(ifelse(i<=k,u[1,],u[z+1,]),ifelse(i<=k,tau,tau_after))
  }
  
  z~dcat(pai)
  
  ## prior
  tau[1,1]~dgamma(1/2,1/2)
  tau[1,2]=0
  tau[2,1]=tau[1,2]
  tau[2,2]~dgamma(1/2,1/2)
  tau_after=c*tau
  c~dunif(0,1)
  lambda~dunif(0,100)
  u[1,1]~dnorm(0,0.001)
  u[1,2]~dnorm(0,0.001)
  u[2,1]~dnorm(0,0.001)
  u[2,2]=0
  u[3,1]=0
  u[3,2]~dnorm(0,0.001)
  u[4,1]~dnorm(0,0.001)
  u[4,2]~dnorm(0,0.001)
  for (i in 1:3){
    alpha[i]=1/3
  }
  pai~ddirch(alpha)
  for (i in 1:(m-1)){
    idxprob[i]=1/(m-1)
  }
  k~dcat(idxprob)
}

## different machines with the same change-point
bayes.mod<-function(){
  ## likelihood
  
  for (i in 1:m){
    z[i]~dcat(pai)
    x[i,]~dmnorm(ifelse(i<=k,u[1,],u[z[i]+1,]),ifelse(i<=k,tau,tau_after))
  
  }
  
  
  
  ## prior
  tau[1,1]~dgamma(1/2,1/2)
  tau[1,2]=0
  tau[2,1]=tau[1,2]
  tau[2,2]~dgamma(1/2,1/2)
  tau_after=c*tau
  c~dunif(0,1)
  lambda~dunif(0,100)
  u[1,1]~dnorm(0,0.001)
  u[1,2]~dnorm(0,0.001)
  u[2,1]~dnorm(0,0.001)
  u[2,2]=0
  u[3,1]=0
  u[3,2]~dnorm(0,0.001)
  u[4,1]~dnorm(0,0.001)
  u[4,2]~dnorm(0,0.001)
  for (i in 1:3){
    alpha[i]=1/3
  }
  pai~ddirch(alpha)
  for (i in 1:(m-1)){
    idxprob[i]=1/(m-1)
  }
  k~dcat(idxprob)
}

















## different machines different change-points
bayes.mod<-function(){
  ## likelihood
  
  for (i in 1:m){
    z[i]~dcat(pai)
    x[i,]~dmnorm(ifelse(i<=k[z[i]],u[1,],u[z[i]+1,]),ifelse(i<=k[z[i]],tau,tau_after))
    
  }
  
  
  
  ## prior
  tau[1,1]~dgamma(1/2,1/2)
  tau[1,2]=0
  tau[2,1]=tau[1,2]
  tau[2,2]~dgamma(1/2,1/2)  
  tau_after=c*tau
  c~dunif(0,1)
  lambda~dunif(0,100)
  u[1,1]~dnorm(0,0.001)
  u[1,2]~dnorm(0,0.001)
  u[2,1]~dnorm(0,0.001)
  u[2,2]=0
  u[3,1]=0
  u[3,2]~dnorm(0,0.001)
  u[4,1]~dnorm(0,0.001)
  u[4,2]~dnorm(0,0.001)
  for (i in 1:3){
    alpha[i]=1/3
  }
  pai~ddirch(alpha)
  for (i in 1:(m-1)){
    idxprob[i]=1/(m-1)
  }
  for (i in 1:3){
    k[i]~dcat(idxprob)}
}








sim.dat.jags <- list("x","m")
bayes.mod.params <- c("k","u","z")

library('R2jags')
library(MASS)





bayes.mod.fit <- jags(data = sim.dat.jags,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                      n.burnin = 1000,
                      model.file = bayes.mod)

print(bayes.mod.fit)
bayes.mod.fit.mcmc <- as.mcmc(bayes.mod.fit)
library(lattice)
densityplot(bayes.mod.fit.mcmc, layout=c(2,2), aspect="fill")









