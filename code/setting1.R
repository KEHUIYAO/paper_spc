## different machines with the same change-point
bayes.mod<-function(){
  ## likelihood
  
  
  for (i in 1:m){
    z[i]~dcat(pai[i,1:2])
  }
  
  for (i in 1:m){
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
  u[1,1]=0
  u[1,2]=0
  u[2,1]=0
  u[2,2]~dnorm(0,0.001)
  u[3,1]~dnorm(0,0.001)
  u[3,2]=0
  
  for (i in 1:2){
    alpha[i]=1/2
  }
  for (i in 1:m){
  pai[i,1:2]~ddirch(alpha)}
  
  for (i in 1:(m-1)){
    idxprob[i]=1/(m-1)
  }
  k~dcat(idxprob)
}

library(MASS)
x0=mvrnorm(30,c(0,0),matrix(c(1,0,0,1),ncol = 2))
x1=mvrnorm(20,c(1,0),matrix(c(1,0,0,1),ncol = 2))
x1=as.data.frame(x1)
x1$index=1
x2=mvrnorm(20,c(0,1),matrix(c(1,0,0,1),ncol = 2))
x2=as.data.frame(x2)
x2$index=2

x=rbind(x1,x2)
index=x$index
x=x[sample(nrow(x),nrow(x),replace = FALSE),]
data=rbind(x0,as.matrix(x[,-ncol(x)]))
x=data
m=nrow(x)
write.table(x,"data.csv",sep="\t",row.names = FALSE)

x=x[1:46,]
m=nrow(x)
sim.dat.jags <- list("x","m")
bayes.mod.params <- c("k","u","z")
#bayes.mod.params <- c("k")
library('R2jags')
library(MASS)





bayes.mod.fit <- jags(data = sim.dat.jags,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                      n.burnin = 1000,
                      model.file = bayes.mod)

print(bayes.mod.fit)

library(lattice)
bayes.mod.fit.mcmc <- as.mcmc(bayes.mod.fit)

#densityplot(bayes.mod.fit.mcmc, layout=c(2,2), aspect="fill")



