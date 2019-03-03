library(MASS)
x0=mvrnorm(30,c(0,0),matrix(c(1,0,0,1),ncol = 2))
x1=mvrnorm(20,c(0,3),matrix(c(1,0,0,1),ncol = 2))
x1=as.data.frame(x1)
x1$index=1
x2=mvrnorm(20,c(0.1,0),matrix(c(1,0,0,1),ncol = 2))
x2=as.data.frame(x2)
x2$index=2
x3=mvrnorm(20,c(1,1),matrix(c(1,0,0,1),ncol = 2))
x3=as.data.frame(x3)
x3$index=3
x=rbind(x1,x2,x3)
index=x$index
x=x[sample(nrow(x),nrow(x),replace = FALSE),]
data=rbind(x0,as.matrix(x[,-ncol(x)]))
x=data
m=nrow(x)


