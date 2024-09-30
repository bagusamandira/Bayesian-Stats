data <- data("airquality")
Y <- (airquality$Temp)
head(Y)
mean(Y)

### Priors

mu0 <- 0
s20 <- 1000
a   <- 0.01
b   <- 0.01
n <- 100
keep.mu <- rep(0,n)
keep.s2 <- rep(0,n)

# Initial values
mu         <- mean(Y)
s2         <- var(Y)
keep.mu[1] <- mu
keep.s2[1] <- s2

for(iter in 2:n){
  
  # sample mu|s2,Y
  
  A  <- sum(Y)/s2+mu0/s20
  B  <- n/s2+1/s20
  mu <- rnorm(1,A/B,1/sqrt(B))
  
  # sample s2|mu,Y
  
  A  <- n/2+a
  B  <- sum((Y-mu)^2)/2+b
  s2 <- 1/rgamma(1,A,B)
  
  # keep track of the results
  keep.mu[iter] <- mu
  keep.s2[iter] <- s2
  
  plt.iter = 5
  # Plot the samples every [plt.iter] iterations
  if(iter%%plt.iter==0){
    par(mfrow=c(1,2))
    plot(keep.mu[1:iter],type="l",ylab="mu")
    plot(keep.s2[1:iter],type="l",ylab="s2")
  }
}

output <- matrix(0,2,4)
colnames(output)<-c("Mean","SD","Q025","Q975")
rownames(output)<-c("mu","sigma^2")

output[1,1]<-mean(keep.mu)
output[1,2]<-sd(keep.mu)
output[1,3]<-quantile(keep.mu,0.025)
output[1,4]<-quantile(keep.mu,0.975)

output[2,1]<-mean(keep.s2)
output[2,2]<-sd(keep.s2)
output[2,3]<-quantile(keep.s2,0.025)
output[2,4]<-quantile(keep.s2,0.975)

output