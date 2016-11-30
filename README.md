echo "# FinEngineering" >> README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/Gianmarcodesimone/FinEngineering.git
git push -u origin master

#PSet 6 Gianmarco de Simone
#Q1

#Data
rf <- 0.03
W0 <- 1
S0 <- 1
Nsims <- 100000
r <- 0.08
p <- 0.5
Sigma_u <- 0.4
Sigma_e <- 0.01
T <- 10
xo <- 0

#simulating stock returns
simulated_r <- matrix(NA, nrow = T + 1, ncol = Nsims)
simulated_x <- matrix(NA, nrow = T + 1, ncol = Nsims)
simulated_x[1, ] <- xo
simulated_r[1, ] <- xo
for(i in 1:Nsims){
  currentx <- simulated_x[, i]
  currentr <- simulated_r[, i]
  z1 <- rnorm(T, 0, Sigma_e)
  z2 <- rnorm(T, 0, Sigma_u)
  for(j in 1:T)
  {
    currentx[j + 1] <- p*currentx[j] + z1[j]
    currentr[j + 1] <- r + currentx[j] + z2[j]
  }
  simulated_x[, i] <- currentx
  simulated_r[, i] <- currentr
}
hist(simulated_r[nrow(simulated_r), ], breaks = 100, main = "Histogram of simulated returns", xlab = "Returns")

#distribution of holding period return
simulated_S <- matrix(NA, T + 1, Nsims)
for(i in 1:Nsims){
  simulated_S[, i] <- cumprod(exp(simulated_r[, i]))
}
simulated_r <- simulated_r[-1,]
simulated_x <- simulated_x[-1,]
gross_r <- exp(simulated_r)

#Part a
incr_a <- matrix(NA, 1, 101)
incr_a[,1] <- 0
for (i in 2:101){
  incr_a[,i] <- incr_a[,i-1]+0.01
}
bond_P <- matrix(NA, T, 1)
for (i in 1:T){
  bond_P[i,] <- exp(rf*(i))
}
Utility <- matrix(NA, 101,1)
Wealth <- matrix(NA,1,Nsims)
for (i in 1:101){
  for (a in 1:Nsims){
    Wealth[1,a] <- incr_a[,i]*simulated_S[T,a]+(1-incr_a[,i])*bond_P[T,]
  }
  Utility[i,] <- mean(log(Wealth[1,]))
}
a_opt <- (which.max(Utility)-1)/100
plot(seq(0,1,0.01), Utility, main = "Utility as function of a", xlab="a",type="l")

#Part b
incr_b <- matrix(NA, 1, 1001)
incr_b[,1] <- 0
for (i in 2:1001){
  incr_b[,i] <- incr_b[,i-1]+0.01
}
weight_b <- matrix(NA, T, Nsims)
Utility_b <- matrix(NA, 1001, 1)
Wealth_b <- matrix(NA, T+1, Nsims)
Wealth_b[1,] <- 1
for (i in 1:1001){
    for (c in 1:T){
      weight_b[c,] <- incr_b[,i]*simulated_x[c,]+a_opt
      weight_b[weight_b>1] <- 1
      weight_b[weight_b<0] <- 0
      Wealth_b[c+1,]<- Wealth_b[c,]*(weight_b[c,]*gross_r[c,]+(1-weight_b[c,])*exp(0.03))
    }
  Utility_b[i,] <- mean(log(Wealth_b[nrow(Wealth_b),]))
  print(i)
}
b_opt <- (which.max(Utility_b)-1)/100
plot(seq(0,10,0.01), Utility_b, main = "Utility as function of b", xlab="b",type="l")
