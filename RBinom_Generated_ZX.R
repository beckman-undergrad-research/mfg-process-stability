
# generates data for random process with a sustained change
x <- c(rbinom(100, 50, 0.9), rbinom(50, 50, 0.7), rbinom(100, 50, 0.9), rbinom(150, 50, 0.8))
N <- length(x)
x.iter <- c(NA, x[1:(N-1)])
x.MR <- abs(x-x.iter)
x.AMR <- mean(x.MR, na.rm = TRUE)
x.mean <- mean(x)
x.sd <- sd(x)
UCL1 <- x.mean + 3*(x.AMR/1.128)
LCL1 <- x.mean - 3*(x.AMR/1.128)

# make the plot
plot(x, ylim = c(UCL1 + 2, LCL1 - 2))
lines(x)
abline(h = x.mean, col = "blue")
abline(h= UCL1, col = "red")
abline(h = LCL1, col = "red")

### Coordinates for the stability chart

# average daily yield
mean(x)

# cpk / ppk (stability ratio)
x.sd / (x.AMR/1.128)

#################################################################

# repeat code below NREP times
set.seed(100)
NREP <- 10
N <- 100
n <- 50
p <- 0.9
FBniom <- matrix(NA, NREP, N)
BMean <- rep(NA, NREP)
BSd <- rep(NA, NREP)
for (i in 1:NREP) {
  x <- rbinom(N,n,p)
  FBniom[i,1:N] <- x
  BMean[i] <- mean(x)
  BSd[i] <- sd(x)
}

# Verify each row is the generated raw data
BMean
FBniom
BSd
mean(FBniom[1,])

# Generate iteration matrix
x.iter <- matrix(NA,NREP, N)
for (i in 1:NREP){
  x.iter[i, 1:N] <- c(NA, FBniom[i,1:(N-1)])
}
x.iter

# x.iter[1:NREP, 1:N] <- c(NA, FBniom[1:NREP,1:(N-1)])
#for (i in 1:NREP){
#  x.MR <- abs(FBniom[1:NREP,1:N] - x.iter[1:NREP,1:N])
#  
#}

# Why the code above not working?

# Generate x moving range
x.MR <- abs(FBniom - x.iter)
x.MR

# x.AMR <- c(tapply(x.MR[i, 1:N], mean)) 
# is the above tapply right?

# Calculate average moving range
x.AMR <- matrix(NA, NREP, 1)
for (i in 1:NREP){
  x.AMR[i,] <- mean(x.MR[i,], na.rm = TRUE)
}
x.AMR
# mean(x.MR[,2], na.rm = TRUE)
# x.MR
# (4+2+2+3+1+1+3+0+1+1)/10
# The comment above is we verified the equation is correct

# Calcualte control limit
UCL <- BMean + 3*(x.AMR/1.128)
LCL <- BMean - 3*(x.AMR/1.128)

# Calcualte stability ratio and convert yeild to proportion
x.meanrat <- BMean/n
x.meanrat
x.rat <- BSd/(x.AMR/1.128)
x.rat <- t(x.rat)
x.rat

# add a unstable process
x1 <- c(rbinom(100, 50, 0.6), rbinom(50, 45, 0.9))
x2 <- c(rbinom(70, 50, 0.7), rbinom(50, 45, 0.4))

# Calcualte everything for x1
N1 <- length(x1)
x1.iter <- c(NA, x1[1:(N1-1)])
x1.MR <- abs(x1-x1.iter)
x1.AMR <- mean(x1.MR, na.rm = TRUE)
x1.mean <- mean(x1)
x1.sd <- sd(x1)
UCL1 <- x1.mean + 3*(x1.AMR/1.128)
LCL1 <- x1.mean - 3*(x1.AMR/1.128)

# Calcualte everything for x2
N2 <- length(x2)
x2.iter <- c(NA, x2[1:(N2-1)])
x2.MR <- abs(x2-x2.iter)
x2.AMR <- mean(x2.MR, na.rm = TRUE)
x2.mean <- mean(x2)
x2.sd <- sd(x2)
UCL2 <- x2.mean + 3*(x2.AMR/1.128)
LCL2 <- x2.mean - 3*(x2.AMR/1.128)

# Calculate ratio for unstable
x1rat <- x1.sd / (x1.AMR/1.128)
x2rat <- x2.sd / (x2.AMR/1.128)
x1.meanrat <- x1.mean/150
x2.meanrat <- x2.mean/120

# plot the graph
plot(x = c(x.rat,x1rat,x2rat) , y= c(x.meanrat,x1.meanrat,
                                     x2.meanrat), main =
       "Yeild versus Stability", ylab = "Average Yeild", 
     xlab = "Stability Ratio")

plot(FBniom[1,1:N], ylim = c(UCL[1] + 2, LCL[1] - 2))
lines(FBniom[1,1:N])
abline(h = BMean[1], col = "blue")
abline(h= UCL[1], col = "red")
abline(h = LCL[1], col = "red")

#################################################################

# geerate 5 binomial samples
x1 <- rbinom(100, 50, 0.9)
x2 <- c(rbinom(100, 50, 0.6), rbinom(50, 45, 0.9))
x3 <- rbinom(100,50 , 0.9)
x4 <- rbinom(150, 50, 0.8)
x5 <- rbinom(100, 50, 0.8)

# for x1 binom sample
N1 <- length(x1)
x1.iter <- c(NA, x1[1:(N1-1)])
x1.MR <- abs(x1-x1.iter)
x1.AMR <- mean(x1.MR, na.rm = TRUE)
x1.mean <- mean(x1)
x1.sd <- sd(x1)
UCL1 <- x1.mean + 3*(x1.AMR/1.128)
LCL1 <- x1.mean - 3*(x1.AMR/1.128)

# for x2 binom sample
N2 <- length(x2)
x2.iter <- c(NA, x2[1:(N2-1)])
x2.MR <- abs(x2-x2.iter)
x2.AMR <- mean(x2.MR, na.rm = TRUE)
x2.mean <- mean(x2)
x2.sd <- sd(x2)
UCL2 <- x2.mean + 3*(x2.AMR/1.128)
LCL2 <- x2.mean - 3*(x2.AMR/1.128)

# for x3 binom sample
N3 <- length(x3)
x3.iter <- c(NA, x3[1:(N3-1)])
x3.MR <- abs(x3-x3.iter)
x3.AMR <- mean(x3.MR, na.rm = TRUE)
x3.mean <- mean(x3)
x3.sd <- sd(x3)
UCL3 <- x3.mean + 3*(x3.AMR/1.128)
LCL3 <- x3.mean - 3*(x3.AMR/1.128)

# for x4 binom sample
N4 <- length(x4)
x4.iter <- c(NA, x4[1:(N4-1)])
x4.MR <- abs(x4-x4.iter)
x4.AMR <- mean(x4.MR, na.rm = TRUE)
x4.mean <- mean(x4)
x4.sd <- sd(x4)
UCL4 <- x4.mean + 3*(x4.AMR/1.128)
LCL4 <- x4.mean - 3*(x4.AMR/1.128)

# for x5 binom sample
N5 <- length(x5)
x5.iter <- c(NA, x5[1:(N5-1)])
x5.MR <- abs(x5-x5.iter)
x5.AMR <- mean(x5.MR, na.rm = TRUE)
x5.mean <- mean(x5)
x5.sd <- sd(x5)
UCL5 <- x5.mean + 3*(x5.AMR/1.128)
LCL5 <- x5.mean - 3*(x5.AMR/1.128)

# calculate the stability ration
x1rat <- x1.sd / (x1.AMR/1.128)
x2rat <- x2.sd / (x2.AMR/1.128)
x3rat <- x3.sd / (x3.AMR/1.128)
x4rat <- x4.sd / (x4.AMR/1.128)
x5rat <- x5.sd / (x5.AMR/1.128)

# plot the field versus stability plot
plot(x = c(x1rat,x2rat, x3rat, x4rat, x5rat), y= c(x1.mean, x2.mean, x3.mean, 
                                                   x4.mean, x5.mean), main =
       "Yeild versus Stability", ylab = "Average Yeild", xlab = "Stability Ratio")
## how to get the regression line?


## Notes: 
# 
# - Avg Yield as a proportion (scale 0 to 1 on y axis)
# - Add one or two unstable processes; can just be a mixture of two binomials; samp <- c(rbinom(35, 50, 0.7), rbinom(20, 45, 0.9))
