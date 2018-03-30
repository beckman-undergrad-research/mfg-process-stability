
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

# geerate 5 binomial samples
x1 <- rbinom(100, 50, 0.9)
x2 <- rbinom(50, 50, 0.7)
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
