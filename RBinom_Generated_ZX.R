
# generates data for random process with a sustained change
x <- c(rbinom(100, 50, 0.9), rbinom(50, 50, 0.7), rbinom(100, 50, 0.9))
N <- length(x)
x.iter <- c(NA, x[1:(N-1)])
x.MR <- abs(x-x.iter)
x.AMR <- mean(x.MR, na.rm = TRUE)
x.mean <- mean(x)
x.sd <- sd(x)
UCL1 <- x.mean + 3*(x.AMR/1.128)
LCL1 <- x.mean - 3*(x.AMR/1.128)

# make the plot
plot(x, ylim = c(28, 51))
lines(x)
abline(h = x.mean, col = "blue")
abline(h= UCL1, col = "red")
abline(h = LCL1, col = "red")

### Coordinates for the stability chart

# average daily yield
mean(x)

# cpk / ppk (stability ratio)
x.sd / (x.AMR/1.128)